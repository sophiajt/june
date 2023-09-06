use crate::{
    compiler::{CallTarget, Compiler},
    errors::SourceError,
    parser::{AstNode, BlockId, NodeId},
    typechecker::{FunId, VarId},
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AllocationLifetime {
    Caller,
    Param { var_id: VarId },
    Scope { level: usize },
    Unknown,
}

pub struct LifetimeChecker {
    compiler: Compiler,
    current_blocks: Vec<BlockId>,
    possible_allocation_sites: Vec<(Vec<BlockId>, usize, NodeId)>,
}

impl LifetimeChecker {
    pub fn new(compiler: Compiler) -> Self {
        Self {
            compiler,
            current_blocks: vec![],
            possible_allocation_sites: vec![],
        }
    }

    pub fn check_block_lifetime(&mut self, block_id: BlockId, scope_level: usize) {
        self.current_blocks.push(block_id);

        // FIXME: remove clone
        let block = self.compiler.blocks[block_id.0].clone();

        for node_id in block.nodes.iter().rev() {
            self.check_node_lifetime(*node_id, scope_level);
        }

        self.current_blocks.pop();
    }

    pub fn error(&mut self, message: impl Into<String>, node_id: NodeId) {
        self.compiler.errors.push(SourceError {
            message: message.into(),

            node_id,
        });
    }

    pub fn expand_lifetime_with_node(&mut self, node_id: NodeId, lifetime_from_node: NodeId) {
        let lifetime = self.compiler.node_lifetimes[lifetime_from_node.0];

        self.expand_lifetime(node_id, lifetime_from_node, lifetime)
    }

    pub fn expand_lifetime(
        &mut self,
        node_id: NodeId,
        lifetime_from_node: NodeId,
        lifetime: AllocationLifetime,
    ) {
        let current_lifetime = self.compiler.node_lifetimes[node_id.0];

        match current_lifetime {
            AllocationLifetime::Unknown => {
                self.compiler.node_lifetimes[node_id.0] = lifetime;
            }
            AllocationLifetime::Param { var_id } => {
                match lifetime {
                    AllocationLifetime::Param {
                        var_id: incoming_var_id,
                    } => {
                        let param_name1 =
                            String::from_utf8_lossy(self.compiler.get_variable_name(var_id));
                        let param_name2 = String::from_utf8_lossy(
                            self.compiler.get_variable_name(incoming_var_id),
                        );
                        if incoming_var_id != var_id {
                            self.error(format!("can't find compatible lifetime between param '{}' and param '{}'", param_name1, param_name2), node_id)
                        }
                    }
                    AllocationLifetime::Scope { .. } => {
                        // Params outlive all scopes
                    }
                    AllocationLifetime::Caller => {
                        let param_name1 =
                            String::from_utf8_lossy(self.compiler.get_variable_name(var_id));
                        self.error(
                            format!(
                                "can't find compatible lifetime between param '{}' and caller",
                                param_name1
                            ),
                            node_id,
                        )
                    }
                    _ => {
                        let param_name =
                            String::from_utf8_lossy(self.compiler.get_variable_name(var_id));
                        self.error(
                            format!("can't find compatible lifetime for param '{}'", param_name),
                            lifetime_from_node,
                        );
                    }
                }
            }
            AllocationLifetime::Caller => {
                // TODO: add fix to check for raw and custom lifetimes

                match lifetime {
                    AllocationLifetime::Param { var_id } => {
                        let param_name =
                            String::from_utf8_lossy(self.compiler.get_variable_name(var_id));

                        self.error(
                            format!("can't find compatible lifetime for param '{}'", param_name),
                            lifetime_from_node,
                        );
                    }
                    AllocationLifetime::Scope { .. } => {
                        // Caller is larger than scope, so ignore
                    }
                    AllocationLifetime::Caller => {
                        // Already caller
                    }
                    _ => {
                        self.error(
                            format!(
                                "can't find compatible lifetime for caller, found {:?}",
                                lifetime
                            ),
                            lifetime_from_node,
                        );
                    }
                }
            }
            AllocationLifetime::Scope {
                level: current_level,
            } => match lifetime {
                AllocationLifetime::Scope { level: new_level } => {
                    if new_level < current_level {
                        self.compiler.node_lifetimes[node_id.0] = lifetime;
                    }
                }
                AllocationLifetime::Param { var_id } => {
                    let var_type = self.compiler.variables[var_id.0].ty;

                    if !self.compiler.is_allocator_type(var_type) {
                        let param_name =
                            String::from_utf8_lossy(self.compiler.get_variable_name(var_id));
                        self.error(
                            format!(
                                "param '{}' is not an allocator, so we can't infer a safe lifetime",
                                param_name
                            ),
                            node_id,
                        );
                    } else {
                        self.compiler.node_lifetimes[node_id.0] = lifetime;
                    }
                }
                AllocationLifetime::Caller => {
                    self.compiler.node_lifetimes[node_id.0] = lifetime;
                }
                _ => {
                    self.error("can't expand lifetime to unknown", node_id);
                }
            },
        }
    }

    pub fn current_block_may_allocate(&mut self, scope_level: usize, node_id: NodeId) {
        self.possible_allocation_sites
            .push((self.current_blocks.clone(), scope_level, node_id))
    }

    pub fn check_lvalue_lifetime(&mut self, lvalue: NodeId) {
        match &self.compiler.get_node(lvalue) {
            AstNode::Variable => {
                let var_id = self.compiler.var_resolution.get(&lvalue);

                if let Some(var_id) = var_id {
                    let definition_node_id = self.compiler.variables[var_id.0].where_defined;

                    self.compiler.node_lifetimes[lvalue.0] =
                        self.compiler.node_lifetimes[definition_node_id.0];
                } else {
                    self.error(
                        "internal error: variable unresolved when checking lifetimes",
                        lvalue,
                    );
                }
            }
            AstNode::MemberAccess { target, .. } => {
                let target = *target;

                self.check_lvalue_lifetime(target);

                self.compiler.node_lifetimes[lvalue.0] = self.compiler.node_lifetimes[target.0];
            }
            _ => {
                self.error("unsupported lvalue, needs variable or field", lvalue);
            }
        }
    }

    pub fn check_node_lifetime(&mut self, node_id: NodeId, scope_level: usize) {
        match self.compiler.get_node(node_id) {
            AstNode::Block(block_id) => {
                self.check_block_lifetime(*block_id, scope_level + 1);
            }
            AstNode::Int
            | AstNode::Float
            | AstNode::True
            | AstNode::False
            | AstNode::String
            | AstNode::None => {}
            AstNode::Let { initializer, .. } => {
                // Push lifetime requirement from let into the variable and initializer
                let initializer = *initializer;

                self.expand_lifetime_with_node(initializer, node_id);
                self.check_node_lifetime(initializer, scope_level);

                // If the assignment is under-constrained, we'll see if we can get our lifetime
                // from the initializer
                self.expand_lifetime_with_node(node_id, initializer);
            }
            AstNode::Variable => {
                // We're seeing a use of a variable at this point, so make sure the variable
                // lives long enough to get here
                self.expand_lifetime(
                    node_id,
                    node_id,
                    AllocationLifetime::Scope { level: scope_level },
                );

                let var_id =
                    self.compiler.var_resolution.get(&node_id).expect(
                        "internal error: unresolved variable found during lifetime checking",
                    );

                let definition_node_id = self.compiler.variables[var_id.0].where_defined;

                self.expand_lifetime_with_node(definition_node_id, node_id);

                self.compiler.node_lifetimes[node_id.0] =
                    self.compiler.node_lifetimes[definition_node_id.0];
            }
            AstNode::MemberAccess { target, .. } => {
                // Check the type of the access. If it isn't something that can
                // affect lifetimes, we don't need to push the lifetime
                // requirement deeper

                let target = *target;

                let field_type = self.compiler.get_node_type(node_id);
                if !self.compiler.is_copyable_type(field_type) {
                    self.expand_lifetime_with_node(target, node_id);
                }
                self.check_node_lifetime(target, scope_level);
            }
            AstNode::MethodCall { target, call } => {
                // Check the type of the access. If it isn't something that can
                // affect lifetimes, we don't need to push the lifetime
                // requirement deeper

                let target = *target;
                let call = *call;

                let field_type = self.compiler.get_node_type(node_id);
                if !self.compiler.is_copyable_type(field_type) {
                    self.expand_lifetime_with_node(target, node_id);
                }
                self.check_node_lifetime(target, scope_level);
                self.check_node_lifetime(call, scope_level);
            }
            AstNode::BinaryOp { lhs, rhs, op } => {
                let lhs = *lhs;
                let rhs = *rhs;
                let op = *op;

                if matches!(self.compiler.get_node(op), AstNode::Assignment) {
                    self.check_lvalue_lifetime(lhs);

                    if matches!(
                        self.compiler.node_lifetimes[lhs.0],
                        AllocationLifetime::Unknown
                    ) {
                        self.expand_lifetime(
                            lhs,
                            lhs,
                            AllocationLifetime::Scope { level: scope_level },
                        )
                    }

                    self.check_node_lifetime(rhs, scope_level);

                    self.expand_lifetime_with_node(rhs, lhs);

                    // Make sure any new lifetimes get back to the variable declaration
                    self.check_node_lifetime(rhs, scope_level);

                    if self.compiler.node_lifetimes[lhs.0] != self.compiler.node_lifetimes[rhs.0] {
                        self.error("assignment has incompatible lifetimes", lhs)
                    }
                } else {
                    self.expand_lifetime_with_node(lhs, node_id);
                    self.expand_lifetime_with_node(rhs, node_id);

                    self.check_node_lifetime(lhs, scope_level);
                    self.check_node_lifetime(rhs, scope_level);
                }
            }
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => {
                let condition = *condition;
                let then_block = *then_block;
                let else_expression = *else_expression;

                self.expand_lifetime_with_node(condition, node_id);
                self.expand_lifetime_with_node(then_block, node_id);

                self.check_node_lifetime(condition, scope_level);
                self.check_node_lifetime(then_block, scope_level);

                if let Some(else_expression) = else_expression {
                    self.expand_lifetime_with_node(else_expression, node_id);
                    self.check_node_lifetime(else_expression, scope_level);
                }
            }
            AstNode::While { condition, block } => {
                let condition = *condition;
                let block = *block;

                self.expand_lifetime_with_node(condition, node_id);
                self.expand_lifetime_with_node(block, node_id);

                self.check_node_lifetime(condition, scope_level);
                self.check_node_lifetime(block, scope_level);
            }
            AstNode::For { block, .. } => {
                let block = *block;
                self.expand_lifetime_with_node(block, node_id);
                self.check_node_lifetime(block, scope_level);
            }
            AstNode::Call { head, args } => {
                let head = *head;
                // FIXME: remove clone
                let args = args.clone();

                // If the call is not constrained, use the local scope level
                if self.compiler.node_lifetimes[node_id.0] == AllocationLifetime::Unknown {
                    self.compiler.node_lifetimes[node_id.0] =
                        AllocationLifetime::Scope { level: scope_level };
                }

                for arg in args {
                    self.expand_lifetime_with_node(arg, node_id);
                    self.check_node_lifetime(arg, scope_level)
                }

                let call_target = self.compiler.call_resolution.get(&head);

                // note: fun_id 0 is currently the built-in print
                if !matches!(call_target, Some(CallTarget::Function(FunId(0)))) {
                    self.current_block_may_allocate(scope_level, node_id);
                }
            }
            AstNode::New(_, allocation_node_id) => {
                let allocation_node_id = *allocation_node_id;

                if self.compiler.node_lifetimes[node_id.0] == AllocationLifetime::Unknown {
                    // If we don't have enough constraints, then allocate at the current local scope level
                    self.expand_lifetime(
                        allocation_node_id,
                        node_id,
                        AllocationLifetime::Scope { level: scope_level },
                    );
                    self.expand_lifetime(
                        node_id,
                        node_id,
                        AllocationLifetime::Scope { level: scope_level },
                    );
                } else {
                    self.expand_lifetime_with_node(allocation_node_id, node_id);
                }

                match &self.compiler.get_node(allocation_node_id) {
                    AstNode::Call { args, .. } => {
                        // FIXME: remove clone
                        let args = args.clone();
                        for arg in args {
                            self.expand_lifetime_with_node(arg, node_id);
                            self.check_node_lifetime(arg, scope_level)
                        }
                    }
                    _ => {
                        self.error("expected call as part of allocation", allocation_node_id);
                    }
                }

                self.current_block_may_allocate(scope_level, node_id);
            }
            AstNode::Return(return_expr) => {
                if let Some(return_expr) = return_expr {
                    let return_expr = *return_expr;

                    self.expand_lifetime(return_expr, node_id, AllocationLifetime::Caller);
                    self.check_node_lifetime(return_expr, scope_level);
                }
            }
            AstNode::NamedValue { value, .. } => {
                let value = *value;

                self.expand_lifetime_with_node(value, node_id);
                self.check_node_lifetime(value, scope_level)
            }
            AstNode::NamespacedLookup { item, .. } => {
                let item = *item;
                if self.compiler.node_lifetimes[node_id.0] == AllocationLifetime::Unknown {
                    self.compiler.node_lifetimes[node_id.0] =
                        AllocationLifetime::Scope { level: scope_level };
                }

                if matches!(self.compiler.get_node(item), AstNode::Variable) {
                    self.expand_lifetime_with_node(item, node_id);
                } else if matches!(self.compiler.get_node(item), AstNode::Call { .. }) {
                    self.expand_lifetime_with_node(item, node_id);
                    self.check_node_lifetime(item, scope_level);
                }
            }
            AstNode::Match { target, match_arms } => {
                let target = *target;
                let match_arms = match_arms.clone();

                self.expand_lifetime_with_node(target, node_id);

                for (_, match_result) in &match_arms {
                    self.check_node_lifetime(*match_result, scope_level)
                }
            }
            AstNode::Fun { .. } | AstNode::Struct { .. } | AstNode::Enum { .. } => {
                // ignore
            }
            AstNode::Statement(node_id) => {
                self.check_node_lifetime(*node_id, scope_level);
            }
            x => {
                panic!("unsupported node: {:?}", x)
            }
        }
    }

    pub fn check_lifetimes(mut self) -> Compiler {
        let num_nodes = self.compiler.num_ast_nodes();
        self.compiler
            .node_lifetimes
            .resize(num_nodes, AllocationLifetime::Unknown);

        // Check functions, skipping over our built-in print
        for fun_id in 1..self.compiler.functions.len() {
            let fun = &self.compiler.functions[fun_id];
            for param in &fun.params {
                let param_node_id = self.compiler.variables[param.var_id.0].where_defined;

                self.compiler.node_lifetimes[param_node_id.0] = AllocationLifetime::Param {
                    var_id: param.var_id,
                };
            }

            let body = fun.body;
            self.check_node_lifetime(body, 0);
        }

        // Before we leave, go through our possible allocation sites and see
        // which local scopes allocate for themselves. If they do, mark their
        // blocks so we can properly deallocate these resources
        for (block_ids, scope_level, node_id) in self.possible_allocation_sites {
            if let AllocationLifetime::Scope { level } = &self.compiler.node_lifetimes[node_id.0] {
                if let Some(block_id) = block_ids.into_iter().rev().nth(scope_level - level) {
                    self.compiler.blocks[block_id.0].may_locally_allocate = Some(*level);
                }
            }
        }

        self.compiler
    }
}
