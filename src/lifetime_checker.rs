use crate::{
    compiler::Compiler,
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
    current_block: Vec<BlockId>,
}

impl LifetimeChecker {
    pub fn new(compiler: Compiler) -> Self {
        Self {
            compiler,
            current_block: vec![],
        }
    }

    pub fn check_block_lifetime(&mut self, block_id: BlockId, scope_level: usize) {
        self.current_block.push(block_id);

        // FIXME: remove clone
        let block = self.compiler.blocks[block_id.0].clone();

        for node_id in block.nodes.iter().rev() {
            self.check_node_lifetime(*node_id, scope_level);
        }

        self.current_block.pop();
    }

    pub fn error(&mut self, message: impl Into<String>, node_id: NodeId) {
        self.compiler.errors.push(SourceError {
            message: message.into(),

            node_id,
        });
    }

    pub fn expand_lifetime_with_node(&mut self, node_id: NodeId, lifetime_from_node: NodeId) {
        let lifetime = self.compiler.node_lifetimes[lifetime_from_node.0];

        self.expand_lifetime(node_id, lifetime)
    }

    pub fn expand_lifetime(&mut self, node_id: NodeId, lifetime: AllocationLifetime) {
        let current_lifetime = self.compiler.node_lifetimes[node_id.0];

        match current_lifetime {
            AllocationLifetime::Unknown => {
                self.compiler.node_lifetimes[node_id.0] = lifetime;
            }
            AllocationLifetime::Param { var_id } => match lifetime {
                AllocationLifetime::Param {
                    var_id: incoming_var_id,
                } => {
                    if incoming_var_id != var_id {
                        self.error("can't find compatible lifetime", node_id)
                    }
                }
                _ => self.error("can't find compatible lifetime", node_id),
            },
            AllocationLifetime::Caller => {
                // TODO: add fix to check for raw and custom lifetimes
            }
            AllocationLifetime::Scope {
                level: current_level,
            } => {
                match lifetime {
                    AllocationLifetime::Scope { level: new_level } => {
                        if new_level < current_level {
                            self.compiler.node_lifetimes[node_id.0] = lifetime;
                        }
                    }
                    AllocationLifetime::Param { .. } => {
                        // FIXME: add logic
                        self.error("can't find compatible lifetime", node_id);
                    }
                    AllocationLifetime::Caller => {
                        self.compiler.node_lifetimes[node_id.0] = lifetime;
                    }
                    _ => {
                        self.error("can't expand lifetime to unknown", node_id);
                    }
                }
            }
        }
    }

    pub fn current_block_may_allocate(&mut self, scope_level: usize) {
        let block_id = *self
            .current_block
            .last()
            .expect("internal error: lifetime checker missing block");

        self.compiler.blocks[block_id.0].allocates_at = Some(scope_level);
    }

    pub fn check_node_lifetime(&mut self, node_id: NodeId, scope_level: usize) {
        match &self.compiler.ast_nodes[node_id.0] {
            AstNode::Block(block_id) => {
                self.check_block_lifetime(*block_id, scope_level + 1);
            }
            AstNode::Int | AstNode::Float | AstNode::True | AstNode::False | AstNode::String => {}
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
                self.expand_lifetime(node_id, AllocationLifetime::Scope { level: scope_level });

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

                let field_type = self.compiler.node_types[node_id.0];
                if !self.compiler.is_copyable_type(field_type) {
                    self.expand_lifetime_with_node(target, node_id);
                }
                self.check_node_lifetime(target, scope_level);
            }
            AstNode::BinaryOp { lhs, rhs, op } => {
                let lhs = *lhs;
                let rhs = *rhs;
                let op = *op;

                if matches!(self.compiler.ast_nodes[op.0], AstNode::Assignment) {
                    self.check_node_lifetime(lhs, scope_level);

                    self.expand_lifetime_with_node(rhs, lhs);
                    self.check_node_lifetime(rhs, scope_level);

                    self.expand_lifetime_with_node(lhs, rhs);
                    self.check_node_lifetime(lhs, scope_level);
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
            AstNode::Call { head, args } => {
                let head = *head;
                // If the call is not constrained, use the local scope level
                if self.compiler.node_lifetimes[node_id.0] == AllocationLifetime::Unknown {
                    self.compiler.node_lifetimes[node_id.0] =
                        AllocationLifetime::Scope { level: scope_level };
                }

                // FIXME: remove clone
                let args = args.clone();
                for arg in args {
                    self.expand_lifetime_with_node(arg, node_id);

                    self.check_node_lifetime(arg, scope_level)
                }

                if let AllocationLifetime::Scope { level } = self.compiler.node_lifetimes[node_id.0]
                {
                    let fun_id = self.compiler.fun_resolution.get(&head);

                    // note: fun_id 0 is currently the built-in print
                    if level == scope_level && !matches!(fun_id, Some(FunId(0))) {
                        self.current_block_may_allocate(level);
                    }
                }
            }
            AstNode::New(_, allocation_node_id) => {
                let allocation_node_id = *allocation_node_id;

                if self.compiler.node_lifetimes[node_id.0] == AllocationLifetime::Unknown {
                    // If we don't have enough constraints, then allocate at the current local scope level
                    self.expand_lifetime(
                        allocation_node_id,
                        AllocationLifetime::Scope { level: scope_level },
                    );
                    self.expand_lifetime(node_id, AllocationLifetime::Scope { level: scope_level });
                } else {
                    self.expand_lifetime_with_node(allocation_node_id, node_id);
                }

                self.check_node_lifetime(allocation_node_id, scope_level);

                if let AllocationLifetime::Scope { level } = self.compiler.node_lifetimes[node_id.0]
                {
                    if level == scope_level {
                        self.current_block_may_allocate(level);
                    }
                }
            }
            AstNode::Return(return_expr) => {
                if let Some(return_expr) = return_expr {
                    let return_expr = *return_expr;

                    self.expand_lifetime(return_expr, AllocationLifetime::Caller);
                    self.check_node_lifetime(return_expr, scope_level);
                }
            }
            AstNode::NamedValue { value, .. } => {
                let value = *value;

                self.expand_lifetime_with_node(value, node_id);

                self.check_node_lifetime(value, scope_level)
            }
            AstNode::Fun { .. } | AstNode::Struct { .. } => {
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
        let num_nodes = self.compiler.ast_nodes.len();
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

        self.compiler
    }
}
