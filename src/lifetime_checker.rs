use std::collections::HashMap;

use crate::{
    compiler::{CallTarget, Compiler},
    errors::{Severity, SourceError},
    parser::{AstNode, BlockId, NodeId},
    typechecker::{Lifetime, LifetimeAnnotation, Type, VarId},
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AllocationLifetime {
    Return,
    Param { var_id: VarId },
    Scope { level: usize },
    Unknown,
}

pub struct LifetimeChecker {
    compiler: Compiler,
    current_blocks: Vec<BlockId>,
    possible_allocation_sites: Vec<(Vec<BlockId>, usize, NodeId)>,
    num_lifetime_inferences: HashMap<BlockId, usize>,
}

impl LifetimeChecker {
    pub fn new(compiler: Compiler) -> Self {
        Self {
            compiler,
            current_blocks: vec![],
            possible_allocation_sites: vec![],
            num_lifetime_inferences: HashMap::new(),
        }
    }

    pub fn check_block_lifetime(&mut self, block_id: BlockId, scope_level: usize) {
        self.current_blocks.push(block_id);

        // FIXME: remove clone
        let block = self.compiler.blocks[block_id.0].clone();

        // Run lifetime inference to fixpoint or error
        loop {
            let num_lifetime_inferences_before =
                *self.num_lifetime_inferences.entry(block_id).or_default();

            let num_errors_before = self.compiler.errors.len();

            for node_id in block.nodes.iter().rev() {
                self.check_node_lifetime(*node_id, scope_level);
            }

            let num_lifetime_inferences_after =
                *self.num_lifetime_inferences.entry(block_id).or_default();

            let num_errors_after = self.compiler.errors.len();

            if num_lifetime_inferences_after == num_lifetime_inferences_before
                || num_errors_before != num_errors_after
            {
                break;
            }
        }

        self.current_blocks.pop();
    }

    pub fn error(&mut self, message: impl Into<String>, node_id: NodeId) {
        self.compiler.errors.push(SourceError {
            message: message.into(),
            node_id,
            severity: Severity::Error,
        });
    }

    pub fn note(&mut self, message: impl Into<String>, node_id: NodeId) {
        self.compiler.errors.push(SourceError {
            message: message.into(),
            node_id,
            severity: Severity::Note,
        });
    }

    // pub fn note(&mut self, message: impl Into<String>, node_id: NodeId) {
    //     self.compiler.errors.push(SourceError {
    //         message: message.into(),
    //         node_id,
    //         severity: Severity::Note,
    //     });
    // }

    pub fn increment_lifetime_inferences(&mut self) {
        let current_block_id = self
            .current_blocks
            .last()
            .expect("internal error: can't find current block id");

        *self
            .num_lifetime_inferences
            .entry(*current_block_id)
            .or_default() += 1;
    }

    pub fn expand_lifetime_with_node(&mut self, node_id: NodeId, lifetime_from_node: NodeId) {
        let lifetime = self.compiler.get_node_lifetime(lifetime_from_node);

        self.expand_lifetime(node_id, lifetime_from_node, lifetime)
    }

    pub fn expand_lifetime(
        &mut self,
        node_id: NodeId,
        lifetime_from_node: NodeId,
        lifetime: AllocationLifetime,
    ) {
        let target_type_id = self.compiler.get_node_type(node_id);
        let source_type_id = self.compiler.get_node_type(lifetime_from_node);

        if self.compiler.is_copyable_type(target_type_id)
            || self.compiler.is_copyable_type(source_type_id)
        {
            // this is a trivially copyable type, so don't calculate the lifetime
            return;
        }

        if lifetime == AllocationLifetime::Unknown {
            // you can't expand to unknown
            return;
        }

        let current_lifetime = self.compiler.get_node_lifetime(node_id);

        match current_lifetime {
            AllocationLifetime::Unknown => {
                self.compiler.set_node_lifetime(node_id, lifetime);
                self.increment_lifetime_inferences();
            }
            AllocationLifetime::Param { var_id } => {
                match lifetime {
                    AllocationLifetime::Param {
                        var_id: incoming_var_id,
                    } => {
                        let param_name1 =
                            String::from_utf8_lossy(self.compiler.get_variable_name(var_id))
                                .to_string();
                        let param_name2 = String::from_utf8_lossy(
                            self.compiler.get_variable_name(incoming_var_id),
                        )
                        .to_string();
                        if incoming_var_id != var_id {
                            self.note(
                                format!(
                                    "add a lifetime annotation to the function, e.g. [{} == {}]",
                                    param_name1, param_name2
                                ),
                                node_id,
                            );
                            self.error(format!("can't find compatible lifetime between param '{}' and param '{}'", param_name1, param_name2), node_id)
                        }
                    }
                    AllocationLifetime::Scope { .. } => {
                        // Params outlive all scopes
                    }
                    AllocationLifetime::Return => {
                        let param_name1 =
                            String::from_utf8_lossy(self.compiler.get_variable_name(var_id))
                                .to_string();
                        self.note(
                            format!(
                                "add a lifetime annotation to the function, e.g. [{} == return]",
                                param_name1
                            ),
                            node_id,
                        );
                        self.error(
                            format!(
                                "can't find compatible lifetime between param '{}' and return",
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
                            node_id,
                        );
                    }
                }
            }
            AllocationLifetime::Return => {
                // TODO: add fix to check for raw and custom lifetimes

                match lifetime {
                    AllocationLifetime::Param { var_id } => {
                        let param_name =
                            String::from_utf8_lossy(self.compiler.get_variable_name(var_id));

                        self.error(
                            format!("can't find compatible lifetime for param '{}'", param_name),
                            node_id,
                        );
                    }
                    AllocationLifetime::Scope { .. } => {
                        // Return is larger than all scopes, so ignore
                    }
                    AllocationLifetime::Return => {
                        // Already return
                    }
                    _ => {
                        self.error(
                            format!(
                                "can't find compatible lifetime for return, found {:?}",
                                lifetime
                            ),
                            node_id,
                        );
                    }
                }
            }
            AllocationLifetime::Scope {
                level: current_level,
            } => match lifetime {
                AllocationLifetime::Scope { level: new_level } => {
                    if new_level < current_level {
                        self.compiler.set_node_lifetime(node_id, lifetime);
                        self.increment_lifetime_inferences();
                    }
                    // else if new_level > current_level {
                    //     self.error(
                    //         "lifetime inferred to be two different scope levels",
                    //         node_id,
                    //     )
                    // }
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
                        self.compiler.set_node_lifetime(node_id, lifetime);

                        self.increment_lifetime_inferences();
                    }
                }
                AllocationLifetime::Return => {
                    self.compiler.set_node_lifetime(node_id, lifetime);

                    self.increment_lifetime_inferences();
                }
                AllocationLifetime::Unknown => {
                    self.error(
                        format!(
                            "can't find compatible lifetime for scoped expression, found {:?}",
                            lifetime
                        ),
                        node_id,
                    );
                }
            },
        }
    }

    pub fn current_block_may_allocate(&mut self, scope_level: usize, node_id: NodeId) {
        if let AllocationLifetime::Scope { level } = self.compiler.get_node_lifetime(node_id) {
            if level > scope_level {
                panic!(
                    "current_block_may_allocate saw an impossible level/scope_level: {} vs {}",
                    level, scope_level
                )
            }
        }
        self.possible_allocation_sites
            .push((self.current_blocks.clone(), scope_level, node_id))
    }

    pub fn check_lvalue_lifetime(&mut self, lvalue: NodeId) {
        match &self.compiler.get_node(lvalue) {
            AstNode::Variable => {
                let var_id = self.compiler.var_resolution.get(&lvalue);

                if let Some(var_id) = var_id {
                    let definition_node_id = self.compiler.variables[var_id.0].where_defined;

                    self.expand_lifetime_with_node(lvalue, definition_node_id);
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

                self.expand_lifetime_with_node(lvalue, target)
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

                self.expand_lifetime(
                    node_id,
                    node_id,
                    AllocationLifetime::Scope { level: scope_level },
                );

                self.expand_lifetime_with_node(initializer, node_id);
                self.check_node_lifetime(initializer, scope_level);

                // If the assignment is under-constrained, we'll see if we can get our lifetime
                // from the initializer
                self.expand_lifetime_with_node(node_id, initializer);
            }
            AstNode::Variable => {
                // We're seeing a use of a variable at this point, so make sure the variable
                // lives long enough to get here

                let var_id =
                    self.compiler.var_resolution.get(&node_id).expect(
                        "internal error: unresolved variable found during lifetime checking",
                    );

                let definition_node_id = self.compiler.variables[var_id.0].where_defined;

                self.expand_lifetime_with_node(definition_node_id, node_id);

                self.expand_lifetime_with_node(node_id, definition_node_id);
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
                        self.compiler.get_node_lifetime(lhs),
                        AllocationLifetime::Unknown
                    ) {
                        self.expand_lifetime(
                            lhs,
                            lhs,
                            AllocationLifetime::Scope { level: scope_level },
                        )
                    }

                    self.check_node_lifetime(rhs, scope_level);

                    let num_errors_before = self.compiler.errors.len();
                    self.expand_lifetime_with_node(rhs, lhs);
                    let num_errors_after = self.compiler.errors.len();

                    // a tiny bit hackish, but we don't need to check
                    // both directions if we've already errored
                    if num_errors_before == num_errors_after {
                        self.expand_lifetime_with_node(lhs, rhs);
                    }

                    // Make sure any new lifetimes get back to the variable declaration
                    self.check_node_lifetime(rhs, scope_level);
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
                if self.compiler.get_node_lifetime(node_id) == AllocationLifetime::Unknown {
                    self.compiler.set_node_lifetime(
                        node_id,
                        AllocationLifetime::Scope { level: scope_level },
                    );
                }

                let call_target = self.compiler.call_resolution.get(&head);

                // note: fun_id 0 is currently the built-in print
                match call_target {
                    Some(CallTarget::Function(fun_id)) if fun_id.0 != 0 => {
                        let fun_id = *fun_id;
                        let params = self.compiler.functions[fun_id.0].params.clone();

                        for (param, arg) in params.iter().zip(args.iter()) {
                            let param_node_id =
                                self.compiler.variables[param.var_id.0].where_defined;

                            let expected_lifetime = self.compiler.get_node_lifetime(param_node_id);

                            match expected_lifetime {
                                AllocationLifetime::Return => {
                                    // This is actually the lifetime of the call itself, which is where
                                    // we are tracking the lifetime of the return value of the call
                                    self.expand_lifetime_with_node(*arg, node_id);
                                    self.check_node_lifetime(*arg, scope_level);
                                }
                                AllocationLifetime::Param { var_id } => {
                                    // figure out which arg corresponds to this var_id
                                    for (param2, arg2) in params.iter().zip(args.iter()) {
                                        if param2.var_id == var_id && arg != arg2 {
                                            self.expand_lifetime_with_node(*arg, *arg2);
                                            break;
                                        }
                                    }

                                    // self.expand_lifetime(arg, param_node_id, expected_lifetime);
                                    self.check_node_lifetime(*arg, scope_level);
                                }
                                _ => {
                                    self.check_node_lifetime(*arg, scope_level);
                                }
                            }
                        }

                        self.current_block_may_allocate(scope_level, node_id);
                    }
                    _ => {
                        for arg in args {
                            self.check_node_lifetime(arg, scope_level)
                        }
                    }
                }
            }
            AstNode::New(_, allocation_node_id) => {
                let allocation_node_id = *allocation_node_id;

                if self.compiler.get_node_lifetime(node_id) == AllocationLifetime::Unknown {
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
                            self.check_node_lifetime(arg, scope_level);

                            self.expand_lifetime_with_node(arg, node_id);

                            self.expand_lifetime_with_node(allocation_node_id, arg);
                            self.expand_lifetime_with_node(node_id, arg);
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

                    self.expand_lifetime(return_expr, return_expr, AllocationLifetime::Return);
                    self.check_node_lifetime(return_expr, scope_level);
                }
            }
            AstNode::NamedValue { value, .. } => {
                let value = *value;

                self.expand_lifetime_with_node(value, node_id);
                self.check_node_lifetime(value, scope_level);

                self.expand_lifetime_with_node(node_id, value)
            }
            AstNode::NamespacedLookup { item, .. } => {
                let item = *item;
                if self.compiler.get_node_lifetime(node_id) == AllocationLifetime::Unknown {
                    self.compiler.set_node_lifetime(
                        node_id,
                        AllocationLifetime::Scope { level: scope_level },
                    );
                }

                if matches!(self.compiler.get_node(item), AstNode::Variable) {
                    self.expand_lifetime_with_node(item, node_id);
                } else {
                    match self.compiler.get_node(item) {
                        AstNode::Call { args, .. } => {
                            let node_type_id = self.compiler.get_node_type(node_id);
                            let node_type = self
                                .compiler
                                .get_type(self.compiler.get_underlying_type_id(node_type_id));
                            if matches!(node_type, Type::Enum { .. }) {
                                // FIXME: clone
                                let args = args.clone();

                                self.expand_lifetime_with_node(item, node_id);
                                self.check_node_lifetime(item, scope_level);

                                for arg in args {
                                    self.expand_lifetime_with_node(arg, item);
                                    self.check_node_lifetime(arg, scope_level);
                                }
                            } else {
                                self.expand_lifetime_with_node(item, node_id);
                                self.check_node_lifetime(item, scope_level);
                            }
                        }
                        _ => {}
                    }
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
            .resize_node_lifetimes(num_nodes, AllocationLifetime::Unknown);

        // Set up param lifetimes, skipping over our built-in print
        for fun_id in 1..self.compiler.functions.len() {
            let fun = self.compiler.functions[fun_id].clone();
            'param: for param in &fun.params {
                let param_node_id = self.compiler.variables[param.var_id.0].where_defined;

                for lifetime_annotation in &fun.lifetime_annotations {
                    match lifetime_annotation {
                        LifetimeAnnotation::Equality(
                            Lifetime::Return,
                            Lifetime::Variable(var_id),
                        ) if var_id == &param.var_id => {
                            self.compiler
                                .set_node_lifetime(param_node_id, AllocationLifetime::Return);
                            continue 'param;
                        }
                        LifetimeAnnotation::Equality(
                            Lifetime::Variable(var_id),
                            Lifetime::Return,
                        ) if var_id == &param.var_id => {
                            self.compiler
                                .set_node_lifetime(param_node_id, AllocationLifetime::Return);
                            continue 'param;
                        }
                        LifetimeAnnotation::Equality(
                            Lifetime::Variable(lhs),
                            Lifetime::Variable(rhs),
                        ) if lhs == &param.var_id => {
                            self.compiler.set_node_lifetime(
                                param_node_id,
                                AllocationLifetime::Param { var_id: *rhs },
                            );
                            continue 'param;
                        }
                        _ => {}
                    }
                }

                self.compiler.set_node_lifetime(
                    param_node_id,
                    AllocationLifetime::Param {
                        var_id: param.var_id,
                    },
                );
            }
        }

        // Check function bodies, skipping over our built-in print
        for fun_id in 1..self.compiler.functions.len() {
            let fun = self.compiler.functions[fun_id].clone();

            let body = fun.body;
            self.check_node_lifetime(body, 0);
        }

        // Before we leave, go through our possible allocation sites and see
        // which local scopes allocate for themselves. If they do, mark their
        // blocks so we can properly deallocate these resources
        for (block_ids, scope_level, node_id) in self.possible_allocation_sites {
            if let AllocationLifetime::Scope { level } = &self.compiler.get_node_lifetime(node_id) {
                if let Some(block_id) = block_ids.into_iter().rev().nth(scope_level - level) {
                    self.compiler.blocks[block_id.0].may_locally_allocate = Some(*level);
                }
            }
        }

        self.compiler
    }
}
