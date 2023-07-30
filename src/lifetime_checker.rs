use crate::{
    compiler::Compiler,
    parser::{AstNode, NodeId},
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AllocationLifetime {
    Local,
    Caller,
}

pub struct LifetimeChecker {
    compiler: Compiler,
}

impl LifetimeChecker {
    pub fn new(compiler: Compiler) -> Self {
        Self { compiler }
    }

    pub fn check_block_lifetime(&mut self, block: &[NodeId]) {
        for node_id in block.iter().rev() {
            self.check_node_lifetime(*node_id);
        }
    }

    pub fn check_node_lifetime(&mut self, node_id: NodeId) {
        match &self.compiler.ast_nodes[node_id.0] {
            AstNode::Block(block) => {
                // FIXME: probably could clean this up
                let block = block.clone();
                self.check_block_lifetime(&block);
            }
            AstNode::Int | AstNode::Float | AstNode::True | AstNode::False | AstNode::String => {}
            AstNode::Let {
                variable_name,
                initializer,
                ..
            } => {
                // Push lifetime requirement from let into the variable and initializer
                self.compiler.node_lifetimes[variable_name.0] =
                    self.compiler.node_lifetimes[node_id.0];

                self.compiler.node_lifetimes[initializer.0] =
                    self.compiler.node_lifetimes[node_id.0];

                self.check_node_lifetime(*initializer);
            }
            AstNode::Variable => {
                if self.compiler.node_lifetimes[node_id.0] != AllocationLifetime::Local {
                    let var_id = self.compiler.var_resolution.get(&node_id).expect(
                        "internal error: unresolved variable found during lifetime checking",
                    );

                    let definition_node_id = self.compiler.variables[var_id.0].where_defined;

                    self.compiler.node_lifetimes[definition_node_id.0] =
                        self.compiler.node_lifetimes[node_id.0];
                }
            }
            AstNode::MemberAccess { target, .. } => {
                // Check the type of the access. If it isn't something that can
                // affect lifetimes, we don't need to push the lifetime
                // requirement deeper

                let field_type = self.compiler.node_types[node_id.0];
                if !self.compiler.is_copyable_type(field_type) {
                    self.compiler.node_lifetimes[target.0] =
                        self.compiler.node_lifetimes[node_id.0];
                }
                self.check_node_lifetime(*target);
            }
            AstNode::BinaryOp { lhs, rhs, .. } => {
                let lhs = *lhs;
                let rhs = *rhs;

                self.compiler.node_lifetimes[lhs.0] = self.compiler.node_lifetimes[node_id.0];
                self.compiler.node_lifetimes[rhs.0] = self.compiler.node_lifetimes[node_id.0];

                self.check_node_lifetime(lhs);
                self.check_node_lifetime(rhs);
            }
            AstNode::Call { args, .. } => {
                // FIXME: remove clone
                let args = args.clone();
                for arg in args {
                    self.compiler.node_lifetimes[arg.0] = self.compiler.node_lifetimes[node_id.0];

                    self.check_node_lifetime(arg)
                }
            }
            AstNode::New(_, allocation_node_id) => {
                self.compiler.node_lifetimes[allocation_node_id.0] =
                    self.compiler.node_lifetimes[node_id.0];

                self.check_node_lifetime(*allocation_node_id)
            }
            AstNode::Return(return_expr) => {
                if let Some(return_expr) = return_expr {
                    self.compiler.node_lifetimes[return_expr.0] = AllocationLifetime::Caller;

                    self.check_node_lifetime(*return_expr);
                }
            }
            AstNode::NamedValue { value, .. } => {
                self.compiler.node_lifetimes[value.0] = self.compiler.node_lifetimes[node_id.0];

                self.check_node_lifetime(*value)
            }
            AstNode::Fun { .. } | AstNode::Struct { .. } => {
                // ignore
            }
            AstNode::Statement(node_id) => {
                self.check_node_lifetime(*node_id);
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
            .resize(num_nodes, AllocationLifetime::Local);

        // Check functions, skipping over our built-in print
        for fun_id in 1..self.compiler.functions.len() {
            let fun = &self.compiler.functions[fun_id];
            for param in &fun.params {
                let param_node_id = self.compiler.variables[param.var_id.0].where_defined;

                self.compiler.node_lifetimes[param_node_id.0] = AllocationLifetime::Caller;
            }

            let body = fun.body;
            self.check_node_lifetime(body);
        }

        self.compiler
    }
}
