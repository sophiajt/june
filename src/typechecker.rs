use std::collections::HashMap;

use crate::{
    compiler::Compiler,
    errors::SourceError,
    parser::{AstNode, NodeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Debug, PartialEq)]
pub struct Type {
    params: Option<Vec<TypeId>>,
}

impl Type {
    pub fn new() -> Type {
        Type { params: None }
    }

    pub fn new_with_params(params: Vec<TypeId>) -> Type {
        Type {
            params: Some(params),
        }
    }
}

pub struct Variable {
    ty: TypeId,
    is_mutable: bool,
    where_defined: NodeId,
}

pub struct Scope {
    variables: HashMap<Vec<u8>, Variable>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
        }
    }
}

pub struct Typechecker {
    pub compiler: Compiler,
    pub types: Vec<Type>,
    pub scope: Vec<Scope>,
}

pub const UNKNOWN_TYPE_ID: TypeId = TypeId(0);
pub const VOID_TYPE_ID: TypeId = TypeId(1);
pub const I64_TYPE_ID: TypeId = TypeId(2);
pub const F64_TYPE_ID: TypeId = TypeId(3);
pub const BOOL_TYPE_ID: TypeId = TypeId(4);

impl Typechecker {
    pub fn new(compiler: Compiler) -> Self {
        Self {
            compiler,
            types: vec![
                // hardwire in the core types before the user-defined types
                Type::new(), // unknown
                Type::new(), // void
                Type::new(), // i64
                Type::new(), // f64
                Type::new(), // bool
            ],
            scope: vec![Scope::new()],
        }
    }

    pub fn typecheck_node(&mut self, node_id: NodeId) {
        match &self.compiler.ast_nodes[node_id.0] {
            AstNode::Block(block) => {
                // FIXME: probably could clean this up
                let block = block.clone();
                for node_id in block {
                    self.typecheck_node(node_id)
                }
                self.compiler.node_types[node_id.0] = VOID_TYPE_ID;
            }
            AstNode::Int => {
                self.compiler.node_types[node_id.0] = I64_TYPE_ID;
            }
            AstNode::Float => {
                self.compiler.node_types[node_id.0] = F64_TYPE_ID;
            }
            AstNode::True | AstNode::False => {
                self.compiler.node_types[node_id.0] = BOOL_TYPE_ID;
            }
            AstNode::Let {
                variable_name,
                initializer,
                is_mutable,
                ..
            } => {
                let variable_name = *variable_name;
                let initializer = *initializer;
                let is_mutable = *is_mutable;

                self.typecheck_node(initializer);

                // FIXME: also check the optional ty above in the Let
                let ty = self.compiler.node_types[initializer.0];

                self.define_variable(variable_name, ty, is_mutable, node_id);
            }
            AstNode::Variable => {
                let variable = self.find_variable_in_scope(node_id);

                if let Some(variable) = variable {
                    self.compiler.node_types[node_id.0] = variable.ty;
                } else {
                    self.error("can't find variable", node_id);
                }
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                let lhs = *lhs;
                let rhs = *rhs;
                let op = *op;
                self.typecheck_node(lhs);
                self.typecheck_node(rhs);

                match &self.compiler.ast_nodes[op.0] {
                    AstNode::Plus | AstNode::Minus | AstNode::Multiply | AstNode::Divide => {
                        self.compiler.node_types[node_id.0] = self.compiler.node_types[lhs.0];
                    }
                    x => panic!("unsupported operator: {:?}", x),
                }
            }
            x => {
                panic!("unsupported node: {:?}", x)
            }
        }
    }

    pub fn typecheck(mut self) -> Compiler {
        let num_nodes = self.compiler.ast_nodes.len();
        self.compiler.node_types.resize(num_nodes, UNKNOWN_TYPE_ID);

        println!("{:?}", self.compiler.ast_nodes);
        self.typecheck_node(NodeId(self.compiler.ast_nodes.len() - 1));

        self.compiler
    }

    pub fn define_variable(
        &mut self,
        variable_name: NodeId,
        ty: TypeId,
        is_mutable: bool,
        where_defined: NodeId,
    ) {
        let variable_name = self.compiler.source
            [self.compiler.span_start[variable_name.0]..self.compiler.span_end[variable_name.0]]
            .to_vec();

        self.scope
            .last_mut()
            .expect("internal error: missing typechecking scope")
            .variables
            .insert(
                variable_name,
                Variable {
                    ty,
                    is_mutable,
                    where_defined,
                },
            );
    }

    pub fn find_variable_in_scope(&self, variable_name: NodeId) -> Option<&Variable> {
        let name = &self.compiler.source
            [self.compiler.span_start[variable_name.0]..self.compiler.span_end[variable_name.0]];
        for scope in self.scope.iter().rev() {
            if let Some(value) = scope.variables.get(name) {
                return Some(value);
            }
        }

        None
    }

    pub fn error(&mut self, message: impl Into<String>, node_id: NodeId) {
        self.compiler.errors.push(SourceError {
            message: message.into(),

            node_id,
        });
    }
}
