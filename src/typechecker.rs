use crate::{
    compiler::Compiler,
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

pub struct Typechecker {
    pub compiler: Compiler,
    pub types: Vec<Type>,
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
}
