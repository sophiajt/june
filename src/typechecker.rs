use std::collections::HashMap;

use crate::{
    compiler::Compiler,
    errors::SourceError,
    parser::{AstNode, NodeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunId(pub usize);

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

#[derive(Debug)]
pub struct Variable {
    ty: TypeId,
    is_mutable: bool,
    where_defined: NodeId,
}

#[derive(Debug)]
pub struct Function {
    params: Vec<(Vec<u8>, TypeId)>,
    return_type: TypeId,
    body: NodeId,
}

pub struct Scope {
    variables: HashMap<Vec<u8>, VarId>,
    functions: HashMap<Vec<u8>, FunId>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
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
pub const STRING_TYPE_ID: TypeId = TypeId(5);

impl Typechecker {
    pub fn new(mut compiler: Compiler) -> Self {
        // temporarily - let's add `println` for now, to get examples to typecheck
        compiler.functions.push(Function {
            params: vec![(b"input".to_vec(), STRING_TYPE_ID)],
            body: NodeId(0),
            return_type: VOID_TYPE_ID,
        });

        let mut scope = vec![Scope::new()];

        scope
            .last_mut()
            .expect("internal error: couldn't access function scope")
            .functions
            .insert(b"println".to_vec(), FunId(0));

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
            scope,
        }
    }

    pub fn typecheck_typename(&mut self, ty: NodeId) -> TypeId {
        VOID_TYPE_ID
    }

    pub fn typecheck_fun(
        &mut self,
        name: NodeId,
        params: NodeId,
        return_ty: Option<NodeId>,
        block: NodeId,
    ) {
        let fun_params = vec![];
        let name = self.compiler.source
            [self.compiler.span_start[name.0]..self.compiler.span_end[name.0]]
            .to_vec();

        self.typecheck_node(block);

        if let AstNode::Params(unchecked_params) = &self.compiler.ast_nodes[params.0] {
            for unchecked_param in unchecked_params {
                if let AstNode::Param { name, ty } = &self.compiler.ast_nodes[unchecked_param.0] {
                } else {
                    self.error("expected function parameter", *unchecked_param);
                    break;
                }
            }
        } else {
            self.error("expected function parameters", params)
        }

        self.compiler.functions.push(Function {
            params: fun_params,
            return_type: VOID_TYPE_ID,
            body: block,
        });

        let fun_id = self.compiler.functions.len() - 1;

        self.scope
            .last_mut()
            .expect("internal error: missing function scope")
            .functions
            .insert(name, FunId(fun_id));
    }

    pub fn typecheck_block(&mut self, node_id: NodeId, nodes: &[NodeId]) {
        for node_id in nodes {
            if let AstNode::Fun {
                name,
                params,
                return_ty,
                block,
            } = &self.compiler.ast_nodes[node_id.0]
            {
                self.typecheck_fun(*name, *params, *return_ty, *block)
            }
        }

        for node_id in nodes {
            self.typecheck_node(*node_id);
        }
        self.compiler.node_types[node_id.0] = VOID_TYPE_ID;
    }

    pub fn is_type_compatible(&self, lhs: TypeId, rhs: TypeId) -> bool {
        lhs == rhs
    }

    pub fn typecheck_call(&mut self, head: NodeId, args: &[NodeId]) -> TypeId {
        if let Some(fun_id) = self.find_function_in_scope(head) {
            let Function {
                params,
                return_type,
                ..
            } = &self.compiler.functions[fun_id.0];

            let params = params.clone();
            let return_type = *return_type;

            if args.len() != params.len() {
                self.error(
                    &format!("expected {} args, found {}", params.len(), args.len()),
                    head,
                );
                return return_type;
            }

            for (arg, param) in args.iter().zip(params) {
                // TODO: add name-checking
                let arg = *arg;

                let arg_type = self.typecheck_node(arg);

                if !self.is_type_compatible(arg_type, param.1) {
                    // FIXME: make this a better type error
                    self.error("type mismatch for arg", arg);
                    return return_type;
                }
            }

            return_type
        } else {
            self.error("unknown function", head);
            UNKNOWN_TYPE_ID
        }
    }

    pub fn typecheck_node(&mut self, node_id: NodeId) -> TypeId {
        match &self.compiler.ast_nodes[node_id.0] {
            AstNode::Block(block) => {
                // FIXME: probably could clean this up
                let block = block.clone();
                self.typecheck_block(node_id, &block);
                VOID_TYPE_ID
            }
            AstNode::Int => {
                self.compiler.node_types[node_id.0] = I64_TYPE_ID;
                I64_TYPE_ID
            }
            AstNode::Float => {
                self.compiler.node_types[node_id.0] = F64_TYPE_ID;
                F64_TYPE_ID
            }
            AstNode::True | AstNode::False => {
                self.compiler.node_types[node_id.0] = BOOL_TYPE_ID;
                BOOL_TYPE_ID
            }
            AstNode::String => {
                self.compiler.node_types[node_id.0] = STRING_TYPE_ID;
                STRING_TYPE_ID
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
                VOID_TYPE_ID
            }
            AstNode::Variable => {
                let var_id = self.find_variable_in_scope(node_id);

                if let Some(var_id) = var_id {
                    let variable = &self.compiler.variables[var_id.0];
                    self.compiler.node_types[node_id.0] = variable.ty;
                    variable.ty
                } else {
                    self.error("can't find variable", node_id);
                    UNKNOWN_TYPE_ID
                }
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                let lhs = *lhs;
                let rhs = *rhs;
                let op = *op;
                let lhs_ty = self.typecheck_node(lhs);
                let rhs_ty = self.typecheck_node(rhs);

                match &self.compiler.ast_nodes[op.0] {
                    AstNode::Plus | AstNode::Minus | AstNode::Multiply | AstNode::Divide => {
                        self.compiler.node_types[node_id.0] = lhs_ty;
                        lhs_ty
                    }
                    x => panic!("unsupported operator: {:?}", x),
                }
            }
            AstNode::Call { head, args } => {
                let head = *head;
                let args = args.clone();
                self.typecheck_call(head, &args)
            }
            AstNode::Fun { .. } => {
                // ignore here, since we checked this in an earlier pass
                VOID_TYPE_ID
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

        self.compiler.variables.push(Variable {
            ty,
            is_mutable,
            where_defined,
        });

        let var_id = self.compiler.variables.len() - 1;

        self.scope
            .last_mut()
            .expect("internal error: missing typechecking scope")
            .variables
            .insert(variable_name, VarId(var_id));
    }

    pub fn find_variable_in_scope(&self, variable_name: NodeId) -> Option<&VarId> {
        let name = &self.compiler.source
            [self.compiler.span_start[variable_name.0]..self.compiler.span_end[variable_name.0]];
        for scope in self.scope.iter().rev() {
            if let Some(value) = scope.variables.get(name) {
                return Some(value);
            }
        }

        None
    }

    pub fn find_function_in_scope(&self, function_name: NodeId) -> Option<&FunId> {
        let name = &self.compiler.source
            [self.compiler.span_start[function_name.0]..self.compiler.span_end[function_name.0]];
        for scope in self.scope.iter().rev() {
            if let Some(value) = scope.functions.get(name) {
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
