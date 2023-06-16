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
    pub ty: TypeId,
    pub is_mutable: bool,
    pub where_defined: NodeId,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Vec<u8>,
    pub var_id: VarId,
}

impl Param {
    pub fn new(name: Vec<u8>, var_id: VarId) -> Param {
        Param { name, var_id }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: NodeId,
    pub params: Vec<Param>,
    pub return_type: TypeId,
    pub body: NodeId,
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
        compiler.variables.push(Variable {
            ty: UNKNOWN_TYPE_ID,
            is_mutable: false,
            where_defined: NodeId(0),
        });
        compiler.functions.push(Function {
            name: NodeId(0),
            params: vec![Param::new(b"input".to_vec(), VarId(0))],
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
                Type::new(), // string
            ],
            scope,
        }
    }

    pub fn typecheck_typename(&mut self, ty: NodeId) -> TypeId {
        let name = self.compiler.get_source(ty);

        match name {
            b"i64" => I64_TYPE_ID,
            b"f64" => F64_TYPE_ID,
            b"string" => STRING_TYPE_ID,
            b"bool" => BOOL_TYPE_ID,
            b"void" => VOID_TYPE_ID,
            _ => {
                self.error(
                    &format!("unknown type: '{}'", String::from_utf8_lossy(name)),
                    ty,
                );
                UNKNOWN_TYPE_ID
            }
        }
    }

    pub fn typecheck_fun_predecl(
        &mut self,
        name: NodeId,
        params: NodeId,
        return_ty: Option<NodeId>,
        block: NodeId,
    ) -> FunId {
        let mut fun_params = vec![];
        let fun_name = self.compiler.source
            [self.compiler.span_start[name.0]..self.compiler.span_end[name.0]]
            .to_vec();

        //FIXME: remove clone?
        if let AstNode::Params(unchecked_params) = self.compiler.ast_nodes[params.0].clone() {
            for unchecked_param in unchecked_params {
                if let AstNode::Param { name, ty } = &self.compiler.ast_nodes[unchecked_param.0] {
                    let name = *name;
                    let param_name = self.compiler.get_source(name).to_vec();
                    let ty = *ty;
                    let ty = self.typecheck_typename(ty);

                    let var_id = self.define_variable(param_name.clone(), ty, false, name);
                    fun_params.push(Param::new(param_name, var_id));
                } else {
                    self.error("expected function parameter", unchecked_param);
                    break;
                }
            }
        } else {
            self.error("expected function parameters", params)
        }

        let return_type = if let Some(return_ty) = return_ty {
            self.typecheck_typename(return_ty)
        } else {
            VOID_TYPE_ID
        };

        self.compiler.functions.push(Function {
            name,
            params: fun_params,
            return_type,
            body: block,
        });

        let fun_id = self.compiler.functions.len() - 1;

        self.scope
            .last_mut()
            .expect("internal error: missing function scope")
            .functions
            .insert(fun_name, FunId(fun_id));

        FunId(fun_id)
    }

    pub fn typecheck_fun(&mut self, fun_id: FunId) {
        let Function { params, body, .. } = self.compiler.functions[fun_id.0].clone();

        self.enter_scope();

        for Param { name, var_id } in &params {
            self.add_variable_to_scope(name.clone(), *var_id)
        }

        self.typecheck_node(body);

        // FIXME: check return type

        self.exit_scope();
    }

    pub fn typecheck_block(&mut self, node_id: NodeId, nodes: &[NodeId]) {
        let mut funs = vec![];

        self.enter_scope();

        for node_id in nodes {
            if let AstNode::Fun {
                name,
                params,
                return_ty,
                block,
            } = &self.compiler.ast_nodes[node_id.0]
            {
                funs.push(self.typecheck_fun_predecl(*name, *params, *return_ty, *block));
            }
        }

        for fun in funs {
            self.typecheck_fun(fun);
        }

        for node_id in nodes {
            self.typecheck_node(*node_id);
        }
        self.compiler.node_types[node_id.0] = VOID_TYPE_ID;

        self.exit_scope()
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

            if fun_id.0 == 0 {
                // Just for now, special-case println
                self.compiler.fun_resolution.insert(head, *fun_id);
                for arg in args {
                    // TODO: add name-checking
                    let arg = *arg;

                    self.typecheck_node(arg);
                }

                return VOID_TYPE_ID;
            }

            if args.len() != params.len() {
                self.error(
                    &format!("expected {} args, found {}", params.len(), args.len()),
                    head,
                );
                return return_type;
            }

            // TODO: do we want to wait until all params are checked
            // before we mark this as resolved?
            self.compiler.fun_resolution.insert(head, *fun_id);

            for (arg, param) in args.iter().zip(params) {
                // TODO: add name-checking
                let arg = *arg;

                let arg_type = self.typecheck_node(arg);
                let variable = &self.compiler.variables[param.var_id.0];

                if !self.is_type_compatible(arg_type, variable.ty) {
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

                let name = self.compiler.get_source(variable_name);

                let var_id = self.define_variable(name.to_vec(), ty, is_mutable, node_id);

                self.compiler.var_resolution.insert(variable_name, var_id);

                VOID_TYPE_ID
            }
            AstNode::Variable => {
                let var_id = self.find_variable_in_scope(node_id);

                if let Some(var_id) = var_id {
                    let var_id = *var_id;
                    self.compiler.var_resolution.insert(node_id, var_id);

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
                        if lhs_ty != rhs_ty {
                            // FIXME: actually say the types
                            self.error("type mismatch during operation", op)
                        }
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
            AstNode::Statement(node_id) => {
                self.typecheck_node(*node_id);
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

        self.typecheck_node(NodeId(self.compiler.ast_nodes.len() - 1));

        self.compiler
    }

    pub fn add_variable_to_scope(&mut self, variable_name: Vec<u8>, var_id: VarId) {
        self.scope
            .last_mut()
            .expect("internal error: missing typechecking scope")
            .variables
            .insert(variable_name, var_id);
    }

    pub fn define_variable(
        &mut self,
        variable_name: Vec<u8>,
        ty: TypeId,
        is_mutable: bool,
        where_defined: NodeId,
    ) -> VarId {
        self.compiler.variables.push(Variable {
            ty,
            is_mutable,
            where_defined,
        });

        let var_id = VarId(self.compiler.variables.len() - 1);

        self.add_variable_to_scope(variable_name, var_id);

        var_id
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

    pub fn enter_scope(&mut self) {
        self.scope.push(Scope::new())
    }

    pub fn exit_scope(&mut self) {
        self.scope.pop();
    }

    pub fn error(&mut self, message: impl Into<String>, node_id: NodeId) {
        self.compiler.errors.push(SourceError {
            message: message.into(),

            node_id,
        });
    }
}
