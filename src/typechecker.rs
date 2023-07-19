use std::collections::HashMap;

use crate::{
    compiler::Compiler,
    errors::SourceError,
    parser::{AllocationLifetime, AllocationType, AstNode, NodeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunId(pub usize);

#[derive(Debug, PartialEq)]
pub enum Type {
    Unknown,
    Void,
    I64,
    F64,
    Bool,
    String,
    Struct(Vec<(Vec<u8>, TypeId)>),
    // Pointer(AllocationLifetime, AllocationType, TypeId),
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
    types: HashMap<Vec<u8>, TypeId>,
    expected_return_type: Option<TypeId>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            expected_return_type: None,
        }
    }
}

pub struct Typechecker {
    pub compiler: Compiler,
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

        compiler.types = vec![
            // hardwire in the core types before the user-defined types
            Type::Unknown,
            Type::Void,
            Type::I64,
            Type::F64,
            Type::Bool,
            Type::String,
        ];

        let mut scope = vec![Scope::new()];

        scope
            .last_mut()
            .expect("internal error: couldn't access function scope")
            .functions
            .insert(b"println".to_vec(), FunId(0));

        Self { compiler, scope }
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
                if let Some(type_id) = self.find_type_in_scope(ty) {
                    *type_id
                } else {
                    self.error(
                        &format!("unknown type: '{}'", String::from_utf8_lossy(name)),
                        ty,
                    );
                    UNKNOWN_TYPE_ID
                }
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
        let Function {
            params,
            body,
            return_type,
            ..
        } = self.compiler.functions[fun_id.0].clone();

        self.enter_scope();

        self.set_expected_return_type(return_type);

        for Param { name, var_id } in &params {
            self.add_variable_to_scope(name.clone(), *var_id)
        }

        self.typecheck_node(body);

        // FIXME: check return type

        self.exit_scope();
    }

    pub fn typecheck_struct(&mut self, name: NodeId, fields: Vec<(NodeId, NodeId)>) -> TypeId {
        let struct_name = self.compiler.get_source(name).to_vec();

        let mut output_fields = vec![];

        for (field_name, field_type) in fields {
            let field_name = self.compiler.get_source(field_name).to_vec();
            let field_type = self.typecheck_typename(field_type);

            output_fields.push((field_name, field_type));
        }

        self.compiler.types.push(Type::Struct(output_fields));

        let type_id = TypeId(self.compiler.types.len() - 1);

        self.add_type_to_scope(struct_name, type_id);

        type_id
    }

    pub fn typecheck_block(&mut self, node_id: NodeId, nodes: &[NodeId]) {
        let mut funs = vec![];

        self.enter_scope();

        for node_id in nodes {
            match &self.compiler.ast_nodes[node_id.0] {
                AstNode::Fun {
                    name,
                    params,
                    return_ty,
                    block,
                } => {
                    funs.push(self.typecheck_fun_predecl(*name, *params, *return_ty, *block));
                }

                AstNode::Struct { name, fields } => {
                    self.typecheck_struct(*name, fields.clone());
                }
                _ => {}
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

                match &self.compiler.ast_nodes[arg.0] {
                    AstNode::NamedValue { name, value } => {
                        let name = *name;
                        let value = *value;

                        let arg_ty = self.typecheck_node(value);

                        if !self
                            .is_type_compatible(arg_ty, self.compiler.variables[param.var_id.0].ty)
                        {
                            // FIXME: make this a better error
                            self.error("types incompatible with function", value);
                        }

                        let arg_name = self.compiler.get_source(name);

                        if arg_name != param.name {
                            self.error(
                                &format!(
                                    "expected name '{}'",
                                    String::from_utf8_lossy(&param.name)
                                ),
                                name,
                            )
                        }
                    }
                    _ => {
                        let arg_type = self.typecheck_node(arg);
                        let variable = &self.compiler.variables[param.var_id.0];

                        if !self.is_type_compatible(arg_type, variable.ty) {
                            // FIXME: make this a better type error
                            self.error("type mismatch for arg", arg);
                            return return_type;
                        }
                    }
                }
            }

            return_type
        } else {
            self.error("unknown function", head);
            UNKNOWN_TYPE_ID
        }
    }

    pub fn typecheck_node(&mut self, node_id: NodeId) -> TypeId {
        let node_type = match &self.compiler.ast_nodes[node_id.0] {
            AstNode::Block(block) => {
                // FIXME: probably could clean this up
                let block = block.clone();
                self.typecheck_block(node_id, &block);
                VOID_TYPE_ID
            }
            AstNode::Int => I64_TYPE_ID,
            AstNode::Float => F64_TYPE_ID,
            AstNode::True | AstNode::False => BOOL_TYPE_ID,
            AstNode::String => STRING_TYPE_ID,
            AstNode::Let {
                variable_name,
                initializer,
                is_mutable,
                ty,
            } => {
                let variable_name = *variable_name;
                let initializer = *initializer;
                let is_mutable = *is_mutable;
                let ty = *ty;

                let name = self.compiler.get_source(variable_name).to_vec();

                let initializer_ty = self.typecheck_node(initializer);

                if let Some(ty) = ty {
                    let ty = self.typecheck_typename(ty);
                    if !self.is_type_compatible(ty, initializer_ty) {
                        self.error("initializer and given type do not match", initializer);
                    }
                }

                let var_id =
                    self.define_variable(name.to_vec(), initializer_ty, is_mutable, node_id);

                self.compiler.var_resolution.insert(variable_name, var_id);

                VOID_TYPE_ID
            }
            AstNode::Variable => {
                let var_id = self.find_variable_in_scope(node_id);

                if let Some(var_id) = var_id {
                    let var_id = *var_id;
                    self.compiler.var_resolution.insert(node_id, var_id);

                    let variable = &self.compiler.variables[var_id.0];
                    variable.ty
                } else {
                    self.error("can't find variable", node_id);
                    UNKNOWN_TYPE_ID
                }
            }
            AstNode::MemberAccess { target, field } => {
                let target = *target;
                let field = *field;

                let type_id = self.typecheck_node(target);

                match &self.compiler.types[type_id.0] {
                    Type::Struct(fields) => {
                        let field_name = self.compiler.get_source(field);
                        for known_field in fields {
                            if known_field.0 == field_name {
                                self.compiler.node_types[node_id.0] = known_field.1;
                                return known_field.1;
                            }
                        }
                        self.error("unknown field", field);
                        UNKNOWN_TYPE_ID
                    }
                    // Type::Pointer(_, _, type_id) => match &self.compiler.types[type_id.0] {
                    //     Type::Struct(fields) => {
                    //         let field_name = self.compiler.get_source(field);
                    //         for known_field in fields {
                    //             if known_field.0 == field_name {
                    //                 self.compiler.node_types[node_id.0] = known_field.1;
                    //                 return known_field.1;
                    //             }
                    //         }
                    //         self.error("unknown field", field);
                    //         UNKNOWN_TYPE_ID
                    //     }
                    //     _ => {
                    //         self.error("field access on non-struct type", target);
                    //         UNKNOWN_TYPE_ID
                    //     }
                    // },
                    _ => {
                        self.error("field access on non-struct type", target);
                        UNKNOWN_TYPE_ID
                    }
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
                        lhs_ty
                    }
                    AstNode::Assignment
                    | AstNode::AddAssignment
                    | AstNode::SubtractAssignment
                    | AstNode::MultiplyAssignment
                    | AstNode::DivideAssignment => {
                        let var_id = self.compiler.var_resolution.get(&lhs);

                        if let Some(var_id) = var_id {
                            let var = &self.compiler.variables[var_id.0];
                            if !var.is_mutable {
                                self.error("variable is not mutable", lhs)
                            }
                        } else {
                            self.error("expected variable on left-hand side of assignment", lhs)
                        }

                        if lhs_ty != rhs_ty {
                            // FIXME: actually say the types
                            self.error("type mismatch during operation", op)
                        }
                        VOID_TYPE_ID
                    }
                    x => panic!("unsupported operator: {:?}", x),
                }
            }
            AstNode::Call { head, args } => {
                let head = *head;
                let args = args.clone();
                self.typecheck_call(head, &args)
            }
            AstNode::New(allocation_lifetime, allocation_type, allocation_node_id) => {
                let allocation_lifetime = *allocation_lifetime;
                let allocation_type = *allocation_type;
                let allocation_node_id = *allocation_node_id;
                self.typecheck_allocation(allocation_lifetime, allocation_type, allocation_node_id)
            }
            AstNode::Return(return_expr) => {
                let return_expr = *return_expr;
                let expected_type = self.find_expected_return_type();

                if let Some(return_expr) = return_expr {
                    let expr_type = self.typecheck_node(return_expr);

                    if let Some(expected_type) = expected_type {
                        if !self.is_type_compatible(expected_type, expr_type) {
                            // FIXME: actually print the types
                            self.error("incompatible type at return", return_expr);
                        }
                    } else {
                        self.error("return used outside of a function", return_expr);
                    }
                } else if expected_type.is_some() {
                    self.error("return needs value", node_id)
                } else {
                    self.error("return used outside of a function", node_id);
                }

                VOID_TYPE_ID
            }
            AstNode::Fun { .. } | AstNode::Struct { .. } => {
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
        };

        self.compiler.node_types[node_id.0] = node_type;

        node_type
    }

    pub fn typecheck_allocation(
        &mut self,
        allocation_lifetime: AllocationLifetime,
        allocation_type: AllocationType,
        node_id: NodeId,
    ) -> TypeId {
        if let AstNode::Call { head, .. } = &self.compiler.ast_nodes[node_id.0] {
            if let Some(type_id) = self.find_type_in_scope(*head) {
                // let type_id = *type_id;
                // self.find_or_create_type(Type::Pointer(
                //     allocation_lifetime,
                //     allocation_type,
                //     type_id,
                // ))
                *type_id
            } else {
                self.error("unknown type in allocation", *head);
                UNKNOWN_TYPE_ID
            }
        } else {
            self.error("expected an allocation call", node_id);
            UNKNOWN_TYPE_ID
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

    pub fn add_type_to_scope(&mut self, variable_name: Vec<u8>, type_id: TypeId) {
        self.scope
            .last_mut()
            .expect("internal error: missing typechecking scope")
            .types
            .insert(variable_name, type_id);
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

    pub fn find_type_in_scope(&self, type_name: NodeId) -> Option<&TypeId> {
        let name = &self.compiler.source
            [self.compiler.span_start[type_name.0]..self.compiler.span_end[type_name.0]];
        for scope in self.scope.iter().rev() {
            if let Some(value) = scope.types.get(name) {
                return Some(value);
            }
        }

        None
    }

    pub fn find_or_create_type(&mut self, ty: Type) -> TypeId {
        for (idx, t) in self.compiler.types.iter().enumerate() {
            if &ty == t {
                return TypeId(idx);
            }
        }

        self.compiler.types.push(ty);

        TypeId(self.compiler.types.len() - 1)
    }

    pub fn find_expected_return_type(&self) -> Option<TypeId> {
        for scope in self.scope.iter().rev() {
            if let Some(ret_type) = scope.expected_return_type {
                return Some(ret_type);
            }
        }

        None
    }

    pub fn set_expected_return_type(&mut self, expected_type: TypeId) {
        let frame = self
            .scope
            .last_mut()
            .expect("internal error: missing expected scope frame");
        frame.expected_return_type = Some(expected_type)
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
