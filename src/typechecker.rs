use std::collections::HashMap;

use crate::{
    compiler::{CallTarget, CaseOffset, Compiler},
    errors::SourceError,
    parser::{AllocationType, AstNode, BlockId, NodeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unknown,
    Void,
    I64,
    F64,
    Bool,
    Range(TypeId),
    String,
    Struct {
        generic_params: Vec<TypeId>,
        fields: Vec<(Vec<u8>, TypeId)>,
        methods: Vec<FunId>,
        is_allocator: bool,
    },
    Enum {
        generic_params: Vec<TypeId>,
        variants: Vec<EnumVariant>,
        methods: Vec<FunId>,
    },
    Pointer {
        allocation_type: AllocationType,
        optional: bool,
        target: TypeId,
    },
    TypeVariable,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariant {
    Simple {
        name: Vec<u8>,
    },
    Single {
        name: Vec<u8>,
        param: TypeId,
    },
    Struct {
        name: Vec<u8>,
        params: Vec<(Vec<u8>, TypeId)>,
    },
}

#[derive(Debug)]
pub struct Variable {
    pub name: NodeId,
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
pub const RANGE_I64_TYPE_ID: TypeId = TypeId(5);
pub const STRING_TYPE_ID: TypeId = TypeId(6);

impl Typechecker {
    pub fn new(mut compiler: Compiler) -> Self {
        // temporarily - let's add `println` for now, to get examples to typecheck
        compiler.variables.push(Variable {
            name: NodeId(0),
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

        // hardwire in the core types before the user-defined types
        compiler.push_type(Type::Unknown);
        compiler.push_type(Type::Void);
        compiler.push_type(Type::I64);
        compiler.push_type(Type::F64);
        compiler.push_type(Type::Bool);
        compiler.push_type(Type::Range(I64_TYPE_ID));
        compiler.push_type(Type::String);

        let mut scope = vec![Scope::new()];

        scope
            .last_mut()
            .expect("internal error: couldn't access function scope")
            .functions
            .insert(b"println".to_vec(), FunId(0));

        Self { compiler, scope }
    }

    pub fn typecheck_typename(&mut self, node_id: NodeId) -> TypeId {
        let AstNode::Type { name, optional, ..} = self.compiler.get_node(node_id) else {
            self.error("expected type name", node_id);
            return VOID_TYPE_ID;
        };

        let name_node_id = *name;
        let optional = *optional;

        let name = self.compiler.get_source(name_node_id);

        match name {
            b"i64" => I64_TYPE_ID,
            b"f64" => F64_TYPE_ID,
            b"string" => STRING_TYPE_ID,
            b"bool" => BOOL_TYPE_ID,
            b"void" => VOID_TYPE_ID,
            _ => {
                if let Some(type_id) = self.find_type_in_scope(name_node_id) {
                    if self.is_type_variable(*type_id) {
                        *type_id
                    } else {
                        // Assume custom types are pointers
                        self.compiler.find_or_create_type(Type::Pointer {
                            allocation_type: AllocationType::Normal,
                            optional,
                            target: *type_id,
                        })
                    }
                } else {
                    self.error(
                        &format!("unknown type: '{}'", String::from_utf8_lossy(name)),
                        node_id,
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
        if let AstNode::Params(unchecked_params) = self.compiler.get_node(params).clone() {
            for unchecked_param in unchecked_params {
                if let AstNode::Param {
                    name,
                    ty,
                    is_mutable,
                } = &self.compiler.get_node(unchecked_param)
                {
                    let name = *name;
                    let param_name = self.compiler.get_source(name).to_vec();
                    let ty = *ty;
                    let is_mutable = *is_mutable;
                    let ty = self.typecheck_typename(ty);

                    let var_id = self.define_variable(name, ty, is_mutable, name);
                    self.compiler.set_node_type(name, ty);
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
            self.add_variable_to_scope(name.clone(), *var_id);
        }

        self.typecheck_node(body);

        // FIXME: check return type

        self.exit_scope();
    }

    pub fn typecheck_enum(
        &mut self,
        typename: NodeId,
        cases: Vec<NodeId>,
        methods: Vec<NodeId>,
    ) -> TypeId {
        let AstNode::Type { name, params, .. } = self.compiler.get_node(typename) else {
            panic!("internal error: enum does not have type as name");
        };

        let name = *name;
        let params = *params;

        let mut generic_params = vec![];

        self.enter_scope();

        if let Some(params) = params {
            let AstNode::Params(params) = self.compiler.get_node(params) else {
                panic!("internal error: enum generic params are not proper ast node");
            };

            let params = params.clone();

            for param in params {
                let type_id = self.compiler.fresh_type_variable();

                generic_params.push(type_id);

                let type_var_name = self.compiler.get_source(param).to_vec();

                self.add_type_to_scope(type_var_name, type_id);
            }
        }

        let enum_name = self.compiler.get_source(name).to_vec();

        let mut output_cases = vec![];

        for enum_case in cases {
            if let AstNode::EnumCase { name, payload } = self.compiler.get_node(enum_case) {
                let case_name = self.compiler.get_source(*name).to_vec();

                match payload {
                    Some(payload) => {
                        if payload.is_empty() {
                            self.error("missing payload in enum case", *name);
                            break;
                        }

                        match &self.compiler.get_node(payload[0]) {
                            AstNode::NamedValue { .. } => {
                                let mut fields = vec![];
                                let payload = payload.clone();
                                for item in payload {
                                    if let AstNode::NamedValue { name, value } =
                                        &self.compiler.get_node(item)
                                    {
                                        let name = *name;
                                        let value = *value;

                                        let field_name = self.compiler.get_source(name).to_vec();

                                        let type_id = self.typecheck_typename(value);

                                        fields.push((field_name, type_id));
                                    } else {
                                        self.error(
                                            "expected 'name: type' for each field in enum case",
                                            item,
                                        );
                                    }
                                }

                                output_cases.push(EnumVariant::Struct {
                                    name: case_name,
                                    params: fields,
                                });
                            }
                            AstNode::Type { .. } => {
                                let type_id = self.typecheck_typename(payload[0]);

                                output_cases.push(EnumVariant::Single {
                                    name: case_name,
                                    param: type_id,
                                });
                            }
                            _ => {
                                self.error("unexpected node in enum cases", payload[0]);
                            }
                        }
                    }
                    None => {
                        output_cases.push(EnumVariant::Simple { name: case_name });
                    }
                }
            } else {
                self.error("expect enum case inside of enum", enum_case)
            }
        }

        let type_id = self.compiler.push_type(Type::Enum {
            generic_params,
            variants: output_cases,
            methods: vec![],
        });

        self.compiler.push_type(Type::Pointer {
            allocation_type: AllocationType::Normal,
            optional: false,
            target: type_id,
        });

        self.add_type_to_scope(enum_name.clone(), type_id);

        if !methods.is_empty() {
            self.enter_scope();

            self.add_type_to_scope(b"self".to_vec(), type_id);

            let mut fun_ids = vec![];

            for method in methods {
                let AstNode::Fun { name, params, return_ty, block } = self.compiler.get_node(method) else {
                    self.error("internal error: can't find method definition during typecheck", method);
                    return VOID_TYPE_ID;
                };
                let name = *name;
                let params = *params;
                let return_ty = *return_ty;
                let block = *block;

                fun_ids.push(self.typecheck_fun_predecl(name, params, return_ty, block));
            }

            let Type::Struct {
                methods, ..
            } = self.compiler.get_type_mut(type_id) else {
                panic!("internal error: previously inserted struct can't be found");
            };

            *methods = fun_ids.clone();

            for fun_id in &fun_ids {
                self.typecheck_fun(*fun_id);
            }

            self.exit_scope();
        }

        self.exit_scope();

        self.add_type_to_scope(enum_name, type_id);

        type_id
    }

    pub fn typecheck_struct(
        &mut self,
        typename: NodeId,
        fields: Vec<(NodeId, NodeId)>,
        methods: Vec<NodeId>,
        is_allocator: bool,
    ) -> TypeId {
        let AstNode::Type { name, params, .. } = self.compiler.get_node(typename) else {
            panic!("internal error: enum does not have type as name");
        };

        let name = *name;
        let params = *params;

        let mut generic_params = vec![];

        self.enter_scope();

        if let Some(params) = params {
            let AstNode::Params(params) = self.compiler.get_node(params) else {
                panic!("internal error: enum generic params are not proper ast node");
            };

            let params = params.clone();

            for param in params {
                let type_id = self.compiler.fresh_type_variable();

                generic_params.push(type_id);

                let type_var_name = self.compiler.get_source(param).to_vec();

                self.add_type_to_scope(type_var_name, type_id);
            }
        }

        let struct_name = self.compiler.get_source(name).to_vec();

        let type_id = self.compiler.push_type(Type::Struct {
            generic_params,
            fields: vec![],
            methods: vec![],
            is_allocator,
        });

        self.add_type_to_scope(struct_name.clone(), type_id);

        let mut output_fields = vec![];

        for (field_name, field_type) in fields {
            let field_name = self.compiler.get_source(field_name).to_vec();
            let field_type = self.typecheck_typename(field_type);

            output_fields.push((field_name, field_type));
        }

        let Type::Struct {
            fields, ..
        } = &mut self.compiler.get_type_mut(type_id) else {
            panic!("internal error: previously inserted struct can't be found");
        };

        *fields = output_fields;

        // self.compiler
        //     .types
        //     .push(Type::Pointer { allocation_type: AllocationType::Normal, type_id));

        if !methods.is_empty() {
            self.enter_scope();

            self.add_type_to_scope(b"self".to_vec(), type_id);

            let mut fun_ids = vec![];

            for method in methods {
                let AstNode::Fun { name, params, return_ty, block } = self.compiler.get_node(method) else {
                    self.error("internal error: can't find method definition during typecheck", method);
                    return VOID_TYPE_ID;
                };
                let name = *name;
                let params = *params;
                let return_ty = *return_ty;
                let block = *block;

                fun_ids.push(self.typecheck_fun_predecl(name, params, return_ty, block));
            }

            let Type::Struct {
                methods, ..
            } = self.compiler.get_type_mut(type_id) else {
                panic!("internal error: previously inserted struct can't be found");
            };

            *methods = fun_ids.clone();

            for fun_id in &fun_ids {
                self.typecheck_fun(*fun_id);
            }

            self.exit_scope();
        }
        self.exit_scope();

        self.add_type_to_scope(struct_name, type_id);

        type_id
    }

    pub fn typecheck_block(&mut self, node_id: NodeId, block_id: BlockId) {
        let mut funs = vec![];

        self.enter_scope();

        // FIXME: sad we have to clone here
        let block = self.compiler.blocks[block_id.0].clone();

        for node_id in &block.nodes {
            match &self.compiler.get_node(*node_id) {
                AstNode::Fun {
                    name,
                    params,
                    return_ty,
                    block,
                } => {
                    funs.push(self.typecheck_fun_predecl(*name, *params, *return_ty, *block));
                }

                AstNode::Struct {
                    typename: name,
                    fields,
                    methods,
                    is_allocator,
                } => {
                    self.typecheck_struct(*name, fields.clone(), methods.clone(), *is_allocator);
                }

                AstNode::Enum {
                    typename,
                    cases,
                    methods,
                } => {
                    self.typecheck_enum(*typename, cases.clone(), methods.clone());
                }
                _ => {}
            }
        }

        for fun in funs {
            self.typecheck_fun(fun);
        }

        for node_id in &block.nodes {
            self.typecheck_node(*node_id);
        }
        self.compiler.set_node_type(node_id, VOID_TYPE_ID);

        self.exit_scope()
    }

    pub fn is_type_variable(&self, type_id: TypeId) -> bool {
        matches!(self.compiler.get_type(type_id), Type::TypeVariable)
    }

    pub fn is_type_compatible(&self, lhs: TypeId, rhs: TypeId) -> bool {
        match (self.compiler.get_type(lhs), self.compiler.get_type(rhs)) {
            (
                Type::Pointer {
                    allocation_type: allocation_type_lhs,
                    optional: optional_lhs,
                    target: target_lhs,
                },
                Type::Pointer {
                    allocation_type: allocation_type_rhs,
                    optional: optional_rhs,
                    target: target_rhs,
                },
            ) => {
                allocation_type_lhs == allocation_type_rhs
                    && target_lhs == target_rhs
                    && (*optional_lhs || optional_lhs == optional_rhs)
            }
            _ => lhs == rhs,
        }
    }

    pub fn typecheck_call_with_fun_id(
        &mut self,
        name: NodeId,
        fun_id: FunId,
        args: &[NodeId],
    ) -> TypeId {
        let Function {
            params,
            return_type,
            ..
        } = &self.compiler.functions[fun_id.0];

        let params = params.clone();
        let return_type = *return_type;

        if fun_id.0 == 0 {
            // Just for now, special-case println
            self.compiler
                .call_resolution
                .insert(name, CallTarget::Function(fun_id));
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
                name,
            );
            return return_type;
        }

        // TODO: do we want to wait until all params are checked
        // before we mark this as resolved?
        self.compiler
            .call_resolution
            .insert(name, CallTarget::Function(fun_id));

        for (arg, param) in args.iter().zip(params) {
            // TODO: add name-checking
            let arg = *arg;

            match &self.compiler.get_node(arg) {
                AstNode::NamedValue { name, value } => {
                    let name = *name;
                    let value = *value;

                    // Set up expected type for inference. Note: if we find concrete values
                    // this inference type will be replaced by the concrete type.
                    self.compiler
                        .set_node_type(value, self.compiler.variables[param.var_id.0].ty);

                    let arg_ty = self.typecheck_node(value);

                    if !self.is_type_compatible(arg_ty, self.compiler.variables[param.var_id.0].ty)
                    {
                        // FIXME: make this a better error
                        self.error("types incompatible with function", value);
                    }

                    let arg_name = self.compiler.get_source(name);

                    if arg_name != param.name {
                        self.error(
                            &format!("expected name '{}'", String::from_utf8_lossy(&param.name)),
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
    }

    pub fn typecheck_call(&mut self, head: NodeId, args: &[NodeId]) -> TypeId {
        if let Some(fun_id) = self.find_function_in_scope(head) {
            self.typecheck_call_with_fun_id(head, *fun_id, args)
        } else {
            self.error("unknown function", head);
            UNKNOWN_TYPE_ID
        }
    }

    pub fn typecheck_node(&mut self, node_id: NodeId) -> TypeId {
        let node_type = match &self.compiler.get_node(node_id) {
            AstNode::Block(block_id) => {
                self.typecheck_block(node_id, *block_id);
                VOID_TYPE_ID
            }
            AstNode::Int => I64_TYPE_ID,
            AstNode::Float => F64_TYPE_ID,
            AstNode::True | AstNode::False => BOOL_TYPE_ID,
            AstNode::None => {
                // FIXME: check that this is an optional type
                let type_id = self.compiler.get_node_type(node_id);

                match self.compiler.get_type(type_id) {
                    Type::Pointer { optional, .. } => {
                        if *optional {
                            // Success, none can point to an optional pointer
                        } else {
                            self.error("'none' used on required (non-optional) pointer", node_id);
                        }
                    }
                    x => {
                        self.error(
                            format!("'none' requires pointer type, found: {:?}", x),
                            node_id,
                        );
                    }
                }

                type_id
            }
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

                let initializer_ty = self.typecheck_node(initializer);

                let var_id = if let Some(ty) = ty {
                    let ty = self.typecheck_typename(ty);
                    if !self.is_type_compatible(ty, initializer_ty) {
                        self.error("initializer and given type do not match", initializer);
                    }
                    self.define_variable(variable_name, ty, is_mutable, node_id)
                } else {
                    self.define_variable(variable_name, initializer_ty, is_mutable, node_id)
                };

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
                    let name = self.compiler.get_source(node_id);

                    // Reserved name for synthetic self variables
                    if name == b"." {
                        self.error("can't find 'self' variable", node_id);
                    } else {
                        self.error("can't find variable", node_id);
                    }
                    UNKNOWN_TYPE_ID
                }
            }
            AstNode::MemberAccess { target, field } => {
                let target = *target;
                let field = *field;

                let type_id = self.typecheck_node(target);

                let type_id = self.get_underlying_type_id(type_id);

                match self.compiler.get_type(type_id) {
                    Type::Struct { fields, .. } => {
                        let field_name = self.compiler.get_source(field);
                        for known_field in fields {
                            if known_field.0 == field_name {
                                let type_id = known_field.1;
                                self.compiler.set_node_type(node_id, type_id);
                                self.compiler.set_node_type(field, type_id);
                                return type_id;
                            }
                        }
                        self.error("unknown field", field);
                        UNKNOWN_TYPE_ID
                    }
                    _ => {
                        self.error("field access on non-struct type", target);
                        UNKNOWN_TYPE_ID
                    }
                }
            }
            AstNode::MethodCall { target, call } => {
                self.typecheck_method_call(*target, *call, node_id)
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                let lhs = *lhs;
                let rhs = *rhs;
                let op = *op;

                match self.compiler.get_node(op) {
                    AstNode::Plus | AstNode::Minus | AstNode::Multiply | AstNode::Divide => {
                        let lhs_ty = self.typecheck_node(lhs);
                        let rhs_ty = self.typecheck_node(rhs);
                        if lhs_ty != rhs_ty {
                            self.error(
                                format!(
                                    "type mismatch during operation. expected: {:?}, found: {:?}",
                                    self.compiler.get_type(lhs_ty),
                                    self.compiler.get_type(rhs_ty),
                                ),
                                op,
                            )
                        }
                        lhs_ty
                    }
                    AstNode::Equal | AstNode::NotEqual => {
                        let lhs_ty = self.typecheck_node(lhs);

                        // use a quick inference for comparison with 'none'
                        if matches!(self.compiler.get_node(rhs), AstNode::None) {
                            self.compiler.set_node_type(rhs, lhs_ty);
                        }
                        let rhs_ty = self.typecheck_node(rhs);
                        if !self.is_type_compatible(lhs_ty, rhs_ty) {
                            self.error(
                                format!(
                                    "type mismatch during operation. expected: {:?}, found: {:?}",
                                    self.compiler.get_type(lhs_ty),
                                    self.compiler.get_type(rhs_ty),
                                ),
                                op,
                            )
                        }
                        BOOL_TYPE_ID
                    }
                    AstNode::LessThan
                    | AstNode::LessThanOrEqual
                    | AstNode::GreaterThan
                    | AstNode::GreaterThanOrEqual => {
                        let lhs_ty = self.typecheck_node(lhs);
                        let rhs_ty = self.typecheck_node(rhs);
                        if !self.is_type_compatible(lhs_ty, rhs_ty) {
                            self.error(
                                format!(
                                    "type mismatch during operation. expected: {:?}, found: {:?}",
                                    self.compiler.get_type(lhs_ty),
                                    self.compiler.get_type(rhs_ty),
                                ),
                                op,
                            )
                        }
                        BOOL_TYPE_ID
                    }
                    AstNode::Assignment
                    | AstNode::AddAssignment
                    | AstNode::SubtractAssignment
                    | AstNode::MultiplyAssignment
                    | AstNode::DivideAssignment => {
                        let lhs_ty = self.typecheck_lvalue(lhs);
                        let rhs_ty = self.typecheck_node(rhs);

                        if !self.is_type_compatible(lhs_ty, rhs_ty) {
                            self.error(
                                format!(
                                    "type mismatch during operation. expected: {:?}, found: {:?}",
                                    self.compiler.get_type(lhs_ty),
                                    self.compiler.get_type(rhs_ty),
                                ),
                                op,
                            );
                        }
                        VOID_TYPE_ID
                    }
                    x => panic!("unsupported operator: {:?}", x),
                }
            }
            AstNode::Range { lhs, rhs } => {
                let lhs = *lhs;
                let rhs = *rhs;

                let lhs_type = self.typecheck_node(lhs);
                let rhs_type = self.typecheck_node(rhs);

                if lhs_type != I64_TYPE_ID {
                    self.error("expected i64 in range", lhs);
                }

                if rhs_type != I64_TYPE_ID {
                    self.error("expected i64 in range", rhs);
                }

                RANGE_I64_TYPE_ID
            }
            AstNode::NamespacedLookup { namespace, item } => {
                self.typecheck_namespaced_lookup(*namespace, *item)
            }
            AstNode::Call { head, args } => {
                let head = *head;
                let args = args.clone();
                self.typecheck_call(head, &args)
            }
            AstNode::New(allocation_type, allocation_node_id) => {
                let allocation_type = *allocation_type;
                let allocation_node_id = *allocation_node_id;
                self.typecheck_allocation(allocation_type, allocation_node_id)
            }
            AstNode::NamedValue { value, .. } => self.typecheck_node(*value),
            AstNode::Return(return_expr) => {
                let return_expr = *return_expr;
                let expected_type = self.find_expected_return_type();

                if let Some(return_expr) = return_expr {
                    let expr_type = self.typecheck_node(return_expr);

                    if let Some(expected_type) = expected_type {
                        if !self.is_type_compatible(expected_type, expr_type) {
                            // FIXME: actually print the types

                            self.error(
                                format!(
                                    "incompatible type at return, found: {:?} expected: {:?}",
                                    self.compiler.get_type(expr_type),
                                    self.compiler.get_type(expected_type)
                                ),
                                return_expr,
                            );
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
            AstNode::If {
                condition,
                then_block,
                else_expression,
            } => self.typecheck_if(*condition, *then_block, *else_expression),
            AstNode::While { condition, block } => {
                let condition = *condition;
                let block = *block;

                self.typecheck_node(condition);
                self.typecheck_node(block);

                if self.compiler.get_node_type(condition) != BOOL_TYPE_ID {
                    self.error("condition not a boolean expression", condition);
                }

                self.compiler.get_node_type(block)
            }
            AstNode::For {
                variable,
                range,
                block,
            } => {
                let variable = *variable;
                let range = *range;
                let block = *block;

                let range_type = self.typecheck_node(range);

                if matches!(self.compiler.get_type(range_type), Type::Range(I64_TYPE_ID)) {
                    self.enter_scope();

                    let var_id = self.define_variable(variable, I64_TYPE_ID, true, variable);
                    self.compiler.var_resolution.insert(variable, var_id);

                    self.typecheck_node(block);

                    self.exit_scope();
                } else {
                    self.error("expected range in for loop", range);
                }

                VOID_TYPE_ID
            }
            AstNode::Match { target, match_arms } => {
                self.typecheck_match(*target, match_arms.clone())
            }
            AstNode::Fun { .. } | AstNode::Struct { .. } | AstNode::Enum { .. } => {
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

        self.compiler.set_node_type(node_id, node_type);

        node_type
    }

    pub fn typecheck_lvalue(&mut self, lvalue: NodeId) -> TypeId {
        match self.compiler.get_node(lvalue) {
            AstNode::Variable => {
                self.typecheck_node(lvalue);
                let var_id = self.compiler.var_resolution.get(&lvalue);

                if let Some(var_id) = var_id {
                    let var = &self.compiler.variables[var_id.0];
                    let ty = var.ty;
                    if !var.is_mutable {
                        if self.compiler.get_source(lvalue) == b"." {
                            self.error("'self' variable is not mutable", lvalue);
                        } else {
                            self.error("variable is not mutable", lvalue);
                        }
                    }
                    ty
                } else {
                    self.error(
                        "internal error: variable unresolved when checking lvalue",
                        lvalue,
                    );
                    VOID_TYPE_ID
                }
            }
            AstNode::MemberAccess { target, field } => {
                let target = *target;
                let field = *field;

                let head_type_id = self.typecheck_lvalue(target);

                let field_name = self.compiler.get_source(field);

                let target_type_id = self.get_underlying_type_id(head_type_id);

                match self.compiler.get_type(target_type_id) {
                    Type::Struct { fields, .. } => {
                        for f in fields {
                            if f.0 == field_name {
                                return f.1;
                            }
                        }
                        self.error("could not find field", field);
                        VOID_TYPE_ID
                    }
                    x => {
                        self.error(
                            format!("field access only supported on structs, not {:?}", x),
                            field,
                        );
                        VOID_TYPE_ID
                    }
                }
            }
            x => {
                self.error(
                    format!("unsupported lvalue, needs variable or field, found {:?}", x),
                    lvalue,
                );
                VOID_TYPE_ID
            }
        }
    }

    pub fn typecheck_allocation(
        &mut self,
        allocation_type: AllocationType,
        node_id: NodeId,
    ) -> TypeId {
        if let AstNode::Call { head, args } = self.compiler.get_node(node_id) {
            // FIXME: remove clone
            let head = *head;
            let args = args.clone();

            let Some(type_id) = self.find_type_in_scope(head) else {
                self.error("unknown type in allocation", head);
                return UNKNOWN_TYPE_ID
            };

            let type_id = *type_id;
            let output_type = self.compiler.find_or_create_type(Type::Pointer {
                allocation_type,
                optional: false,
                target: type_id,
            });

            let type_id = self.get_underlying_type_id(type_id);

            let mut replacements = vec![];

            match &self.compiler.get_type(type_id) {
                Type::Struct { fields, .. } => {
                    let fields = fields.clone();

                    'arg: for arg in args {
                        let AstNode::NamedValue { name, value } = self.compiler.get_node(arg) else {
                            self.error("unexpected argument in allocation", arg);
                            return UNKNOWN_TYPE_ID
                        };

                        let name = *name;
                        let value = *value;

                        let field_name = self.compiler.get_source(name);
                        for known_field in &fields {
                            if known_field.0 == field_name {
                                let known_field_type = known_field.1;

                                if self.is_type_variable(known_field_type) {
                                    let value_type = self.typecheck_node(value);

                                    replacements.push((known_field_type, value_type));

                                    self.compiler.set_node_type(arg, value_type);

                                    // Set up expected type for inference. Note: if we find concrete values
                                    // this inference type will be replaced by the concrete type.
                                    self.compiler.set_node_type(value, value_type);
                                } else {
                                    self.compiler.set_node_type(arg, known_field_type);

                                    // Set up expected type for inference. Note: if we find concrete values
                                    // this inference type will be replaced by the concrete type.
                                    self.compiler.set_node_type(value, known_field_type);
                                    let value_type = self.typecheck_node(value);

                                    if !self.is_type_compatible(known_field_type, value_type) {
                                        self.error(format!("incompatible type for argument, found: {:?}, expected: {:?}",
                                            self.compiler.get_type(known_field_type), self.compiler.get_type(value_type)), value)
                                    }
                                };

                                continue 'arg;
                            }
                        }
                        self.error("unknown field", name);
                        return UNKNOWN_TYPE_ID;
                    }
                }
                _ => {
                    self.error("internal error: allocation of non-struct type", node_id);
                    return UNKNOWN_TYPE_ID;
                }
            }

            let output_type = if !replacements.is_empty() {
                let type_id = self.instantiate_generic(type_id, &replacements);

                self.compiler.find_or_create_type(Type::Pointer {
                    allocation_type,
                    optional: false,
                    target: type_id,
                })
            } else {
                output_type
            };

            output_type
        } else {
            self.error("expected an allocation call", node_id);
            UNKNOWN_TYPE_ID
        }
    }

    pub fn typecheck_namespaced_lookup(&mut self, namespace: NodeId, item: NodeId) -> TypeId {
        let type_id = self.find_type_in_scope(namespace);

        let Some(type_id) = type_id else {
            self.error("could not find namespace", namespace);
            return VOID_TYPE_ID;
        };

        let mut type_id = *type_id;

        match self.compiler.get_type(type_id) {
            Type::Struct { methods, .. } => {
                let AstNode::Call { head, args } = self.compiler.get_node(item) else {
                    self.error("expected static method call on struct", item);
                    return VOID_TYPE_ID;
                };

                let head = *head;
                let args = args.clone();

                let call_name = self.compiler.get_source(head);

                for method in methods {
                    let method_name = self
                        .compiler
                        .get_source(self.compiler.functions[method.0].name);
                    if method_name == call_name {
                        return self.typecheck_call_with_fun_id(head, *method, &args);
                    }
                }
            }
            Type::Enum {
                variants: cases, ..
            } => {
                // FIXME: remove clone
                let cases = cases.clone();

                match self.compiler.get_node(item) {
                    AstNode::Call { head, args } => {
                        // FIXME: remove clone
                        let head = *head;
                        let args = args.clone();
                        let case_name = self.compiler.get_source(head);

                        for (case_offset, case) in cases.iter().enumerate() {
                            match case {
                                EnumVariant::Single { name, param } => {
                                    if name == case_name {
                                        let param = *param;
                                        if args.len() == 1 {
                                            let arg_type_id = self.typecheck_node(args[0]);

                                            if self.is_type_variable(param) {
                                                type_id = self.instantiate_generic(
                                                    type_id,
                                                    &[(param, arg_type_id)],
                                                );
                                            } else if !self.is_type_compatible(param, arg_type_id) {
                                                self.error(
                                                    "incompatible types for enum case",
                                                    args[0],
                                                );
                                                return VOID_TYPE_ID;
                                            }

                                            self.compiler.call_resolution.insert(
                                                head,
                                                CallTarget::EnumConstructor(
                                                    type_id,
                                                    CaseOffset(case_offset),
                                                ),
                                            );

                                            return self.compiler.find_or_create_type(
                                                Type::Pointer {
                                                    allocation_type: AllocationType::Normal,
                                                    optional: false,
                                                    target: type_id,
                                                },
                                            );
                                        } else {
                                            self.error(
                                                format!(
                                                    "enum case has {} values, but should have 1",
                                                    args.len()
                                                ),
                                                item,
                                            );
                                            return VOID_TYPE_ID;
                                        }
                                    }
                                }
                                EnumVariant::Struct { name, params } => {
                                    if name == case_name {
                                        if args.len() == params.len() {
                                            let mut replacements = vec![];

                                            for (arg, (param_name, param_type_id)) in
                                                args.into_iter().zip(params)
                                            {
                                                if let AstNode::NamedValue { name, value } =
                                                    self.compiler.get_node(arg)
                                                {
                                                    let name = *name;
                                                    let value = *value;

                                                    let name_contents =
                                                        self.compiler.get_source(name);

                                                    if name_contents != param_name {
                                                        self.error(
                                                            "name mismatch in enum case",
                                                            name,
                                                        );
                                                        return VOID_TYPE_ID;
                                                    }

                                                    let arg_type_id = self.typecheck_node(value);

                                                    if self.is_type_variable(*param_type_id) {
                                                        replacements
                                                            .push((*param_type_id, arg_type_id));
                                                    } else if !self.is_type_compatible(
                                                        *param_type_id,
                                                        arg_type_id,
                                                    ) {
                                                        self.error(
                                                            "incompatible types for enum case",
                                                            arg,
                                                        );
                                                        return VOID_TYPE_ID;
                                                    }
                                                }
                                            }

                                            // instantiate, if we have replacements available
                                            type_id = if !replacements.is_empty() {
                                                self.instantiate_generic(type_id, &replacements)
                                            } else {
                                                type_id
                                            };

                                            self.compiler.call_resolution.insert(
                                                head,
                                                CallTarget::EnumConstructor(
                                                    type_id,
                                                    CaseOffset(case_offset),
                                                ),
                                            );

                                            return self.compiler.find_or_create_type(
                                                Type::Pointer {
                                                    allocation_type: AllocationType::Normal,
                                                    optional: false,
                                                    target: type_id,
                                                },
                                            );
                                        } else {
                                            self.error(
                                                format!(
                                                    "enum case has {} values, but should have 1",
                                                    args.len()
                                                ),
                                                item,
                                            );
                                            return VOID_TYPE_ID;
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    AstNode::Name | AstNode::Variable => {
                        let case_name = self.compiler.get_source(item);

                        for (case_offset, case) in cases.iter().enumerate() {
                            match case {
                                EnumVariant::Simple { name } => {
                                    if name == case_name {
                                        self.compiler.call_resolution.insert(
                                            item,
                                            CallTarget::EnumConstructor(
                                                type_id,
                                                CaseOffset(case_offset),
                                            ),
                                        );

                                        return self.compiler.find_or_create_type(Type::Pointer {
                                            allocation_type: AllocationType::Normal,
                                            optional: false,
                                            target: type_id,
                                        });
                                    }
                                }
                                _ => {}
                            }
                        }

                        self.error("can't find matche enum case", item);
                    }
                    x => {
                        self.error(
                            format!("expected enum case when created enum value: {:?}", x),
                            item,
                        );
                    }
                }
            }
            _ => {
                self.error("expected struct or enum", namespace);
            }
        }

        VOID_TYPE_ID
    }

    pub fn typecheck_if(
        &mut self,
        condition: NodeId,
        then_block: NodeId,
        else_expression: Option<NodeId>,
    ) -> TypeId {
        self.typecheck_node(condition);
        self.typecheck_node(then_block);

        if self.compiler.get_node_type(condition) != BOOL_TYPE_ID {
            self.error("condition not a boolean expression", condition);
        }

        if let Some(else_expression) = else_expression {
            self.typecheck_node(else_expression);

            // FIXME: add type compatibility
            if self.compiler.get_node_type(then_block)
                != self.compiler.get_node_type(else_expression)
            {
                self.error("return used outside of a function", else_expression);
            }
        }

        self.compiler.get_node_type(then_block)
    }

    pub fn typecheck_match(&mut self, target: NodeId, match_arms: Vec<(NodeId, NodeId)>) -> TypeId {
        let target_type_id = self.typecheck_node(target);

        let inner_type_id = match self.compiler.get_type(target_type_id) {
            Type::Pointer { target, .. } => *target,
            _ => target_type_id,
        };

        match self.compiler.get_type(inner_type_id) {
            Type::Enum { variants, .. } => {
                let variants = variants.clone();

                let mut seen_variants = vec![];

                for _ in 0..variants.len() {
                    seen_variants.push(false);
                }

                'arm: for (arm_pattern, arm_result) in match_arms {
                    self.enter_scope();
                    match self.compiler.get_node(arm_pattern) {
                        AstNode::Variable | AstNode::Name => {
                            let var_id = self.define_variable(
                                arm_pattern,
                                target_type_id,
                                false,
                                arm_pattern,
                            );

                            let variable_name = self.compiler.get_source(arm_pattern).to_vec();
                            self.add_variable_to_scope(variable_name, var_id);
                            self.compiler.var_resolution.insert(arm_pattern, var_id);

                            self.typecheck_node(arm_result);

                            seen_variants.fill(true);
                        }
                        AstNode::NamespacedLookup { namespace, item } => {
                            let namespace = *namespace;
                            let item = *item;

                            // For now, let's keep things simple. The namespace has to be the enum name
                            // and the item has to be the case/arm to match

                            // FIXME/TODO: Confirm that the namespace given is a valid namespace
                            // for the type being matched
                            // let namespace_type_id = self.find_type_in_scope(namespace);
                            let namespace_type_id = Some(inner_type_id);

                            if let Some(namespace_type_id) = namespace_type_id {
                                // let namespace_type_id = *namespace_type_id;

                                if namespace_type_id != inner_type_id {
                                    self.error(
                                        "expected match case to be the same type as matched value",
                                        namespace,
                                    )
                                } else {
                                    match self.compiler.get_node(item) {
                                        AstNode::Name | AstNode::Variable => {
                                            let arm_name = self.compiler.get_source(item);

                                            for (idx, variant) in variants.iter().enumerate() {
                                                match variant {
                                                    EnumVariant::Simple { name: variant_name } => {
                                                        if variant_name == arm_name {
                                                            self.compiler.call_resolution.insert(
                                                                arm_pattern,
                                                                CallTarget::EnumConstructor(
                                                                    inner_type_id,
                                                                    CaseOffset(idx),
                                                                ),
                                                            );
                                                            self.typecheck_node(arm_result);
                                                            seen_variants[idx] = true;
                                                            self.exit_scope();
                                                            continue 'arm;
                                                        }
                                                    }
                                                    _ => {}
                                                }
                                            }

                                            self.error("could not find match enum case", item)
                                        }
                                        AstNode::Call { head, args } => {
                                            let arm_name = self.compiler.get_source(*head);
                                            let args = args.clone();

                                            for (idx, variant) in variants.iter().enumerate() {
                                                match variant {
                                                    EnumVariant::Single {
                                                        name: variant_name,
                                                        param,
                                                    } => {
                                                        if variant_name == arm_name {
                                                            self.compiler.call_resolution.insert(
                                                                arm_pattern,
                                                                CallTarget::EnumConstructor(
                                                                    inner_type_id,
                                                                    CaseOffset(idx),
                                                                ),
                                                            );
                                                            if matches!(
                                                                self.compiler.get_node(args[0]),
                                                                AstNode::Variable
                                                            ) {
                                                                let var_id = self.define_variable(
                                                                    args[0], *param, false, args[0],
                                                                );
                                                                self.compiler
                                                                    .var_resolution
                                                                    .insert(args[0], var_id);
                                                            }
                                                            self.typecheck_node(arm_result);
                                                            seen_variants[idx] = true;
                                                            self.exit_scope();
                                                            continue 'arm;
                                                        }
                                                    }
                                                    EnumVariant::Struct {
                                                        name: variant_name,
                                                        params,
                                                    } => {
                                                        if variant_name == arm_name {
                                                            self.compiler.call_resolution.insert(
                                                                arm_pattern,
                                                                CallTarget::EnumConstructor(
                                                                    inner_type_id,
                                                                    CaseOffset(idx),
                                                                ),
                                                            );

                                                            seen_variants[idx] = true;

                                                            let mut idx = 0;

                                                            while idx < params.len() {
                                                                let (_, param_type_id) =
                                                                    &params[idx];
                                                                let arg = args[idx];
                                                                if matches!(
                                                                    self.compiler.get_node(args[0]),
                                                                    AstNode::Variable
                                                                ) {
                                                                    let var_id = self
                                                                        .define_variable(
                                                                            arg,
                                                                            *param_type_id,
                                                                            false,
                                                                            arg,
                                                                        );
                                                                    self.compiler
                                                                        .var_resolution
                                                                        .insert(args[idx], var_id);
                                                                }

                                                                idx += 1;
                                                            }
                                                            self.typecheck_node(arm_result);
                                                            self.exit_scope();
                                                            continue 'arm;
                                                        }
                                                    }
                                                    _ => {}
                                                }
                                            }

                                            self.error("could not find match enum case", item)
                                        }
                                        _ => {
                                            panic!("not yet supported")
                                        }
                                    }
                                }
                            } else {
                                self.error("unknown match variant type", namespace)
                            }
                        }
                        _ => self.error("unexpected kind of match case in match", arm_pattern),
                    }
                    self.exit_scope();
                }

                for (variant, seen) in seen_variants.iter().enumerate() {
                    if !*seen {
                        match &variants[variant] {
                            EnumVariant::Simple { name } => self.error(
                                format!(
                                    "missing pattern match for {}",
                                    String::from_utf8_lossy(&name)
                                ),
                                target,
                            ),
                            EnumVariant::Single { name, .. } => self.error(
                                format!(
                                    "missing pattern match for {}(..)",
                                    String::from_utf8_lossy(&name)
                                ),
                                target,
                            ),
                            EnumVariant::Struct { name, .. } => self.error(
                                format!(
                                    "missing pattern match for {}(..)",
                                    String::from_utf8_lossy(&name)
                                ),
                                target,
                            ),
                        }
                    }
                }

                VOID_TYPE_ID
            }
            x => {
                //FIXME: add support for other value types
                self.error(
                    format!("currently only enums are supported in matches: {:?}", x),
                    target,
                );
                VOID_TYPE_ID
            }
        }
    }

    pub fn typecheck_method_call(
        &mut self,
        target: NodeId,
        call: NodeId,
        node_id: NodeId,
    ) -> TypeId {
        let AstNode::Call { head, args } = self.compiler.get_node(call) else {
            panic!("Internal error: method call using a non-call")
        };

        let head = *head;
        // FIXME: fix clone
        let args = args.clone();

        let name = self.compiler.get_source(head).to_vec();
        let type_id = self.typecheck_node(target);

        let type_id = self.get_underlying_type_id(type_id);

        match self.compiler.get_type(type_id) {
            Type::Struct { methods, .. } => {
                for method in methods {
                    let method_name = self
                        .compiler
                        .get_source(self.compiler.functions[method.0].name);

                    if method_name == name {
                        let type_id = self.typecheck_call_with_fun_id(head, *method, &args);
                        self.compiler.set_node_type(node_id, type_id);
                        return type_id;
                    }
                }
                self.error("can't find method in struct", head);
            }
            _ => self.error("expected struct type for method call", target),
        }

        VOID_TYPE_ID
    }

    pub fn typecheck(mut self) -> Compiler {
        let num_nodes = self.compiler.num_ast_nodes();
        self.compiler.resize_node_types(num_nodes, UNKNOWN_TYPE_ID);

        let top_level = NodeId(self.compiler.num_ast_nodes() - 1);
        self.typecheck_node(top_level);

        let top_level_type = self.compiler.get_node_type(top_level);

        // If we haven't seen a main, create one from the top-level node
        if !self.compiler.has_main() {
            // Synthesis of a fake 'main' node
            self.compiler.source.extend_from_slice(b"main");
            let main_node = self.compiler.push_node(AstNode::Name);
            self.compiler
                .span_start
                .push(self.compiler.source.len() - 4);
            self.compiler.span_end.push(self.compiler.source.len());
            // re-resize to make sure we have enough nodes
            self.compiler
                .resize_node_types(main_node.0 + 1, UNKNOWN_TYPE_ID);

            self.compiler.functions.push(Function {
                name: main_node,
                params: vec![],
                return_type: top_level_type,
                body: top_level,
            })
        }

        self.compiler
    }

    pub fn add_variable_to_scope(&mut self, variable_name: Vec<u8>, var_id: VarId) {
        self.scope
            .last_mut()
            .expect("internal error: missing typechecking scope")
            .variables
            .insert(variable_name, var_id);
    }

    pub fn add_type_to_scope(&mut self, type_name: Vec<u8>, type_id: TypeId) {
        self.scope
            .last_mut()
            .expect("internal error: missing typechecking scope")
            .types
            .insert(type_name, type_id);
    }

    pub fn define_variable(
        &mut self,
        name: NodeId,
        ty: TypeId,
        is_mutable: bool,
        where_defined: NodeId,
    ) -> VarId {
        let variable_name = self.compiler.get_source(name).to_vec();
        self.compiler.variables.push(Variable {
            name,
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

        // Expand the shorthand, and look for 'self' instead
        let name = if name == b"." { b"self" } else { name };

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

    pub fn find_expected_return_type(&self) -> Option<TypeId> {
        for scope in self.scope.iter().rev() {
            if let Some(ret_type) = scope.expected_return_type {
                return Some(ret_type);
            }
        }

        None
    }

    pub fn instantiate_generic(
        &mut self,
        type_id: TypeId,
        replacements: &[(TypeId, TypeId)],
    ) -> TypeId {
        match self.compiler.get_type(type_id) {
            Type::Enum {
                variants, methods, ..
            } => {
                // TODO instantiate generic function
                let mut new_variants = vec![];
                let new_methods = methods.clone();

                'variant: for variant in variants {
                    match variant {
                        EnumVariant::Simple { .. } => {
                            new_variants.push(variant.clone());
                        }
                        EnumVariant::Single { name, param } => {
                            for replacement in replacements {
                                if param == &replacement.0 {
                                    new_variants.push(EnumVariant::Single {
                                        name: name.clone(),
                                        param: replacement.1,
                                    });
                                    continue 'variant;
                                }
                            }
                            new_variants.push(EnumVariant::Single {
                                name: name.clone(),
                                param: *param,
                            })
                        }
                        EnumVariant::Struct { name, params } => {
                            let mut new_params = vec![];

                            for param in params {
                                for replacement in replacements {
                                    if param.1 == replacement.0 {
                                        new_params.push((param.0.clone(), replacement.1));
                                        break;
                                    }
                                }
                            }
                            new_variants.push(EnumVariant::Struct {
                                name: name.clone(),
                                params: new_params,
                            })
                        }
                    }
                }

                self.compiler.find_or_create_type(Type::Enum {
                    generic_params: vec![], // we're now fully instantiated
                    variants: new_variants,
                    methods: new_methods,
                })
            }
            Type::Struct {
                fields,
                methods,
                is_allocator,
                ..
            } => {
                let mut new_fields = vec![];
                let new_methods = methods.clone();

                for field in fields {
                    for replacement in replacements {
                        if field.1 == replacement.0 {
                            new_fields.push((field.0.clone(), replacement.1));
                            break;
                        }
                    }
                }

                //TODO: instantiate methods
                self.compiler.find_or_create_type(Type::Struct {
                    generic_params: vec![], // we're now fully instantiated
                    fields: new_fields,
                    methods: new_methods,
                    is_allocator: *is_allocator,
                })
            }
            _ => {
                panic!("not yet supported variant for generic instantiation")
            }
        }
    }

    pub fn set_expected_return_type(&mut self, expected_type: TypeId) {
        let frame = self
            .scope
            .last_mut()
            .expect("internal error: missing expected scope frame");
        frame.expected_return_type = Some(expected_type)
    }

    pub fn get_underlying_type_id(&self, type_id: TypeId) -> TypeId {
        match self.compiler.get_type(type_id) {
            Type::Pointer {
                target: type_id, ..
            } => *type_id,
            _ => type_id,
        }
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
