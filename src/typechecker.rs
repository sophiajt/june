use std::{collections::HashMap, path::PathBuf};

use tracing::{debug, trace};

use crate::{
    compiler::{CallTarget, CaseOffset, Compiler},
    errors::{Severity, SourceError},
    parser::{AstNode, BlockId, MemberAccess, NodeId, PointerType},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct TypedField {
    pub member_access: MemberAccess,
    pub name: Vec<u8>,
    pub ty: TypeId,
    pub where_defined: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unknown,
    Void,
    I64,
    F64,
    Bool,
    Range(TypeId),
    RawBuffer(TypeId),
    Fun {
        params: Vec<Param>,
        ret: TypeId,
    },
    CInt,
    CSizeT,
    CVoidPtr,
    CChar,
    CString,
    CExternalType(NodeId),
    Struct {
        generic_params: Vec<TypeId>,
        fields: Vec<TypedField>,
        is_allocator: bool,
    },
    Enum {
        generic_params: Vec<TypeId>,
        variants: Vec<EnumVariant>,
    },
    Pointer {
        pointer_type: PointerType,
        optional: bool,
        target: TypeId,
    },
    TypeVariable(NodeId),
    FunLocalTypeVar {
        offset: usize,
    },
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

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: NodeId,
    pub ty: TypeId,
    pub is_mutable: bool,
    pub where_defined: NodeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: Vec<u8>,
    pub var_id: VarId,
}

impl Param {
    pub fn new(name: Vec<u8>, var_id: VarId) -> Param {
        Param { name, var_id }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    pub name: Vec<u8>,
    pub type_id: TypeId,
}

impl TypeParam {
    pub fn new(name: Vec<u8>, type_id: TypeId) -> TypeParam {
        TypeParam { name, type_id }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lifetime {
    Variable(VarId),
    Return,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LifetimeAnnotation {
    Equality(Lifetime, Lifetime),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: NodeId,
    pub params: Vec<Param>,
    pub lifetime_annotations: Vec<LifetimeAnnotation>,
    pub type_params: Vec<TypeParam>,
    pub inference_vars: Vec<TypeId>,
    pub return_node: Option<NodeId>,
    pub return_type: TypeId,
    pub initial_node_id: Option<NodeId>,
    pub body: Option<NodeId>,
    pub is_extern: bool,
}

#[derive(Debug)]
pub struct Scope {
    modules: HashMap<PathBuf, ModuleId>,
    variables: HashMap<Vec<u8>, VarId>,
    functions: HashMap<Vec<u8>, FunId>,
    types: HashMap<Vec<u8>, TypeId>,
    expected_return_type: Option<TypeId>,
    moved_owned_values: HashMap<VarId, NodeId>,
    allow_unsafe: bool,
}

#[derive(Debug)]
pub struct Module {
    scope: Scope,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            modules: HashMap::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            expected_return_type: None,
            moved_owned_values: HashMap::new(),
            allow_unsafe: false,
        }
    }

    pub fn set_unsafe(&mut self) {
        self.allow_unsafe = true;
    }

    fn find_type(&self, name: &[u8]) -> Option<TypeId> {
        self.types.get(name).copied()
    }

    fn find_fun(&self, name: &[u8]) -> Option<FunId> {
        if let Some(fun_id) = self.functions.get(name) {
            return Some(*fun_id);
        }

        None
    }

    fn find_name(&self, name: &[u8]) -> Option<VarOrFunId> {
        if let Some(var_id) = self.variables.get(name) {
            return Some(VarOrFunId::VarId(*var_id));
        }

        if let Some(fun_id) = self.functions.get(name) {
            return Some(VarOrFunId::FunId(*fun_id));
        }

        None
    }
}

pub struct Typechecker {
    pub compiler: Compiler,
    pub scope: Vec<Scope>,
}

pub enum VarOrFunId {
    VarId(VarId),
    FunId(FunId),
}

pub const UNKNOWN_TYPE_ID: TypeId = TypeId(0);
pub const VOID_TYPE_ID: TypeId = TypeId(1);
pub const I64_TYPE_ID: TypeId = TypeId(2);
pub const F64_TYPE_ID: TypeId = TypeId(3);
pub const BOOL_TYPE_ID: TypeId = TypeId(4);
pub const RANGE_I64_TYPE_ID: TypeId = TypeId(5);
pub const C_INT_TYPE_ID: TypeId = TypeId(6);
pub const C_SIZE_T_TYPE_ID: TypeId = TypeId(7);
pub const C_VOID_PTR_TYPE_ID: TypeId = TypeId(8);
pub const C_CHAR_TYPE_ID: TypeId = TypeId(9);
pub const C_STRING_TYPE_ID: TypeId = TypeId(10);

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
            initial_node_id: None,
            body: None,
            type_params: vec![],
            lifetime_annotations: vec![],
            inference_vars: vec![],
            return_node: None,
            return_type: VOID_TYPE_ID,
            is_extern: true,
        });

        // hardwire in the core types before the user-defined types
        compiler.push_type(Type::Unknown);
        compiler.push_type(Type::Void);
        compiler.push_type(Type::I64);
        compiler.push_type(Type::F64);
        compiler.push_type(Type::Bool);
        compiler.push_type(Type::Range(I64_TYPE_ID));
        compiler.push_type(Type::CInt);
        compiler.push_type(Type::CSizeT);
        compiler.push_type(Type::CVoidPtr);
        compiler.push_type(Type::CChar);
        compiler.push_type(Type::CString);

        let mut scope = vec![Scope::new()];

        scope
            .last_mut()
            .expect("internal error: couldn't access function scope")
            .functions
            .insert(b"println".to_vec(), FunId(0));

        Self { compiler, scope }
    }

    pub fn unsafe_allowed(&self) -> bool {
        self.scope.iter().any(|x| x.allow_unsafe)
    }

    pub fn set_unsafe(&mut self) {
        self.scope
            .last_mut()
            .expect("internal error: missing scope during typecheck")
            .set_unsafe()
    }

    pub fn typecheck_typename(&mut self, node_id: NodeId) -> TypeId {
        match self.compiler.get_node(node_id) {
            AstNode::Type {
                name,
                optional,
                pointer_type,
                ..
            } => {
                let name_node_id = *name;
                let optional = *optional;
                let pointer_type = *pointer_type;

                let name = self.compiler.get_source(name_node_id);

                match name {
                    b"i64" => I64_TYPE_ID,
                    b"f64" => F64_TYPE_ID,
                    b"c_string" => C_STRING_TYPE_ID,
                    b"c_voidptr" => C_VOID_PTR_TYPE_ID,
                    b"c_char" => C_CHAR_TYPE_ID,
                    b"c_int" => C_INT_TYPE_ID,
                    b"c_size_t" => C_SIZE_T_TYPE_ID,
                    b"bool" => BOOL_TYPE_ID,
                    b"void" => VOID_TYPE_ID,
                    _ => {
                        if let Some(type_id) = self.find_type_in_scope(name_node_id) {
                            if self.compiler.is_type_variable(type_id) {
                                type_id
                            } else {
                                // Assume custom types are pointers
                                self.compiler.find_or_create_type(Type::Pointer {
                                    pointer_type,
                                    optional,
                                    target: type_id,
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
            AstNode::FunType { params, ret } => {
                let params = params.clone();
                let ret = *ret;

                let mut typed_params = vec![];

                for param in params {
                    let param_ty = self.typecheck_typename(param);
                    let var_id = self.define_variable(param, param_ty, false, param);

                    typed_params.push(Param {
                        name: vec![],
                        var_id,
                    });
                }

                let typed_ret = self.typecheck_typename(ret);

                self.compiler.find_or_create_type(Type::Fun {
                    params: typed_params,
                    ret: typed_ret,
                })
            }
            AstNode::RawBufferType { inner } => {
                let inner = *inner;

                let inner_ty = self.typecheck_typename(inner);

                self.compiler.find_or_create_type(Type::RawBuffer(inner_ty))
            }
            _ => {
                self.error(
                    format!(
                        "expected type name: {:?}",
                        self.compiler.get_node(node_id).clone()
                    ),
                    node_id,
                );
                VOID_TYPE_ID
            }
        }
    }

    pub fn typecheck_fun_predecl(
        &mut self,
        name: NodeId,
        type_params: Option<NodeId>,
        params: NodeId,
        lifetime_annotations: &[NodeId],
        return_ty: Option<NodeId>,
        initial_node_id: Option<NodeId>,
        block: Option<NodeId>,
        is_extern: bool,
    ) -> FunId {
        let mut fun_params = vec![];
        let mut fun_type_params = vec![];

        let fun_name = self.compiler.source
            [self.compiler.span_start[name.0]..self.compiler.span_end[name.0]]
            .to_vec();

        self.enter_scope();

        if let Some(type_params) = type_params {
            let AstNode::Params(type_params) = self.compiler.get_node(type_params) else {
                panic!("internal error: enum generic params are not proper ast node");
            };

            let type_params = type_params.clone();

            for type_param in type_params {
                let type_id = self.compiler.fresh_type_variable(type_param);

                let type_var_name = self.compiler.get_source(type_param).to_vec();

                fun_type_params.push(TypeParam::new(type_var_name.clone(), type_id));

                self.add_type_to_scope(type_var_name, type_id);
            }
        }

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

        let mut checked_lifetime_annotations = vec![];

        for lifetime_annotation in lifetime_annotations {
            let AstNode::BinaryOp { lhs, rhs, .. } = self.compiler.get_node(*lifetime_annotation)
            else {
                panic!("internal error: lifetime anotation is not a binary op")
            };

            let lhs = match self.compiler.get_node(*lhs) {
                AstNode::Name => {
                    if let Some(var_id) = self.find_variable_in_scope(*lhs) {
                        Lifetime::Variable(var_id)
                    } else {
                        self.error("couldn't find parameter for lifetime", *lhs);
                        continue;
                    }
                }
                AstNode::ReturnLifetime => Lifetime::Return,
                _ => {
                    panic!("internal error: non-variable and non-return lifetime")
                }
            };

            let rhs = match self.compiler.get_node(*rhs) {
                AstNode::Name => {
                    if let Some(var_id) = self.find_variable_in_scope(*rhs) {
                        Lifetime::Variable(var_id)
                    } else {
                        self.error("couldn't find parameter for lifetime", *rhs);
                        continue;
                    }
                }
                AstNode::ReturnLifetime => Lifetime::Return,
                _ => {
                    panic!("internal error: non-variable and non-return lifetime")
                }
            };

            checked_lifetime_annotations.push(LifetimeAnnotation::Equality(lhs, rhs));
        }

        let return_type = if let Some(return_ty) = return_ty {
            self.typecheck_typename(return_ty)
        } else {
            VOID_TYPE_ID
        };

        self.exit_scope();

        self.compiler.functions.push(Function {
            name,
            params: fun_params,
            type_params: fun_type_params,
            lifetime_annotations: checked_lifetime_annotations,
            inference_vars: vec![],
            return_type,
            return_node: return_ty,
            initial_node_id,
            body: block,
            is_extern,
        });

        let fun_id = self.compiler.functions.len() - 1;

        self.scope
            .last_mut()
            .expect("internal error: missing function scope")
            .functions
            .insert(fun_name, FunId(fun_id));

        // Mark the name of the function as resolved, in case type inference needs to run again
        // we know we don't have to recreate this function
        self.compiler.fun_resolution.insert(name, FunId(fun_id));

        FunId(fun_id)
    }

    pub fn typecheck_fun(&mut self, fun_id: FunId) {
        let Function {
            params,
            type_params,
            body,
            return_type,
            return_node,
            name,
            inference_vars: type_vars,
            ..
        } = self.compiler.functions[fun_id.0].clone();

        let Some(body) = body else {
            return;
        };

        self.enter_scope();

        for type_param in type_params {
            self.add_type_to_scope(type_param.name.clone(), type_param.type_id)
        }

        self.set_expected_return_type(return_type);

        for Param { name, var_id } in &params {
            self.add_variable_to_scope(name.clone(), *var_id);
        }

        // Create our local inference variable list we'll use as we infer types
        let mut local_inferences = type_vars.clone();

        // Typecheck until we hit a fix point for our inferences
        loop {
            let before = local_inferences.clone();

            self.typecheck_node(body, &mut local_inferences);

            if local_inferences == before || !self.compiler.errors.is_empty() {
                break;
            }
        }

        if return_type != VOID_TYPE_ID && !self.ends_in_return(body) {
            if let Some(return_node) = return_node {
                self.error(
                    "function is missing expected return at end of function",
                    return_node,
                )
            } else {
                self.error(
                    "function is missing expected return at end of function",
                    name,
                )
            }
        }

        self.exit_scope();

        let Function {
            inference_vars: type_vars,
            ..
        } = &mut self.compiler.functions[fun_id.0];

        *type_vars = local_inferences;
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
                let type_id = self.compiler.fresh_type_variable(param);

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
        });

        self.compiler.push_type(Type::Pointer {
            pointer_type: PointerType::Shared,
            optional: false,
            target: type_id,
        });

        self.add_type_to_scope(enum_name.clone(), type_id);

        if !methods.is_empty() {
            self.enter_scope();

            self.add_type_to_scope(b"self".to_vec(), type_id);

            let mut fun_ids = vec![];

            for method in methods {
                let AstNode::Fun {
                    name,
                    type_params,
                    params,
                    lifetime_annotations,
                    return_ty,
                    initial_node_id,
                    block,
                    is_extern,
                } = self.compiler.get_node(method)
                else {
                    self.error(
                        "internal error: can't find method definition during typecheck",
                        method,
                    );
                    return VOID_TYPE_ID;
                };
                let name = *name;
                let type_params = *type_params;
                let params = *params;
                let lifetime_annotations = lifetime_annotations.clone();
                let return_ty = *return_ty;
                let initial_node_id = *initial_node_id;
                let block = *block;

                fun_ids.push(self.typecheck_fun_predecl(
                    name,
                    type_params,
                    params,
                    &lifetime_annotations,
                    return_ty,
                    initial_node_id,
                    block,
                    *is_extern,
                ));
            }

            self.compiler
                .methods_on_type
                .insert(type_id, fun_ids.clone());

            for fun_id in &fun_ids {
                self.typecheck_fun(*fun_id);
            }

            self.exit_scope();
        }

        self.exit_scope();

        self.add_type_to_scope(enum_name, type_id);

        self.compiler.type_resolution.insert(typename, type_id);

        type_id
    }

    pub fn typecheck_struct(
        &mut self,
        typename: NodeId,
        fields: Vec<NodeId>,
        methods: Vec<NodeId>,
        explicit_no_alloc: bool,
        base_classes: Option<NodeId>,
    ) -> TypeId {
        let AstNode::Type { name, params, .. } = self.compiler.get_node(typename) else {
            panic!("internal error: struct does not have type as name");
        };

        let name = *name;
        let params = *params;

        let mut has_pointers = false;

        let mut generic_params = vec![];

        self.enter_scope();

        if let Some(params) = params {
            let AstNode::Params(params) = self.compiler.get_node(params) else {
                panic!("internal error: struct generic params are not proper ast node");
            };

            let params = params.clone();

            for param in params {
                let type_id = self.compiler.fresh_type_variable(param);

                // be conservative and assume generic parameters might be pointers
                has_pointers = true;

                generic_params.push(type_id);

                let type_var_name = self.compiler.get_source(param).to_vec();

                self.add_type_to_scope(type_var_name, type_id);
            }
        }

        let struct_name = self.compiler.get_source(name).to_vec();

        let type_id = self.compiler.push_type(Type::Struct {
            generic_params,
            fields: Default::default(),
            is_allocator: false, // will be replaced later
        });

        self.add_type_to_scope(struct_name.clone(), type_id);
        self.add_type_to_scope(b"Self".to_vec(), type_id);

        let mut output_fields = vec![];

        for field in fields {
            let AstNode::Field {
                member_access,
                name: field_name,
                typename: field_type,
            } = self.compiler.get_node(field)
            else {
                panic!("internal error: field expected inside of struct typechecking");
            };

            let field_name = *field_name;
            let field_type = *field_type;
            let member_access = *member_access;

            let field_name_text = self.compiler.get_source(field_name).to_vec();
            let field_type = self.typecheck_typename(field_type);

            if !self.compiler.is_copyable_type(field_type) {
                has_pointers = true;
            }

            output_fields.push(TypedField {
                member_access,
                name: field_name_text,
                ty: field_type,
                where_defined: field_name,
            });
        }

        let Type::Struct {
            fields,
            is_allocator,
            ..
        } = &mut self.compiler.get_type_mut(type_id)
        else {
            panic!("internal error: previously inserted struct can't be found");
        };

        *fields = output_fields;
        *is_allocator = if explicit_no_alloc {
            false
        } else {
            has_pointers
        };

        // self.compiler
        //     .types
        //     .push(Type::Pointer { allocation_type: AllocationType::Normal, type_id));

        if !methods.is_empty() {
            self.enter_scope();

            self.add_type_to_scope(b"self".to_vec(), type_id);

            let mut fun_ids = vec![];
            let mut virtual_fun_ids = vec![];

            for method in methods {
                let AstNode::Fun {
                    name,
                    type_params,
                    params,
                    lifetime_annotations,
                    return_ty,
                    initial_node_id,
                    block,
                    is_extern,
                } = self.compiler.get_node(method)
                else {
                    self.error(
                        "internal error: can't find method definition during typecheck",
                        method,
                    );
                    return VOID_TYPE_ID;
                };
                let name = *name;
                let type_params = *type_params;
                let params = *params;
                let lifetime_annotations = lifetime_annotations.clone();
                let return_ty = *return_ty;
                let initial_node_id = *initial_node_id;
                let block = *block;
                let is_extern = *is_extern;

                let fun_id = self.typecheck_fun_predecl(
                    name,
                    type_params,
                    params,
                    &lifetime_annotations,
                    return_ty,
                    initial_node_id,
                    block,
                    is_extern,
                );

                if block.is_none() && !is_extern {
                    virtual_fun_ids.push(fun_id);
                } else {
                    fun_ids.push(fun_id);
                }
            }

            if let Some(base_classes) = base_classes {
                let Some(base_classes) = self.find_type_in_scope(base_classes) else {
                    todo!()
                };

                self.compiler
                    .base_classes
                    .insert(type_id, vec![base_classes]);

                let empty_methods = vec![];
                let methods = self
                    .compiler
                    .methods_on_type
                    .get(&base_classes)
                    .unwrap_or(&empty_methods);
                let virtual_methods = self
                    .compiler
                    .virtual_methods_on_type
                    .get(&base_classes)
                    .unwrap_or(&empty_methods);

                // TODO: proper equality for methods, have some way to check equality between two functions to see if one is an implementation of the other
                let mut implemented_methods = vec![];

                for method in methods {
                    let fun = &self.compiler.functions[method.0];
                    let method_name = self.compiler.get_source_str(fun.name);
                    implemented_methods.push(method_name);
                    fun_ids.push(*method);
                }

                for method in virtual_methods {
                    let fun = &self.compiler.functions[method.0];
                    let method_name = self.compiler.get_source_str(fun.name);
                    if implemented_methods.contains(&method_name) {
                        virtual_fun_ids.push(*method);
                    }
                }
            }

            self.compiler
                .methods_on_type
                .insert(type_id, fun_ids.clone());
            self.compiler
                .virtual_methods_on_type
                .insert(type_id, virtual_fun_ids.clone());

            for fun_id in &fun_ids {
                self.typecheck_fun(*fun_id);
            }

            self.exit_scope();
        }
        self.exit_scope();

        self.add_type_to_scope(struct_name, type_id);

        self.compiler.type_resolution.insert(typename, type_id);

        type_id
    }

    pub fn typecheck_block(
        &mut self,
        node_id: NodeId,
        block_id: BlockId,
        local_inferences: &mut Vec<TypeId>,
    ) -> Scope {
        let mut funs = vec![];

        self.enter_scope();

        // FIXME: sad we have to clone here
        let block = self.compiler.blocks[block_id.0].clone();

        for node_id in &block.nodes {
            let node = self.compiler.get_node(*node_id);
            debug!(?node);
            match &node {
                AstNode::Fun {
                    name,
                    type_params,
                    params,
                    lifetime_annotations,
                    return_ty,
                    initial_node_id,
                    block,
                    is_extern,
                } => {
                    if let Some(fun_id) = self.compiler.fun_resolution.get(name) {
                        let fun_id = *fun_id;

                        let fun_name = self.compiler.source
                            [self.compiler.span_start[name.0]..self.compiler.span_end[name.0]]
                            .to_vec();

                        self.scope
                            .last_mut()
                            .expect("internal error: missing function scope")
                            .functions
                            .insert(fun_name, fun_id);

                        continue;
                    }
                    let lifetime_annotations = lifetime_annotations.clone();
                    funs.push(self.typecheck_fun_predecl(
                        *name,
                        *type_params,
                        *params,
                        &lifetime_annotations,
                        *return_ty,
                        *initial_node_id,
                        *block,
                        *is_extern,
                    ));
                }

                AstNode::Struct {
                    typename,
                    fields,
                    methods,
                    explicit_no_alloc,
                    base_class,
                } => {
                    if let Some(type_id) = self.compiler.type_resolution.get(typename) {
                        // we've already created this. Instead of recreating it, put the previous
                        // definition into scope
                        let struct_name = self.compiler.get_source(*typename).to_vec();

                        self.add_type_to_scope(struct_name, *type_id);

                        continue;
                    }
                    self.typecheck_struct(
                        *typename,
                        fields.clone(),
                        methods.clone(),
                        *explicit_no_alloc,
                        *base_class,
                    );
                }

                AstNode::Enum {
                    typename,
                    cases,
                    methods,
                } => {
                    if let Some(type_id) = self.compiler.type_resolution.get(typename) {
                        // we've already created this. Instead of recreating it, put the previous
                        // definition into scope
                        let struct_name = self.compiler.get_source(*typename).to_vec();

                        self.add_type_to_scope(struct_name, *type_id);

                        continue;
                    }
                    self.typecheck_enum(*typename, cases.clone(), methods.clone());
                }

                AstNode::ExternType { name } => {
                    let type_name = self.compiler.get_source(*name).to_vec();
                    let ty = Type::CExternalType(*name);
                    let type_id = self.compiler.find_or_create_type(ty);

                    self.add_type_to_scope(type_name, type_id);
                }

                AstNode::Use { path } => {
                    self.typecheck_module(*path, local_inferences);
                }

                _ => {}
            }
        }

        for fun in funs {
            self.typecheck_fun(fun);
        }

        for node_id in &block.nodes {
            self.typecheck_node(*node_id, local_inferences);
        }
        self.compiler.set_node_type(node_id, VOID_TYPE_ID);

        // self.exit_scope()
        self.scope
            .pop()
            .expect("internal error: no scope object found for current block")
    }

    pub fn var_was_previously_moved(&self, var_id: VarId) -> Option<NodeId> {
        for scope_frame in self.scope.iter().rev() {
            if let Some(where_moved) = scope_frame.moved_owned_values.get(&var_id) {
                return Some(*where_moved);
            }
        }

        None
    }

    pub fn maybe_move_variable(&mut self, node_id: NodeId, local_inferences: &[TypeId]) {
        if let AstNode::Name = self.compiler.get_node(node_id) {
            let var_id = self.compiler.var_resolution.get(&node_id);

            // Assume the mistyped variable error has already been reported
            if let Some(var_id) = var_id {
                let var_type = self.compiler.get_variable(*var_id).ty;
                let var_type = self.compiler.resolve_type(var_type, local_inferences);
                let var_ty = self.compiler.get_type(var_type);

                if let Type::Pointer { pointer_type, .. } = var_ty {
                    if pointer_type == &PointerType::Owned {
                        self.scope
                            .last_mut()
                            .expect("internal error: missing scope frame")
                            .moved_owned_values
                            .insert(*var_id, node_id);
                    }
                }
            }
        }
    }

    pub fn unify_types(
        &mut self,
        lhs: TypeId,
        rhs: TypeId,
        local_inferences: &mut Vec<TypeId>,
    ) -> bool {
        match (self.compiler.get_type(lhs), self.compiler.get_type(rhs)) {
            (Type::FunLocalTypeVar { offset }, _) => {
                let offset = *offset;

                if local_inferences[offset] == UNKNOWN_TYPE_ID {
                    local_inferences[offset] = rhs;
                    true
                } else {
                    self.unify_types(local_inferences[offset], rhs, local_inferences)
                }
            }
            (_, Type::FunLocalTypeVar { offset }) => {
                let offset = *offset;

                if local_inferences[offset] == UNKNOWN_TYPE_ID {
                    local_inferences[offset] = lhs;
                    true
                } else {
                    self.unify_types(lhs, local_inferences[offset], local_inferences)
                }
            }
            (
                Type::Pointer {
                    pointer_type: pointer_type_lhs,
                    optional: optional_lhs,
                    target: target_lhs,
                },
                Type::Pointer {
                    pointer_type: pointer_type_rhs,
                    optional: optional_rhs,
                    target: target_rhs,
                },
            ) => {
                // We allow for unknown pointer types to assign in from the other types,
                // which allows us to not have to guess how `self` will be used
                // Also, if an owned pointer is assigned to a shared pointer, then we'll
                // allow the move into a shared pointer, effectively removing the owned-ness.
                // We can do this because the ownership will move.
                (pointer_type_lhs == &PointerType::Unknown
                    || pointer_type_lhs == pointer_type_rhs
                    || (pointer_type_lhs == &PointerType::Shared
                        && pointer_type_rhs == &PointerType::Owned))
                    && target_lhs == target_rhs
                    && (*optional_lhs || optional_lhs == optional_rhs)
            }
            (Type::RawBuffer(lhs_inner), Type::RawBuffer(rhs_inner)) => {
                let lhs_inner = *lhs_inner;
                let rhs_inner = *rhs_inner;

                if self.unify_types(lhs_inner, rhs_inner, local_inferences) {
                    let lhs_resolved = self.compiler.resolve_type(lhs_inner, local_inferences);
                    let rhs_resolved = self.compiler.resolve_type(rhs_inner, local_inferences);

                    // Make sure we have concrete versions of both types for later stages in the compiler
                    self.compiler
                        .find_or_create_type(Type::RawBuffer(lhs_resolved));
                    self.compiler
                        .find_or_create_type(Type::RawBuffer(rhs_resolved));

                    true
                } else {
                    false
                }
            }
            (
                Type::Fun {
                    params: lhs_params,
                    ret: lhs_ret,
                },
                Type::Fun {
                    params: rhs_params,
                    ret: rhs_ret,
                },
            ) => {
                let lhs_ret = *lhs_ret;
                let rhs_ret = *rhs_ret;

                let lhs_params = lhs_params.clone();
                let rhs_params = rhs_params.clone();

                if lhs_params.len() != rhs_params.len() {
                    return false;
                }

                for (x, y) in lhs_params.iter().zip(rhs_params.iter()) {
                    let x_ty = self.compiler.get_variable(x.var_id).ty;
                    let y_ty = self.compiler.get_variable(y.var_id).ty;

                    if !self.unify_types(x_ty, y_ty, local_inferences) {
                        return false;
                    }
                }

                self.unify_types(lhs_ret, rhs_ret, local_inferences)
            }
            (Type::CInt, Type::I64) => true, // FIXME: do we want these?
            (Type::I64, Type::CInt) => true,
            (Type::I64, Type::CSizeT) => true,
            (Type::CSizeT, Type::I64) => true,
            _ => lhs == rhs,
        }
    }

    fn unify_subtypes(
        &self,
        expected_type: TypeId,
        actual_type: TypeId,
        local_inferences: &mut Vec<TypeId>,
    ) -> bool {
        let expected_type = self.compiler.resolve_type(expected_type, local_inferences);
        let expected_type = self.compiler.get_underlying_type_id(expected_type);
        let actual_type = self.compiler.resolve_type(actual_type, local_inferences);
        let actual_type = self.compiler.get_underlying_type_id(actual_type);
        let base_classes = match self.compiler.base_classes.get(&actual_type) {
            Some(baseclasses) => baseclasses,
            None => return false,
        };
        base_classes.contains(&expected_type)
    }

    pub fn is_binding_mutable(&self, node_id: NodeId) -> bool {
        match self.compiler.get_node(node_id) {
            AstNode::Name => {
                if let Some(var_id) = self.compiler.var_resolution.get(&node_id) {
                    return self.compiler.get_variable(*var_id).is_mutable;
                }

                false
            }
            AstNode::MemberAccess { target, .. } => self.is_binding_mutable(*target),
            _ => false,
        }
    }

    pub fn typecheck_call_with_node_id(
        &mut self,
        name: NodeId,
        node_id: NodeId,
        args: &[NodeId],
        local_inferences: &mut Vec<TypeId>,
    ) -> TypeId {
        let type_id = self.compiler.get_node_type(name);
        let type_id = self.compiler.resolve_type(type_id, local_inferences);

        if let Type::Fun { params, ret } = self.compiler.get_type(type_id) {
            let params = params.clone();
            let ret = *ret;

            self.compiler
                .call_resolution
                .insert(name, CallTarget::NodeId(node_id));

            let mut type_var_replacements = HashMap::new();

            self.typecheck_call_helper(
                args,
                params,
                None,
                local_inferences,
                &mut type_var_replacements,
            );

            if self.compiler.is_type_variable(ret) {
                if let Some(ret) = type_var_replacements.get(&ret) {
                    *ret
                } else {
                    self.error("unknown type variable in return", name);
                    UNKNOWN_TYPE_ID
                }
            } else {
                ret
            }
        } else {
            self.error("attempt to call a non-function", name);

            UNKNOWN_TYPE_ID
        }
    }

    pub fn typecheck_call_with_fun_id(
        &mut self,
        name: NodeId,
        fun_id: FunId,
        args: &[NodeId],
        method_target: Option<NodeId>,
        local_inferences: &mut Vec<TypeId>,
    ) -> TypeId {
        let Function {
            params,
            return_type,
            ..
        } = &self.compiler.functions[fun_id.0];

        let params = params.clone();
        let return_type = *return_type;

        let args = if let Some(method_target) = method_target {
            let mut output = vec![method_target];
            output.extend_from_slice(args);
            output
        } else {
            args.to_vec()
        };

        if fun_id.0 == 0 {
            // Just for now, special-case println
            self.compiler
                .call_resolution
                .insert(name, CallTarget::Function(fun_id));
            for arg in &args {
                // TODO: add name-checking
                let arg = *arg;

                self.typecheck_node(arg, local_inferences);
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

        if self.compiler.functions[fun_id.0].is_extern && !self.unsafe_allowed() {
            // This is an external function. For now, assume an `extern "C"` function
            // These must be called in an unsafe block
            self.error(
                "call to extern \"C\" functions requires 'unsafe' block",
                name,
            )
        }

        if method_target.is_some() {
            self.enter_scope();
        }

        let mut type_var_replacements = HashMap::new();

        self.typecheck_call_helper(
            &args,
            params,
            method_target,
            local_inferences,
            &mut type_var_replacements,
        );

        let fun_id = if !type_var_replacements.is_empty() {
            self.instantiate_generic_fun(fun_id, &type_var_replacements)
        } else {
            fun_id
        };

        // TODO: do we want to wait until all params are checked
        // before we mark this as resolved?
        self.compiler
            .call_resolution
            .insert(name, CallTarget::Function(fun_id));

        if method_target.is_some() {
            self.exit_scope();
        }

        if self.compiler.is_type_variable(return_type) {
            if let Some(ret) = type_var_replacements.get(&return_type) {
                *ret
            } else {
                self.error("unknown type variable in return", name);
                UNKNOWN_TYPE_ID
            }
        } else {
            return_type
        }
    }

    fn typecheck_call_helper(
        &mut self,
        args: &[NodeId],
        params: Vec<Param>,
        method_target: Option<NodeId>,
        local_inferences: &mut Vec<TypeId>,
        type_var_replacements: &mut HashMap<TypeId, TypeId>,
    ) {
        for (idx, (arg, param)) in args.iter().zip(params).enumerate() {
            let arg = *arg;

            match &self.compiler.get_node(arg) {
                AstNode::NamedValue { name, value } => {
                    let name = *name;
                    let value = *value;

                    // Set up expected type for inference. Note: if we find concrete values
                    // this inference type will be replaced by the concrete type.
                    self.compiler
                        .set_node_type(value, self.compiler.get_variable(param.var_id).ty);

                    // If this is a method, we've already checked the first argument (aka
                    // the target of the method)
                    let arg_ty = if idx == 0 && method_target.is_some() {
                        self.compiler.get_node_type(value)
                    } else {
                        self.typecheck_node(value, local_inferences)
                    };

                    self.maybe_move_variable(value, local_inferences);

                    if self.compiler.get_variable(param.var_id).is_mutable
                        && !self.is_binding_mutable(value)
                    {
                        self.error("argument to function needs to be mutable", value);
                        self.note(
                            "parameter defined here",
                            self.compiler.get_variable(param.var_id).where_defined,
                        );
                    }

                    if self.unify_types(
                        arg_ty,
                        self.compiler.get_variable(param.var_id).ty,
                        local_inferences,
                    ) {
                    } else if self.unify_subtypes(
                        arg_ty,
                        self.compiler.get_variable(param.var_id).ty,
                        local_inferences,
                    ) {
                        todo!()
                    } else {
                        self.error(
                            format!(
                                "type mismatch for arg. expected {}, found {}",
                                self.compiler.pretty_type(
                                    self.compiler.resolve_type(arg_ty, local_inferences)
                                ),
                                self.compiler.pretty_type(self.compiler.resolve_type(
                                    self.compiler.get_variable(param.var_id).ty,
                                    local_inferences
                                ))
                            ),
                            arg,
                        );
                        self.note(
                            "parameter defined here",
                            self.compiler.get_variable(param.var_id).where_defined,
                        );
                    }

                    let arg_name = self.compiler.get_source(name);

                    if arg_name != param.name {
                        self.error(
                            &format!("expected name '{}'", String::from_utf8_lossy(&param.name)),
                            name,
                        );
                        self.note(
                            "parameter defined here",
                            self.compiler.get_variable(param.var_id).where_defined,
                        )
                    }
                }
                _ => {
                    let arg_type = if idx == 0 && method_target.is_some() {
                        self.compiler.get_node_type(arg)
                    } else {
                        self.typecheck_node(arg, local_inferences)
                    };

                    self.maybe_move_variable(arg, local_inferences);

                    let variable = &self.compiler.get_variable(param.var_id);

                    let variable_ty = variable.ty;
                    let variable_nodeid = variable.name;

                    if self.compiler.is_type_variable(variable_ty) {
                        if let Some(replacement) = type_var_replacements.get(&variable_ty) {
                            if self.unify_types(*replacement, arg_type, local_inferences) {
                            } else if self.unify_subtypes(*replacement, arg_type, local_inferences)
                            {
                                todo!()
                            } else {
                                self.error(
                                    format!(
                                        "type mismatch for arg. expected {}, found {}",
                                        self.compiler.pretty_type(
                                            self.compiler
                                                .resolve_type(*replacement, local_inferences)
                                        ),
                                        self.compiler.pretty_type(
                                            self.compiler.resolve_type(arg_type, local_inferences)
                                        )
                                    ),
                                    arg,
                                );
                                self.note(
                                    "parameter defined here",
                                    self.compiler.get_variable(param.var_id).where_defined,
                                );
                            }
                        } else {
                            type_var_replacements.insert(
                                self.compiler.get_underlying_type_id(variable_ty),
                                self.compiler.get_underlying_type_id(arg_type),
                            );
                        }
                    } else if self.unify_types(variable_ty, arg_type, local_inferences) {
                    } else if self.unify_subtypes(variable_ty, arg_type, local_inferences) {
                        self.compiler
                            .replace_node(arg, |_old_node, old_nodes_new_id| {
                                AstNode::TypeCoercion {
                                    source_node: old_nodes_new_id,
                                    target_type: variable_nodeid,
                                }
                            });
                    } else {
                        self.error(
                            format!(
                                "type mismatch for arg. expected {}, found {}",
                                self.compiler.pretty_type(
                                    self.compiler.resolve_type(variable_ty, local_inferences)
                                ),
                                self.compiler.pretty_type(
                                    self.compiler.resolve_type(arg_type, local_inferences)
                                )
                            ),
                            arg,
                        );
                        self.note(
                            "parameter defined here",
                            self.compiler.get_variable(param.var_id).where_defined,
                        );
                    }

                    if self.compiler.get_variable(param.var_id).is_mutable
                        && !self.is_binding_mutable(arg)
                    {
                        self.error("argument to function needs to be mutable", arg);
                        self.note(
                            "parameter defined here",
                            self.compiler.get_variable(param.var_id).where_defined,
                        );
                    }
                }
            }
        }
    }

    pub fn typecheck_call(
        &mut self,
        node_id: NodeId,
        head: NodeId,
        args: &[NodeId],
        local_inferences: &mut Vec<TypeId>,
    ) -> TypeId {
        let type_id = self.typecheck_node(head, local_inferences);
        self.compiler.set_node_type(head, type_id);

        match self.compiler.get_node(head) {
            AstNode::MemberAccess { target, .. } => {
                let target = *target;

                if let Some(fun_id) = self.compiler.fun_resolution.get(&head) {
                    self.typecheck_call_with_fun_id(
                        head,
                        *fun_id,
                        args,
                        Some(target),
                        local_inferences,
                    )
                } else {
                    // We're not looking at the name of a defined function, but likely looking at a first-class function instead
                    self.typecheck_call_with_node_id(head, node_id, args, local_inferences)
                }
            }
            _ => {
                if let Some(fun_id) = self.compiler.fun_resolution.get(&head) {
                    self.typecheck_call_with_fun_id(head, *fun_id, args, None, local_inferences)
                } else {
                    // We're not looking at the name of a defined function, but likely looking at a first-class function instead
                    self.typecheck_call_with_node_id(head, node_id, args, local_inferences)
                }
            }
        }
    }

    pub fn typecheck_node(
        &mut self,
        node_id: NodeId,
        local_inferences: &mut Vec<TypeId>,
    ) -> TypeId {
        let node_type = match &self.compiler.get_node(node_id) {
            AstNode::Block(block_id) => {
                self.typecheck_block(node_id, *block_id, local_inferences);
                VOID_TYPE_ID
            }
            AstNode::UnsafeBlock(block) => {
                let block = *block;
                self.enter_scope();
                self.set_unsafe();
                self.typecheck_node(block, local_inferences);
                self.exit_scope();
                VOID_TYPE_ID
            }
            AstNode::Int => I64_TYPE_ID,
            AstNode::Float => F64_TYPE_ID,
            AstNode::True | AstNode::False => BOOL_TYPE_ID,
            AstNode::None => {
                // FIXME: check that this is an optional type
                let type_id = self.compiler.get_node_type(node_id);
                let type_id = self.compiler.resolve_type(type_id, local_inferences);

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
            AstNode::String => {
                self.error("strings not yet supported", node_id);
                UNKNOWN_TYPE_ID
            }
            AstNode::CString => C_STRING_TYPE_ID,
            AstNode::CChar => C_CHAR_TYPE_ID,
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

                let initializer_ty = self.typecheck_node(initializer, local_inferences);
                let initializer_ty = self.compiler.resolve_type(initializer_ty, local_inferences);

                self.maybe_move_variable(initializer, local_inferences);

                let var_id = if let Some(ty) = ty {
                    let ty = self.typecheck_typename(ty);
                    if !self.unify_types(ty, initializer_ty, local_inferences) {
                        self.error(
                            format!(
                                "initializer and given type do not match (expected: {}, found: {})",
                                self.compiler.pretty_type(ty),
                                self.compiler.pretty_type(initializer_ty),
                            ),
                            initializer,
                        );
                    }
                    if let Some(var_id) = self.compiler.var_resolution.get(&variable_name) {
                        let var_id = *var_id;

                        let variable_name = self.compiler.get_source(variable_name).to_vec();
                        self.add_variable_to_scope(variable_name, var_id);

                        var_id
                    } else {
                        self.define_variable(variable_name, ty, is_mutable, node_id)
                    }
                } else {
                    let node_type_id = self.compiler.get_node_type(variable_name);
                    let ty = if node_type_id == UNKNOWN_TYPE_ID {
                        let ty = self.compiler.find_or_create_type(Type::FunLocalTypeVar {
                            offset: local_inferences.len(),
                        });
                        local_inferences.push(UNKNOWN_TYPE_ID);
                        self.compiler.set_node_type(variable_name, ty);

                        ty
                    } else {
                        node_type_id
                    };

                    let ty = if self.compiler.resolve_type(ty, local_inferences) == UNKNOWN_TYPE_ID
                    {
                        ty
                    } else {
                        self.compiler.resolve_type(ty, local_inferences)
                    };

                    if !self.unify_types(ty, initializer_ty, local_inferences) {
                        self.error(
                            format!(
                                "initializer and given type do not match (expected: {}, found: {})",
                                self.compiler.pretty_type(ty),
                                self.compiler.pretty_type(initializer_ty),
                            ),
                            initializer,
                        );
                    }
                    if let Some(var_id) = self.compiler.var_resolution.get(&variable_name) {
                        let var_id = *var_id;

                        let variable_name = self.compiler.get_source(variable_name).to_vec();
                        self.add_variable_to_scope(variable_name, var_id);
                        var_id
                    } else {
                        self.define_variable(variable_name, ty, is_mutable, node_id)
                    }
                };

                self.compiler.var_resolution.insert(variable_name, var_id);

                VOID_TYPE_ID
            }
            AstNode::Name => {
                // This looks like a variable, but may also be the name of a function
                let var_or_fun_id = self.find_name_in_scope(node_id);

                self.typecheck_var_or_fun(node_id, var_or_fun_id)
            }
            AstNode::MemberAccess { target, field } => {
                let target = *target;
                let field = *field;

                let type_id = self.typecheck_node(target, local_inferences);
                let type_id = self.compiler.resolve_type(type_id, local_inferences);

                let target_name = self.compiler.get_source(target);

                let type_id = self.compiler.get_underlying_type_id(type_id);

                let field_name = self.compiler.get_source(field);
                match self.compiler.get_type(type_id) {
                    Type::Struct { fields, .. } => {
                        for TypedField {
                            member_access,
                            name,
                            ty,
                            ..
                        } in fields
                        {
                            let type_id = *ty;

                            if name == field_name {
                                if member_access == &MemberAccess::Private
                                    && target_name != b"."
                                    && target_name != b"self"
                                {
                                    // We're private and not accessing 'self'
                                    self.error("access of private field", field);
                                }

                                self.compiler.set_node_type(node_id, type_id);
                                self.compiler.set_node_type(field, type_id);
                                return type_id;
                            }
                        }
                        if let Some(methods) = self.compiler.methods_on_type.get(&type_id) {
                            for method in methods {
                                let method = *method;
                                let fun = &self.compiler.functions[method.0];
                                let method_name = self.compiler.get_source(fun.name);
                                if field_name == method_name {
                                    self.compiler.fun_resolution.insert(field, method);
                                    self.compiler.fun_resolution.insert(node_id, method);
                                    return self.compiler.find_or_create_type(Type::Fun {
                                        params: fun.params.clone(),
                                        ret: fun.return_type,
                                    });
                                }
                            }
                        }
                        if let Some(virtual_methods) =
                            self.compiler.virtual_methods_on_type.get(&type_id)
                        {
                            for method in virtual_methods {
                                let method = *method;
                                let fun = &self.compiler.functions[method.0];
                                let method_name = self.compiler.get_source(fun.name);
                                if field_name == method_name {
                                    self.compiler.fun_resolution.insert(field, method);
                                    self.compiler.fun_resolution.insert(node_id, method);
                                    return self.compiler.find_or_create_type(Type::Fun {
                                        params: fun.params.clone(),
                                        ret: fun.return_type,
                                    });
                                }
                            }
                        }
                        self.error("unknown field or method", field);
                        UNKNOWN_TYPE_ID
                    }
                    Type::Enum { .. } => {
                        if let Some(methods) = self.compiler.methods_on_type.get(&type_id) {
                            for method in methods {
                                let method = *method;
                                let fun = &self.compiler.functions[method.0];
                                let method_name = self.compiler.get_source(fun.name);
                                if field_name == method_name {
                                    self.compiler.fun_resolution.insert(field, method);
                                    self.compiler.fun_resolution.insert(node_id, method);
                                    return self.compiler.find_or_create_type(Type::Fun {
                                        params: fun.params.clone(),
                                        ret: fun.return_type,
                                    });
                                }
                            }
                        }
                        self.error("unknown method", field);
                        UNKNOWN_TYPE_ID
                    }
                    _ => {
                        self.error(
                            "field or method access on type without fields or methods",
                            target,
                        );
                        UNKNOWN_TYPE_ID
                    }
                }
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                let lhs = *lhs;
                let rhs = *rhs;
                let op = *op;

                match self.compiler.get_node(op) {
                    AstNode::Plus
                    | AstNode::Minus
                    | AstNode::Multiply
                    | AstNode::Divide
                    | AstNode::ShiftLeft
                    | AstNode::ShiftRight
                    | AstNode::BitwiseAnd
                    | AstNode::BitwiseOr => {
                        let lhs_ty = self.typecheck_node(lhs, local_inferences);
                        let rhs_ty = self.typecheck_node(rhs, local_inferences);
                        if !self.unify_types(lhs_ty, rhs_ty, local_inferences) {
                            self.error(
                                format!(
                                    "type mismatch during operation. expected: {}, found: {}",
                                    self.compiler.pretty_type(lhs_ty),
                                    self.compiler.pretty_type(rhs_ty),
                                ),
                                op,
                            )
                        }
                        lhs_ty
                    }
                    AstNode::Equal | AstNode::NotEqual => {
                        let lhs_ty = self.typecheck_node(lhs, local_inferences);

                        // use a quick inference for comparison with 'none'
                        if matches!(self.compiler.get_node(rhs), AstNode::None) {
                            self.compiler.set_node_type(rhs, lhs_ty);
                        }
                        let rhs_ty = self.typecheck_node(rhs, local_inferences);
                        if !self.unify_types(lhs_ty, rhs_ty, local_inferences) {
                            self.error(
                                format!(
                                    "type mismatch during operation. expected: {}, found: {}",
                                    self.compiler.pretty_type(lhs_ty),
                                    self.compiler.pretty_type(rhs_ty),
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
                        let lhs_ty = self.typecheck_node(lhs, local_inferences);
                        let rhs_ty = self.typecheck_node(rhs, local_inferences);
                        if !self.unify_types(lhs_ty, rhs_ty, local_inferences) {
                            self.error(
                                format!(
                                    "type mismatch during operation. expected: {}, found: {}",
                                    self.compiler.pretty_type(lhs_ty),
                                    self.compiler.pretty_type(rhs_ty),
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
                        let lhs_ty = self.typecheck_lvalue(lhs, local_inferences);
                        let rhs_ty = self.typecheck_node(rhs, local_inferences);
                        self.maybe_move_variable(rhs, local_inferences);

                        if !self.unify_types(lhs_ty, rhs_ty, local_inferences) {
                            self.error(
                                format!(
                                    "type mismatch during operation. expected: {}, found: {}",
                                    self.compiler.pretty_type(lhs_ty),
                                    self.compiler.pretty_type(rhs_ty),
                                ),
                                op,
                            );
                        }
                        VOID_TYPE_ID
                    }
                    AstNode::And | AstNode::Or => {
                        let lhs_ty = self.typecheck_node(lhs, local_inferences);
                        let rhs_ty = self.typecheck_node(rhs, local_inferences);

                        if !self.unify_types(lhs_ty, BOOL_TYPE_ID, local_inferences) {
                            self.error(
                                format!(
                                    "type mismatch during operation. expected: bool, found: {}",
                                    self.compiler.pretty_type(lhs_ty),
                                ),
                                lhs,
                            )
                        }
                        if !self.unify_types(rhs_ty, BOOL_TYPE_ID, local_inferences) {
                            self.error(
                                format!(
                                    "type mismatch during operation. expected: bool, found: {}",
                                    self.compiler.pretty_type(rhs_ty),
                                ),
                                rhs,
                            )
                        }

                        BOOL_TYPE_ID
                    }
                    AstNode::As => {
                        let _lhs_ty = self.typecheck_node(lhs, local_inferences);
                        let rhs_ty = self.typecheck_typename(rhs);

                        self.compiler.set_node_type(rhs, rhs_ty);
                        self.compiler.set_node_type(node_id, rhs_ty);

                        //FIXME: Add type conversion compatibility check
                        rhs_ty
                    }
                    x => panic!("unsupported operator: {:?}", x),
                }
            }
            AstNode::Range { lhs, rhs } => {
                let lhs = *lhs;
                let rhs = *rhs;

                let lhs_type = self.typecheck_node(lhs, local_inferences);
                let lhs_type = self.compiler.resolve_type(lhs_type, local_inferences);

                let rhs_type = self.typecheck_node(rhs, local_inferences);
                let rhs_type = self.compiler.resolve_type(rhs_type, local_inferences);

                if lhs_type != I64_TYPE_ID {
                    self.error("expected i64 in range", lhs);
                }

                if rhs_type != I64_TYPE_ID {
                    self.error("expected i64 in range", rhs);
                }

                RANGE_I64_TYPE_ID
            }
            AstNode::NamespacedLookup { namespace, item } => {
                self.typecheck_namespaced_lookup(*namespace, *item, local_inferences)
            }
            AstNode::Use { path } => self.typecheck_module(*path, local_inferences),
            AstNode::Call { head, args } => {
                let head = *head;
                let args = args.clone();
                self.typecheck_call(node_id, head, &args, local_inferences)
            }
            AstNode::New(allocation_type, _, allocation_node_id) => {
                let allocation_type = *allocation_type;
                let allocation_node_id = *allocation_node_id;
                self.typecheck_new(allocation_type, allocation_node_id, local_inferences)
            }
            AstNode::NamedValue { value, .. } => self.typecheck_node(*value, local_inferences),
            AstNode::RawBuffer(items) => {
                let items = items.clone();
                let mut ty = self.compiler.get_node_type(node_id);

                if let Type::RawBuffer(x) = self.compiler.get_type(ty) {
                    ty = *x
                }

                for item in items {
                    let item_ty = self.typecheck_node(item, local_inferences);

                    if ty == UNKNOWN_TYPE_ID {
                        ty = item_ty;
                    } else if ty != item_ty {
                        self.error(
                            format!(
                                "type mismatch in buffer. expected {}, found: {}",
                                self.compiler.pretty_type(ty),
                                self.compiler.pretty_type(item_ty)
                            ),
                            node_id,
                        );
                    }
                }

                if ty == UNKNOWN_TYPE_ID {
                    ty = self.compiler.find_or_create_type(Type::FunLocalTypeVar {
                        offset: local_inferences.len(),
                    });

                    local_inferences.push(UNKNOWN_TYPE_ID);
                }

                self.compiler.find_or_create_type(Type::RawBuffer(ty))
            }
            AstNode::Index { target, index } => {
                let target = *target;
                let index = *index;
                let target_type_id = self.typecheck_node(target, local_inferences);

                let target_type_id = self.compiler.resolve_type(target_type_id, local_inferences);

                let index_type_id = self.typecheck_node(index, local_inferences);

                match self.compiler.get_type(target_type_id) {
                    Type::RawBuffer(inner_type_id) => {
                        let inner_type_id = *inner_type_id;

                        if index_type_id != I64_TYPE_ID {
                            self.error("index with a non-integer type", index);
                        }
                        if !self.unsafe_allowed() {
                            self.error("index into raw buffer requires 'unsafe' block", node_id)
                        }
                        inner_type_id
                    }
                    _ => {
                        self.error("index on a non-buffer type", target);
                        UNKNOWN_TYPE_ID
                    }
                }
            }
            AstNode::Break => {
                //FIXME: ensure that we're inside a loop
                VOID_TYPE_ID
            }
            AstNode::Return(return_expr) => {
                let return_expr = *return_expr;
                let expected_type = self.find_expected_return_type();

                if let Some(return_expr) = return_expr {
                    let expr_type = self.typecheck_node(return_expr, local_inferences);

                    if let Some(expected_type) = expected_type {
                        if !self.unify_types(expected_type, expr_type, local_inferences) {
                            self.error(
                                format!(
                                    "incompatible type at return, found: {} expected: {}",
                                    self.compiler.pretty_type(expr_type),
                                    self.compiler.pretty_type(expected_type)
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
            } => self.typecheck_if(*condition, *then_block, *else_expression, local_inferences),
            AstNode::While { condition, block } => {
                let condition = *condition;
                let block = *block;

                self.typecheck_node(condition, local_inferences);
                self.typecheck_node(block, local_inferences);

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

                let range_type = self.typecheck_node(range, local_inferences);

                if matches!(self.compiler.get_type(range_type), Type::Range(I64_TYPE_ID)) {
                    self.enter_scope();

                    let var_id = self.define_variable(variable, I64_TYPE_ID, true, variable);
                    self.compiler.var_resolution.insert(variable, var_id);

                    self.typecheck_node(block, local_inferences);

                    self.exit_scope();
                } else {
                    self.error("expected range in for loop", range);
                }

                VOID_TYPE_ID
            }
            AstNode::Defer { pointer, callback } => {
                let pointer = *pointer;
                let callback = *callback;

                let pointer_type_id = self.typecheck_node(pointer, local_inferences);
                let callback_type_id = self.typecheck_node(callback, local_inferences);

                match self.compiler.get_type(callback_type_id) {
                    Type::Fun { params, ret } => {
                        if *ret != VOID_TYPE_ID {
                            self.error("callback for 'defer' should not return a value", callback);
                        } else if params.len() == 1 {
                            let var_id = params[0].var_id;

                            if !self.unify_types(
                                self.compiler.get_variable(var_id).ty,
                                pointer_type_id,
                                local_inferences,
                            ) {
                                // FIXME: improve the error message with type name
                                self.error("incompatible type in callback for 'defer'", callback);
                            }
                        } else if params.is_empty() {
                            // we don't use the pointer, ignore it
                        } else {
                            self.error("incompatible callback for 'defer'", callback);
                        }
                    }
                    _ => {
                        self.error("expected function for callback in 'defer'", callback);
                    }
                }

                VOID_TYPE_ID
            }
            AstNode::ResizeRawBuffer { pointer, new_size } => {
                let pointer = *pointer;
                let new_size = *new_size;

                let pointer_type_id = self.typecheck_node(pointer, local_inferences);
                let pointer_type_id = self
                    .compiler
                    .resolve_type(pointer_type_id, local_inferences);

                let new_size_type_id = self.typecheck_node(new_size, local_inferences);
                let new_size_type_id = self
                    .compiler
                    .resolve_type(new_size_type_id, local_inferences);

                let pointer_type_id = self
                    .compiler
                    .resolve_type(pointer_type_id, local_inferences);

                if !self.is_binding_mutable(pointer) {
                    self.error("variable is not mutable", pointer);
                }
                if !matches!(self.compiler.get_type(pointer_type_id), Type::RawBuffer(_)) {
                    self.error("expected raw buffer for resize", pointer);
                }
                if !matches!(self.compiler.get_type(new_size_type_id), Type::I64) {
                    self.error("expected integer size for resize", new_size);
                }

                if !self.unsafe_allowed() {
                    self.error("buffer resize requires 'unsafe' block", node_id);
                }

                VOID_TYPE_ID
            }
            AstNode::Match { target, match_arms } => {
                self.typecheck_match(*target, match_arms.clone(), local_inferences)
            }
            AstNode::Fun { .. }
            | AstNode::Struct { .. }
            | AstNode::Enum { .. }
            | AstNode::ExternType { .. } => {
                // ignore here, since we checked this in an earlier pass
                VOID_TYPE_ID
            }
            AstNode::Statement(node_id) => {
                self.typecheck_node(*node_id, local_inferences);
                VOID_TYPE_ID
            }
            AstNode::TypeCoercion { target_type, .. } => self.compiler.get_node_type(*target_type),
            x => {
                panic!("unsupported node: {:?}", x)
            }
        };

        self.compiler.set_node_type(node_id, node_type);

        node_type
    }

    pub fn typecheck_lvalue(
        &mut self,
        lvalue: NodeId,
        local_inferences: &mut Vec<TypeId>,
    ) -> TypeId {
        match self.compiler.get_node(lvalue) {
            AstNode::Name => {
                self.typecheck_node(lvalue, local_inferences);
                let var_id = self.compiler.var_resolution.get(&lvalue);

                if let Some(var_id) = var_id {
                    let var = &self.compiler.get_variable(*var_id);
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
            AstNode::Index { target, index } => {
                let target = *target;
                let index = *index;

                let target_type_id = self.typecheck_lvalue(target, local_inferences);

                let target_type_id = self.compiler.resolve_type(target_type_id, local_inferences);

                let index_type_id = self.typecheck_node(index, local_inferences);

                if index_type_id != I64_TYPE_ID {
                    self.error("expected integer type for indexing", index);
                }
                if !self.unsafe_allowed() {
                    self.error("index into raw buffer requires 'unsafe' block", lvalue)
                }

                match self.compiler.get_type(target_type_id) {
                    Type::RawBuffer(inner_type_id) => *inner_type_id,
                    _ => {
                        self.error("expected buffer when indexing", target);
                        VOID_TYPE_ID
                    }
                }
            }
            AstNode::MemberAccess { target, field } => {
                let target = *target;
                let field = *field;

                let head_type_id = self.typecheck_lvalue(target, local_inferences);
                let head_type_id = self.compiler.resolve_type(head_type_id, local_inferences);

                let field_name = self.compiler.get_source(field);

                let target_name = self.compiler.get_source(target);

                let target_type_id = self.compiler.get_underlying_type_id(head_type_id);

                match self.compiler.get_type(target_type_id) {
                    Type::Struct { fields, .. } => {
                        for TypedField {
                            member_access,
                            name,
                            ty,
                            ..
                        } in fields
                        {
                            if name == field_name {
                                let ty = *ty;

                                if member_access == &MemberAccess::Private
                                    && target_name != b"."
                                    && target_name != b"self"
                                {
                                    // Private and not accessing 'self'
                                    self.error("modifying private member field", field);
                                }

                                self.compiler.set_node_type(lvalue, ty);
                                self.compiler.set_node_type(field, ty);
                                return ty;
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

    pub fn type_is_owned(&mut self, type_id: TypeId) -> bool {
        match self.compiler.get_type(type_id) {
            Type::Bool | Type::F64 | Type::I64 | Type::Void => true,
            Type::Enum {
                generic_params,
                variants,
                ..
            } => {
                // FIXME: clone
                let generic_params = generic_params.clone();
                let variants = variants.clone();

                for generic_param in generic_params {
                    if !self.type_is_owned(generic_param) {
                        return false;
                    }
                }

                for variant in variants {
                    match variant {
                        EnumVariant::Single { param, .. } => {
                            if !self.type_is_owned(param) {
                                return false;
                            }
                        }
                        EnumVariant::Struct { params, .. } => {
                            for (_, param_type) in params {
                                if !self.type_is_owned(param_type) {
                                    return false;
                                }
                            }
                        }
                        _ => {}
                    }
                }
                true
            }
            Type::Struct {
                generic_params,
                fields,
                ..
            } => {
                // FIXME: clone
                let generic_params = generic_params.clone();
                let fields = fields.clone();

                for generic_param in generic_params {
                    if !self.type_is_owned(generic_param) {
                        return false;
                    }
                }

                for TypedField {
                    member_access,
                    ty: field_type,
                    where_defined,
                    ..
                } in fields
                {
                    if member_access == MemberAccess::Public && !self.type_is_owned(field_type) {
                        self.note("public field is a shared pointer", where_defined);
                        return false;
                    }
                }

                if let Some(methods) = self.compiler.methods_on_type.get(&type_id) {
                    let methods = methods.clone();
                    for method in methods {
                        let fun = &self.compiler.functions[method.0];

                        let return_node = fun.return_node;

                        let mut self_is_mutable = false;

                        // FIXME: clone
                        let params = fun.params.clone();

                        for param in &params {
                            if param.name == b"self" {
                                let var_id = param.var_id;

                                if self.compiler.get_variable(var_id).is_mutable {
                                    self_is_mutable = true;
                                }
                            }
                        }

                        let return_type = fun.return_type;
                        if self_is_mutable {
                            for param in &params {
                                let var_id = param.var_id;

                                let var = &self.compiler.get_variable(var_id);

                                let var_type_id = var.ty;
                                let where_defined = var.where_defined;

                                if !self.type_is_owned(var_type_id) && param.name != b"self" {
                                    self.note(
                                        "param is a shared pointer, and self is mutable",
                                        where_defined,
                                    );
                                    return false;
                                }
                            }
                        }

                        if !self.type_is_owned(return_type) {
                            if let Some(return_node) = return_node {
                                self.note("return type is a shared pointer", return_node);
                            }

                            return false;
                        }
                    }
                }

                true
            }
            Type::Pointer { pointer_type, .. } => pointer_type == &PointerType::Owned,
            _ => true,
        }
    }

    pub fn typecheck_new(
        &mut self,
        pointer_type: PointerType,
        node_id: NodeId,
        local_inferences: &mut Vec<TypeId>,
    ) -> TypeId {
        if let AstNode::Call { head, args } = self.compiler.get_node(node_id) {
            // FIXME: remove clone
            let head = *head;
            let args = args.clone();

            let Some(type_id) = self.find_type_in_scope(head) else {
                self.error("unknown type in allocation", head);
                return UNKNOWN_TYPE_ID;
            };

            let output_type = self.compiler.find_or_create_type(Type::Pointer {
                pointer_type,
                optional: false,
                target: type_id,
            });

            // FIXME: remember the reason why something isn't safe to be owned
            // so we can give a better error
            if pointer_type == PointerType::Owned && !self.type_is_owned(type_id) {
                self.error(
                    "tried to create owned pointer on type that shares its pointers",
                    node_id,
                );
            }

            let type_id = self.compiler.get_underlying_type_id(type_id);

            let mut replacements = HashMap::new();

            match &self.compiler.get_type(type_id) {
                Type::Struct { fields, .. } => {
                    let mut fields = fields.clone();
                    if let Some(base_classes) = self.compiler.base_classes.get(&type_id) {
                        for type_id in base_classes {
                            match self.compiler.get_type(*type_id) {
                                Type::Struct {
                                    fields: base_fields,
                                    ..
                                } => fields.extend_from_slice(&base_fields),
                                _ => {}
                            }
                        }
                    }

                    if args.len() != fields.len() {
                        self.error(
                            format!(
                                "mismatch in number of arguments. expected: {}, found: {}",
                                fields.len(),
                                args.len()
                            ),
                            head,
                        );
                    }

                    'arg: for arg in args {
                        let AstNode::NamedValue { name, value } = self.compiler.get_node(arg)
                        else {
                            self.error("unexpected argument in allocation", arg);
                            return UNKNOWN_TYPE_ID;
                        };

                        let name = *name;
                        let value = *value;

                        let field_name = self.compiler.get_source(name);

                        for TypedField {
                            name,
                            ty,
                            member_access,
                            ..
                        } in &fields
                        {
                            if name == field_name {
                                let member_access = *member_access;

                                if member_access == MemberAccess::Private {
                                    let result = self.find_type_in_scope_by_name(b"Self");

                                    if let Some(scoped_type_id) = result {
                                        if scoped_type_id != type_id {
                                            // FIXME: add a hint to say you need to create your own constructor
                                            self.error("'new' used on private member field from outside struct or class", arg)
                                        }
                                    } else {
                                        self.error("'new' used on private member field from outside struct or class", arg)
                                    }
                                }
                                let known_field_type = *ty;

                                if self.compiler.is_type_variable(known_field_type) {
                                    let value_type = self.typecheck_node(value, local_inferences);

                                    replacements.insert(
                                        self.compiler.get_underlying_type_id(known_field_type),
                                        self.compiler.get_underlying_type_id(value_type),
                                    );

                                    self.compiler.set_node_type(arg, value_type);

                                    // Set up expected type for inference. Note: if we find concrete values
                                    // this inference type will be replaced by the concrete type.
                                    self.compiler.set_node_type(value, value_type);
                                } else {
                                    self.compiler.set_node_type(arg, known_field_type);

                                    // Set up expected type for inference. Note: if we find concrete values
                                    // this inference type will be replaced by the concrete type.
                                    self.compiler.set_node_type(value, known_field_type);
                                    let value_type = self.typecheck_node(value, local_inferences);

                                    if !self.unify_types(
                                        known_field_type,
                                        value_type,
                                        local_inferences,
                                    ) {
                                        self.error(format!("incompatible type for argument, expected: {}, found: {}, ",
                                                    self.compiler.pretty_type(value_type), self.compiler.pretty_type(known_field_type)), value)
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

            if !replacements.is_empty() {
                let type_id = self.instantiate_generic_type(type_id, &replacements);

                self.compiler.find_or_create_type(Type::Pointer {
                    pointer_type,
                    optional: false,
                    target: type_id,
                })
            } else {
                output_type
            }
        } else {
            self.error("expected an allocation call", node_id);
            UNKNOWN_TYPE_ID
        }
    }

    pub fn typecheck_namespaced_lookup(
        &mut self,
        namespace: NodeId,
        item: NodeId,
        local_inferences: &mut Vec<TypeId>,
    ) -> TypeId {
        if let Some(module_id) = self.find_module_in_scope(namespace) {
            self.typecheck_namespaced_item_lookup(namespace, module_id, item, local_inferences)
        } else if let Some(type_id) = self.find_type_in_scope(namespace) {
            self.typecheck_namespaced_type_lookup(namespace, type_id, item, local_inferences)
        } else {
            self.error("could not find namespace", namespace);
            VOID_TYPE_ID
        }
    }

    // enter the module's scope then recurse and call typecheck_namespaced_lookup?
    fn typecheck_namespaced_item_lookup(
        &mut self,
        namespace: NodeId,
        module: ModuleId,
        item: NodeId,
        local_inferences: &mut Vec<TypeId>,
    ) -> TypeId {
        let module = self.compiler.get_module(module);
        let node = self.compiler.get_node(item).clone();
        match &node {
            AstNode::Call { head, args } => {
                // self.typecheck_call(item, head, &args, local_inferences)
                let name = self.compiler.get_source(*head);
                debug!(
                    ?node,
                    "looking for {name} in module",
                    name = std::str::from_utf8(name).unwrap()
                );
                let fun_id = module.scope.find_fun(name);
                if let Some(fun_id) = fun_id {
                    self.typecheck_call_with_fun_id(*head, fun_id, args, None, local_inferences)
                } else {
                    debug!(?node);
                    self.error("could not find item in namespace", namespace);
                    VOID_TYPE_ID
                }
            }
            _ => {
                debug!(?node);
                self.error("could not find item in namespace", namespace);
                VOID_TYPE_ID
            }
        }
    }

    // lookup what kind of type we're working with (struct vs enum)
    // resolve matching method
    // typecheck against the resolved method
    fn typecheck_namespaced_type_lookup(
        &mut self,
        namespace: NodeId,
        mut type_id: TypeId,
        item: NodeId,
        local_inferences: &mut Vec<TypeId>,
    ) -> TypeId {
        match self.compiler.get_type(type_id) {
            Type::Struct { .. } => {
                let AstNode::Call { head, args } = self.compiler.get_node(item) else {
                    self.error("expected static method call on struct", item);
                    return VOID_TYPE_ID;
                };

                let head = *head;
                let args = args.clone();

                let call_name = self.compiler.get_source(head);

                if let Some(methods) = self.compiler.methods_on_type.get(&type_id) {
                    for method in methods {
                        let method_name = self
                            .compiler
                            .get_source(self.compiler.functions[method.0].name);
                        if method_name == call_name {
                            return self.typecheck_call_with_fun_id(
                                head,
                                *method,
                                &args,
                                None,
                                local_inferences,
                            );
                        }
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
                                            let arg_type_id =
                                                self.typecheck_node(args[0], local_inferences);

                                            if self.compiler.is_type_variable(param) {
                                                let mut replacements = HashMap::new();
                                                replacements.insert(
                                                    self.compiler.get_underlying_type_id(param),
                                                    self.compiler
                                                        .get_underlying_type_id(arg_type_id),
                                                );

                                                type_id = self.instantiate_generic_type(
                                                    type_id,
                                                    &replacements,
                                                );
                                            } else if !self.unify_types(
                                                param,
                                                arg_type_id,
                                                local_inferences,
                                            ) {
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
                                                    pointer_type: PointerType::Shared,
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
                                            let mut replacements = HashMap::new();

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

                                                    let arg_type_id = self
                                                        .typecheck_node(value, local_inferences);

                                                    if self
                                                        .compiler
                                                        .is_type_variable(*param_type_id)
                                                    {
                                                        replacements
                                                            .insert(*param_type_id, arg_type_id);
                                                    } else if !self.unify_types(
                                                        *param_type_id,
                                                        arg_type_id,
                                                        local_inferences,
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
                                                self.instantiate_generic_type(
                                                    type_id,
                                                    &replacements,
                                                )
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
                                                    pointer_type: PointerType::Shared,
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
                        let call_name = self.compiler.get_source(head);

                        if let Some(methods) = self.compiler.methods_on_type.get(&type_id) {
                            for method in methods {
                                let method_name = self
                                    .compiler
                                    .get_source(self.compiler.functions[method.0].name);
                                if method_name == call_name {
                                    return self.typecheck_call_with_fun_id(
                                        head,
                                        *method,
                                        &args,
                                        None,
                                        local_inferences,
                                    );
                                }
                            }
                        }

                        self.error("could not find enum case when created enum value", item);
                    }
                    AstNode::Name => {
                        let case_name = self.compiler.get_source(item);

                        for (case_offset, case) in cases.iter().enumerate() {
                            if let EnumVariant::Simple { name } = case {
                                if name == case_name {
                                    self.compiler.call_resolution.insert(
                                        item,
                                        CallTarget::EnumConstructor(
                                            type_id,
                                            CaseOffset(case_offset),
                                        ),
                                    );

                                    return self.compiler.find_or_create_type(Type::Pointer {
                                        pointer_type: PointerType::Shared,
                                        optional: false,
                                        target: type_id,
                                    });
                                }
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
        local_inferences: &mut Vec<TypeId>,
    ) -> TypeId {
        self.typecheck_node(condition, local_inferences);
        self.typecheck_node(then_block, local_inferences);

        if self.compiler.get_node_type(condition) != BOOL_TYPE_ID {
            self.error("condition not a boolean expression", condition);
        }

        if let Some(else_expression) = else_expression {
            self.typecheck_node(else_expression, local_inferences);

            // FIXME: add type compatibility
            if self.compiler.get_node_type(then_block)
                != self.compiler.get_node_type(else_expression)
            {
                self.error("return used outside of a function", else_expression);
            }
        }

        self.compiler.get_node_type(then_block)
    }

    pub fn typecheck_match(
        &mut self,
        target: NodeId,
        match_arms: Vec<(NodeId, NodeId)>,
        local_inferences: &mut Vec<TypeId>,
    ) -> TypeId {
        let target_type_id = self.typecheck_node(target, local_inferences);
        self.maybe_move_variable(target, local_inferences);

        let target_type_id = self.compiler.resolve_type(target_type_id, local_inferences);

        let inner_type_id = match self.compiler.get_type(target_type_id) {
            Type::Pointer { target, .. } => *target,
            _ => target_type_id,
        };

        match self.compiler.get_type(inner_type_id) {
            Type::Enum { variants, .. } => {
                let variants = variants.clone();

                let mut seen_variants = vec![false; variants.len()];

                'arm: for (arm_pattern, arm_result) in match_arms {
                    self.enter_scope();
                    match self.compiler.get_node(arm_pattern) {
                        AstNode::Name => {
                            let var_id = self.define_variable(
                                arm_pattern,
                                target_type_id,
                                false,
                                arm_pattern,
                            );

                            let variable_name = self.compiler.get_source(arm_pattern).to_vec();
                            self.add_variable_to_scope(variable_name, var_id);
                            self.compiler.var_resolution.insert(arm_pattern, var_id);

                            self.typecheck_node(arm_result, local_inferences);

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
                                        AstNode::Name => {
                                            let arm_name = self.compiler.get_source(item);

                                            for (idx, variant) in variants.iter().enumerate() {
                                                if let EnumVariant::Simple { name: variant_name } =
                                                    variant
                                                {
                                                    if variant_name == arm_name {
                                                        self.compiler.call_resolution.insert(
                                                            arm_pattern,
                                                            CallTarget::EnumConstructor(
                                                                inner_type_id,
                                                                CaseOffset(idx),
                                                            ),
                                                        );
                                                        self.typecheck_node(
                                                            arm_result,
                                                            local_inferences,
                                                        );
                                                        seen_variants[idx] = true;
                                                        self.exit_scope();
                                                        continue 'arm;
                                                    }
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
                                                                AstNode::Name
                                                            ) {
                                                                let var_id = self.define_variable(
                                                                    args[0], *param, false, args[0],
                                                                );
                                                                self.compiler
                                                                    .var_resolution
                                                                    .insert(args[0], var_id);
                                                            }
                                                            self.typecheck_node(
                                                                arm_result,
                                                                local_inferences,
                                                            );
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
                                                                    AstNode::Name
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
                                                            self.typecheck_node(
                                                                arm_result,
                                                                local_inferences,
                                                            );
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
                                    String::from_utf8_lossy(name)
                                ),
                                target,
                            ),
                            EnumVariant::Single { name, .. } => self.error(
                                format!(
                                    "missing pattern match for {}(..)",
                                    String::from_utf8_lossy(name)
                                ),
                                target,
                            ),
                            EnumVariant::Struct { name, .. } => self.error(
                                format!(
                                    "missing pattern match for {}(..)",
                                    String::from_utf8_lossy(name)
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

    // pub fn typecheck_method_call(
    //     &mut self,
    //     target: NodeId,
    //     call: NodeId,
    //     node_id: NodeId,
    // ) -> TypeId {
    //     let AstNode::Call { head, args } = self.compiler.get_node(call) else {
    //         panic!("Internal error: method call using a non-call")
    //     };

    //     let head = *head;
    //     // FIXME: fix clone
    //     let args = args.clone();

    //     let name = self.compiler.get_source(head).to_vec();
    //     let type_id = self.typecheck_node(target);

    //     let type_id = self.compiler.get_underlying_type_id(type_id);

    //     match self.compiler.get_type(type_id) {
    //         Type::Struct { methods, .. } => {
    //             for method in methods {
    //                 let method_name = self
    //                     .compiler
    //                     .get_source(self.compiler.functions[method.0].name);

    //                 if method_name == name {
    //                     let type_id =
    //                         self.typecheck_call_with_fun_id(head, *method, &args, Some(target));
    //                     self.compiler.set_node_type(node_id, type_id);
    //                     return type_id;
    //                 }
    //             }
    //             self.error("can't find method in struct", head);
    //         }
    //         _ => self.error("expected struct type for method call", target),
    //     }

    //     VOID_TYPE_ID
    // }

    pub fn typecheck(mut self) -> Compiler {
        let num_nodes = self.compiler.num_ast_nodes();
        self.compiler.resize_node_types(num_nodes, UNKNOWN_TYPE_ID);

        let top_level = NodeId(self.compiler.num_ast_nodes() - 1);
        // Top-level local inferences
        let mut local_inferences = vec![];

        // Typecheck until we hit a fix point for our inferences
        loop {
            let before = local_inferences.clone();

            self.typecheck_node(top_level, &mut local_inferences);

            if local_inferences == before || !self.compiler.errors.is_empty() {
                break;
            }
        }

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
                type_params: vec![],
                lifetime_annotations: vec![],
                inference_vars: local_inferences,
                return_type: top_level_type,
                return_node: None,
                initial_node_id: Some(NodeId(num_nodes)),
                body: Some(top_level),
                is_extern: false,
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

    pub fn add_module_to_scope(&mut self, path: PathBuf, module_id: ModuleId) {
        self.scope
            .last_mut()
            .expect("internal error: missing typechecking scope")
            .modules
            .insert(path, module_id);
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

    pub fn find_name_in_scope(&self, name: NodeId) -> Option<VarOrFunId> {
        let name = self.compiler.get_source(name);

        // Expand the shorthand, and look for 'self' instead
        let name = if name == b"." { b"self" } else { name };

        for scope in self.scope.iter().rev() {
            let opt = scope.find_name(name);
            if opt.is_some() {
                return opt;
            }
        }

        None
    }

    pub fn find_variable_in_scope(&self, variable_name: NodeId) -> Option<VarId> {
        let name = &self.compiler.source
            [self.compiler.span_start[variable_name.0]..self.compiler.span_end[variable_name.0]];

        // Expand the shorthand, and look for 'self' instead
        let name = if name == b"." { b"self" } else { name };

        for scope in self.scope.iter().rev() {
            if let Some(var_id) = scope.variables.get(name) {
                return Some(*var_id);
            }
        }

        None
    }

    // pub fn find_function_in_scope(&self, function_name: NodeId) -> Option<FunId> {
    //     let name = self.compiler.get_source(function_name);
    //     for scope in self.scope.iter().rev() {
    //         if let Some(fun_id) = scope.functions.get(name) {
    //             return Some(*fun_id);
    //         }
    //     }

    //     None
    // }

    // find a module based on the given path segment naming it
    pub fn find_module_in_scope(&self, namespace: NodeId) -> Option<ModuleId> {
        let name = self.compiler.get_source(namespace);
        debug!(
            "searching for module \"{name}\"",
            name = std::str::from_utf8(name).unwrap()
        );
        for scope in self.scope.iter().rev() {
            trace!(num_modules = scope.modules.len());
            for (path, module_id) in scope.modules.iter() {
                trace!(?path, ?module_id);
                // definitely incorrect, but we currently only have one path segment
                // this needs to somehow be able to resolve the path against all the segments of the path
                for path in path.ancestors() {
                    if let Some(file_name) = path.file_stem() {
                        if file_name
                            == unsafe { std::ffi::OsStr::from_encoded_bytes_unchecked(name) }
                        {
                            return Some(*module_id);
                        }
                    }
                }
            }
        }

        None
    }

    pub fn find_type_in_scope(&self, type_name: NodeId) -> Option<TypeId> {
        let name = self.compiler.get_source(type_name);
        for scope in self.scope.iter().rev() {
            let opt = scope.find_type(name);
            if opt.is_some() {
                return opt;
            }
        }

        None
    }

    pub fn find_type_in_scope_by_name(&self, name: &[u8]) -> Option<TypeId> {
        for scope in self.scope.iter().rev() {
            if let Some(type_id) = scope.types.get(name) {
                return Some(*type_id);
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

    pub fn instantiate_generic_fun(
        &mut self,
        fun_id: FunId,
        replacements: &HashMap<TypeId, TypeId>,
    ) -> FunId {
        let Function {
            name,
            params,
            lifetime_annotations,
            return_node,
            return_type,
            initial_node_id,
            body,
            is_extern,
            ..
        } = &self.compiler.functions[fun_id.0];

        let name = *name;
        let params = params.clone();
        let mut new_params = params.clone();
        let mut lifetime_annotations = lifetime_annotations.clone();
        let type_params = vec![];
        let inference_vars = vec![];
        let return_node = *return_node;
        let mut return_type = *return_type;
        let mut initial_node_id = *initial_node_id;
        let mut body = *body;
        let is_extern = *is_extern;

        for new_param in new_params.iter_mut() {
            let mut new_var = self.compiler.get_variable(new_param.var_id).clone();

            new_var.ty = self.instantiate_generic_type(new_var.ty, replacements);

            self.compiler.variables.push(new_var);
            new_param.var_id = VarId(self.compiler.variables.len() - 1);
        }

        for lifetime_annotation in lifetime_annotations.iter_mut() {
            match lifetime_annotation {
                LifetimeAnnotation::Equality(lhs, rhs) => {
                    if let Lifetime::Variable(var_id) = lhs {
                        for (param, new_param) in params.iter().zip(new_params.iter()) {
                            if *var_id == param.var_id {
                                *var_id = new_param.var_id;
                            }
                        }
                    }

                    if let Lifetime::Variable(var_id) = rhs {
                        for (param, new_param) in params.iter().zip(new_params.iter()) {
                            if *var_id == param.var_id {
                                *var_id = new_param.var_id;
                            }
                        }
                    }
                }
            }
        }

        return_type = self.instantiate_generic_type(return_type, replacements);

        if let (Some(inner_initial_node_id), Some(inner_body)) = (initial_node_id, body) {
            let offset = self.compiler.num_ast_nodes() - inner_initial_node_id.0;

            self.compiler.resize_node_types(
                self.compiler.num_ast_nodes() + (inner_body.0 - inner_initial_node_id.0 + 1),
                UNKNOWN_TYPE_ID,
            );

            for raw_node_id in inner_initial_node_id.0..=inner_body.0 {
                let mut ast_node = self.compiler.get_node(NodeId(raw_node_id)).clone();

                match &mut ast_node {
                    AstNode::BinaryOp { lhs, op, rhs } => {
                        *lhs = NodeId(lhs.0 + offset);
                        *op = NodeId(op.0 + offset);
                        *rhs = NodeId(rhs.0 + offset);
                    }
                    AstNode::Block(block_id) => {
                        let mut block = self.compiler.blocks[block_id.0].clone();
                        for node in block.nodes.iter_mut() {
                            *node = NodeId(node.0 + offset)
                        }
                        self.compiler.blocks.push(block);

                        *block_id = BlockId(self.compiler.blocks.len() - 1);
                    }
                    AstNode::Call { head, args } => {
                        *head = NodeId(head.0 + offset);

                        for arg in args {
                            *arg = NodeId(arg.0 + offset);
                        }
                    }
                    AstNode::Defer { pointer, callback } => {
                        *pointer = NodeId(pointer.0 + offset);
                        *callback = NodeId(callback.0 + offset);
                    }
                    AstNode::Enum {
                        typename,
                        cases,
                        methods,
                    } => {
                        *typename = NodeId(typename.0 + offset);

                        for c in cases {
                            *c = NodeId(c.0 + offset)
                        }

                        for method in methods {
                            *method = NodeId(method.0 + offset)
                        }
                    }
                    AstNode::EnumCase { name, payload } => {
                        *name = NodeId(name.0 + offset);
                        if let Some(payload) = payload {
                            for pay in payload {
                                *pay = NodeId(pay.0 + offset)
                            }
                        }
                    }
                    AstNode::ExternType { name } => {
                        *name = NodeId(name.0 + offset);
                    }
                    AstNode::Field { name, typename, .. } => {
                        *name = NodeId(name.0 + offset);
                        *typename = NodeId(typename.0 + offset);
                    }
                    AstNode::For {
                        variable,
                        range,
                        block,
                    } => {
                        *variable = NodeId(variable.0 + offset);
                        *range = NodeId(range.0 + offset);
                        *block = NodeId(block.0 + offset);
                    }
                    AstNode::FunType { params, ret } => {
                        for param in params {
                            *param = NodeId(param.0 + offset)
                        }
                        *ret = NodeId(ret.0 + offset)
                    }
                    AstNode::If {
                        condition,
                        then_block,
                        else_expression,
                    } => {
                        *condition = NodeId(condition.0 + offset);
                        *then_block = NodeId(then_block.0 + offset);
                        if let Some(else_expression) = else_expression {
                            *else_expression = NodeId(else_expression.0 + offset);
                        }
                    }
                    AstNode::Index { target, index } => {
                        *target = NodeId(target.0 + offset);
                        *index = NodeId(index.0 + offset);
                    }
                    AstNode::Let {
                        variable_name,
                        ty,
                        initializer,
                        ..
                    } => {
                        *variable_name = NodeId(variable_name.0 + offset);
                        if let Some(ty) = ty {
                            *ty = NodeId(ty.0 + offset);
                        }
                        *initializer = NodeId(initializer.0 + offset);
                    }
                    AstNode::Match { target, match_arms } => {
                        *target = NodeId(target.0 + offset);
                        for match_arm in match_arms {
                            let lhs = &mut match_arm.0;
                            let rhs = &mut match_arm.1;

                            *lhs = NodeId(lhs.0 + offset);
                            *rhs = NodeId(rhs.0 + offset);
                        }
                    }
                    AstNode::MemberAccess { target, field } => {
                        *target = NodeId(target.0 + offset);
                        *field = NodeId(field.0 + offset);
                    }
                    AstNode::NamedValue { name, value } => {
                        *name = NodeId(name.0 + offset);
                        *value = NodeId(value.0 + offset);
                    }
                    AstNode::NamespacedLookup { namespace, item } => {
                        *namespace = NodeId(namespace.0 + offset);
                        *item = NodeId(item.0 + offset);
                    }
                    AstNode::New(_, _, node_id) => *node_id = NodeId(node_id.0 + offset),
                    AstNode::Param { name, ty, .. } => {
                        *name = NodeId(name.0 + offset);
                        *ty = NodeId(name.0 + offset)
                    }
                    AstNode::Range { lhs, rhs } => {
                        *lhs = NodeId(lhs.0 + offset);
                        *rhs = NodeId(rhs.0 + offset)
                    }
                    AstNode::RawBuffer(items) => {
                        for item in items {
                            *item = NodeId(item.0 + offset)
                        }
                    }
                    AstNode::RawBufferType { inner } => *inner = NodeId(inner.0 + offset),
                    AstNode::ResizeRawBuffer { pointer, new_size } => {
                        *pointer = NodeId(pointer.0 + offset);
                        *new_size = NodeId(new_size.0 + offset)
                    }
                    AstNode::Return(Some(val)) => *val = NodeId(val.0 + offset),
                    AstNode::Statement(stmt) => *stmt = NodeId(stmt.0 + offset),
                    AstNode::Type { name, params, .. } => {
                        *name = NodeId(name.0 + offset);
                        if let Some(params) = params {
                            *params = NodeId(params.0 + offset)
                        }
                    }
                    AstNode::UnsafeBlock(block) => *block = NodeId(block.0 + offset),
                    AstNode::Use { path } => *path = NodeId(path.0 + offset),
                    AstNode::While { condition, block } => {
                        *condition = NodeId(condition.0 + offset);
                        *block = NodeId(block.0 + offset);
                    }
                    _ => {}
                }

                self.compiler.push_node(ast_node);
                self.compiler
                    .span_start
                    .push(self.compiler.span_start[raw_node_id]);
                self.compiler
                    .span_end
                    .push(self.compiler.span_end[raw_node_id]);
            }

            initial_node_id = Some(NodeId(inner_initial_node_id.0 + offset));
            body = Some(NodeId(inner_body.0 + offset));
        }

        self.compiler.functions.push(Function {
            name,
            params: new_params,
            lifetime_annotations,
            type_params,
            inference_vars,
            return_node,
            return_type,
            initial_node_id,
            body,
            is_extern,
        });

        let fun_id = FunId(self.compiler.functions.len() - 1);

        self.typecheck_fun(fun_id);

        fun_id
    }

    pub fn instantiate_generic_type(
        &mut self,
        type_id: TypeId,
        replacements: &HashMap<TypeId, TypeId>,
    ) -> TypeId {
        let mut replacements = replacements.clone();

        match self.compiler.get_type(type_id) {
            Type::Enum { variants, .. } => {
                let mut new_variants = vec![];
                let methods = self
                    .compiler
                    .methods_on_type
                    .get(&type_id)
                    .cloned()
                    .unwrap_or_default();
                let mut new_methods = vec![];

                'variant: for variant in variants {
                    match variant {
                        EnumVariant::Simple { .. } => {
                            new_variants.push(variant.clone());
                        }
                        EnumVariant::Single { name, param } => {
                            if let Some(replacement) = replacements.get(param) {
                                new_variants.push(EnumVariant::Single {
                                    name: name.clone(),
                                    param: *replacement,
                                });
                                continue 'variant;
                            }
                            new_variants.push(EnumVariant::Single {
                                name: name.clone(),
                                param: *param,
                            })
                        }
                        EnumVariant::Struct { name, params } => {
                            let mut new_params = vec![];

                            for param in params {
                                if let Some(replacement) = replacements.get(&param.1) {
                                    new_params.push((param.0.clone(), *replacement));
                                }
                            }
                            new_variants.push(EnumVariant::Struct {
                                name: name.clone(),
                                params: new_params,
                            })
                        }
                    }
                }

                let new_type_id = self.compiler.find_or_create_type(Type::Enum {
                    generic_params: vec![], // we're now fully instantiated
                    variants: new_variants,
                });

                replacements.insert(
                    self.compiler.get_underlying_type_id(type_id),
                    self.compiler.get_underlying_type_id(new_type_id),
                );

                for method in methods {
                    new_methods.push(self.instantiate_generic_fun(method, &replacements));
                }

                self.compiler
                    .methods_on_type
                    .insert(new_type_id, new_methods);

                new_type_id
            }
            Type::Struct {
                fields,
                is_allocator,
                ..
            } => {
                let mut new_fields = vec![];
                let mut new_methods = vec![];
                let methods = self
                    .compiler
                    .methods_on_type
                    .get(&type_id)
                    .cloned()
                    .unwrap_or_default();
                let is_allocator = *is_allocator;

                for TypedField {
                    member_access,
                    name,
                    ty,
                    where_defined,
                } in fields
                {
                    if let Some(replacement) = replacements.get(ty) {
                        new_fields.push(TypedField {
                            member_access: *member_access,
                            name: name.clone(),
                            ty: *replacement,
                            where_defined: *where_defined,
                        });
                        break;
                    }
                }

                let new_type_id = self.compiler.find_or_create_type(Type::Struct {
                    generic_params: vec![], // we're now fully instantiated
                    fields: new_fields,
                    is_allocator,
                });

                replacements.insert(
                    self.compiler.get_underlying_type_id(type_id),
                    self.compiler.get_underlying_type_id(new_type_id),
                );

                for method in methods {
                    let fun_id = self.instantiate_generic_fun(method, &replacements);

                    new_methods.push(fun_id);
                }

                self.compiler
                    .methods_on_type
                    .insert(new_type_id, new_methods);

                new_type_id
            }
            &Type::Pointer {
                pointer_type,
                optional,
                target,
            } => {
                if let Some(replacement) = replacements.get(&target) {
                    return self.compiler.find_or_create_type(Type::Pointer {
                        pointer_type,
                        optional,
                        target: *replacement,
                    });
                }

                type_id
            }
            _ => {
                // Check to see if we have a replacement for this exact TypeId. If so, return the replacement.
                // Otherwise return the original type_id

                if let Some(replacement) = replacements.get(&type_id) {
                    return *replacement;
                }

                type_id
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

    pub fn ends_in_return(&self, node_id: NodeId) -> bool {
        match self.compiler.get_node(node_id) {
            AstNode::Return(_) => true,
            AstNode::Block(stmts) => {
                let block = &self.compiler.blocks[stmts.0];
                if block.nodes.is_empty() {
                    return false;
                }
                self.ends_in_return(
                    *block
                        .nodes
                        .last()
                        .expect("internal error: missing last statement in block"),
                )
            }
            AstNode::UnsafeBlock(block) => self.ends_in_return(*block),
            AstNode::If {
                then_block,
                else_expression,
                ..
            } => {
                let then_block_ends_in_return = self.ends_in_return(*then_block);

                if let Some(else_expression) = else_expression {
                    let else_ends_in_return = self.ends_in_return(*else_expression);

                    return then_block_ends_in_return && else_ends_in_return;
                }

                then_block_ends_in_return
            }
            _ => false,
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

    fn typecheck_module(&mut self, path: NodeId, local_inferences: &mut Vec<TypeId>) -> TypeId {
        let Some(&block) = self.compiler.module_lookup_use.get(&path) else {
            unreachable!("all paths should have valid module blocks associated with them")
        };

        if self.compiler.module_resolution.contains_key(&block) {
            VOID_TYPE_ID
        } else {
            let AstNode::Block(block_id) = &self.compiler.get_node(block) else {
                unreachable!(
                    "module block node ids should always refer to a valid Block in the ast"
                )
            };
            let scope = self.typecheck_block(block, *block_id, local_inferences);
            let module = Module { scope };
            let module_id = self.compiler.add_module(block, module);
            let path = self.compiler.get_source_path(block).to_owned();
            self.add_module_to_scope(path.clone(), module_id);
            debug!(?path, ?module_id, "successfully typechecked module");
            VOID_TYPE_ID
        }
    }

    fn typecheck_var_or_fun(
        &mut self,
        node_id: NodeId,
        var_or_fun_id: Option<VarOrFunId>,
    ) -> TypeId {
        match var_or_fun_id {
            Some(VarOrFunId::VarId(var_id)) => {
                if let Some(where_moved) = self.var_was_previously_moved(var_id) {
                    self.error("moved variable accessed after move", node_id);
                    self.note("location of variable move", where_moved);
                }
                self.compiler.var_resolution.insert(node_id, var_id);

                let variable = &self.compiler.get_variable(var_id);
                variable.ty
            }
            Some(VarOrFunId::FunId(fun_id)) => {
                let fun = &self.compiler.functions[fun_id.0];

                self.compiler.fun_resolution.insert(node_id, fun_id);

                // FIXME: I'm going to hate having these workarounds soon, I bet
                if fun_id.0 != 0 {
                    self.compiler.find_or_create_type(Type::Fun {
                        params: fun.params.clone(),
                        ret: fun.return_type,
                    })
                } else {
                    UNKNOWN_TYPE_ID
                }
            }
            None => {
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
    }
}
