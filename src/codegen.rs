use crate::{
    compiler::Compiler,
    parser::{AstNode, NodeId},
    typechecker::{
        FunId, Function, Param, Type, TypeId, BOOL_TYPE_ID, F64_TYPE_ID, I64_TYPE_ID,
        STRING_TYPE_ID, UNKNOWN_TYPE_ID, VOID_TYPE_ID,
    },
};

pub struct Codegen {
    compiler: Compiler,
}

impl Codegen {
    pub fn new(compiler: Compiler) -> Self {
        Codegen { compiler }
    }

    pub fn codegen_typename(&self, type_id: TypeId, output: &mut Vec<u8>) {
        match &self.compiler.types[type_id.0] {
            Type::Struct(..) => {
                output.extend_from_slice(b"struct struct_");
                output.extend_from_slice(type_id.0.to_string().as_bytes());
            }
            Type::Pointer(_, type_id) => {
                self.codegen_typename(*type_id, output);
                output.push(b'*');
            }
            _ => {
                if type_id == VOID_TYPE_ID {
                    output.extend_from_slice(b"void");
                } else if type_id == I64_TYPE_ID {
                    output.extend_from_slice(b"int64_t");
                } else if type_id == F64_TYPE_ID {
                    output.extend_from_slice(b"double");
                } else if type_id == STRING_TYPE_ID {
                    output.extend_from_slice(b"char*");
                } else if type_id == BOOL_TYPE_ID {
                    output.extend_from_slice(b"bool");
                } else if type_id == UNKNOWN_TYPE_ID {
                    panic!("(unknown) type should be resolved before codegen");
                } else {
                    panic!("unknown type")
                }
            }
        }
    }

    pub fn codegen_allocator_function(
        &self,
        type_id: TypeId,
        fields: &[(Vec<u8>, TypeId)],
        output: &mut Vec<u8>,
    ) {
        let Type::Pointer(_, inner_type_id) = &self.compiler.types[type_id.0] else {
            panic!("internal error: pointer to unknown type");
        };

        output.extend_from_slice(b"struct struct_");
        output.extend_from_slice(inner_type_id.0.to_string().as_bytes());
        output.extend_from_slice(b"* allocator_");
        output.extend_from_slice(inner_type_id.0.to_string().as_bytes());
        output.push(b'(');

        let mut first = true;
        for field in fields {
            if !first {
                output.extend_from_slice(b", ");
            } else {
                first = false;
            }
            self.codegen_typename(field.1, output);
            output.push(b' ');
            output.extend_from_slice(&field.0);
        }
        output.extend_from_slice(b") {\n");

        self.codegen_typename(type_id, output);
        output.extend_from_slice(b" tmp = (");
        self.codegen_typename(type_id, output);
        output.extend_from_slice(b")malloc(sizeof(struct struct_");
        output.extend_from_slice(inner_type_id.0.to_string().as_bytes());
        output.extend_from_slice(b"));\n");

        for field in fields {
            output.extend_from_slice(b"tmp->");
            output.extend_from_slice(&field.0);
            output.extend_from_slice(b" = ");
            output.extend_from_slice(&field.0);
            output.extend_from_slice(b";\n");
        }

        output.extend_from_slice(b"return tmp;\n}\n");
    }

    pub fn codegen_structs(&self, output: &mut Vec<u8>) {
        for (idx, ty) in self.compiler.types.iter().enumerate() {
            if let Type::Struct(fields) = ty {
                output.extend_from_slice(b"struct struct_");
                output.extend_from_slice(idx.to_string().as_bytes());
                output.extend_from_slice(b"{\n");
                for field in fields {
                    self.codegen_typename(field.1, output);
                    output.push(b' ');
                    output.extend_from_slice(&field.0);
                    output.extend_from_slice(b";\n");
                }

                output.extend_from_slice(b"};\n");

                if let Some(ptr) = self.compiler.find_pointer_to(TypeId(idx)) {
                    self.codegen_allocator_function(ptr, fields, output);
                } else {
                    panic!("internal error: can't find pointer to type")
                }
            }
        }
    }

    pub fn codegen_fun_signature(
        &self,
        fun_id: FunId,
        params: &[Param],
        return_type: TypeId,
        output: &mut Vec<u8>,
    ) {
        self.codegen_typename(return_type, output);
        output.push(b' ');
        output.extend_from_slice(b"function_");
        output.extend_from_slice(fun_id.0.to_string().as_bytes());
        output.push(b'(');

        let mut first = true;
        for param in params {
            if !first {
                output.extend_from_slice(b", ");
            } else {
                first = false;
            }

            let variable = &self.compiler.variables[param.var_id.0];
            self.codegen_typename(variable.ty, output);
            output.push(b' ');
            output.extend_from_slice(b"variable_");
            output.extend_from_slice(param.var_id.0.to_string().as_bytes());
        }

        output.push(b')');
    }

    pub fn codegen_fun_decls(&self, output: &mut Vec<u8>) {
        for (
            idx,
            Function {
                params,
                return_type,
                ..
            },
        ) in self.compiler.functions.iter().enumerate().skip(1)
        {
            self.codegen_fun_signature(FunId(idx), params, *return_type, output);

            output.extend_from_slice(b";\n");
        }

        for (
            idx,
            Function {
                params,
                return_type,
                body,
                ..
            },
        ) in self.compiler.functions.iter().enumerate().skip(1)
        {
            self.codegen_fun_signature(FunId(idx), params, *return_type, output);

            output.extend_from_slice(b"{\n");
            self.codegen_block(*body, output);
            output.extend_from_slice(b"}\n");
        }
    }

    pub fn codegen_node(&self, node_id: NodeId, output: &mut Vec<u8>) {
        match &self.compiler.ast_nodes[node_id.0] {
            AstNode::String => {
                let src = self.compiler.get_source(node_id);

                output.extend_from_slice(src);
            }
            AstNode::Int => {
                let src = self.compiler.get_source(node_id);

                output.extend_from_slice(src);
                output.extend_from_slice(b"LL");
            }
            AstNode::Float => {
                let src = self.compiler.get_source(node_id);

                output.extend_from_slice(src);
            }
            AstNode::Name => {
                let src = self.compiler.get_source(node_id);

                output.extend_from_slice(src);
            }
            AstNode::Call { head, args } => {
                let fun_id = self
                    .compiler
                    .fun_resolution
                    .get(head)
                    .expect("internal error: missing call resolution in codegen");

                if fun_id.0 == 0 {
                    // special case for println
                    if self.compiler.node_types[args[0].0] == STRING_TYPE_ID {
                        output.extend_from_slice(b"printf(\"%s\\n\", ");
                        self.codegen_node(args[0], output);
                    } else if self.compiler.node_types[args[0].0] == I64_TYPE_ID {
                        output.extend_from_slice(b"printf(\"%lli\\n\", ");
                        self.codegen_node(args[0], output);
                    } else if self.compiler.node_types[args[0].0] == F64_TYPE_ID {
                        output.extend_from_slice(b"printf(\"%lf\\n\", ");
                        self.codegen_node(args[0], output);
                    } else if self.compiler.node_types[args[0].0] == BOOL_TYPE_ID {
                        output.extend_from_slice(b"printf(\"%s\\n\", (");
                        self.codegen_node(args[0], output);
                        output.extend_from_slice(br#")?"true":"false""#);
                    } else {
                        panic!(
                            "unknown type for printf: {}",
                            self.compiler.node_types[args[0].0].0
                        );
                    }
                    output.extend_from_slice(b");\n");
                    return;
                }

                output.extend_from_slice(b"function_");
                output.extend_from_slice(fun_id.0.to_string().as_bytes());
                output.push(b'(');
                let mut first = true;

                for arg in args {
                    if !first {
                        output.extend_from_slice(b", ");
                    } else {
                        first = false;
                    }

                    self.codegen_node(*arg, output)
                }
                output.push(b')');
            }
            AstNode::Variable => {
                //let src = self.compiler.get_source(node_id);
                let var_id = self
                    .compiler
                    .var_resolution
                    .get(&node_id)
                    .unwrap_or_else(|| {
                        panic!(
                            "internal error: unresolved variable in codegen: {}",
                            node_id.0
                        )
                    });

                output.extend_from_slice(b"variable_");
                output.extend_from_slice(var_id.0.to_string().as_bytes());
            }
            AstNode::Let {
                variable_name,
                initializer,
                ..
            } => {
                let var_id = self
                    .compiler
                    .var_resolution
                    .get(variable_name)
                    .expect("internal error: unresolved variable in codegen");

                let ty = self.compiler.variables[var_id.0].ty;

                self.codegen_typename(ty, output);

                output.extend_from_slice(b" variable_");
                output.extend_from_slice(var_id.0.to_string().as_bytes());

                output.extend_from_slice(b" = ");
                self.codegen_node(*initializer, output);
            }
            AstNode::Plus => {
                output.push(b'+');
            }
            AstNode::Minus => {
                output.push(b'-');
            }
            AstNode::Multiply => {
                output.push(b'*');
            }
            AstNode::Divide => {
                output.push(b'/');
            }
            AstNode::Assignment => {
                output.push(b'=');
            }
            AstNode::AddAssignment => {
                output.extend_from_slice(b"+=");
            }
            AstNode::SubtractAssignment => {
                output.extend_from_slice(b"-=");
            }
            AstNode::MultiplyAssignment => {
                output.extend_from_slice(b"*=");
            }
            AstNode::DivideAssignment => {
                output.extend_from_slice(b"/=");
            }
            AstNode::BinaryOp { lhs, op, rhs } => {
                output.push(b'(');
                self.codegen_node(*lhs, output);
                output.push(b')');

                self.codegen_node(*op, output);

                output.push(b'(');
                self.codegen_node(*rhs, output);
                output.push(b')');
            }
            AstNode::New(_, allocation_call) => {
                let type_id = self.compiler.node_types[node_id.0];

                let Type::Pointer(_, type_id) = &self.compiler.types[type_id.0] else {
                    panic!("internal error: 'new' creating non-pointer type")
                };
                let type_id = *type_id;

                output.extend_from_slice(b"allocator_");
                output.extend_from_slice(type_id.0.to_string().as_bytes());
                output.push(b'(');
                let mut first = true;

                if let AstNode::Call { args, .. } = &self.compiler.ast_nodes[allocation_call.0] {
                    for arg in args {
                        if !first {
                            output.extend_from_slice(b", ");
                        } else {
                            first = false;
                        }

                        self.codegen_node(*arg, output)
                    }
                    output.push(b')');
                } else {
                    panic!("internal error: expected allocation call during allocation")
                }
            }
            AstNode::NamedValue { value, .. } => {
                // FIXME: this should probably be handled cleanly via typecheck+codegen
                // rather than ignoring the name
                self.codegen_node(*value, output)
            }
            AstNode::MemberAccess { target, field } => {
                self.codegen_node(*target, output);
                output.extend_from_slice(b"->");
                self.codegen_node(*field, output);
            }
            AstNode::Statement(node_id) => {
                self.codegen_node(*node_id, output);
                output.extend_from_slice(b";\n");
            }
            AstNode::Block(..) => {
                self.codegen_block(node_id, output);
            }
            AstNode::True => {
                output.extend_from_slice(b"true");
            }
            AstNode::False => {
                output.extend_from_slice(b"false");
            }
            AstNode::Return(expr) => {
                output.extend_from_slice(b"return");
                if let Some(expr) = expr {
                    output.extend_from_slice(b" (");
                    self.codegen_node(*expr, output);
                    output.push(b')');
                }

                output.extend_from_slice(b";\n");
            }
            AstNode::Fun { .. } | AstNode::Struct { .. } => {
                // ignore this, as we handle it elsewhere
            }
            x => {
                panic!("unsupported node: {:?}", x)
            }
        }
    }

    pub fn codegen_block(&self, block: NodeId, output: &mut Vec<u8>) {
        if let AstNode::Block(nodes) = &self.compiler.ast_nodes[block.0] {
            for node_id in nodes {
                self.codegen_node(*node_id, output);
                output.extend_from_slice(b";\n");
            }
        } else {
            panic!("codegen of a block that isn't a block")
        }
    }

    pub fn codegen(self) -> Vec<u8> {
        let mut output = vec![];

        output.extend_from_slice(
            b"#include <stdio.h>\n#include <stdint.h>\n#include <stdbool.h>\n#include <stdlib.h>\n",
        );

        self.codegen_structs(&mut output);
        self.codegen_fun_decls(&mut output);

        let mut main_was_output = false;
        for (idx, fun) in self.compiler.functions.iter().enumerate().skip(1) {
            let name = self.compiler.get_source(fun.name);

            if name == b"main" {
                output.extend_from_slice(b"int main() {\n");
                self.codegen_node(NodeId(self.compiler.ast_nodes.len() - 1), &mut output);
                output.extend_from_slice(b"function_");
                output.extend_from_slice(idx.to_string().as_bytes());
                output.extend_from_slice(b"();\n}\n");
                main_was_output = true;
            }
        }

        if !main_was_output {
            output.extend_from_slice(b"int main() {\n");
            self.codegen_block(NodeId(self.compiler.ast_nodes.len() - 1), &mut output);
            output.extend_from_slice(b"}\n");
        }

        output
    }
}
