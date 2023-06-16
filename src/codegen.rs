use crate::{
    compiler::Compiler,
    parser::{AstNode, NodeId},
    typechecker::{
        FunId, Function, Param, TypeId, BOOL_TYPE_ID, F64_TYPE_ID, I64_TYPE_ID, STRING_TYPE_ID,
        VOID_TYPE_ID,
    },
};

pub struct Codegen {
    compiler: Compiler,
}

impl Codegen {
    pub fn new(compiler: Compiler) -> Self {
        Codegen { compiler }
    }

    pub fn codegen_typename(&self, ty: TypeId, output: &mut Vec<u8>) {
        if ty == VOID_TYPE_ID {
            output.extend_from_slice(b"void");
        } else if ty == I64_TYPE_ID {
            output.extend_from_slice(b"int64_t");
        } else if ty == F64_TYPE_ID {
            output.extend_from_slice(b"double");
        } else if ty == STRING_TYPE_ID {
            output.extend_from_slice(b"char*");
        } else if ty == BOOL_TYPE_ID {
            output.extend_from_slice(b"bool");
        } else {
            panic!("unsupported type");
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

            self.codegen_typename(param.ty, output);
            output.push(b' ');
            output.extend_from_slice(&param.name);
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
            AstNode::Call { head, args } => {
                let fun_id = self
                    .compiler
                    .fun_resolution
                    .get(head)
                    .expect("internal error: missing call resolution in codegen");

                if fun_id.0 == 0 {
                    // special case for println
                    output.extend_from_slice(b"printf(\"%s\\n\", ");
                    self.codegen_node(args[0], output);
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
                let src = self.compiler.get_source(node_id);

                output.extend_from_slice(src);
            }
            _ => {}
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

        output
            .extend_from_slice(b"#include <stdio.h>\n#include <stdint.h>\n#include <stdbool.h>\n");

        self.codegen_fun_decls(&mut output);

        for (idx, fun) in self.compiler.functions.iter().enumerate().skip(1) {
            let name = self.compiler.get_source(fun.name);

            if name == b"main" {
                output.extend_from_slice(b"int main() {\n");
                output.extend_from_slice(b"function_");
                output.extend_from_slice(idx.to_string().as_bytes());
                output.extend_from_slice(b"();\n}\n");
            }
        }

        output
    }
}
