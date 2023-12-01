use crate::compiler::Compiler;
use crate::errors::{Severity, SourceError};

pub struct Parser {
    pub compiler: Compiler,
    pub span_offset: usize,
    content_length: usize,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PointerType {
    Shared,
    Owned,
    Unknown,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MemberAccess {
    Public,
    Private,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RequiredLifetime {
    Local,
    //Stack,
    Unknown,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    Int,
    Float,
    String,
    Name,
    Type {
        name: NodeId,
        params: Option<NodeId>,
        optional: bool,
        pointer_type: PointerType,
    },
    Variable,

    // Booleans
    True,
    False,

    // Special lifetimes
    ReturnLifetime,

    // Empty optional values
    None,

    // Operators
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Plus,
    Append,
    Minus,
    Multiply,
    Divide,
    // Modulo,
    And,
    Or,
    Pow,

    // Assignments
    Assignment,
    AddAssignment,
    SubtractAssignment,
    MultiplyAssignment,
    DivideAssignment,

    // Statements
    Let {
        variable_name: NodeId,
        ty: Option<NodeId>,
        initializer: NodeId,
        is_mutable: bool,
    },
    While {
        condition: NodeId,
        block: NodeId,
    },
    For {
        variable: NodeId,
        range: NodeId,
        block: NodeId,
    },
    Return(Option<NodeId>),
    Break,

    NamespacedLookup {
        namespace: NodeId,
        item: NodeId,
    },

    // Definitions
    Fun {
        name: NodeId,
        params: NodeId,
        lifetime_annotations: Vec<NodeId>,
        return_ty: Option<NodeId>,
        block: Option<NodeId>,
    },
    Params(Vec<NodeId>),
    Param {
        name: NodeId,
        ty: NodeId,
        is_mutable: bool,
    },
    Struct {
        typename: NodeId,
        fields: Vec<NodeId>,
        methods: Vec<NodeId>,
        explicit_no_alloc: bool,
    },
    Field {
        member_access: MemberAccess,
        name: NodeId,
        typename: NodeId,
    },
    Enum {
        typename: NodeId,
        cases: Vec<NodeId>,
        methods: Vec<NodeId>,
    },
    EnumCase {
        name: NodeId,
        payload: Option<Vec<NodeId>>,
    },

    ExternType {
        name: NodeId,
    },

    // Closure {
    //     params: NodeId,
    //     block: NodeId,
    // },

    // Expressions
    Call {
        head: NodeId,
        args: Vec<NodeId>,
    },
    NamedValue {
        name: NodeId,
        value: NodeId,
    },
    BinaryOp {
        lhs: NodeId,
        op: NodeId,
        rhs: NodeId,
    },
    Range {
        lhs: NodeId,
        rhs: NodeId,
    },
    MemberAccess {
        target: NodeId,
        field: NodeId,
    },
    MethodCall {
        target: NodeId,
        call: NodeId,
    },
    Block(BlockId),
    If {
        condition: NodeId,
        then_block: NodeId,
        else_expression: Option<NodeId>,
    },
    Match {
        target: NodeId,
        match_arms: Vec<(NodeId, NodeId)>,
    },
    New(PointerType, RequiredLifetime, NodeId),
    Defer {
        pointer: NodeId,
        callback: NodeId,
    },
    Statement(NodeId),
    Garbage,
}

pub const ASSIGNMENT_PRECEDENCE: usize = 10;

impl AstNode {
    pub fn precedence(&self) -> usize {
        match self {
            AstNode::Pow => 100,
            AstNode::Multiply | AstNode::Divide => 95,
            //AstNode::Modulo => 95,
            AstNode::Plus | AstNode::Minus => 90,
            AstNode::LessThan
            | AstNode::LessThanOrEqual
            | AstNode::GreaterThan
            | AstNode::GreaterThanOrEqual
            | AstNode::Equal
            | AstNode::NotEqual => 80,
            AstNode::And => 50,
            AstNode::Or => 40,
            AstNode::Assignment
            | AstNode::AddAssignment
            | AstNode::SubtractAssignment
            | AstNode::MultiplyAssignment
            | AstNode::DivideAssignment => ASSIGNMENT_PRECEDENCE,
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Debug)]
pub enum TokenType {
    Number,
    Comma,
    String,
    Dot,
    DotDot,
    Name,
    Pipe,
    PipePipe,
    Colon,
    ColonColon,
    Semicolon,
    Plus,
    PlusPlus,
    PlusEquals,
    Dash,
    DashEquals,
    Exclamation,
    Asterisk,
    AsteriskAsterisk,
    AsteriskEquals,
    ForwardSlash,
    ForwardSlashForwardSlash,
    ForwardSlashEquals,
    Equals,
    EqualsEquals,
    EqualsTilde,
    ExclamationTilde,
    ExclamationEquals,
    LParen,
    LSquare,
    LCurly,
    LessThan,
    LessThanEqual,
    RParen,
    RSquare,
    RCurly,
    GreaterThan,
    GreaterThanEqual,
    Ampersand,
    AmpersandAmpersand,
    QuestionMark,
    ThinArrow,
    ThickArrow,
    Newline,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub span_start: usize,
    pub span_end: usize,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub nodes: Vec<NodeId>,
    pub may_locally_allocate: Option<usize>,
}

impl Block {
    pub fn new(nodes: Vec<NodeId>) -> Block {
        Block {
            nodes,
            may_locally_allocate: None,
        }
    }
}

fn is_symbol(b: u8) -> bool {
    [
        b'+', b'-', b'*', b'/', b'.', b',', b'(', b'[', b'{', b'<', b')', b']', b'}', b'>', b':',
        b';', b'=', b'$', b'|', b'!', b'~', b'&', b'\'', b'"', b'?',
    ]
    .contains(&b)
}

impl Parser {
    pub fn new(compiler: Compiler, span_offset: usize) -> Self {
        let content_length = compiler.source.len() - span_offset;
        Self {
            compiler,
            content_length,
            span_offset,
        }
    }

    fn position(&mut self) -> usize {
        if let Some(Token { span_start, .. }) = self.peek() {
            span_start
        } else {
            self.content_length
        }
    }

    fn get_span_end(&self, node_id: NodeId) -> usize {
        self.compiler.span_end[node_id.0]
    }

    pub fn parse(mut self) -> Compiler {
        self.program();

        self.compiler
    }

    pub fn program(&mut self) -> NodeId {
        self.block(false)
    }

    pub fn has_tokens(&mut self) -> bool {
        self.peek().is_some()
    }

    pub fn is_operator(&mut self) -> bool {
        match self.peek() {
            Some(Token { token_type, .. }) => matches!(
                token_type,
                TokenType::Asterisk
                    | TokenType::AsteriskAsterisk
                    | TokenType::Dash
                    | TokenType::EqualsEquals
                    | TokenType::ExclamationEquals
                    | TokenType::ForwardSlash
                    | TokenType::LessThan
                    | TokenType::LessThanEqual
                    | TokenType::Plus
                    | TokenType::GreaterThan
                    | TokenType::GreaterThanEqual
                    | TokenType::AmpersandAmpersand
                    | TokenType::PipePipe
                    | TokenType::Equals
                    | TokenType::PlusEquals
                    | TokenType::DashEquals
                    | TokenType::AsteriskEquals
                    | TokenType::ForwardSlashEquals
            ),
            _ => false,
        }
    }

    pub fn is_comma(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Comma,
                ..
            })
        )
    }

    pub fn is_lcurly(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::LCurly,
                ..
            })
        )
    }

    pub fn is_rcurly(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::RCurly,
                ..
            })
        )
    }

    pub fn is_lparen(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::LParen,
                ..
            })
        )
    }

    pub fn is_rparen(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::RParen,
                ..
            })
        )
    }

    pub fn is_lsquare(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::LSquare,
                ..
            })
        )
    }

    pub fn is_rsquare(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::RSquare,
                ..
            })
        )
    }

    pub fn is_less_than(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::LessThan,
                ..
            })
        )
    }

    pub fn is_greater_than(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::GreaterThan,
                ..
            })
        )
    }

    pub fn is_pipe(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Pipe,
                ..
            })
        )
    }

    pub fn is_question_mark(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::QuestionMark,
                ..
            })
        )
    }

    pub fn is_thin_arrow(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::ThinArrow,
                ..
            })
        )
    }

    pub fn is_thick_arrow(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::ThickArrow,
                ..
            })
        )
    }

    // pub fn is_double_pipe(&mut self) -> bool {
    //     matches!(
    //         self.peek(),
    //         Some(Token {
    //             token_type: TokenType::PipePipe,
    //             ..
    //         })
    //     )
    // }

    // pub fn is_double_ampersand(&mut self) -> bool {
    //     matches!(
    //         self.peek(),
    //         Some(Token {
    //             token_type: TokenType::AmpersandAmpersand,
    //             ..
    //         })
    //     )
    // }

    // pub fn is_dash(&mut self) -> bool {
    //     matches!(
    //         self.peek(),
    //         Some(Token {
    //             token_type: TokenType::Dash,
    //             ..
    //         })
    //     )
    // }

    pub fn is_colon(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Colon,
                ..
            })
        )
    }

    pub fn is_newline(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Newline,
                ..
            })
        )
    }

    pub fn is_semicolon(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Semicolon,
                ..
            })
        )
    }

    pub fn is_dot(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Dot,
                ..
            })
        )
    }

    pub fn is_dotdot(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::DotDot,
                ..
            })
        )
    }

    pub fn is_coloncolon(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::ColonColon,
                ..
            })
        )
    }

    pub fn is_number(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Number,
                ..
            })
        )
    }

    pub fn is_minus(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Dash,
                ..
            })
        )
    }

    pub fn is_string(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::String,
                ..
            })
        )
    }

    pub fn is_keyword(&mut self, keyword: &[u8]) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
            }) if &self.compiler.source[span_start..span_end] == keyword
        )
    }

    pub fn is_name(&mut self) -> bool {
        matches!(
            self.peek(),
            Some(Token {
                token_type: TokenType::Name,
                ..
            })
        )
    }

    pub fn is_expression(&mut self) -> bool {
        self.is_simple_expression() || self.is_keyword(b"if") || self.is_keyword(b"where")
    }

    pub fn is_simple_expression(&mut self) -> bool {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Number,
                ..
            })
            | Some(Token {
                token_type: TokenType::Dash,
                ..
            })
            | Some(Token {
                token_type: TokenType::String,
                ..
            })
            | Some(Token {
                token_type: TokenType::LCurly,
                ..
            })
            | Some(Token {
                token_type: TokenType::LSquare,
                ..
            })
            | Some(Token {
                token_type: TokenType::LParen,
                ..
            }) => true,
            Some(Token {
                token_type: TokenType::Dot,
                ..
            }) => true,
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
            }) if &self.compiler.source[span_start..span_end] == b"true" => true,
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
            }) if &self.compiler.source[span_start..span_end] == b"false" => true,
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
            }) if &self.compiler.source[span_start..span_end] == b"new" => true,
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
            }) if &self.compiler.source[span_start..span_end] == b"local" => true,
            Some(Token {
                token_type: TokenType::Name,
                ..
            }) => true,
            _ => false,
        }
    }

    pub fn error_on_node(&mut self, message: impl Into<String>, node_id: NodeId) {
        self.compiler.errors.push(SourceError {
            message: message.into(),
            node_id,
            severity: Severity::Error,
        });
    }

    pub fn error(&mut self, message: impl Into<String>) -> NodeId {
        if let Some(Token {
            span_start,
            span_end,
            ..
        }) = self.next()
        {
            let node_id = self.create_node(AstNode::Garbage, span_start, span_end);
            self.compiler.errors.push(SourceError {
                message: message.into(),
                node_id,
                severity: Severity::Error,
            });

            node_id
        } else {
            let node_id =
                self.create_node(AstNode::Garbage, self.content_length, self.content_length);
            self.compiler.errors.push(SourceError {
                message: message.into(),
                node_id,
                severity: Severity::Error,
            });

            node_id
        }
    }

    pub fn create_node(&mut self, ast_node: AstNode, span_start: usize, span_end: usize) -> NodeId {
        self.compiler.span_start.push(span_start);
        self.compiler.span_end.push(span_end);
        self.compiler.push_node(ast_node)
    }

    pub fn block(&mut self, expect_curly_braces: bool) -> NodeId {
        let span_start = self.position();
        let mut span_end = self.position();

        let mut code_body = vec![];
        if expect_curly_braces {
            self.lcurly();
        }

        while self.has_tokens() {
            if self.is_rcurly() && expect_curly_braces {
                span_end = self.position() + 1;
                self.rcurly();
                break;
            } else if self.is_semicolon() || self.is_newline() {
                self.next();
                continue;
            } else if self.is_keyword(b"fun") {
                code_body.push(self.fun_definition());
            } else if self.is_keyword(b"extern") {
                code_body.push(self.extern_definition());
            } else if self.is_keyword(b"struct") {
                code_body.push(self.class_struct_definition(false));
            } else if self.is_keyword(b"class") {
                code_body.push(self.class_struct_definition(true));
            } else if self.is_keyword(b"enum") {
                code_body.push(self.enum_definition());
            } else if self.is_keyword(b"let") {
                code_body.push(self.let_statement());
            } else if self.is_keyword(b"mut") {
                code_body.push(self.mut_statement());
            } else if self.is_keyword(b"while") {
                code_body.push(self.while_statement());
            } else if self.is_keyword(b"for") {
                code_body.push(self.for_statement());
            } else if self.is_keyword(b"return") {
                code_body.push(self.return_statement());
            } else if self.is_keyword(b"break") {
                code_body.push(self.break_statement());
            } else if self.is_keyword(b"defer") {
                code_body.push(self.defer_statement());
            } else {
                let span_start = self.position();
                let expression = self.expression_or_assignment();
                let span_end = self.get_span_end(expression);

                if self.is_semicolon() {
                    // This is a statement, not an expression
                    self.next();
                    code_body.push(self.create_node(
                        AstNode::Statement(expression),
                        span_start,
                        span_end,
                    ))
                } else {
                    code_body.push(expression);
                }
            }
        }

        self.compiler.blocks.push(Block::new(code_body));

        self.create_node(
            AstNode::Block(BlockId(self.compiler.blocks.len() - 1)),
            span_start,
            span_end,
        )
    }

    pub fn extern_definition(&mut self) -> NodeId {
        let span_start = self.position();

        self.keyword(b"extern");

        if self.is_keyword(b"type") {
            self.keyword(b"type");

            let name = self.name();
            let span_start = self.compiler.span_start[name.0];
            let span_end = self.compiler.span_end[name.0];

            self.create_node(AstNode::ExternType { name }, span_start, span_end)
        } else {
            // Assume "C" for now?
            self.string();

            self.keyword(b"fun");

            let name = self.name();
            let params = self.params();

            let span_end;

            let return_ty = if self.is_thin_arrow() {
                self.next();
                let typename = self.typename();
                span_end = self.get_span_end(typename);
                Some(typename)
            } else {
                span_end = self.get_span_end(params);
                None
            };

            self.create_node(
                AstNode::Fun {
                    name,
                    params,
                    lifetime_annotations: vec![],
                    return_ty,
                    block: None,
                },
                span_start,
                span_end,
            )
        }
    }

    pub fn fun_definition(&mut self) -> NodeId {
        let span_start = self.position();
        self.keyword(b"fun");

        let name = self.name();
        let params = self.params();

        let mut lifetime_annotations = vec![];

        if self.is_lsquare() {
            // we have lifetime constraints/annotations

            self.lsquare();

            loop {
                if self.is_rsquare() {
                    self.rsquare();
                    break;
                } else if self.is_newline() {
                    self.newline();
                } else if self.is_comma() {
                    self.next();
                } else if self.is_name() {
                    let lhs = if self.is_keyword(b"return") {
                        self.return_lifetime()
                    } else {
                        self.variable()
                    };

                    let op = self.equalsequals();

                    let rhs = if self.is_keyword(b"return") {
                        self.return_lifetime()
                    } else {
                        self.variable()
                    };

                    let span_start = self.compiler.span_start[lhs.0];
                    let span_end = self.compiler.span_end[rhs.0];

                    lifetime_annotations.push(self.create_node(
                        AstNode::BinaryOp { lhs, op, rhs },
                        span_start,
                        span_end,
                    ));
                } else {
                    self.error("expected lifetime annotation");
                    break;
                }
            }
        }

        let return_ty = if self.is_thin_arrow() {
            self.next();
            Some(self.typename())
        } else {
            None
        };

        let block = self.block(true);

        let span_end = self.get_span_end(block);

        self.create_node(
            AstNode::Fun {
                name,
                params,
                lifetime_annotations,
                return_ty,
                block: Some(block),
            },
            span_start,
            span_end,
        )
    }

    pub fn class_struct_definition(&mut self, private_by_default: bool) -> NodeId {
        let mut fields = vec![];
        let mut methods = vec![];

        let span_start = self.position();
        let mut span_end = self.position();

        if private_by_default {
            self.keyword(b"class");
        } else {
            self.keyword(b"struct");
        }

        let explicit_no_alloc = if self.is_keyword(b"noalloc") {
            self.next();
            true
        } else {
            false
        };

        let name = self.typename();
        self.lcurly();

        // parse fields
        while self.has_tokens() {
            if self.is_rcurly() {
                span_end = self.position() + 1;
                self.rcurly();
                break;
            }

            if self.is_keyword(b"fun") {
                let fun = self.fun_definition();

                methods.push(fun);
            } else if self.is_newline() {
                self.next();
            } else {
                // field
                let member_access = if self.is_keyword(b"private") {
                    self.next();
                    MemberAccess::Private
                } else if self.is_keyword(b"public") {
                    self.next();
                    MemberAccess::Public
                } else if private_by_default {
                    MemberAccess::Private
                } else {
                    MemberAccess::Public
                };

                let field_name = self.name();
                self.colon();
                let field_type = self.typename();
                if self.is_comma() {
                    self.comma();
                }

                let field = self.create_node(
                    AstNode::Field {
                        member_access,
                        name: field_name,
                        typename: field_type,
                    },
                    span_start,
                    span_end,
                );
                fields.push(field);
            }
        }

        self.create_node(
            AstNode::Struct {
                typename: name,
                fields,
                methods,
                explicit_no_alloc,
            },
            span_start,
            span_end,
        )
    }

    pub fn enum_definition(&mut self) -> NodeId {
        let mut cases = vec![];
        let mut methods = vec![];

        let span_start = self.position();
        let mut span_end = self.position();

        self.keyword(b"enum");

        let name = self.typename();
        self.lcurly();

        // parse fields
        while self.has_tokens() {
            if self.is_rcurly() {
                span_end = self.position() + 1;
                self.rcurly();
                break;
            }

            if self.is_keyword(b"fun") {
                let fun = self.fun_definition();

                methods.push(fun);
            } else if self.is_newline() {
                self.next();
            } else {
                // enum case
                let case = self.enum_case();

                cases.push(case);
            }
        }

        self.create_node(
            AstNode::Enum {
                typename: name,
                cases,
                methods,
            },
            span_start,
            span_end,
        )
    }

    pub fn enum_case(&mut self) -> NodeId {
        let span_start = self.position();
        let name = self.name();
        let mut span_end = self.get_span_end(name);
        let payload = if self.is_lparen() {
            self.next();
            let payload = self.typename();
            if !self.is_rparen() {
                self.error("expected right paren ')'");
            } else {
                self.next();
            }
            Some(vec![payload])
        } else if self.is_lcurly() {
            self.lcurly();
            let mut payload = vec![];
            while self.has_tokens() {
                if self.is_rcurly() {
                    span_end = self.position() + 1;
                    self.rcurly();
                    break;
                }

                // field
                let span_start = self.position();
                let field_name = self.name();
                self.colon();
                let field_type = self.typename();
                if self.is_comma() {
                    self.comma();
                }

                let named_field = self.create_node(
                    AstNode::NamedValue {
                        name: field_name,
                        value: field_type,
                    },
                    span_start,
                    span_end,
                );
                payload.push(named_field);
            }
            Some(payload)
        } else {
            None
        };

        self.create_node(AstNode::EnumCase { name, payload }, span_start, span_end)
    }

    pub fn expression_or_assignment(&mut self) -> NodeId {
        self.math_expression(true)
    }

    pub fn expression(&mut self) -> NodeId {
        self.math_expression(false)
    }

    pub fn math_expression(&mut self, allow_assignment: bool) -> NodeId {
        let mut expr_stack = vec![];

        let mut last_prec = 1000000;

        let span_start = self.position();

        // Check for special forms
        if self.is_keyword(b"if") {
            return self.if_expression();
        } else if self.is_keyword(b"new") || self.is_keyword(b"local") {
            return self.new_allocation();
        } else if self.is_keyword(b"match") {
            return self.match_expression();
        }

        // Otherwise assume a math expression
        let lhs = if self.is_simple_expression() {
            self.simple_expression()
        } else {
            return self.error("incomplete math expression");
        };

        if let Some(Token {
            token_type: TokenType::Equals,
            ..
        }) = self.peek()
        {
            if !allow_assignment {
                self.error("assignment found in expression");
            }
            let op = self.operator();

            let rhs = self.expression();
            let span_end = self.get_span_end(rhs);

            return self.create_node(AstNode::BinaryOp { lhs, op, rhs }, span_start, span_end);
        }

        expr_stack.push(lhs);

        while self.has_tokens() {
            if self.is_operator() {
                let op = self.operator();
                let op_prec = self.operator_precedence(op);

                if op_prec == ASSIGNMENT_PRECEDENCE && !allow_assignment {
                    self.error_on_node("assignment found in expression", op);
                }

                let rhs = if self.is_simple_expression() {
                    self.simple_expression()
                } else {
                    self.error("incomplete math expression")
                };

                while op_prec <= last_prec && expr_stack.len() > 1 {
                    let rhs = expr_stack
                        .pop()
                        .expect("internal error: expression stack empty");
                    let op = expr_stack
                        .pop()
                        .expect("internal error: expression stack empty");

                    last_prec = self.operator_precedence(op);

                    if last_prec < op_prec {
                        expr_stack.push(op);
                        expr_stack.push(rhs);
                        break;
                    }

                    let lhs = expr_stack
                        .pop()
                        .expect("internal error: expression stack empty");

                    let (span_start, span_end) = self.spanning(lhs, rhs);
                    expr_stack.push(self.create_node(
                        AstNode::BinaryOp { lhs, op, rhs },
                        span_start,
                        span_end,
                    ))
                }

                expr_stack.push(op);
                expr_stack.push(rhs);

                last_prec = op_prec;
            } else {
                break;
            }
        }

        while expr_stack.len() > 1 {
            let rhs = expr_stack
                .pop()
                .expect("internal error: expression stack empty");
            let op = expr_stack
                .pop()
                .expect("internal error: expression stack empty");
            let lhs = expr_stack
                .pop()
                .expect("internal error: expression stack empty");

            let (span_start, span_end) = self.spanning(lhs, rhs);

            expr_stack.push(self.create_node(
                AstNode::BinaryOp { lhs, op, rhs },
                span_start,
                span_end,
            ))
        }

        expr_stack
            .pop()
            .expect("internal error: expression stack empty")
    }

    pub fn simple_expression(&mut self) -> NodeId {
        let span_start = self.position();

        let mut expr = if self.is_lcurly() {
            self.block(true)
        } else if self.is_lparen() {
            self.lparen();
            let output = self.expression();
            self.rparen();
            output
        } else if self.is_keyword(b"true") || self.is_keyword(b"false") {
            self.boolean()
        } else if self.is_keyword(b"none") {
            self.none()
        } else if self.is_keyword(b"new") || self.is_keyword(b"local") {
            self.new_allocation()
        } else if self.is_string() {
            self.string()
        } else if self.is_number() || self.is_minus() {
            self.number()
        } else if self.is_name() {
            self.variable_or_call()
        } else if self.is_dot() {
            let span_start = self.position();
            let span_end = self.position() + 1;

            self.create_node(AstNode::Variable, span_start, span_end)
        } else {
            self.error("incomplete expression")
        };

        loop {
            if self.is_dotdot() {
                // Range
                self.next();

                let rhs = self.simple_expression();
                let span_end = self.get_span_end(rhs);

                expr = self.create_node(AstNode::Range { lhs: expr, rhs }, span_start, span_end);
            } else if self.is_dot() {
                // Member access
                self.next();

                let prev_offset = self.span_offset;

                let name = self.name();

                let field_or_call = if self.is_lparen() {
                    self.span_offset = prev_offset;
                    self.variable_or_call()
                } else {
                    name
                };
                let span_end = self.get_span_end(field_or_call);

                match self.compiler.get_node_mut(field_or_call) {
                    AstNode::Variable | AstNode::Name => {
                        expr = self.create_node(
                            AstNode::MemberAccess {
                                target: expr,
                                field: field_or_call,
                            },
                            span_start,
                            span_end,
                        );
                    }
                    AstNode::Call { args, .. } => {
                        args.insert(0, expr);
                        expr = self.create_node(
                            AstNode::MethodCall {
                                target: expr,
                                call: field_or_call,
                            },
                            span_start,
                            span_end,
                        )
                    }
                    _ => {
                        self.error("expected field or method call");
                    }
                }
            } else if self.is_coloncolon() {
                self.next();

                let item = self.simple_expression();
                let span_end = self.get_span_end(item);

                expr = self.create_node(
                    AstNode::NamespacedLookup {
                        namespace: expr,
                        item,
                    },
                    span_start,
                    span_end,
                );
            } else {
                return expr;
            }
        }
    }

    pub fn number(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Number,
                span_start,
                span_end,
            }) => {
                self.next();
                let contents = &self.compiler.source[span_start..span_end];

                if contents.contains(&b'.') {
                    self.create_node(AstNode::Float, span_start, span_end)
                } else {
                    self.create_node(AstNode::Int, span_start, span_end)
                }
            }
            Some(Token {
                token_type: TokenType::Dash,
                span_start,
                ..
            }) => {
                self.next();
                let remaining = self.number();
                let span_end = self.get_span_end(remaining);
                let contents = &self.compiler.source[span_start..span_end];

                if contents.contains(&b'.') {
                    self.create_node(AstNode::Float, span_start, span_end)
                } else {
                    self.create_node(AstNode::Int, span_start, span_end)
                }
            }
            _ => self.error("expected: number"),
        }
    }

    pub fn boolean(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
            }) if &self.compiler.source[span_start..span_end] == b"true" => {
                self.next();
                self.create_node(AstNode::True, span_start, span_end)
            }
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
            }) if &self.compiler.source[span_start..span_end] == b"false" => {
                self.next();
                self.create_node(AstNode::False, span_start, span_end)
            }
            _ => self.error("expected: boolean"),
        }
    }

    pub fn none(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
            }) if &self.compiler.source[span_start..span_end] == b"none" => {
                self.next();

                self.create_node(AstNode::None, span_start, span_end)
            }
            _ => self.error("expected: none"),
        }
    }

    pub fn operator(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type,
                span_start,
                span_end,
                ..
            }) => match token_type {
                TokenType::Plus => {
                    self.next();
                    self.create_node(AstNode::Plus, span_start, span_end)
                }
                TokenType::PlusPlus => {
                    self.next();
                    self.create_node(AstNode::Append, span_start, span_end)
                }
                TokenType::Dash => {
                    self.next();
                    self.create_node(AstNode::Minus, span_start, span_end)
                }
                TokenType::Asterisk => {
                    self.next();
                    self.create_node(AstNode::Multiply, span_start, span_end)
                }
                TokenType::ForwardSlash => {
                    self.next();
                    self.create_node(AstNode::Divide, span_start, span_end)
                }
                TokenType::LessThan => {
                    self.next();
                    self.create_node(AstNode::LessThan, span_start, span_end)
                }
                TokenType::LessThanEqual => {
                    self.next();
                    self.create_node(AstNode::LessThanOrEqual, span_start, span_end)
                }
                TokenType::GreaterThan => {
                    self.next();
                    self.create_node(AstNode::GreaterThan, span_start, span_end)
                }
                TokenType::GreaterThanEqual => {
                    self.next();
                    self.create_node(AstNode::GreaterThanOrEqual, span_start, span_end)
                }
                TokenType::EqualsEquals => {
                    self.next();
                    self.create_node(AstNode::Equal, span_start, span_end)
                }
                TokenType::ExclamationEquals => {
                    self.next();
                    self.create_node(AstNode::NotEqual, span_start, span_end)
                }
                TokenType::AsteriskAsterisk => {
                    self.next();
                    self.create_node(AstNode::Pow, span_start, span_end)
                }
                TokenType::AmpersandAmpersand => {
                    self.next();
                    self.create_node(AstNode::And, span_start, span_end)
                }
                TokenType::PipePipe => {
                    self.next();
                    self.create_node(AstNode::Or, span_start, span_end)
                }
                TokenType::Equals => {
                    self.next();
                    self.create_node(AstNode::Assignment, span_start, span_end)
                }
                TokenType::PlusEquals => {
                    self.next();
                    self.create_node(AstNode::AddAssignment, span_start, span_end)
                }
                TokenType::DashEquals => {
                    self.next();
                    self.create_node(AstNode::SubtractAssignment, span_start, span_end)
                }
                TokenType::AsteriskEquals => {
                    self.next();
                    self.create_node(AstNode::MultiplyAssignment, span_start, span_end)
                }
                TokenType::ForwardSlashEquals => {
                    self.next();
                    self.create_node(AstNode::DivideAssignment, span_start, span_end)
                }
                _ => self.error("expected: operator"),
            },
            _ => self.error("expected: operator"),
        }
    }

    pub fn operator_precedence(&mut self, operator: NodeId) -> usize {
        self.compiler.get_node(operator).precedence()
    }

    pub fn spanning(&mut self, from: NodeId, to: NodeId) -> (usize, usize) {
        (
            self.compiler.span_start[from.0],
            self.compiler.span_end[to.0],
        )
    }

    pub fn string(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::String,
                span_start,
                span_end,
                ..
            }) => {
                self.next();
                self.create_node(AstNode::String, span_start, span_end)
            }
            _ => self.error("expected: string"),
        }
    }

    pub fn name(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
                ..
            }) => {
                self.next();
                self.create_node(AstNode::Name, span_start, span_end)
            }
            _ => self.error("expect name"),
        }
    }

    pub fn equalsequals(&mut self) -> NodeId {
        match self.peek() {
            Some(Token {
                token_type: TokenType::EqualsEquals,
                span_start,
                span_end,
                ..
            }) => {
                self.next();
                self.create_node(AstNode::Equal, span_start, span_end)
            }
            _ => self.error("expect name"),
        }
    }

    pub fn typename(&mut self) -> NodeId {
        let pointer_type = if self.is_keyword(b"owned") {
            self.next();
            PointerType::Owned
        } else {
            PointerType::Shared
        };

        match self.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
                ..
            }) => {
                let name = self.name();
                let mut params = None;
                if self.is_less_than() {
                    // We have generics
                    params = Some(self.type_params());
                }

                let optional = if self.is_question_mark() {
                    // We have an optional type
                    self.next();
                    true
                } else {
                    false
                };

                self.create_node(
                    AstNode::Type {
                        name,
                        params,
                        optional,
                        pointer_type,
                    },
                    span_start,
                    span_end,
                )
            }
            _ => self.error("expect name"),
        }
    }

    pub fn type_params(&mut self) -> NodeId {
        let span_start = self.position();
        let span_end;
        let param_list = {
            self.less_than();

            let mut output = vec![];

            while self.has_tokens() {
                if self.is_greater_than() {
                    break;
                }

                if self.is_comma() {
                    self.next();
                    continue;
                }

                output.push(self.name());
            }

            span_end = self.position() + 1;
            self.greater_than();

            output
        };

        self.create_node(AstNode::Params(param_list), span_start, span_end)
    }

    pub fn params(&mut self) -> NodeId {
        let span_start = self.position();
        let span_end;
        let param_list = {
            self.lparen();
            let output = self.param_list();
            span_end = self.position() + 1;
            self.rparen();

            output
        };

        self.create_node(AstNode::Params(param_list), span_start, span_end)
    }

    pub fn param_list(&mut self) -> Vec<NodeId> {
        let mut params = vec![];
        while self.has_tokens() {
            if self.is_rparen() || self.is_rsquare() || self.is_pipe() {
                break;
            }

            if self.is_comma() {
                self.next();
                continue;
            }

            // Parse param
            let span_start = self.position();

            let is_mutable = if self.is_keyword(b"mut") {
                self.next();
                true
            } else {
                false
            };

            let name = self.name();
            if self.is_colon() {
                self.colon();

                let ty = self.typename();

                let span_end = self.get_span_end(ty);

                params.push(self.create_node(
                    AstNode::Param {
                        name,
                        ty,
                        is_mutable,
                    },
                    span_start,
                    span_end,
                ))
            } else {
                let name_contents = self.compiler.get_source(name);

                if name_contents == b"self" {
                    let span_end = self.get_span_end(name);

                    let ty = self.create_node(
                        AstNode::Type {
                            name,
                            params: None,
                            optional: false,
                            pointer_type: PointerType::Unknown,
                        },
                        span_start,
                        span_end,
                    );

                    params.push(self.create_node(
                        AstNode::Param {
                            name,
                            ty,
                            is_mutable,
                        },
                        span_start,
                        span_end,
                    ))
                } else {
                    params.push(self.error("parameter missing type"))
                }
            }
        }

        params
    }

    pub fn new_allocation(&mut self) -> NodeId {
        let span_start = self.position();

        let required_lifetime = if self.is_keyword(b"local") {
            self.keyword(b"local");
            RequiredLifetime::Local
        } else {
            self.keyword(b"new");
            if self.is_lparen() {
                self.lparen();
                let require_lifetime = if self.is_keyword(b"local") {
                    self.keyword(b"local");
                    RequiredLifetime::Local
                } else {
                    return self.error("unknown lifetime specifier");
                };
                self.rparen();
                require_lifetime
            } else {
                RequiredLifetime::Unknown
            }
        };

        let pointer_type = if self.is_keyword(b"owned") {
            self.next();
            PointerType::Owned
        } else {
            PointerType::Shared
        };

        let allocated = self.variable_or_call();
        let span_end = self.get_span_end(allocated);

        self.create_node(
            AstNode::New(pointer_type, required_lifetime, allocated),
            span_start,
            span_end,
        )
    }

    pub fn match_expression(&mut self) -> NodeId {
        let span_start = self.position();
        let span_end;

        self.keyword(b"match");
        let target = self.simple_expression();

        let mut match_arms = vec![];

        if !self.is_lcurly() {
            return self.error("expected left curly brace '{'");
        }

        self.lcurly();

        loop {
            if self.is_rcurly() {
                span_end = self.position() + 1;
                self.rcurly();
                break;
            } else if self.is_simple_expression() {
                let pattern = self.simple_expression();

                if !self.is_thick_arrow() {
                    return self.error("expected thick arrow (=>) between match cases");
                }
                self.next();

                let pattern_result = self.simple_expression();

                match_arms.push((pattern, pattern_result));
            } else if self.is_newline() {
                self.next();
            } else {
                return self.error("expected match arm in match");
            }
        }

        self.create_node(AstNode::Match { target, match_arms }, span_start, span_end)
    }

    pub fn if_expression(&mut self) -> NodeId {
        let span_start = self.position();
        let span_end;

        self.keyword(b"if");

        let condition = self.expression();

        let then_block = self.block(true);

        let else_expression = if self.is_keyword(b"else") {
            self.next();
            let expr = self.expression();
            span_end = self.get_span_end(expr);
            Some(expr)
        } else {
            span_end = self.get_span_end(then_block);
            None
        };

        self.create_node(
            AstNode::If {
                condition,
                then_block,
                else_expression,
            },
            span_start,
            span_end,
        )
    }

    pub fn let_statement(&mut self) -> NodeId {
        let is_mutable = false;
        let span_start = self.position();

        self.keyword(b"let");

        let variable_name = self.variable();

        let ty = if self.is_colon() {
            // We have a type
            self.colon();

            Some(self.typename())
        } else {
            None
        };

        self.equals();

        let initializer = self.expression();

        let span_end = self.get_span_end(initializer);

        self.create_node(
            AstNode::Let {
                variable_name,
                ty,
                initializer,
                is_mutable,
            },
            span_start,
            span_end,
        )
    }

    pub fn mut_statement(&mut self) -> NodeId {
        let is_mutable = true;
        let span_start = self.position();

        self.keyword(b"mut");

        let variable_name = self.variable();

        let ty = if self.is_colon() {
            // We have a type
            self.colon();

            Some(self.typename())
        } else {
            None
        };

        self.equals();

        let initializer = self.expression();

        let span_end = self.get_span_end(initializer);

        self.create_node(
            AstNode::Let {
                variable_name,
                ty,
                initializer,
                is_mutable,
            },
            span_start,
            span_end,
        )
    }

    pub fn while_statement(&mut self) -> NodeId {
        let span_start = self.position();
        self.keyword(b"while");

        let condition = self.expression();
        let block = self.block(true);
        let span_end = self.get_span_end(block);

        self.create_node(AstNode::While { condition, block }, span_start, span_end)
    }

    pub fn for_statement(&mut self) -> NodeId {
        let span_start = self.position();
        self.keyword(b"for");

        let variable = self.variable();
        self.keyword(b"in");

        let range = self.simple_expression();
        let block = self.block(true);
        let span_end = self.get_span_end(block);

        self.create_node(
            AstNode::For {
                variable,
                range,
                block,
            },
            span_start,
            span_end,
        )
    }

    pub fn return_statement(&mut self) -> NodeId {
        let span_start = self.position();
        let span_end;

        self.keyword(b"return");

        let ret_val = if self.is_expression() {
            let expr = self.expression();
            span_end = self.get_span_end(expr);
            Some(expr)
        } else {
            span_end = span_start + b"return".len();
            None
        };

        self.create_node(AstNode::Return(ret_val), span_start, span_end)
    }

    pub fn break_statement(&mut self) -> NodeId {
        let span_start = self.position();
        let span_end = self.position() + 5;

        self.keyword(b"break");

        self.create_node(AstNode::Break, span_start, span_end)
    }

    pub fn defer_statement(&mut self) -> NodeId {
        let span_start = self.position();

        self.keyword(b"defer");

        let pointer = self.variable();

        let callback = self.expression();

        let span_end = self.get_span_end(callback);

        self.create_node(AstNode::Defer { pointer, callback }, span_start, span_end)
    }

    pub fn variable(&mut self) -> NodeId {
        if self.is_name() {
            let name = self
                .next()
                .expect("internal error: missing token that was expected to be there");
            let name_start = name.span_start;
            let name_end = name.span_end;
            self.create_node(AstNode::Variable, name_start, name_end)
        } else {
            self.error("expected variable")
        }
    }

    pub fn variable_or_call(&mut self) -> NodeId {
        if self.is_name() {
            let span_start = self.position();

            let name = self
                .next()
                .expect("internal error: missing token that was expected to be there");
            let name_start = name.span_start;
            let name_end = name.span_end;

            if self.is_lparen() {
                let head = self.create_node(AstNode::Name, name_start, name_end);
                // We're a call
                self.lparen();
                let mut args = vec![];
                loop {
                    if self.is_expression() {
                        let value_start = self.position();
                        let val = self.expression();

                        if self.is_comma() {
                            args.push(val);
                            self.comma();
                            continue;
                        } else if self.is_rparen() {
                            args.push(val);
                            break;
                        } else if self.is_colon() {
                            // we have a named value
                            self.colon();
                            let name = val;
                            let value = self.expression();
                            let value_end = self.position();
                            args.push(self.create_node(
                                AstNode::NamedValue { name, value },
                                value_start,
                                value_end,
                            ));
                            if self.is_comma() {
                                self.comma();
                                continue;
                            } else if self.is_rparen() {
                                break;
                            }
                        } else {
                            args.push(val);
                            args.push(self.error("unexpected value in call arguments"));
                        }
                    } else {
                        break;
                    }
                }
                let span_end = self.position() + 1;
                self.rparen();

                self.create_node(AstNode::Call { head, args }, span_start, span_end)
            } else {
                // We're a variable
                self.create_node(AstNode::Variable, name_start, name_end)
            }
        } else {
            self.error("expected variable or call")
        }
    }

    pub fn keyword(&mut self, keyword: &[u8]) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Name,
                span_start,
                span_end,
            }) if &self.compiler.source[span_start..span_end] == keyword => {
                self.next();
            }
            _ => {
                self.error(format!(
                    "expected keyword: {}",
                    String::from_utf8_lossy(keyword)
                ));
            }
        }
    }

    pub fn return_lifetime(&mut self) -> NodeId {
        if self.is_keyword(b"return") {
            let name = self
                .next()
                .expect("internal error: missing token that was expected to be there");
            let name_start = name.span_start;
            let name_end = name.span_end;
            self.create_node(AstNode::ReturnLifetime, name_start, name_end)
        } else {
            self.error("expected 'return' lifetime")
        }
    }

    pub fn lparen(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::LParen,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: left paren '('");
            }
        }
    }

    pub fn rparen(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::RParen,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: right paren ')'");
            }
        }
    }

    pub fn lsquare(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::LSquare,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: left bracket '['");
            }
        }
    }

    pub fn rsquare(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::RSquare,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: right bracket ']'");
            }
        }
    }

    pub fn lcurly(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::LCurly,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: left bracket '{'");
            }
        }
    }

    pub fn rcurly(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::RCurly,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: right bracket '}'");
            }
        }
    }

    pub fn less_than(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::LessThan,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: less than/left angle bracket '<'");
            }
        }
    }

    pub fn greater_than(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::GreaterThan,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: greater than/right angle bracket '>'");
            }
        }
    }

    pub fn equals(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Equals,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: equals '='");
            }
        }
    }

    pub fn colon(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Colon,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: colon ':'");
            }
        }
    }
    pub fn comma(&mut self) {
        match self.peek() {
            Some(Token {
                token_type: TokenType::Comma,
                ..
            }) => {
                self.next();
            }
            _ => {
                self.error("expected: comma ','");
            }
        }
    }

    pub fn lex_quoted_string(&mut self) -> Option<Token> {
        let span_start = self.span_offset;
        let mut span_position = span_start + 1;
        let mut is_escaped = false;
        while span_position < self.compiler.source.len() {
            if is_escaped {
                is_escaped = false;
            } else if self.compiler.source[span_position] == b'\\' {
                is_escaped = true;
            } else if self.compiler.source[span_position] == b'"' {
                span_position += 1;
                break;
            }
            span_position += 1;
        }

        self.span_offset = span_position;

        Some(Token {
            token_type: TokenType::String,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn lex_number(&mut self) -> Option<Token> {
        let span_start = self.span_offset;
        let mut span_position = span_start;
        while span_position < self.compiler.source.len() {
            if !self.compiler.source[span_position].is_ascii_digit() {
                break;
            }
            span_position += 1;
        }

        // Check to see if we have a hex/octal/binary number
        if span_position < self.compiler.source.len() && self.compiler.source[span_position] == b'x'
        {
            span_position += 1;
            while span_position < self.compiler.source.len() {
                if !self.compiler.source[span_position].is_ascii_hexdigit() {
                    break;
                }
                span_position += 1;
            }
        } else if span_position < self.compiler.source.len()
            && self.compiler.source[span_position] == b'o'
        {
            span_position += 1;
            while span_position < self.compiler.source.len() {
                if !(self.compiler.source[span_position] >= b'0'
                    && self.compiler.source[span_position] <= b'7')
                {
                    break;
                }
                span_position += 1;
            }
        } else if span_position < self.compiler.source.len()
            && self.compiler.source[span_position] == b'b'
        {
            span_position += 1;
            while span_position < self.compiler.source.len() {
                if !(self.compiler.source[span_position] >= b'0'
                    && self.compiler.source[span_position] <= b'1')
                {
                    break;
                }
                span_position += 1;
            }
        } else if span_position < self.compiler.source.len()
            && self.compiler.source[span_position] == b'.'
            && (span_position + 1 < self.compiler.source.len())
            && self.compiler.source[span_position + 1].is_ascii_digit()
        {
            // Looks like a float
            span_position += 1;
            while span_position < self.compiler.source.len() {
                if !self.compiler.source[span_position].is_ascii_digit() {
                    break;
                }
                span_position += 1;
            }

            if span_position < self.compiler.source.len()
                && (self.compiler.source[span_position] == b'e'
                    || self.compiler.source[span_position] == b'E')
            {
                span_position += 1;

                if span_position < self.compiler.source.len()
                    && self.compiler.source[span_position] == b'-'
                {
                    span_position += 1;
                }

                while span_position < self.compiler.source.len() {
                    if !self.compiler.source[span_position].is_ascii_digit() {
                        break;
                    }
                    span_position += 1;
                }
            }
        }

        self.span_offset = span_position;

        Some(Token {
            token_type: TokenType::Number,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn skip_space(&mut self) {
        let mut span_position = self.span_offset;
        let whitespace: &[u8] = &[b' ', b'\t'];
        while span_position < self.compiler.source.len() {
            if !whitespace.contains(&self.compiler.source[span_position]) {
                break;
            }
            span_position += 1;
        }
        self.span_offset = span_position;
    }

    pub fn newline(&mut self) -> Option<Token> {
        let mut span_position = self.span_offset;
        let whitespace: &[u8] = &[b'\r', b'\n'];
        while span_position < self.compiler.source.len() {
            if !whitespace.contains(&self.compiler.source[span_position]) {
                break;
            }
            span_position += 1;
        }

        if self.span_offset == span_position {
            None
        } else {
            let output = Some(Token {
                token_type: TokenType::Newline,
                span_start: self.span_offset,
                span_end: span_position,
            });
            self.span_offset = span_position;
            output
        }
    }

    pub fn skip_comment(&mut self) {
        let mut span_position = self.span_offset;
        while span_position < self.compiler.source.len()
            && self.compiler.source[span_position] != b'\n'
        {
            span_position += 1;
        }
        self.span_offset = span_position;
    }

    pub fn lex_name(&mut self) -> Option<Token> {
        let span_start = self.span_offset;
        let mut span_position = span_start;
        while span_position < self.compiler.source.len()
            && ((!self.compiler.source[span_position].is_ascii_whitespace()
                && !self.compiler.source[span_position].is_ascii_punctuation())
                || self.compiler.source[span_position] == b'_')
        {
            span_position += 1;
        }
        self.span_offset = span_position;

        Some(Token {
            token_type: TokenType::Name,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn lex_symbol(&mut self) -> Option<Token> {
        let span_start = self.span_offset;

        let result = match self.compiler.source[span_start] {
            b'(' => Token {
                token_type: TokenType::LParen,
                span_start,
                span_end: span_start + 1,
            },
            b'[' => Token {
                token_type: TokenType::LSquare,
                span_start,
                span_end: span_start + 1,
            },
            b'{' => Token {
                token_type: TokenType::LCurly,
                span_start,
                span_end: span_start + 1,
            },
            b'<' => {
                if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'='
                {
                    Token {
                        token_type: TokenType::LessThanEqual,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::LessThan,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b')' => Token {
                token_type: TokenType::RParen,
                span_start,
                span_end: span_start + 1,
            },
            b']' => Token {
                token_type: TokenType::RSquare,
                span_start,
                span_end: span_start + 1,
            },
            b'}' => Token {
                token_type: TokenType::RCurly,
                span_start,
                span_end: span_start + 1,
            },
            b'>' => {
                if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'='
                {
                    Token {
                        token_type: TokenType::GreaterThanEqual,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::GreaterThan,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'+' => {
                if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'+'
                {
                    Token {
                        token_type: TokenType::PlusPlus,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'='
                {
                    Token {
                        token_type: TokenType::PlusEquals,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Plus,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'-' => {
                if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'>'
                {
                    Token {
                        token_type: TokenType::ThinArrow,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'='
                {
                    Token {
                        token_type: TokenType::DashEquals,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Dash,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'*' => {
                if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'*'
                {
                    Token {
                        token_type: TokenType::AsteriskAsterisk,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'='
                {
                    Token {
                        token_type: TokenType::AsteriskEquals,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Asterisk,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'/' => {
                if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'/'
                {
                    Token {
                        token_type: TokenType::ForwardSlashForwardSlash,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'='
                {
                    Token {
                        token_type: TokenType::ForwardSlashEquals,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::ForwardSlash,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'=' => {
                if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'='
                {
                    Token {
                        token_type: TokenType::EqualsEquals,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'~'
                {
                    Token {
                        token_type: TokenType::EqualsTilde,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'>'
                {
                    Token {
                        token_type: TokenType::ThickArrow,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Equals,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b':' => {
                if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b':'
                {
                    Token {
                        token_type: TokenType::ColonColon,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Colon,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b';' => Token {
                token_type: TokenType::Semicolon,
                span_start,
                span_end: span_start + 1,
            },
            b'.' => {
                if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'.'
                {
                    Token {
                        token_type: TokenType::DotDot,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Dot,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'!' => {
                if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'='
                {
                    Token {
                        token_type: TokenType::ExclamationEquals,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'~'
                {
                    Token {
                        token_type: TokenType::ExclamationTilde,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Exclamation,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'|' => {
                if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'|'
                {
                    Token {
                        token_type: TokenType::PipePipe,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Pipe,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'&' => {
                if self.span_offset < (self.compiler.source.len() - 1)
                    && self.compiler.source[self.span_offset + 1] == b'&'
                {
                    Token {
                        token_type: TokenType::AmpersandAmpersand,
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Ampersand,
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b',' => Token {
                token_type: TokenType::Comma,
                span_start,
                span_end: span_start + 1,
            },
            b'?' => Token {
                token_type: TokenType::QuestionMark,
                span_start,
                span_end: span_start + 1,
            },
            x => {
                panic!(
                    "Internal compiler error: symbol character mismatched in lexer: {}",
                    x as char
                )
            }
        };

        self.span_offset = result.span_end;

        Some(result)
    }

    pub fn peek(&mut self) -> Option<Token> {
        let prev_offset = self.span_offset;
        let output = self.next();
        self.span_offset = prev_offset;

        output
    }

    pub fn next(&mut self) -> Option<Token> {
        loop {
            if self.span_offset >= self.compiler.source.len() {
                return None;
            } else if self.compiler.source[self.span_offset].is_ascii_digit() {
                return self.lex_number();
            } else if self.compiler.source[self.span_offset] == b'"' {
                return self.lex_quoted_string();
            } else if self.compiler.source[self.span_offset] == b'/'
                && self.span_offset < (self.compiler.source.len() - 1)
                && self.compiler.source[self.span_offset + 1] == b'/'
            {
                // Comment
                self.skip_comment();
            } else if is_symbol(self.compiler.source[self.span_offset]) {
                return self.lex_symbol();
            } else if self.compiler.source[self.span_offset] == b' '
                || self.compiler.source[self.span_offset] == b'\t'
            {
                self.skip_space()
            } else if self.compiler.source[self.span_offset] == b'\r'
                || self.compiler.source[self.span_offset] == b'\n'
            {
                return self.newline();
            // } else if self.compiler.source[self.span_offset].is_ascii_alphanumeric()
            //     || self.compiler.source[self.span_offset] == b'_'
            // {
            //     return self.lex_name();
            } else {
                return self.lex_name();
                // panic!(
                //         "unsupported character: {}",
                //         self.compiler.source[self.span_offset] as char
                //     )
            }
        }
    }
}
