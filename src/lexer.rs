pub struct Lexer<'source> {
    source: &'source [u8],
    span_offset: usize,
}

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
    Semicolon,
    Plus,
    PlusPlus,
    Dash,
    Exclamation,
    Asterisk,
    AsteriskAsterisk,
    ForwardSlash,
    ForwardSlashForwardSlash,
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
}

#[derive(Debug)]
pub struct Token<'source> {
    pub token_type: TokenType,
    pub contents: &'source [u8],
    pub span_start: usize,
    pub span_end: usize,
}

fn is_symbol(b: u8) -> bool {
    [
        b'+', b'-', b'*', b'/', b'.', b',', b'(', b'[', b'{', b'<', b')', b']', b'}', b'>', b':',
        b';', b'=', b'$', b'|', b'!', b'~', b'&', b'\'', b'"',
    ]
    .contains(&b)
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source [u8], span_offset: usize) -> Self {
        Self {
            source,
            span_offset,
        }
    }

    pub fn lex_quoted_string(&mut self) -> Option<Token<'source>> {
        let span_start = self.span_offset;
        let mut token_offset = 1;
        let mut is_escaped = false;
        while token_offset < self.source.len() {
            if is_escaped {
                is_escaped = false;
            } else if self.source[token_offset] == b'\\' {
                is_escaped = true;
            } else if self.source[token_offset] == b'"' {
                token_offset += 1;
                break;
            }
            token_offset += 1;
        }

        self.span_offset += token_offset;

        let contents = &self.source[..token_offset];
        self.source = &self.source[token_offset..];

        Some(Token {
            token_type: TokenType::String,
            contents,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn lex_number(&mut self) -> Option<Token<'source>> {
        let span_start = self.span_offset;
        let mut token_offset = 0;
        while token_offset < self.source.len() {
            if !self.source[token_offset].is_ascii_digit() {
                break;
            }
            token_offset += 1;
        }

        // Check to see if we have a hex/octal/binary number
        if token_offset < self.source.len() && self.source[token_offset] == b'x' {
            token_offset += 1;
            while token_offset < self.source.len() {
                if !self.source[token_offset].is_ascii_hexdigit() {
                    break;
                }
                token_offset += 1;
            }
        } else if token_offset < self.source.len() && self.source[token_offset] == b'o' {
            token_offset += 1;
            while token_offset < self.source.len() {
                if !(self.source[token_offset] >= b'0' && self.source[token_offset] <= b'7') {
                    break;
                }
                token_offset += 1;
            }
        } else if token_offset < self.source.len() && self.source[token_offset] == b'b' {
            token_offset += 1;
            while token_offset < self.source.len() {
                if !(self.source[token_offset] >= b'0' && self.source[token_offset] <= b'1') {
                    break;
                }
                token_offset += 1;
            }
        } else if token_offset < self.source.len()
            && self.source[token_offset] == b'.'
            && (token_offset + 1 < self.source.len())
            && self.source[token_offset + 1].is_ascii_digit()
        {
            // Looks like a float
            token_offset += 1;
            while token_offset < self.source.len() {
                if !self.source[token_offset].is_ascii_digit() {
                    break;
                }
                token_offset += 1;
            }

            if token_offset < self.source.len()
                && (self.source[token_offset] == b'e' || self.source[token_offset] == b'E')
            {
                token_offset += 1;

                if token_offset < self.source.len() && self.source[token_offset] == b'-' {
                    token_offset += 1;
                }

                while token_offset < self.source.len() {
                    if !self.source[token_offset].is_ascii_digit() {
                        break;
                    }
                    token_offset += 1;
                }
            }
        }

        self.span_offset += token_offset;

        let contents = &self.source[..token_offset];
        self.source = &self.source[token_offset..];

        Some(Token {
            token_type: TokenType::Number,
            contents,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn skip_space(&mut self) {
        let mut token_offset = 0;
        let whitespace: &[u8] = &[b' ', b'\t', b'\r', b'\n'];
        while token_offset < self.source.len() {
            if !whitespace.contains(&self.source[token_offset]) {
                break;
            }
            token_offset += 1;
        }
        self.span_offset += token_offset;
        self.source = &self.source[token_offset..];
    }

    pub fn skip_comment(&mut self) {
        let mut token_offset = 0;
        while token_offset < self.source.len() && self.source[token_offset] != b'\n' {
            token_offset += 1;
        }
        self.span_offset += token_offset;
        self.source = &self.source[token_offset..];
    }

    pub fn lex_name(&mut self) -> Option<Token<'source>> {
        let span_start = self.span_offset;
        let mut token_offset = 0;
        while token_offset < self.source.len()
            && (self.source[token_offset].is_ascii_alphanumeric()
                || self.source[token_offset] == b'_')
        {
            token_offset += 1;
        }
        self.span_offset += token_offset;
        let contents = &self.source[..token_offset];
        self.source = &self.source[token_offset..];

        Some(Token {
            token_type: TokenType::Name,
            contents,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn lex_symbol(&mut self) -> Option<Token<'source>> {
        let span_start = self.span_offset;

        let result = match self.source[0] {
            b'(' => Token {
                token_type: TokenType::LParen,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'[' => Token {
                token_type: TokenType::LSquare,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'{' => Token {
                token_type: TokenType::LCurly,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'<' => {
                if self.source.len() > 1 && self.source[1] == b'=' {
                    Token {
                        token_type: TokenType::LessThanEqual,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::LessThan,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b')' => Token {
                token_type: TokenType::RParen,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b']' => Token {
                token_type: TokenType::RSquare,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'}' => Token {
                token_type: TokenType::RCurly,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'>' => {
                if self.source.len() > 1 && self.source[1] == b'=' {
                    Token {
                        token_type: TokenType::GreaterThanEqual,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::GreaterThan,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'+' => {
                if self.source.len() > 1 && self.source[1] == b'+' {
                    Token {
                        token_type: TokenType::PlusPlus,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Plus,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'-' => Token {
                token_type: TokenType::Dash,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'*' => {
                if self.source.len() > 1 && self.source[1] == b'*' {
                    Token {
                        token_type: TokenType::AsteriskAsterisk,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Asterisk,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'/' => {
                if self.source.len() > 1 && self.source[1] == b'/' {
                    Token {
                        token_type: TokenType::ForwardSlashForwardSlash,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::ForwardSlash,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'=' => {
                if self.source.len() > 1 && self.source[1] == b'=' {
                    Token {
                        token_type: TokenType::EqualsEquals,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.source.len() > 1 && self.source[1] == b'~' {
                    Token {
                        token_type: TokenType::EqualsTilde,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Equals,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b':' => Token {
                token_type: TokenType::Colon,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b';' => Token {
                token_type: TokenType::Semicolon,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'.' => {
                if self.source.len() > 1 && self.source[1] == b'.' {
                    Token {
                        token_type: TokenType::DotDot,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Dot,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'!' => {
                if self.source.len() > 1 && self.source[1] == b'=' {
                    Token {
                        token_type: TokenType::ExclamationEquals,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.source.len() > 1 && self.source[1] == b'~' {
                    Token {
                        token_type: TokenType::ExclamationTilde,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Exclamation,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'|' => {
                if self.source.len() > 1 && self.source[1] == b'|' {
                    Token {
                        token_type: TokenType::PipePipe,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Pipe,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'&' => {
                if self.source.len() > 1 && self.source[1] == b'&' {
                    Token {
                        token_type: TokenType::AmpersandAmpersand,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Ampersand,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b',' => Token {
                token_type: TokenType::Comma,
                contents: &self.source[..1],
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
        self.source = &self.source[(result.span_end - span_start)..];

        Some(result)
    }
}

impl<'source> Lexer<'source> {
    pub fn peek(&mut self) -> Option<Token<'source>> {
        let prev_offset = self.span_offset;
        let prev_source = self.source;
        let output = self.next();
        self.span_offset = prev_offset;
        self.source = prev_source;

        output
    }

    pub fn next(&mut self) -> Option<Token<'source>> {
        loop {
            if self.source.is_empty() {
                return None;
            } else if self.source[0].is_ascii_digit() {
                return self.lex_number();
            } else if self.source[0] == b'"' {
                return self.lex_quoted_string();
            } else if self.source[0] == b'/' && self.source.len() > 1 && self.source[1] == b'/' {
                // Comment
                self.skip_comment();
            } else if is_symbol(self.source[0]) {
                return self.lex_symbol();
            } else if self.source[0] == b' '
                || self.source[0] == b'\t'
                || self.source[0] == b'\r'
                || self.source[0] == b'\n'
            {
                self.skip_space()
            } else if self.source[0].is_ascii_alphanumeric() || self.source[0] == b'_' {
                return self.lex_name();
            } else {
                panic!("unsupported character: {}", self.source[0])
            }
        }
    }
}
