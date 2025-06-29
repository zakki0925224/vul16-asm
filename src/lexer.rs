use crate::isa;
use core::iter::Peekable;
use std::io::{Bytes, Read};

#[derive(Debug, Clone, PartialEq)]
pub struct TokenInfo {
    pub token: Token,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Label(String),
    TargetLabel(String),
    Comment(String),
    Register(u8),
    Immediate(isize),
    Comma,
    Eos,
}

pub struct Lexer<R: Read> {
    input: Peekable<Bytes<R>>,
    line: usize,
}

impl<R: Read> Lexer<R> {
    pub fn new(input: R) -> Self {
        Self {
            input: input.bytes().peekable(),
            line: 1,
        }
    }

    pub fn next(&mut self) -> TokenInfo {
        while let Some(Ok(b)) = self.input.next() {
            match b {
                b'a'..=b'z' | b'A'..=b'Z' | b'#' => {
                    let mut ident = String::new();
                    ident.push(b as char);
                    while let Some(Ok(nb)) = self.input.peek() {
                        if nb.is_ascii_alphanumeric() || *nb == b'_' {
                            ident.push(*nb as char);
                            self.input.next();
                        } else {
                            break;
                        }
                    }

                    // register
                    // start with 'r'/'R' and followed by a digit
                    if ident.len() == 2
                        && (ident.starts_with('r') || ident.starts_with('R'))
                        && ident.chars().nth(1).unwrap().is_ascii_digit()
                    {
                        let reg_num = ident.chars().nth(1).unwrap() as u8 - b'0';
                        return TokenInfo {
                            token: Token::Register(reg_num),
                            line: self.line,
                        };
                    }

                    // label
                    if let Some(Ok(nb)) = self.input.peek() {
                        if *nb == b':' && !isa::is_reserved_word(&ident) {
                            self.input.next();
                            return TokenInfo {
                                token: Token::Label(ident),
                                line: self.line,
                            };
                        }
                    }

                    // target label
                    if ident.len() > 1
                        && ident.starts_with('#')
                        && !isa::is_reserved_word(&ident[1..])
                    {
                        return TokenInfo {
                            token: Token::TargetLabel(ident[1..].to_string()),
                            line: self.line,
                        };
                    }

                    // ident
                    return TokenInfo {
                        token: Token::Ident(ident),
                        line: self.line,
                    };
                }
                b';' => {
                    let mut comment = String::new();
                    comment.push(b as char);
                    while let Some(Ok(nb)) = self.input.peek() {
                        if *nb != b'\n' {
                            comment.push(*nb as char);
                            self.input.next();
                        } else {
                            break;
                        }
                    }
                    return TokenInfo {
                        token: Token::Comment(comment),
                        line: self.line,
                    };
                }
                b'0'..=b'9' | b'+' | b'-' => {
                    let mut num = String::new();
                    let mut sign = 1;

                    let mut first = b;
                    if b == b'+' || b == b'-' {
                        if b == b'-' {
                            sign = -1;
                        }

                        if let Some(Ok(nb)) = self.input.peek() {
                            first = *nb;
                            self.input.next();
                        } else {
                            continue;
                        }
                    }

                    num.push(first as char);

                    if first == b'0' {
                        if let Some(Ok(nb)) = self.input.peek() {
                            match *nb {
                                b'x' | b'X' | b'b' | b'B' | b'o' | b'O' => {
                                    num.push(self.input.next().unwrap().unwrap() as char);
                                }
                                _ => (),
                            }
                        }
                    }

                    while let Some(Ok(nb)) = self.input.peek() {
                        if nb.is_ascii_digit()
                            || (*nb >= b'a' && *nb <= b'f')
                            || (*nb >= b'A' && *nb <= b'F')
                        {
                            num.push(*nb as char);
                            self.input.next();
                        } else {
                            break;
                        }
                    }

                    let value = if num.starts_with("0x") || num.starts_with("0X") {
                        isize::from_str_radix(&num[2..], 16)
                    } else if num.starts_with("0b") || num.starts_with("0B") {
                        isize::from_str_radix(&num[2..], 2)
                    } else if num.starts_with("0o") || num.starts_with("0O") {
                        isize::from_str_radix(&num[2..], 8)
                    } else {
                        isize::from_str_radix(&num, 10)
                    };

                    if let Ok(value) = value {
                        return TokenInfo {
                            token: Token::Immediate(sign * value),
                            line: self.line,
                        };
                    }
                }
                b',' => {
                    return TokenInfo {
                        token: Token::Comma,
                        line: self.line,
                    };
                }
                b' ' | b'\t' => continue,
                b'\n' => {
                    self.line += 1;
                    continue;
                }
                _ => continue,
            }
        }

        TokenInfo {
            token: Token::Eos,
            line: self.line,
        }
    }
}
