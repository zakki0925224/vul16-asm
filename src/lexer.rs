use core::iter::Peekable;
use std::io::{Bytes, Read};

#[derive(Debug, PartialEq)]
pub struct TokenInfo {
    pub token: Token,
    pub line: usize,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String),
    Comment(String),
    Register(u8),
    Immediate(i8),
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
                b'r' => {
                    let mut res = None;
                    if let Some(Ok(d)) = self.input.peek() {
                        if d.is_ascii_digit() {
                            res = Some(Token::Register(d - b'0'));
                            self.input.next();
                        }
                    }

                    if let Some(res) = res {
                        return TokenInfo {
                            token: res,
                            line: self.line,
                        };
                    } else {
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

                        return TokenInfo {
                            token: Token::Ident(ident),
                            line: self.line,
                        };
                    }
                }
                b'a'..=b'z' | b'A'..=b'Z' => {
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
                        i8::from_str_radix(&num[2..], 16)
                    } else if num.starts_with("0b") || num.starts_with("0B") {
                        i8::from_str_radix(&num[2..], 2)
                    } else if num.starts_with("0o") || num.starts_with("0O") {
                        i8::from_str_radix(&num[2..], 8)
                    } else {
                        i8::from_str_radix(&num, 10)
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
