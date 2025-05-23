use core::iter::Peekable;
use std::io::{Bytes, Read};

#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String),
    Register(u8),
    Comma,
    Eos,
}

pub struct Lexer<R: Read> {
    input: Peekable<Bytes<R>>,
}

impl<R: Read> Lexer<R> {
    pub fn new(input: R) -> Self {
        Self {
            input: input.bytes().peekable(),
        }
    }

    pub fn next(&mut self) -> Token {
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
                        return res;
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

                        return Token::Ident(ident);
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

                    return Token::Ident(ident);
                }
                b',' => return Token::Comma,
                b' ' | b'\t' | b'\n' => continue,
                _ => continue,
            }
        }

        Token::Eos
    }
}
