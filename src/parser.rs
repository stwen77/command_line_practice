use crate::lexer::Token;
use crate::logos::Logos;

pub type Lexer<S> = logos::Lexer<Token, S>;

#[derive(Debug, Clone)]
pub struct FnDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Statment>,
}

#[derive(Debug, Clone)]
pub enum Statment {
    If(Box<Expr>, Box<Statment>),
    IfElse(Box<Expr>, Box<Statment>, Box<Statment>),
    While(Box<Expr>, Box<Statment>),
    Loop(Box<Statment>),
    Var(String, Option<Box<Expr>>),
    Block(Vec<Statment>),
    Expr(Box<Expr>),
    Break,
    Return,
    ReturnWithVal(Box<Expr>),
    TempStub,
}

#[derive(Debug, Clone)]
pub enum Expr {
    IntConst(i32),
    FloatConst(f32),
    Identifier(String),
    CharConst(char),
    StringConst(String),
    FnCall(String, Vec<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
    Dot(Box<Expr>, Box<Expr>),
    Index(String, Box<Expr>),
    Array(Vec<Expr>),
    True,
    False,
    Unit,
    TempStub,
}
pub type Tokeniter<'a> = std::iter::Peekable<std::slice::Iter<'a, (Token, &'a str)>>;

pub fn parse<'a>(lexer: &mut Vec<(Token, &'a str)>) -> Result<(Vec<Statment>, Vec<FnDef>), ()> {
    let mut stmts = Vec::new();
    let mut fndefs = Vec::new();
    let mut lexer = lexer.iter().peekable();
    while lexer.peek().is_some() {
        //println!("xxxxx token: {:?} ", lexer.peek());
        match lexer.peek().unwrap().0 {
            Token::DeclarationFunction => fndefs.push(parse_fn(&mut lexer).unwrap()),
            _ => stmts.push(parse_statement(&mut lexer).unwrap()),
        }
        if let Some((Token::Semicolon, _)) = lexer.peek() {
            lexer.next(); //skip the semicolon
        }
    }
    println!("parsed statment vec :{:?}", stmts);
    println!("parsed function vec :{:?}", fndefs);
    Ok((stmts, fndefs))
}

fn parse_fn<'a>(lexer: &mut Tokeniter<'a>) -> Result<FnDef, ()> {
    lexer.next();

    let name = match lexer.next() {
        Some((Token::Identifier, s)) => s.to_string(),
        _ => return Err(()),
    };

    match lexer.peek().unwrap().0 {
        Token::LParen => {
            lexer.next();
        }
        _ => panic!(),
    }

    let mut params = Vec::new();

    let skip_params = match lexer.peek().unwrap().0 {
        Token::RParen => {
            lexer.next();
            true
        }
        _ => false,
    };

    if !skip_params {
        loop {
            match lexer.next() {
                Some((Token::RParen, _)) => break,
                Some((Token::Comma, _)) => (),
                Some((Token::Identifier, s)) => {
                    params.push(s.to_string());
                }
                _ => panic!(),
            }
        }
    }

    let body = parse_block(lexer)?;

    Ok(FnDef {
        name: name,
        params: params,
        body: Box::new(body),
    })
}

pub fn parse_statement<'a>(lexer: &mut Tokeniter<'a>) -> Result<Statment, ()> {
    match lexer.peek().unwrap().0 {
        Token::If => parse_if(lexer),
        Token::While => parse_while(lexer),
        Token::Break => {
            lexer.next();
            Ok(Statment::Break)
        }
        Token::Return => {
            lexer.next();
            match lexer.peek().unwrap().0 {
                Token::Semicolon => Ok(Statment::Return),
                _ => {
                    let ret = parse_expr(lexer)?;
                    Ok(Statment::ReturnWithVal(Box::new(ret)))
                }
            }
        }
        Token::DeclarationLet => parse_var(lexer),
        Token::LCurly => parse_block(lexer),
        _ => Ok(Statment::TempStub),
    }
}

fn parse_if<'a>(lexer: &mut Tokeniter<'a>) -> Result<Statment, ()> {
    lexer.next();

    let guard = parse_expr(lexer)?;
    let body = parse_block(lexer)?;

    match lexer.peek().unwrap().0 {
        Token::Else => {
            lexer.next();
            let else_body = parse_block(lexer)?;
            Ok(Statment::IfElse(
                Box::new(guard),
                Box::new(body),
                Box::new(else_body),
            ))
        }
        _ => Ok(Statment::If(Box::new(guard), Box::new(body))),
    }
}

fn parse_while<'a>(lexer: &mut Tokeniter<'a>) -> Result<Statment, ()> {
    lexer.next();

    let guard = parse_expr(lexer)?;
    let body = parse_block(lexer)?;

    Ok(Statment::While(Box::new(guard), Box::new(body)))
}

fn parse_block<'a>(lexer: &mut Tokeniter<'a>) -> Result<Statment, ()> {
    match lexer.peek() {
        Some((Token::LCurly, _)) => (),
        _ => panic!(),
    }
    lexer.next();

    let mut stmts = Vec::new();

    let skip_body = match lexer.peek() {
        Some((Token::RCurly, _)) => true,
        _ => false,
    };

    if !skip_body {
        while let Some(_) = lexer.peek() {
            stmts.push(parse_statement(lexer)?);

            if let Some((Token::Semicolon, _)) = lexer.peek() {
                lexer.next();
            }

            if let Some((Token::RCurly, _)) = lexer.peek() {
                break;
            }
        }
    }

    match lexer.peek() {
        Some((Token::RCurly, _)) => {
            lexer.next();
            Ok(Statment::Block(stmts))
        }
        _ => panic!(),
    }
}

pub fn parse_var<'a>(lexer: &mut Tokeniter<'a>) -> Result<Statment, ()> {
    lexer.next();

    let name: String = match lexer.peek() {
        Some((Token::Identifier, slice)) => slice.to_string(),
        _ => panic!("should be ident,not {:?}", lexer.peek().unwrap().0),
    };

    lexer.next();
    match lexer.peek().unwrap().0 {
        Token::Assign => {
            lexer.next();
            let initializer = parse_expr(lexer)?;
            Ok(Statment::Var(name, Some(Box::new(initializer))))
        }
        _ => Ok(Statment::Var(name, None)),
    }
}
fn parse_expr<'a>(lexer: &mut Tokeniter<'a>) -> Result<Expr, ()> {
    match lexer.peek().unwrap().0 {
        Token::RParen => Ok(Expr::Unit),
        _ => {
            let lhs = parse_unary(lexer)?;

            parse_binary_operation(lexer, 0, lhs)
        }
    }
}

fn parse_unary<'a>(lexer: &mut Tokeniter<'a>) -> Result<Expr, ()> {
    match lexer.peek().unwrap().0 {
        Token::OperatorSubtraction => {
            lexer.next();
            Ok(Expr::FnCall("-".to_string(), vec![parse_primary(lexer)?]))
        }
        Token::OperatorAddition => {
            lexer.next();
            parse_primary(lexer)
        }
        Token::OperatorLogicalNot => {
            lexer.next();
            Ok(Expr::FnCall("!".to_string(), vec![parse_primary(lexer)?]))
        }
        _ => parse_primary(lexer),
    }
}

fn parse_paren_expr<'a>(lexer: &mut Tokeniter<'a>) -> Result<Expr, ()> {
    let expr = parse_expr(lexer)?;

    match lexer.next().unwrap().0 {
        Token::RParen => Ok(expr),
        _ => panic!(),
    }
}

fn parse_ident_expr<'a>(id: String, lexer: &mut Tokeniter<'a>) -> Result<Expr, ()> {
    match lexer.peek() {
        Some((Token::LParen, _)) => {
            lexer.next();
            //parse_call_expr(id, lexer) todo
            panic!()
        }
        Some((Token::LSquare, _)) => {
            lexer.next();
            //parse_index_expr(id, lexer)
            panic!()
        }
        _ => Ok(Expr::Identifier(id)),
    }
}

fn parse_primary<'a>(lexer: &mut Tokeniter<'a>) -> Result<Expr, ()> {
    let (token, slice) = lexer.next().unwrap();

    let result = match token {
        Token::LiteralInteger => Ok(Expr::IntConst(slice.parse::<i32>().unwrap())),
        //Token::Float
        Token::LiteralString => Ok(Expr::StringConst(slice.to_string())),
        Token::Identifier => parse_ident_expr(slice.to_string(), lexer),
        Token::LParen => parse_paren_expr(lexer),
        //Token::LSquare => parse_array_expr(lexer),
        Token::LiteralTrue => Ok(Expr::True),
        Token::LiteralFalse => Ok(Expr::False),
        _ => Ok(Expr::TempStub),
    };

    result
}

fn parse_binary_operation<'a>(lexer: &mut Tokeniter<'a>, prec: i32, lhs: Expr) -> Result<Expr, ()> {
    let mut lhs_curr = lhs;

    loop {
        let mut curr_prec = -1;

        if let Some((curr_op, _)) = lexer.peek() {
            curr_prec = get_precedence(curr_op);
        }

        if curr_prec < prec {
            return Ok(lhs_curr);
        }

        if let Some((op_token, _)) = lexer.next() {
            let mut rhs = parse_unary(lexer)?;

            let mut next_prec = -1;

            if let Some((next_op, _)) = lexer.peek() {
                next_prec = get_precedence(next_op);
            }

            if curr_prec < next_prec {
                rhs = parse_binary_operation(lexer, curr_prec + 1, rhs)?;
            } else if curr_prec >= 100 {
                // Always bind right to left for precedence over 100
                rhs = parse_binary_operation(lexer, curr_prec, rhs)?;
            }

            lhs_curr = match op_token {
                Token::OperatorAddition => Expr::FnCall("+".to_string(), vec![lhs_curr, rhs]),
                Token::OperatorSubtraction => Expr::FnCall("-".to_string(), vec![lhs_curr, rhs]),
                Token::OperatorMultiplication => Expr::FnCall("*".to_string(), vec![lhs_curr, rhs]),
                Token::OperatorDivision => Expr::FnCall("/".to_string(), vec![lhs_curr, rhs]),
                Token::Assign => Expr::Assignment(Box::new(lhs_curr), Box::new(rhs)),
                Token::AssignAddition => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("+".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::AssignSubtraction => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("-".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::OperatorEquality => Expr::FnCall("==".to_string(), vec![lhs_curr, rhs]),
                Token::OperatorInequality => Expr::FnCall("!=".to_string(), vec![lhs_curr, rhs]),
                Token::OperatorLesser => Expr::FnCall("<".to_string(), vec![lhs_curr, rhs]),
                Token::OperatorLesserEquals => Expr::FnCall("<=".to_string(), vec![lhs_curr, rhs]),
                Token::OperatorGreater => Expr::FnCall(">".to_string(), vec![lhs_curr, rhs]),
                Token::OperatorGreaterEquals => Expr::FnCall(">=".to_string(), vec![lhs_curr, rhs]),
                Token::OperatorLogicalOr => Expr::FnCall("||".to_string(), vec![lhs_curr, rhs]),
                Token::OperatorLogicalAnd => Expr::FnCall("&&".to_string(), vec![lhs_curr, rhs]),
                Token::AssignMultiplication => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("*".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::AssignDivision => {
                    let lhs_copy = lhs_curr.clone();
                    Expr::Assignment(
                        Box::new(lhs_curr),
                        Box::new(Expr::FnCall("/".to_string(), vec![lhs_copy, rhs])),
                    )
                }
                Token::OperatorBitShiftLeft => Expr::FnCall("<<".to_string(), vec![lhs_curr, rhs]),
                Token::OperatorBitShiftRight => Expr::FnCall(">>".to_string(), vec![lhs_curr, rhs]),
                Token::OperatorRemainder => Expr::FnCall("%".to_string(), vec![lhs_curr, rhs]),
                _ => return Err(()),
            };
        }
    }
}

fn get_precedence(token: &Token) -> i32 {
    match *token {
        Token::Assign
        | Token::AssignAddition
        | Token::AssignSubtraction
        | Token::AssignMultiplication
        | Token::AssignDivision => 10,

        Token::OperatorLogicalOr => 11,
        Token::OperatorLogicalAnd => 12,
        Token::OperatorLesser
        | Token::OperatorLesserEquals
        | Token::OperatorGreater
        | Token::OperatorGreaterEquals
        | Token::OperatorEquality
        | Token::OperatorInequality => 15,
        Token::OperatorAddition | Token::OperatorSubtraction => 20,
        Token::OperatorDivision | Token::OperatorMultiplication | Token::OperatorBitNot => 40,
        Token::OperatorBitShiftLeft | Token::OperatorBitShiftRight => 50,
        Token::OperatorRemainder => 60,
        Token::OperatorExponent => 100,
        _ => -1,
    }
}
