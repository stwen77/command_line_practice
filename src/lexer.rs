use logos::Logos;

#[derive(Debug, PartialEq, Clone, Copy, Logos)]
pub enum Token {
    #[end]
    End,

    #[error]
    UnexpectedToken,

    #[token = ";"]
    Semicolon,

    #[token = ":"]
    Colon,

    #[token = ","]
    Comma,

    #[token = "."]
    Accessor,

    #[token = "("]
    LParen,

    #[token = ")"]
    RParen,

    #[token = "{"]
    LCurly,

    #[token = "}"]
    RCurly,

    #[token = "["]
    LSquare,

    #[token = "]"]
    RSquare,

    #[token = "=>"]
    Arrow,

    #[regex = "[a-zA-Z_$][a-zA-Z0-9_$]*"]
    Identifier,

    #[token = "enum"]
    DeclarationEnum,

    #[token = "struct"]
    DeclarationStruct,

    #[token = "fn"]
    DeclarationFunction,

    #[token = "let"]
    DeclarationLet,

    #[token = "bool"]
    TypeBool,

    #[token = "string"]
    TypeString,

    #[token = "int"]
    TypeInt,

    #[token = "true"]
    LiteralTrue,

    #[token = "false"]
    LiteralFalse,

    #[regex = "[0-9]+"]
    LiteralInteger,

    #[regex = "\"([^\"\\\\]|\\\\.)*\""]
    #[regex = "'([^'\\\\]|\\\\.)*'"]
    LiteralString,

    #[token = "++"]
    OperatorIncrement,

    #[token = "--"]
    OperatorDecrement,

    #[token = "!"]
    OperatorLogicalNot,

    #[token = "~"]
    OperatorBitNot,

    #[token = "*"]
    OperatorMultiplication,

    #[token = "/"]
    OperatorDivision,

    #[token = "%"]
    OperatorRemainder,

    #[token = "**"]
    OperatorExponent,

    #[token = "+"]
    OperatorAddition,

    #[token = "-"]
    OperatorSubtraction,

    #[token = "<<"]
    OperatorBitShiftLeft,

    #[token = ">>"]
    OperatorBitShiftRight,

    #[token = "<"]
    OperatorLesser,

    #[token = "<="]
    OperatorLesserEquals,

    #[token = ">"]
    OperatorGreater,

    #[token = ">="]
    OperatorGreaterEquals,

    #[token = "=="]
    OperatorEquality,

    #[token = "!="]
    OperatorInequality,

    #[token = "&"]
    OperatorBitAnd,

    #[token = "^"]
    OperatorBitXor,

    #[token = "|"]
    OperatorBitOr,

    #[token = "&&"]
    OperatorLogicalAnd,

    #[token = "||"]
    OperatorLogicalOr,
}
