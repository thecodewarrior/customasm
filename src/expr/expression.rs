use diagn::Span;
use num_bigint::BigInt;
use std::fmt;

#[derive(Debug)]
pub enum Expression {
    Literal(Span, ExpressionValue),
    Variable(Span, String),
    UnaryOp(Span, Span, UnaryOp, Box<Expression>),
    BinaryOp(Span, Span, BinaryOp, Box<Expression>, Box<Expression>),
    TernaryOp(Span, Box<Expression>, Box<Expression>, Box<Expression>),
    BitSlice(Span, Span, usize, usize, Box<Expression>),
    Block(Span, Vec<Expression>),
    Call(Span, Box<Expression>, Vec<Expression>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExpressionValue {
    Void,
    Integer(BigInt),
    Bool(bool),
    String(String),
    Function(String),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BinaryOp {
    Assign,

    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    And,
    Or,
    Xor,

    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    LazyAnd,
    LazyOr,

    Concat,
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            &Expression::Literal(ref span, ..) => span.clone(),
            &Expression::Variable(ref span, ..) => span.clone(),
            &Expression::UnaryOp(ref span, ..) => span.clone(),
            &Expression::BinaryOp(ref span, ..) => span.clone(),
            &Expression::TernaryOp(ref span, ..) => span.clone(),
            &Expression::BitSlice(ref span, ..) => span.clone(),
            &Expression::Block(ref span, ..) => span.clone(),
            &Expression::Call(ref span, ..) => span.clone(),
        }
    }
}

impl ExpressionValue {
    pub fn make_literal(&self) -> Expression {
        Expression::Literal(Span::new_dummy(), self.clone())
    }
}

impl fmt::Display for ExpressionValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ExpressionValue::Void => format!("Void"),
                ExpressionValue::Integer(ref value) => format!("Integer({})", value),
                ExpressionValue::Bool(value) => format!("Bool({})", value),
                ExpressionValue::String(value) => format!("String({:?})", value),
                ExpressionValue::Function(value) => format!("Function({:?})", value),
            }
        )
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::Neg => "-",
                UnaryOp::Not => "!",
            }
        )
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Assign => "=",
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
                BinaryOp::Mod => "%",

                BinaryOp::Shl => "<<",
                BinaryOp::Shr => ">>",

                BinaryOp::And => "&",
                BinaryOp::Or => "|",
                BinaryOp::Xor => "^",

                BinaryOp::Eq => "==",
                BinaryOp::Ne => "!=",

                BinaryOp::Lt => "<",
                BinaryOp::Le => "<=",

                BinaryOp::Gt => ">",
                BinaryOp::Ge => ">=",

                BinaryOp::LazyAnd => "&&",
                BinaryOp::LazyOr => "||",

                BinaryOp::Concat => "@",
            }
        )
    }
}
