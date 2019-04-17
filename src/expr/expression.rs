use diagn::Span;
use num_bigint::BigInt;
use std::fmt;

#[derive(Debug)]
pub enum Expression {
    Literal {
        span: Span,
        value: ExpressionValue,
    },
    Variable {
        span: Span,
        name: String,
    },
    UnaryOp {
        span: Span,
        op_span: Span,
        op: UnaryOp,
        expr: Box<Expression>,
    },
    BinaryOp {
        span: Span,
        op_span: Span,
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    TernaryOp {
        span: Span,
        test: Box<Expression>,
        if_true: Box<Expression>,
        if_false: Box<Expression>,
    },
    BitSlice {
        span: Span,
        op_span: Span,
        left: usize,
        right: usize,
        expr: Box<Expression>,
    },
    Block {
        span: Span,
        expressions: Vec<Expression>,
    },
    Call {
        span: Span,
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
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
            &Expression::Literal { ref span, .. } => span.clone(),
            &Expression::Variable { ref span, .. } => span.clone(),
            &Expression::UnaryOp { ref span, .. } => span.clone(),
            &Expression::BinaryOp { ref span, .. } => span.clone(),
            &Expression::TernaryOp { ref span, .. } => span.clone(),
            &Expression::BitSlice { ref span, .. } => span.clone(),
            &Expression::Block { ref span, .. } => span.clone(),
            &Expression::Call { ref span, .. } => span.clone(),
        }
    }
}

impl ExpressionValue {
    pub fn make_literal(&self) -> Expression {
        Expression::Literal {
            span: Span::new_dummy(),
            value: self.clone(),
        }
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
