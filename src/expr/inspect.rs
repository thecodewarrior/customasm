use super::BinaryOp;
use super::Expression;
use asm::FunctionManager;
use diagn::Span;
use util::TreeNode;

impl Expression {
    pub fn width(&self, functions: &FunctionManager) -> Option<usize> {
        match self {
            &Expression::BinaryOp {
                span: _,
                op_span: _,
                op: BinaryOp::Concat,
                ref left,
                ref right,
            } => {
                let lhs_width = left.width(functions);
                let rhs_width = right.width(functions);

                if lhs_width.is_none() || rhs_width.is_none() {
                    return None;
                }

                Some(lhs_width.unwrap() + rhs_width.unwrap())
            }

            &Expression::BitSlice {
                span: _,
                op_span: _,
                left,
                right,
                expr: _,
            } => Some(left - right),

            &Expression::TernaryOp {
                span: _,
                test: _,
                ref if_true,
                ref if_false,
            } => {
                let true_width = if_true.width(functions);
                let false_width = if_false.width(functions);

                if true_width.is_none() || false_width.is_none() {
                    return None;
                }

                if true_width.unwrap() != false_width.unwrap() {
                    return None;
                }

                Some(true_width.unwrap())
            }

            &Expression::Block {
                span: _,
                ref expressions,
            } => match expressions.last() {
                None => None,
                Some(expr) => expr.width(functions),
            },

            &Expression::Variable { span: _, ref name } if name == "void" => Some(0),

            &Expression::Call {
                ref span,
                ref callee,
                ref arguments,
            } => match **callee {
                Expression::Variable { span: _, ref name } => {
                    match functions.get_func(name, arguments.len()) {
                        Some(func) => func.expression.width(functions),
                        None => None,
                    }
                }
                _ => None,
            },

            _ => None,
        }
    }

    pub fn returned_value_span(&self) -> Span {
        match self {
            &Expression::Block {
                ref span,
                ref expressions,
            } => match expressions.last() {
                None => span.clone(),
                Some(expr) => expr.returned_value_span(),
            },

            _ => self.span(),
        }
    }

    pub fn tree(&self, functions: &FunctionManager) -> TreeNode {
        let width_str = self
            .width(functions)
            .map_or("?".to_string(), |w| format!("{}", w));
        match self {
            &Expression::Literal { span: _, ref value } => {
                TreeNode::new(format!("`{}` -- {}", value, width_str))
            }
            &Expression::Variable { span: _, ref name } => {
                TreeNode::new(format!("var: {} -- {}", name, width_str))
            }
            &Expression::UnaryOp {
                span: _,
                op_span: _,
                ref op,
                ref expr,
            } => {
                let mut node = TreeNode::new(format!("{}___ -- {}", op, width_str));

                node.add_node("expr=", expr.tree(functions));

                node
            }
            &Expression::BinaryOp {
                span: _,
                op_span: _,
                ref op,
                ref left,
                ref right,
            } => {
                let mut node = TreeNode::new(format!("___ {} ___ -- {}", op, width_str));

                node.add_node("left=", left.tree(functions));
                node.add_node("right=", right.tree(functions));

                node
            }
            &Expression::TernaryOp {
                span: _,
                ref test,
                ref if_true,
                ref if_false,
            } => {
                let mut node = TreeNode::new(format!("Ternary -- {}", width_str));

                node.add_node("test=", test.tree(functions));
                node.add_node("if_true=", if_true.tree(functions));
                node.add_node("if_false=", if_false.tree(functions));

                node
            }
            &Expression::BitSlice {
                span: _,
                op_span: _,
                left,
                right,
                ref expr,
            } => {
                let mut node = TreeNode::new(format!("[{}:{}] -- {}", left, right, width_str));

                node.add_node("expr=", expr.tree(functions));

                node
            }
            &Expression::Block {
                span: _,
                ref expressions,
            } => {
                let mut node = TreeNode::new(format!("Block -- {}", width_str));

                for expr in expressions {
                    node.add_node("", expr.tree(functions));
                }

                node
            }
            &Expression::Call {
                span: _,
                ref callee,
                ref arguments,
            } => {
                let mut node = TreeNode::new(format!("Call {}", width_str));

                node.add_node("callee=", callee.tree(functions));

                let mut arg_node = TreeNode::new(format!("Args: {}", arguments.len()));

                for expr in arguments {
                    arg_node.add_node("", expr.tree(functions))
                }

                node.add_node("", arg_node);

                match **callee {
                    Expression::Variable { span: _, ref name } => {
                        match functions.get_func(name, arguments.len()) {
                            Some(func) => node.add_node("func=", func.expression.tree(functions)),
                            None => node.add_leaf("func=<missing>".to_string()),
                        }
                    }
                    _ => node.add_leaf("func=<no_name>".to_string()),
                }

                node
            }
        }
    }
}
