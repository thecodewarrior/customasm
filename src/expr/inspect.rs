use diagn::Span;
use super::Expression;
use super::BinaryOp;
use asm::FunctionManager;
use util::TreeNode;


impl Expression
{
	pub fn width(&self, functions: &FunctionManager) -> Option<usize>
	{
		match self
		{
			&Expression::BinaryOp(_, _, BinaryOp::Concat, ref lhs, ref rhs) =>
			{
				let lhs_width = lhs.width(functions);
				let rhs_width = rhs.width(functions);
				
				if lhs_width.is_none() || rhs_width.is_none()
					{ return None; }
					
				Some(lhs_width.unwrap() + rhs_width.unwrap())
			}
			
			&Expression::BitSlice(_, _, left, right, _) => Some(left - right),
			
			&Expression::TernaryOp(_, _, ref true_branch, ref false_branch) =>
			{
				let true_width = true_branch.width(functions);
				let false_width = false_branch.width(functions);
				
				if true_width.is_none() || false_width.is_none()
					{ return None; }
					
				if true_width.unwrap() != false_width.unwrap()
					{ return None; }
					
				Some(true_width.unwrap())
			}
			
			&Expression::Block(_, ref exprs) =>
			{
				match exprs.last()
				{
					None => None,
					Some(expr) => expr.width(functions)
				}
			}

			&Expression::Variable(_, ref name) if name == "void" => Some(0),

			&Expression::Call(ref span, ref target, ref arg_exprs) => {
                match **target {
					Expression::Variable(_, ref name) => {
                        match functions.get_func(name, arg_exprs.len()) {
							Some(func) => func.expression.width(functions),
							None => None
						}
					}
					_ => None
				}
			}
			
			_ => None
		}
	}
	
	
	pub fn slice(&self, functions: &FunctionManager) -> Option<(usize, usize)>
	{
		match self
		{
			&Expression::BinaryOp(_, _, BinaryOp::Concat, _, _) => self.width(functions).map(|w| (w, 0)),
			&Expression::BitSlice(_, _, left, right, _) => Some((left, right)),
			
			&Expression::TernaryOp(_, _, _, _) => self.width(functions).map(|w| (w, 0)),
			
			&Expression::Block(_, ref exprs) =>
			{
				match exprs.last()
				{
					None => None,
					Some(expr) => expr.slice(functions)
				}
			}

			&Expression::Variable(_, ref name) if name == "void" => Some((0, 0)),

			&Expression::Call(ref span, ref target, ref arg_exprs) => {
				match **target {
					Expression::Variable(_, ref name) => {
						match functions.get_func(name, arg_exprs.len()) {
							Some(func) => func.expression.slice(functions),
							None => None
						}
					}
					_ => None
				}
			}

			_ => None
		}
	}
	
	
	pub fn returned_value_span(&self) -> Span
	{
		match self
		{
			&Expression::Block(ref span, ref exprs) =>
			{
				match exprs.last()
				{
					None => span.clone(),
					Some(expr) => expr.returned_value_span()
				}
			}
			
			_ => self.span()
		}
	}

	pub fn tree(&self, functions: &FunctionManager) -> TreeNode
	{
		let width_str = self.width(functions).map_or("?".to_string(), |w| format!("{}", w));
		match self {
			&Expression::Literal(_, ref value) => {
				TreeNode::new(format!("`{}` -- {}", value, width_str))
			}
			&Expression::Variable(_, ref name) => {
				TreeNode::new(format!("var: {} -- {}", name, width_str))
			}
			&Expression::UnaryOp(_, _, ref op, ref expr) => {
				let mut node = TreeNode::new(format!("{}___ -- {}", op, width_str));

				node.add_node("expr=", expr.tree(functions));

				node
			}
			&Expression::BinaryOp(_, _, ref op, ref left, ref right) => {
				let mut node = TreeNode::new(format!("___ {} ___ -- {}", op, width_str));

				node.add_node("left=", left.tree(functions));
				node.add_node("right=", right.tree(functions));

				node
			}
			&Expression::TernaryOp(_, ref test, ref if_true, ref if_false) => {
				let mut node = TreeNode::new(format!("Ternary -- {}", width_str));

				node.add_node("test=", test.tree(functions));
                node.add_node("if_true=", if_true.tree(functions));
                node.add_node("if_false=", if_false.tree(functions));

				node
			}
			&Expression::BitSlice(_, _, first, last, ref expr) => {
				let mut node = TreeNode::new(format!("[{}:{}] -- {}", first, last, width_str));

				node.add_node("expr=", expr.tree(functions));

                node
			}
			&Expression::Block(_, ref expressions) => {
				let mut node = TreeNode::new(format!("Block -- {}", width_str));

				for expr in expressions {
					node.add_node("", expr.tree(functions));
				}

                node
			}
			&Expression::Call(_, ref target, ref arg_exprs) => {
				let mut node = TreeNode::new(format!("Call {}", width_str));

				node.add_node("target=", target.tree(functions));

				let mut arg_node = TreeNode::new(format!("Args: {}", arg_exprs.len()));

				for expr in arg_exprs {
					arg_node.add_node("", expr.tree(functions))
				}

				node.add_node("", arg_node);

				match **target {
					Expression::Variable(_, ref name) => {
						match functions.get_func(name, arg_exprs.len()) {
							Some(func) => node.add_node("func=", func.expression.tree(functions)),
							None => node.add_leaf("func=<missing>".to_string())
						}
					}
					_ => node.add_leaf("func=<no_name>".to_string())
				}

				node
			}
		}
	}
}