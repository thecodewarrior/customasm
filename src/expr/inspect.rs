use diagn::Span;
use super::Expression;
use super::BinaryOp;
use asm::FunctionManager;


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
			
			&Expression::BitSlice(_, _, left, right, _) => Some(left + 1 - right),
			
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

			&Expression::Call(ref span, ref target, ref arg_exprs) => {
                match **target {
					Expression::Variable(_, ref name) => {
                        match functions.get_func(name) {
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
			&Expression::BinaryOp(_, _, BinaryOp::Concat, _, _) => self.width(functions).map(|w| (w - 1, 0)),
			&Expression::BitSlice(_, _, left, right, _) => Some((left, right)),
			
			&Expression::TernaryOp(_, _, _, _) => self.width(functions).map(|w| (w - 1, 0)),
			
			&Expression::Block(_, ref exprs) =>
			{
				match exprs.last()
				{
					None => None,
					Some(expr) => expr.slice(functions)
				}
			}

			&Expression::Call(ref span, ref target, ref arg_exprs) => {
				match **target {
					Expression::Variable(_, ref name) => {
						match functions.get_func(name) {
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
}