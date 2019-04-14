use expr::ExpressionValue;
use num_bigint::BigInt;
use num_traits::Zero;
use std::collections::HashMap;
use asm::LabelContext;
use expr::Expression;
use asm::ExpressionContext;


pub struct FunctionManager
{
	functions: HashMap<String, Function>
}

pub struct Function {
	pub expression: Expression,
	pub parameters: Vec<String>,
	pub ctx: ExpressionContext
}

impl FunctionManager
{
	pub fn new() -> FunctionManager
	{
		let mut mngr = FunctionManager
		{
			functions: HashMap::new()
		};
		
		mngr
	}

	pub fn add_func<S>(&mut self, ctx: ExpressionContext, name: S, parameters: Vec<String>, expr: Expression)
	where S: Into<String>
	{
        self.functions.insert(name.into(), Function { expression: expr, parameters: parameters, ctx: ctx });
	}
	
	
	pub fn get_func(&self, name: &str) -> Option<&Function>
	{
		self.functions.get(name)
	}

	pub fn func_exists(&self, name: &str) -> bool
	{
		self.get_func(name).is_some()
	}
}