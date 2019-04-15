use expr::ExpressionValue;
use num_bigint::BigInt;
use num_traits::Zero;
use std::collections::HashMap;
use asm::LabelContext;
use expr::Expression;
use asm::ExpressionContext;


pub struct FunctionManager
{
	functions: HashMap<FunctionSignature, Function>
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct FunctionSignature {
	pub name: String,
	pub parameters: usize
}

pub struct Function {
    pub signature: FunctionSignature,
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
		let signature = FunctionSignature {
			name: name.into(),
			parameters: parameters.len()
		};

        self.functions.insert(signature.clone(), Function { signature: signature, expression: expr, parameters: parameters, ctx: ctx });
	}

	pub fn all_functions(&self) -> Vec<&Function> {
		self.functions.values().into_iter().collect()
	}

	pub fn get_func(&self, name: &str, parameters: usize) -> Option<&Function>
	{
		self.functions.get(&FunctionSignature { name: name.into(), parameters: parameters })
	}

	pub fn func_exists(&self, name: &str, parameters: usize) -> bool
	{
		self.get_func(name, parameters).is_some()
	}
}