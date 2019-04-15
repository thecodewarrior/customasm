use diagn::{Span, RcReport};
use syntax::{Token, TokenKind, tokenize, Parser};
use syntax::excerpt_as_string_contents;
use expr::{Expression, ExpressionValue};
use asm::cpudef::{Rule, RuleParameterType, RulePatternMatcher};
use util::filename_navigate;
use util::FileServer;
use num_bigint::BigInt;
use std::collections::HashMap;
use AssemblerState;
use asm::AssemblerParser;


#[derive(Debug)]
pub struct CpuDef
{
	pub bits: usize,
	pub label_align: Option<usize>,
	pub rules: Vec<Rule>,
	pub pattern_matcher: RulePatternMatcher,
	pub custom_token_defs: Vec<CustomTokenDef>
}


struct CpuDefParser<'t>
{
	parser: &'t mut Parser,
	fileserver: &'t FileServer,
	cur_filename: String,
	state: &'t mut AssemblerState,

	bits: Option<usize>,
	label_align: Option<usize>,
	rules: Vec<Rule>,
	custom_token_defs: Vec<CustomTokenDef>
}


#[derive(Debug)]
pub struct CustomTokenDef
{
	pub name: String,
	pub excerpt_to_value_map: HashMap<String, ExpressionValue>
}


impl CpuDef
{
	fn parse_file<'t, S>(report: RcReport, parser: &'t mut CpuDefParser, fileserver: &FileServer, filename: S, filename_span: Option<&Span>) -> Result<(), ()>
		where S: Into<String>
	{
		let filename_owned = filename.into();
		let chars = fileserver.get_chars(report.clone(), &filename_owned, filename_span)?;
		let tokens = tokenize(report.clone(), filename_owned.as_ref(), &chars)?;

		let mut new_parser = Parser::new(report.clone(), tokens);
        let mut rules: Vec<Rule> = Vec::new();
		let mut custom_token_defs: Vec<CustomTokenDef> = Vec::new();

        // grab the rules/defs from the passed parser
		std::mem::swap(&mut parser.rules, &mut rules);
		std::mem::swap(&mut parser.custom_token_defs, &mut custom_token_defs);

		let mut sub_parser = CpuDefParser {
			parser: &mut new_parser,
			fileserver: fileserver,
			cur_filename: filename_owned,
			state: parser.state,

			bits: parser.bits,
			label_align: parser.label_align,
			rules: rules,
			custom_token_defs: custom_token_defs
		};

		sub_parser.parse()?;

		parser.bits = sub_parser.bits;
		parser.label_align = sub_parser.label_align;
		// swap the rules/defs (which were moved into sub_parser) back into the passed parser
        std::mem::swap(&mut parser.rules, &mut sub_parser.rules);
        std::mem::swap(&mut parser.custom_token_defs, &mut sub_parser.custom_token_defs);

		Ok(())
	}

	pub fn parse(parser: &mut Parser, state: &mut AssemblerState, fileserver: &FileServer, cur_filename: &String) -> Result<CpuDef, ()>
	{
		let report = parser.report.clone();
		
		let mut cpudef_parser = CpuDefParser
		{
			parser: parser,
			fileserver: fileserver,
			cur_filename: cur_filename.clone(),
			state: state,

			bits: None,
			label_align: None,
			rules: Vec::new(),
			custom_token_defs: Vec::new()
		};

		cpudef_parser.parse()?;

		let pattern_matcher = RulePatternMatcher::new(report, &cpudef_parser.rules, &cpudef_parser.custom_token_defs)?;
		
		//println!("[pattern tree for cpudef]");
		//pattern_matcher.print_debug();
		//println!();
		
		let cpudef = CpuDef
		{
			bits: cpudef_parser.bits.unwrap(),
			label_align: cpudef_parser.label_align,
			rules: cpudef_parser.rules,
			pattern_matcher: pattern_matcher,
			custom_token_defs: cpudef_parser.custom_token_defs
		};
		
		Ok(cpudef)
	}
}


impl<'t> CpuDefParser<'t>
{
	fn parse(&mut self) -> Result<(), ()>
	{
		self.parse_directives()?;

		if self.bits.is_none()
			{ self.bits = Some(8); }

		self.parse_rules()?;

		Ok(())
	}

	fn parse_directives(&mut self) -> Result<(), ()>
	{
		while self.parser.maybe_expect(TokenKind::Hash).is_some()
		{
            self.parse_directive()?;

			self.parser.expect_linebreak()?;
		}
	
		Ok(())
	}

	fn parse_directive(&mut self) -> Result<(), ()>
	{
		let tk_name = self.parser.expect_msg(TokenKind::Identifier, "expected directive name")?;
		match tk_name.excerpt.as_ref().unwrap().as_ref()
			{
				"align"      => self.parse_directive_align(&tk_name)?,
				"bits"       => self.parse_directive_bits(&tk_name)?,
				"labelalign" => self.parse_directive_labelalign(&tk_name)?,
				"tokendef"   => self.parse_directive_tokendef(&tk_name)?,
				"include"    => self.parse_directive_include()?,
				"fun"        => AssemblerParser::parse_directive_fun(self.state, self.parser)?,
				_ => return Err(self.parser.report.error_span("unknown directive", &tk_name.span))
			}

        Ok(())
	}
	
	fn parse_directive_align(&mut self, tk_name: &Token) -> Result<(), ()>
	{
		self.parser.report.warning_span("deprecated directive; use `#bits`", &tk_name.span);
		self.parse_directive_bits(tk_name)
	}
	
	
	fn parse_directive_bits(&mut self, tk_name: &Token) -> Result<(), ()>
	{
		let (tk_bits, bits) = self.parser.expect_usize()?;
		
		if self.bits.is_some()
			{ return Err(self.parser.report.error_span("duplicate `bits` directive", &tk_name.span)); }
			
		if bits == 0
			{ return Err(self.parser.report.error_span("invalid byte size", &tk_bits.span)); }
		
		self.bits = Some(bits);
		
		Ok(())
	}
	
	
	fn parse_directive_labelalign(&mut self, tk_name: &Token) -> Result<(), ()>
	{
		let (tk_value, value) = self.parser.expect_usize()?;
		
		if self.label_align.is_some()
			{ return Err(self.parser.report.error_span("duplicate `labelalign` directive", &tk_name.span)); }
			
		if value == 0
			{ return Err(self.parser.report.error_span("invalid alignment", &tk_value.span)); }
		
		self.label_align = Some(value);
		
		Ok(())
	}
	
	
	fn parse_directive_tokendef(&mut self, _tk_name: &Token) -> Result<(), ()>
	{
		let tk_defname = self.parser.expect(TokenKind::Identifier)?;
		
		let defname = tk_defname.excerpt.unwrap().clone();
		
		if self.custom_token_defs.iter().find(|def| def.name == defname).is_some()
			{ return Err(self.parser.report.error_span("duplicate tokendef name", &tk_defname.span)); }
		
		let mut tokendef = CustomTokenDef
		{
			name: defname,
			excerpt_to_value_map: HashMap::new()
		};
		
		self.parser.expect(TokenKind::BraceOpen)?;
		
		while !self.parser.is_over() && !self.parser.next_is(0, TokenKind::BraceClose)
		{
			let tk_token = self.parser.expect(TokenKind::Identifier)?;
			let token_excerpt = tk_token.excerpt.unwrap().clone();
			
			if tokendef.excerpt_to_value_map.contains_key(&token_excerpt)
				{ return Err(self.parser.report.error_span("duplicate tokendef entry", &tk_token.span)); }
			
			self.parser.expect(TokenKind::Equal)?;
			let value = ExpressionValue::Integer(BigInt::from(self.parser.expect_usize()?.1));
			
			tokendef.excerpt_to_value_map.insert(token_excerpt, value);
			
			if self.parser.maybe_expect_linebreak().is_some()
				{ continue; }
				
			if self.parser.next_is(0, TokenKind::BraceClose)
				{ continue; }
				
			self.parser.expect(TokenKind::Comma)?;
		}
		
		self.parser.expect(TokenKind::BraceClose)?;
		
		self.custom_token_defs.push(tokendef);
		
		Ok(())
	}

	fn parse_directive_include(&mut self) -> Result<(), ()>
	{
		let tk_filename = self.parser.expect(TokenKind::String)?;
		let filename = excerpt_as_string_contents(self.parser.report.clone(), tk_filename.excerpt.as_ref().unwrap().as_ref(), &tk_filename.span)?;

		let new_filename = filename_navigate(self.parser.report.clone(), &self.cur_filename, &filename, &tk_filename.span)?;

		CpuDef::parse_file(self.parser.report.clone(), self, self.fileserver, new_filename, Some(&tk_filename.span))
	}

	fn parse_rules(&mut self) -> Result<(), ()>
	{
		while !self.parser.is_over() && !self.parser.next_is(0, TokenKind::BraceClose)
		{
            if self.parser.maybe_expect(TokenKind::Hash).is_some() {
				self.parse_directive()?;
			} else {
				self.parse_rule()?;
			}
			self.parser.expect_linebreak()?;
		}
		
		Ok(())
	}

	fn parse_rule(&mut self) -> Result<(), ()>
	{
		let mut rule = Rule::new();
		
		let pattern_span_start = self.parser.next().span.clone();
	
		self.parse_rule_pattern(&mut rule)?;
		
		if rule.pattern_parts.len() == 0
		{
			let span = self.parser.next().span.before();
			return Err(self.parser.report.error_span("empty rule pattern", &span));
		}
		
		rule.pattern_span = pattern_span_start.join(&self.parser.prev().span);
		
		self.parser.expect(TokenKind::Arrow)?;
		self.parse_rule_production(&mut rule)?;
		
		self.rules.push(rule);
		
		Ok(())
	}
	

	fn parse_rule_pattern(&mut self, rule: &mut Rule) -> Result<(), ()>
	{
		let mut prev_was_expr_parameter = false;
		
		while !self.parser.next_is(0, TokenKind::Arrow) && !self.parser.next_is(0, TokenKind::ColonColon)
		{
			let tk = self.parser.advance();
			
			let is_beginning_of_pattern = rule.pattern_parts.len() == 0;
			let mut is_allowed_at_beginning = false;
			
			// Read a parameter.
			if tk.kind == TokenKind::BraceOpen
			{
				// Check for consecutive parameters without a separating token.
				if prev_was_expr_parameter
					{ return Err(self.parser.report.error_span("expected a separating token between parameters; try using a `,`", &tk.span.before())); }
			
				prev_was_expr_parameter = self.parse_rule_parameter(rule)?;
				
				is_allowed_at_beginning = !prev_was_expr_parameter;
			}
			
			// Read an exact pattern part.
			else if tk.kind.is_allowed_pattern_token()
			{
				// Check for a stricter set of tokens if an expression parameter came just before.
				if prev_was_expr_parameter && !tk.kind.is_allowed_after_pattern_parameter()
					{ return Err(self.parser.report.error_span("potentially ambiguous token after parameter; try using a separating `,`", &tk.span)); }
					
				if tk.kind == TokenKind::Identifier
					{ is_allowed_at_beginning = true; }
				
				rule.pattern_add_exact(&tk);
				prev_was_expr_parameter = false;
			}
			
			// Check for end of file.
			else if tk.kind == TokenKind::End
				{ return Ok(()) }
			
			// Else, it's illegal to appear in a pattern.
			else
				{ return Err(self.parser.report.error_span("invalid pattern token", &tk.span)); }
				
			// Error if the pattern didn't start with an identifier or tokendef parameter.
			if is_beginning_of_pattern && !is_allowed_at_beginning
				{ return Err(self.parser.report.error_span("expected identifier as first pattern token", &tk.span.before())); }
		}
	
		Ok(())
	}
	

	fn parse_rule_parameter(&mut self, rule: &mut Rule) -> Result<bool, ()>
	{
		let tk_name = self.parser.expect(TokenKind::Identifier)?;
		
		let name = tk_name.excerpt.unwrap().clone();
		
		if is_reserved_var(&name)
			{ return Err(self.parser.report.error_span("reserved variable name", &tk_name.span)); }
		
		if rule.param_exists(&name)
			{ return Err(self.parser.report.error_span("duplicate parameter name", &tk_name.span)); }
			
		let (is_expr_parameter, typ) =
			if self.parser.maybe_expect(TokenKind::Colon).is_some()
			{
				let tk_type = self.parser.expect(TokenKind::Identifier)?;
				let typename = tk_type.excerpt.unwrap().clone();
				
				let mut tokendef_index = None;
				for i in 0..self.custom_token_defs.len()
				{
					if typename == self.custom_token_defs[i].name
						{ tokendef_index = Some(i); }
				}
				
				if tokendef_index.is_none()
					{ return Err(self.parser.report.error_span("unknown parameter type", &tk_type.span)); }
						
				(false, RuleParameterType::CustomTokenDef(tokendef_index.unwrap()))
			}
			else
				{ (true, RuleParameterType::Expression) };
			
		rule.pattern_add_param(name, typ);
		
		self.parser.expect(TokenKind::BraceClose)?;
		
		Ok(is_expr_parameter)
	}
	

	fn parse_rule_production(&mut self, rule: &mut Rule) -> Result<(), ()>
	{
		let expr = Expression::parse(&mut self.parser)?;
		
		let width = match expr.width(&self.state.functions)
		{
			Some(w) => w,
			None => {
				self.parser.report.debug_span(format!("\n{}", expr.tree(&self.state.functions)), &expr.span());
				self.parser.report.error_span("width of expression not known; try using a bit slice like `x[hi:lo]`", &expr.returned_value_span());
				return Err(())
			}
		};
		
		if width % self.bits.unwrap() != 0
			{ return Err(self.parser.report.error_span(format!("expression width (= {}) is not a multiple of the CPU's byte width (= {})", width, self.bits.unwrap()), &expr.returned_value_span())); }
		
		rule.production = expr;
		
		Ok(())
	}
}


fn is_reserved_var(name: &str) -> bool
{
	name == "pc"
}