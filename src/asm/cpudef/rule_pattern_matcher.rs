use asm::cpudef::{CustomTokenDef, Rule, RuleParameterType, RulePatternPart};
use diagn::{RcReport, Span};
use expr::{Expression, ExpressionValue};
use std::collections::HashMap;
use syntax::{Parser, TokenKind};

#[derive(Debug)]
pub struct RulePatternMatcher {
    root_step: MatchStep,
}

#[derive(Debug)]
struct MatchStep {
    rule_indices: Vec<usize>,
    children_exact: HashMap<MatchStepExact, MatchStep>,
    children_compound: Vec<(MatchStepCompound, MatchStep)>,
    children_param: HashMap<MatchStepParameter, MatchStep>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct MatchStepExact {
    kind: TokenKind,
    excerpt: Option<String>,
}

#[derive(Debug, Clone)]
struct MatchStepCompound {
    span: Span,
    tokens: HashMap<Vec<MatchStepExact>, ExpressionValue>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
struct MatchStepParameter;

#[derive(Debug)]
pub struct Match {
    pub rule_indices: Vec<usize>,
    pub exprs: Vec<Expression>,
}

struct MatchStats {
    exact_count: usize,
    compound_count: usize,
    param_count: usize
}

impl MatchStats {
    fn new(root_step: &MatchStep) -> MatchStats {
        let mut stats = MatchStats {
            exact_count: 0,
            compound_count: 0,
            param_count: 0
        };
        stats.walk(root_step);
        return stats
    }

    fn walk(&mut self, step: &MatchStep) {
        self.exact_count += step.children_exact.len();
        self.param_count += step.children_param.len();
        self.compound_count += step.children_compound.len();
        step.children_exact.iter().for_each(|s| self.walk(&s.1));
        step.children_param.iter().for_each(|s| self.walk(&s.1));
        step.children_compound.iter().for_each(|s| self.walk(&s.1));
    }
}

impl RulePatternMatcher {
    pub fn new(
        report: RcReport,
        rules: &[Rule],
        custom_token_defs: &Vec<CustomTokenDef>,
    ) -> Result<RulePatternMatcher, ()> {
        let mut root_step = MatchStep::new();

        for i in 0..rules.len() {
            RulePatternMatcher::build_step(
                report.clone(),
                &mut root_step,
                &rules[i],
                &rules[i].pattern_parts,
                i,
                custom_token_defs,
            )?;
        }

        let stats = MatchStats::new(&root_step);
        println!(
            "Pattern stats:\n    exact steps: {}\n    param steps: {}\n    compound steps: {}",
            stats.exact_count, stats.param_count, stats.compound_count
        );

        Ok(RulePatternMatcher {
            root_step: root_step,
        })
    }

    fn build_step(
        report: RcReport,
        step: &mut MatchStep,
        rule: &Rule,
        next_parts: &[RulePatternPart],
        rule_index: usize,
        custom_token_defs: &Vec<CustomTokenDef>,
    ) -> Result<(), ()> {
        if next_parts.len() == 0 {
            step.rule_indices.push(rule_index);
            step.rule_indices.sort();
            return Ok(());
        }

        match next_parts[0] {
            RulePatternPart::Exact(ref span, kind, ref excerpt) => {
                let step_kind = MatchStepExact {
                    kind,
                    excerpt: excerpt.as_ref().map(|s| s.to_ascii_lowercase()),
                };

                if let Some(ref mut next_step) = step.children_exact.get_mut(&step_kind) {
                    return RulePatternMatcher::build_step(
                        report.clone(),
                        next_step,
                        rule,
                        &next_parts[1..],
                        rule_index,
                        custom_token_defs,
                    );
                }

                let mut next_step = MatchStep::new();
                RulePatternMatcher::build_step(
                    report.clone(),
                    &mut next_step,
                    rule,
                    &next_parts[1..],
                    rule_index,
                    custom_token_defs,
                )?;
                step.children_exact.insert(step_kind, next_step);
            }

            RulePatternPart::Parameter(ref span, param_index) => {
                if let RuleParameterType::CustomTokenDef(tokendef_index) =
                    rule.params[param_index].typ
                {
                    let custom_token_def = &custom_token_defs[tokendef_index];

                    let mut token_matches: HashMap<Vec<MatchStepExact>, ExpressionValue>
                        = HashMap::new();

                    for (tokens, value) in custom_token_def.tokens.iter() {
                        let token_match: Vec<MatchStepExact> = tokens.iter()
                            .map(|it| MatchStepExact {
                                kind: it.0,
                                excerpt: it.1.clone()
                            })
                            .collect();
                        token_matches.insert(token_match, value.clone());
                    }

                    for existing in step.children_compound.iter() {
                        for (token, value) in existing.0.tokens.iter() {
                            let existing_value = token_matches.get(token);
                            if existing_value.is_some() && existing_value != Some(value) {
                                report.error_build("pattern clashes with a previous instruction pattern")
                                    .span(span)
                                    .annotate_note("clashes here", &existing.0.span);
                                return Err(());
                            }
                        }
                    }

                    let step_kind = MatchStepCompound {
                        span: span.clone(),
                        tokens: token_matches,
                    };

                    let mut next_step = MatchStep::new();
                    RulePatternMatcher::build_step(
                        report.clone(),
                        &mut next_step,
                        rule,
                        &next_parts[1..],
                        rule_index,
                        custom_token_defs,
                    )?;

                    step.children_compound.push((step_kind, next_step));
                } else {
                    let step_kind = MatchStepParameter;

                    if let Some(next_step) = step.children_param.get_mut(&step_kind) {
                        return RulePatternMatcher::build_step(
                            report.clone(),
                            next_step,
                            rule,
                            &next_parts[1..],
                            rule_index,
                            custom_token_defs,
                        );
                    }

                    let mut next_step = MatchStep::new();
                    RulePatternMatcher::build_step(
                        report.clone(),
                        &mut next_step,
                        rule,
                        &next_parts[1..],
                        rule_index,
                        custom_token_defs,
                    )?;
                    step.children_param.insert(step_kind, next_step);
                }
            }
        }

        return Ok(());
    }

    pub fn parse_match(&self, parser: &mut Parser) -> Option<Match> {
        let mut exprs = Vec::new();

        match self.parse_match_step(parser, vec![&self.root_step], &mut exprs) {
            Some(indices) => {
                let result = Match {
                    rule_indices: indices.iter().cloned().collect(),
                    exprs: exprs,
                };

                Some(result)
            }

            None => None,
        }
    }

    fn parse_match_step<'s>(
        &'s self,
        parser: &mut Parser,
        steps: Vec<&'s MatchStep>,
        exprs: &mut Vec<Expression>,
    ) -> Option<&'s [usize]> {
        let parser_state = parser.save();
        if !parser.next_is_linebreak() {
            // Try to match fixed tokens first, if some rule accepts that.

            let tk = parser.advance();

            let step_exact = MatchStepExact {
                kind: tk.kind,
                excerpt: tk.excerpt.map(|s| s.to_ascii_lowercase()),
            };

            let mut next_steps: Vec<&MatchStep> = Vec::new();

            for step in steps.iter() {
                if let Some(next_step) = step.children_exact.get(&step_exact) {
                    next_steps.push(next_step);
                }
            }
            if !next_steps.is_empty() {
                if let Some(result) = self.parse_match_step(parser, next_steps.clone(), exprs) {
                    return Some(result);
                }
            }

            // Then try to match compound tokens (e.g. #tokendef tokens), if some rule accepts that.

            let mut match_steps: HashMap<(&Vec<MatchStepExact>, &ExpressionValue), Vec<&MatchStep>>
                = HashMap::new();

            for step in steps.iter() {
                for (compound_step, next_step) in step.children_compound.iter() {

                    // search through individual token values
                    for (token, token_value) in compound_step.tokens.iter() {
                        parser.restore(parser_state.clone());
                        if token.iter().all(|key_tk| {
                            let tk = parser.advance();
                            key_tk.kind == tk.kind &&
                                key_tk.excerpt == tk.excerpt.map(|s| s.to_ascii_lowercase())
                        }) {
                            match_steps.entry((token, &token_value)).or_insert(Vec::new())
                                .push(next_step);
                        }
                    }
                }
            }

            if !match_steps.is_empty() {
                let mut sorted_match_steps: Vec<(&(&Vec<MatchStepExact>, &ExpressionValue), &Vec<&MatchStep>)> = match_steps
                    .iter()
                    .collect();
                sorted_match_steps.sort_unstable_by_key(|((it, _), _)| -(it.len() as isize));

                for ((token, value), next) in sorted_match_steps {
                    exprs.push(value.make_literal());
                    parser.restore(parser_state.clone());
                    for _ in 0 .. token.len() {
                        parser.advance();
                    }

                    if let Some(result) = self.parse_match_step(parser, next.clone(), exprs) {
                        return Some(result);
                    }

                    exprs.pop();
                }
            }


            // Then try to match argument expressions, if some rule accepts that.
            if steps.iter().any(|step| step.children_param.get(&MatchStepParameter).is_some()) {
                parser.restore(parser_state.clone());

                let expr = match Expression::parse(parser) {
                    Ok(expr) => expr,
                    Err(()) => return None,
                };

                exprs.push(expr);

                for step in steps.iter() {
                    if let Some(next_step) = step.children_param.get(&MatchStepParameter) {
                        next_steps.push(next_step);
                    }
                }

                if let Some(result) = self.parse_match_step(parser, next_steps.clone(), exprs) {
                    return Some(result);
                }

                exprs.pop();
            }
        }

        // Finally, return a match if some rule ends here.
        for step in steps.iter() {
            parser.restore(parser_state.clone());
            if step.rule_indices.len() != 0 {
                if !parser.next_is_linebreak() {
                    return None;
                }

                return Some(&step.rule_indices);
            }
        }

        // Else, return no match.
        None
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn print_debug(&self) {
        self.print_debug_inner(&self.root_step, 1);
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn print_debug_inner(&self, step: &MatchStep, indent: usize) {
        for rule_index in &step.rule_indices {
            for _ in 0..indent {
                print!("   ");
            }

            println!("match #{}", rule_index);
        }

        for (key, next_step) in &step.children_exact {
            for _ in 0..indent {
                print!("   ");
            }

            print!(
                "{}",
                key.kind
                    .printable_excerpt(key.excerpt.as_ref().map(|s| s as &str))
            );

            println!();

            self.print_debug_inner(&next_step, indent + 1);
        }

        for (compound, next_step) in step.children_compound.iter() {
            for _ in 0..indent {
                print!("   ");
            }

            for (tokens, value) in compound.tokens.iter() {
                let token_strings: Vec<String> = tokens.iter()
                    .map(|it| {
                        it.kind
                            .printable_excerpt(it.excerpt.as_ref().map(|s| s as &str))
                    })
                    .collect();
                print!("{}", token_strings.join("-"));

                match &value {
                    &ExpressionValue::Integer(ref bigint, _) => print!(" (= {})", bigint),
                    _ => unreachable!(),
                }

                println!();
            }

            self.print_debug_inner(&next_step, indent + 1);
        }

        for (_, next_step) in &step.children_param {
            for _ in 0..indent {
                print!("   ");
            }

            println!("expr");
            self.print_debug_inner(&next_step, indent + 1);
        }
    }
}

impl MatchStep {
    fn new() -> MatchStep {
        MatchStep {
            rule_indices: Vec::new(),
            children_exact: HashMap::new(),
            children_compound: Vec::new(),
            children_param: HashMap::new(),
        }
    }
}
