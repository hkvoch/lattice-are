use smol_str::SmolStr;

#[expect(clippy::wildcard_imports, reason = "")]
use crate::nodes::*;

use crate::{
	BinaryOperator, BinaryPredicate, BoolExpression, BoxPunctuated, Grouping, Predicate, PrefixOperator,
	PrefixUnaryPredicate, Punctuated, ValueReference, ValueReferenceSegment,
};

pub(crate) use access_rule_set_parser::{bool_expression_top as parse_bool_expression, root as parse_root};

peg::parser! {
	grammar access_rule_set_parser() for str {
		pub rule root() -> ArsRoot
			= __ statements:stmt() ** __ __ { ArsRoot { statements } }

		pub rule bool_expression_top() -> BoolExpression = __ expr:bool_expression() __ { expr }

		rule stmt() -> Stmt
			= stmt:stmt_allow() { Stmt::Allow(stmt) }
			/ stmt:stmt_deny() { Stmt::Deny(stmt) }
			/ stmt:stmt_if() { Stmt::If(stmt) }
			/ expected!("statement")

		rule stmt_allow() -> StmtAllow
			= keyword:kw_allow() __ scope:punctuated_ws(<punctuated(<action_spec_item()>, <slash()>)>, <comma()>)? __ semi:semi() {
				StmtAllow { keyword, scope, semi }
			}

		rule stmt_deny() -> StmtDeny
			= keyword:kw_deny() __ scope:punctuated_ws(<punctuated(<action_spec_item()>, <slash()>)>, <comma()>)? __ semi:semi() {
				StmtDeny { keyword, scope, semi }
			}

		rule stmt_if() -> StmtIf
			= keyword:kw_if() __ condition:bool_expression() __ then:body() {
				let (open, body, close) = then;
				StmtIf { body, close, condition, keyword, open }
			}

		rule action_spec_item() -> ActionSpecItem
			= item:action_spec_item_match() { ActionSpecItem::Match(item) }
			/ item:star() { ActionSpecItem::Any(item) }

		rule action_spec_item_match() -> ActionSpecItemMatch
			= wildcard_start:star()? ident:ident() wildcard_end:star()? { ActionSpecItemMatch { ident, wildcard_end, wildcard_start } }

		rule ident() -> Ident
			= offset:position!() ident:quiet!{ $(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) } {
				Ident { ident: SmolStr::new(ident), offset }
			}
			/ expected!("identifier")

		rule body() -> (PunctCurlyLeft, Vec<Stmt>, PunctCurlyRight)
			= open:curly_left() __ statements:(stmt() ** __) __ close:curly_right() { (open, statements, close) }

		rule bool_expression() -> BoolExpression = precedence! {
			lhs:(@) __ op:kw_or() space() not:kw_not() __ rhs:@ {
				let operator = BinaryOperator::OrNot(op, not);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			lhs:(@) __ op:kw_or() __ rhs:@ {
				let operator = BinaryOperator::Or(op);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			--
			lhs:(@) __ op:kw_and() space() not:kw_not() __ rhs:@ {
				let operator = BinaryOperator::AndNot(op, not);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			lhs:(@) __ op:kw_and() __ rhs:@ {
				let operator = BinaryOperator::And(op);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			--
			lhs:(@) __ not:kw_not() space() op:kw_in() __ rhs:@ {
				let operator = BinaryOperator::NotIn(not, op);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			lhs:(@) __ op:kw_in() __ rhs:@ {
				let operator = BinaryOperator::In(op);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			lhs:(@) __ op1:greater_than() op2:equal() __ rhs:@ {
				let operator = BinaryOperator::GreaterThanOrEqual(op1, op2);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			lhs:(@) __ op:greater_than() __ rhs:@ {
				let operator = BinaryOperator::GreaterThan(op);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			lhs:(@) __ op1:less_than() op2:equal() __ rhs:@ {
				let operator = BinaryOperator::LessThanOrEqual(op1, op2);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			lhs:(@) __ op:less_than() __ rhs:@ {
				let operator = BinaryOperator::LessThan(op);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			lhs:(@) __ op:equal() __ rhs:@ {
				let operator = BinaryOperator::Equal(op);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			lhs:(@) __ op:kw_is() space() not:kw_not() __ rhs:@ {
				let operator = BinaryOperator::IsNot(op, not);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			lhs:(@) __ op:kw_is() __ rhs:@ {
				let operator = BinaryOperator::Is(op);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			lhs:(@) __ not:exclamation() op:equal() __ rhs:@ {
				let operator = BinaryOperator::NotEqual(not, op);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			--
			lhs:(@) __ op:minus() __ rhs:@ {
				let operator = BinaryOperator::Minus(op);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			lhs:(@) __ op:plus() __ rhs:@ {
				let operator = BinaryOperator::Plus(op);
				let left = Box::new(lhs);
				let right = Box::new(rhs);
				BoolExpression::Predicate(Predicate::Binary(BinaryPredicate { left, operator, right }))
			}
			--
			op:minus() __ child:(@) {
				let operator = PrefixOperator::Minus(op);
				let right = Box::new(child);
				BoolExpression::Predicate(Predicate::PrefixUnary(PrefixUnaryPredicate { operator, right }))
			}
			op:kw_not() space() child:(@) {
				let operator = PrefixOperator::Not(op);
				let right = Box::new(child);
				BoolExpression::Predicate(Predicate::PrefixUnary(PrefixUnaryPredicate { operator, right }))
			}
			op:plus() __ child:(@) {
				let operator = PrefixOperator::Plus(op);
				let right = Box::new(child);
				BoolExpression::Predicate(Predicate::PrefixUnary(PrefixUnaryPredicate { operator, right }))
			}
			--
			this:value_reference() { BoolExpression::ValueReference(this) }
			keyword:kw_true() { BoolExpression::True(keyword) }
			keyword:kw_false() { BoolExpression::False(keyword) }
			open:parenthesis_left() __ expression:bool_expression() __ close:parenthesis_right() {
				BoolExpression::Grouping(Grouping { close, expression: Box::new(expression), open })
			}
		}

		rule value_reference() -> ValueReference
			= dollar:dollar() segments:punctuated_ws(<value_reference_segment()>, <dot()>) { ValueReference { dollar, segments } }

		rule value_reference_segment() -> ValueReferenceSegment
			= ident:ident() { ValueReferenceSegment { ident } }

		rule kw_allow() -> KwAllow = offset:position!() "allow" { KwAllow { offset } }
		rule kw_and() -> KwAnd = offset:position!() "and" { KwAnd { offset } }
		rule kw_deny() -> KwDeny = offset:position!() "deny" { KwDeny { offset } }
		rule kw_false() -> KwFalse = offset:position!() "false" { KwFalse { offset } }
		rule kw_if() -> KwIf = offset:position!() "if" { KwIf { offset } }
		rule kw_in() -> KwIn = offset:position!() "in" { KwIn { offset } }
		rule kw_is() -> KwIs = offset:position!() "is" { KwIs { offset } }
		rule kw_not() -> KwNot = offset:position!() "not" { KwNot { offset } }
		rule kw_or() -> KwOr = offset:position!() "or" { KwOr { offset } }
		rule kw_true() -> KwTrue = offset:position!() "true" { KwTrue { offset } }

		rule comma() -> PunctComma = offset:position!() "," { PunctComma { offset } }
		rule curly_left() -> PunctCurlyLeft = offset:position!() "{" { PunctCurlyLeft { offset } }
		rule curly_right() -> PunctCurlyRight = offset:position!() "}" { PunctCurlyRight { offset } }
		rule dollar() -> PunctDollar = offset:position!() "$" { PunctDollar { offset } }
		rule dot() -> PunctDot = offset:position!() "." { PunctDot { offset } }
		rule equal() -> PunctEqual = offset:position!() "=" { PunctEqual { offset } }
		rule exclamation() -> PunctExclamation = offset:position!() "!" { PunctExclamation { offset } }
		rule greater_than() -> PunctGreaterThan = offset:position!() ">" { PunctGreaterThan { offset } }
		rule less_than() -> PunctLessThan = offset:position!() "<" { PunctLessThan { offset } }
		rule minus() -> PunctMinus = offset:position!() "-" { PunctMinus { offset } }
		rule parenthesis_left() -> PunctParenthesisLeft = offset:position!() "(" { PunctParenthesisLeft { offset } }
		rule parenthesis_right() -> PunctParenthesisRight = offset:position!() ")" { PunctParenthesisRight { offset } }
		rule plus() -> PunctPlus = offset:position!() "+" { PunctPlus { offset } }
		rule semi() -> PunctSemi = offset:position!() ";" { PunctSemi { offset } }
		rule slash() -> PunctSlash = offset:position!() "/" { PunctSlash { offset } }
		rule star() -> PunctStar = offset:position!() "*" { PunctStar { offset } }

		rule space() = ___+

		rule __() = ___*
		rule ___() = quiet!{[' ' | '\n' | '\t']}

		rule punctuated<T, P>(item: rule<T>, punctuation: rule<P>) -> Punctuated<T, P>
			= first:item() rest:tuple(<punctuation()>, <item()>)* { Punctuated::new(first, rest) }

		rule punctuated_ws<T, P>(item: rule<T>, punctuation: rule<P>) -> Punctuated<T, P>
			= first:item() __ rest:(tuple_ws(<punctuation()>, <item()>) ** __) { Punctuated::new(first, rest) }

		rule punctuated_ws_box<T, P>(item: rule<T>, punctuation: rule<P>) -> BoxPunctuated<T, P>
			= first:item() __ rest:(tuple_ws(<punctuation()>, <item()>) ** __) { Punctuated::new(first, rest) }

		rule tuple<T1, T2>(rule1: rule<T1>, rule2: rule<T2>) -> (T1, T2) = item1:rule1() item2:rule2() { (item1, item2) }

		rule tuple_ws<T1, T2>(rule1: rule<T1>, rule2: rule<T2>) -> (T1, T2) = item1:rule1() __ item2:rule2() { (item1, item2) }
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn parse_simple_rule_set() {
		let ruleset = "
			if $requiredCapability not in $subject.capabilities {
				deny;
			}
		";

		access_rule_set_parser::root(ruleset).unwrap();
		// panic!("{:#?}", access_rule_set_parser::root(ruleset).unwrap());
	}
}
