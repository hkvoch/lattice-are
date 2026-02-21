use crate::grammar::parse_bool_expression;
use crate::{
	KwAnd, KwFalse, KwIn, KwIs, KwNot, KwOr, KwTrue, PunctEqual, PunctExclamation, PunctGreaterThan, PunctLessThan,
	PunctMinus, PunctParenthesisLeft, PunctParenthesisRight, PunctPlus, ValueReference, impl_spanned,
};

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum BoolExpression {
	False(KwFalse),
	Grouping(Grouping),
	Predicate(Predicate),
	True(KwTrue),
	ValueReference(ValueReference),
}
impl_spanned!(for BoolExpression);

impl BoolExpression {
	#[must_use]
	#[inline]
	pub fn end_offset(&self) -> usize {
		match self {
			| BoolExpression::False(this) => this.end_offset(),
			| BoolExpression::Grouping(this) => this.end_offset(),
			| BoolExpression::Predicate(this) => this.end_offset(),
			| BoolExpression::True(this) => this.end_offset(),
			| BoolExpression::ValueReference(this) => this.end_offset(),
		}
	}

	/// # Errors
	/// This function returns an error when the syntax of `source` is invalid.
	#[inline]
	pub fn parse(source: &str) -> crate::Result<Self> {
		match parse_bool_expression(source) {
			| Ok(root) => Ok(root),
			| Err(err) => Err(crate::Error::from_peg(err)),
		}
	}

	#[must_use]
	#[inline]
	pub fn start_offset(&self) -> usize {
		match self {
			| BoolExpression::False(this) => this.start_offset(),
			| BoolExpression::Grouping(this) => this.start_offset(),
			| BoolExpression::Predicate(this) => this.start_offset(),
			| BoolExpression::True(this) => this.start_offset(),
			| BoolExpression::ValueReference(this) => this.start_offset(),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Predicate {
	Binary(BinaryPredicate),
	PrefixUnary(PrefixUnaryPredicate),
}
impl_spanned!(for Predicate);

impl Predicate {
	#[must_use]
	#[inline]
	pub fn end_offset(&self) -> usize {
		match self {
			| Predicate::Binary(this) => this.end_offset(),
			| Predicate::PrefixUnary(this) => this.end_offset(),
		}
	}

	#[must_use]
	#[inline]
	pub fn start_offset(&self) -> usize {
		match self {
			| Predicate::Binary(this) => this.start_offset(),
			| Predicate::PrefixUnary(this) => this.start_offset(),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryPredicate {
	pub(crate) left: Box<BoolExpression>,
	pub(crate) operator: BinaryOperator,
	pub(crate) right: Box<BoolExpression>,
}
impl_spanned!(for BinaryPredicate);

impl BinaryPredicate {
	#[must_use]
	#[inline]
	pub fn end_offset(&self) -> usize { self.right.end_offset() }

	#[must_use]
	#[inline]
	pub fn start_offset(&self) -> usize { self.left.start_offset() }
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum BinaryOperator {
	And(KwAnd),
	AndNot(KwAnd, KwNot),
	Equal(PunctEqual),
	GreaterThan(PunctGreaterThan),
	GreaterThanOrEqual(PunctGreaterThan, PunctEqual),
	In(KwIn),
	Is(KwIs),
	IsNot(KwIs, KwNot),
	LessThan(PunctLessThan),
	LessThanOrEqual(PunctLessThan, PunctEqual),
	Minus(PunctMinus),
	NotEqual(PunctExclamation, PunctEqual),
	NotIn(KwNot, KwIn),
	Or(KwOr),
	OrNot(KwOr, KwNot),
	Plus(PunctPlus),
}
impl_spanned!(for BinaryOperator);

impl BinaryOperator {
	#[must_use]
	#[inline]
	pub fn end_offset(&self) -> usize {
		match self {
			| BinaryOperator::And(last) => last.end_offset(),
			| BinaryOperator::GreaterThan(last) => last.end_offset(),
			| BinaryOperator::LessThan(last) => last.end_offset(),
			| BinaryOperator::Minus(last) => last.end_offset(),
			| BinaryOperator::LessThanOrEqual(_, last)
			| BinaryOperator::GreaterThanOrEqual(_, last)
			| BinaryOperator::Equal(last)
			| BinaryOperator::NotEqual(_, last) => last.end_offset(),
			| BinaryOperator::In(last) | BinaryOperator::NotIn(_, last) => last.end_offset(),
			| BinaryOperator::AndNot(_, last) | BinaryOperator::OrNot(_, last) | BinaryOperator::IsNot(_, last) => {
				last.end_offset()
			}
			| BinaryOperator::Is(last) => last.end_offset(),
			| BinaryOperator::Or(last) => last.end_offset(),
			| BinaryOperator::Plus(last) => last.end_offset(),
		}
	}

	#[must_use]
	#[inline]
	pub fn start_offset(&self) -> usize {
		match self {
			| BinaryOperator::And(first) | BinaryOperator::AndNot(first, _) => first.start_offset(),
			| BinaryOperator::Equal(first) => first.start_offset(),
			| BinaryOperator::GreaterThan(first) | BinaryOperator::GreaterThanOrEqual(first, _) => first.start_offset(),
			| BinaryOperator::In(first) => first.start_offset(),
			| BinaryOperator::Is(first) | BinaryOperator::IsNot(first, _) => first.start_offset(),
			| BinaryOperator::LessThan(first) | BinaryOperator::LessThanOrEqual(first, _) => first.start_offset(),
			| BinaryOperator::Minus(first) => first.start_offset(),
			| BinaryOperator::NotEqual(first, _) => first.start_offset(),
			| BinaryOperator::NotIn(first, _) => first.start_offset(),
			| BinaryOperator::Or(first) | BinaryOperator::OrNot(first, _) => first.start_offset(),
			| BinaryOperator::Plus(first) => first.start_offset(),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixUnaryPredicate {
	pub(crate) operator: PrefixOperator,
	pub(crate) right: Box<BoolExpression>,
}
impl_spanned!(for PrefixUnaryPredicate);

impl PrefixUnaryPredicate {
	#[must_use]
	#[inline]
	pub fn end_offset(&self) -> usize { self.right.end_offset() }

	#[must_use]
	#[inline]
	pub fn start_offset(&self) -> usize { self.operator.start_offset() }
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum PrefixOperator {
	Minus(PunctMinus),
	Not(KwNot),
	Plus(PunctPlus),
}
impl_spanned!(for PrefixOperator);

impl PrefixOperator {
	#[must_use]
	#[inline]
	pub fn end_offset(&self) -> usize {
		match self {
			| PrefixOperator::Minus(this) => this.end_offset(),
			| PrefixOperator::Not(this) => this.end_offset(),
			| PrefixOperator::Plus(this) => this.end_offset(),
		}
	}

	#[must_use]
	#[inline]
	pub fn start_offset(&self) -> usize {
		match self {
			| PrefixOperator::Minus(this) => this.start_offset(),
			| PrefixOperator::Not(this) => this.start_offset(),
			| PrefixOperator::Plus(this) => this.start_offset(),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Grouping {
	pub(crate) close: PunctParenthesisRight,
	pub(crate) expression: Box<BoolExpression>,
	pub(crate) open: PunctParenthesisLeft,
}
impl_spanned!(for Grouping);

impl Grouping {
	#[must_use]
	#[inline]
	pub fn end_offset(&self) -> usize { self.close.end_offset() }

	#[must_use]
	#[inline]
	pub fn start_offset(&self) -> usize { self.open.start_offset() }
}
