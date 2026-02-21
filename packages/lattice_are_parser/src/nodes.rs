use smol_str::SmolStr;

use crate::grammar::parse_root;
use crate::{BoolExpression, Punctuated, impl_spanned};

macro_rules! define_basic {
	($vis:vis struct $name:ident {} = $label:literal) => {
		#[derive(Debug, Clone, Copy, PartialEq)]
		$vis struct $name {
			pub(crate) offset: usize,
		}
		impl_spanned!(for $name);

		impl $name {
			#[inline]
			$vis fn end_offset(&self) -> usize {
				#[expect(
					clippy::arithmetic_side_effects,
					reason = "This is OK because this would mean the token was parsed from a very massive string.",
				)]
				{self.offset + $label.len()}
			}

			#[inline]
			$vis fn start_offset(&self) -> usize { self.offset }
		}
	};
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArsRoot {
	pub(crate) statements: Vec<Stmt>,
}

impl ArsRoot {
	/// # Errors
	/// This function returns an error when the syntax of `source` is invalid.
	#[inline]
	pub fn parse(source: &str) -> crate::Result<Self> {
		match parse_root(source) {
			| Ok(root) => Ok(root),
			| Err(err) => Err(crate::Error::from_peg(err)),
		}
	}
}

define_basic! {pub struct KwAllow {} = "allow"}
define_basic! {pub struct KwAnd {} = "and"}
define_basic! {pub struct KwDeny {} = "deny"}
define_basic! {pub struct KwFalse {} = "false"}
define_basic! {pub struct KwIf {} = "if"}
define_basic! {pub struct KwIn {} = "in"}
define_basic! {pub struct KwIs {} = "is"}
define_basic! {pub struct KwNot {} = "not"}
define_basic! {pub struct KwOr {} = "or"}
define_basic! {pub struct KwTrue {} = "true"}

define_basic! {pub struct PunctComma {} = ","}
define_basic! {pub struct PunctCurlyLeft {} = "{"}
define_basic! {pub struct PunctCurlyRight {} = "}"}
define_basic! {pub struct PunctDollar {} = "$"}
define_basic! {pub struct PunctDot {} = "."}
define_basic! {pub struct PunctEqual {} = "="}
define_basic! {pub struct PunctExclamation {} = "!"}
define_basic! {pub struct PunctGreaterThan {} = ">"}
define_basic! {pub struct PunctLessThan {} = "<"}
define_basic! {pub struct PunctMinus {} = "-"}
define_basic! {pub struct PunctParenthesisLeft {} = "("}
define_basic! {pub struct PunctParenthesisRight {} = ")"}
define_basic! {pub struct PunctPlus {} = "+"}
define_basic! {pub struct PunctSemi {} = ";"}
define_basic! {pub struct PunctSlash {} = "/"}
define_basic! {pub struct PunctStar {} = "*"}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Stmt {
	Allow(StmtAllow),
	Deny(StmtDeny),
	If(StmtIf),
}
impl_spanned!(for Stmt);

impl Stmt {
	#[must_use]
	#[inline]
	pub fn end_offset(&self) -> usize {
		match self {
			| Self::Allow(this) => this.end_offset(),
			| Self::Deny(this) => this.end_offset(),
			| Self::If(this) => this.end_offset(),
		}
	}

	#[must_use]
	#[inline]
	pub fn start_offset(&self) -> usize {
		match self {
			| Self::Allow(this) => this.start_offset(),
			| Self::Deny(this) => this.start_offset(),
			| Self::If(this) => this.start_offset(),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtAllow {
	pub(crate) keyword: KwAllow,
	pub(crate) scope: Option<Punctuated<Punctuated<ActionSpecItem, PunctSlash>, PunctComma>>,
	pub(crate) semi: PunctSemi,
}
impl_spanned!(for StmtAllow);

impl StmtAllow {
	#[inline]
	#[must_use]
	pub fn end_offset(&self) -> usize { self.semi.end_offset() }

	#[inline]
	#[must_use]
	pub fn start_offset(&self) -> usize { self.keyword.start_offset() }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtDeny {
	pub(crate) keyword: KwDeny,
	pub(crate) scope: Option<Punctuated<Punctuated<ActionSpecItem, PunctSlash>, PunctComma>>,
	pub(crate) semi: PunctSemi,
}
impl_spanned!(for StmtDeny);

impl StmtDeny {
	#[inline]
	#[must_use]
	pub fn end_offset(&self) -> usize { self.semi.end_offset() }

	#[inline]
	#[must_use]
	pub fn start_offset(&self) -> usize { self.keyword.start_offset() }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtIf {
	pub(crate) body: Vec<Stmt>,
	pub(crate) close: PunctCurlyRight,
	pub(crate) condition: BoolExpression,
	pub(crate) keyword: KwIf,
	pub(crate) open: PunctCurlyLeft,
}
impl_spanned!(for StmtIf);

impl StmtIf {
	#[inline]
	#[must_use]
	pub fn end_offset(&self) -> usize { self.close.end_offset() }

	#[inline]
	#[must_use]
	pub fn start_offset(&self) -> usize { self.keyword.start_offset() }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
	pub(crate) ident: SmolStr,
	pub(crate) offset: usize,
}
impl_spanned!(for Ident);

impl Ident {
	#[inline]
	#[must_use]
	pub fn end_offset(&self) -> usize {
		#[expect(
			clippy::arithmetic_side_effects,
			reason = "This is OK because this would mean the token was parsed from a very massive string."
		)]
		{
			self.offset + self.ident.len()
		}
	}

	#[inline]
	#[must_use]
	pub fn start_offset(&self) -> usize { self.offset }
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum ActionSpecItem {
	Any(PunctStar),
	Match(ActionSpecItemMatch),
}
impl_spanned!(for ActionSpecItem);

impl ActionSpecItem {
	#[must_use]
	#[inline]
	pub fn end_offset(&self) -> usize {
		match self {
			| Self::Match(this) => this.end_offset(),
			| Self::Any(this) => this.end_offset(),
		}
	}

	#[must_use]
	#[inline]
	pub fn start_offset(&self) -> usize {
		match self {
			| Self::Match(this) => this.start_offset(),
			| Self::Any(this) => this.start_offset(),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActionSpecItemMatch {
	pub(crate) ident: Ident,
	pub(crate) wildcard_end: Option<PunctStar>,
	pub(crate) wildcard_start: Option<PunctStar>,
}
impl_spanned!(for ActionSpecItemMatch);

impl ActionSpecItemMatch {
	#[inline]
	#[must_use]
	pub fn end_offset(&self) -> usize { self.wildcard_start.map_or(self.ident.end_offset(), |x| x.end_offset()) }

	#[inline]
	#[must_use]
	pub fn start_offset(&self) -> usize {
		self.wildcard_start
			.map_or(self.ident.start_offset(), |x| x.start_offset())
	}
}
