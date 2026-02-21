use core::borrow::Borrow;
use core::result::Result as StdResult;

use peg::error::ParseError;
use peg::str::LineCol;

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
#[error("{message}")]
pub struct Error {
	pub(crate) column: usize,
	pub(crate) line: usize,
	pub(crate) message: Box<str>,
	pub(crate) offset: usize,
}

impl Error {
	#[must_use]
	#[inline]
	pub(crate) fn from_peg(err: impl Borrow<ParseError<LineCol>>) -> Self {
		let err: &ParseError<LineCol> = err.borrow();
		Self {
			column: err.location.column,
			line: err.location.line,
			message: format!(
				"syntax error at {}:{}: expected {}",
				err.location.line, err.location.column, err.expected,
			)
			.into_boxed_str(),
			offset: err.location.offset,
		}
	}
}

pub type Result<T> = StdResult<T, Error>;
