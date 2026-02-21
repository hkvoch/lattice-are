use crate::{Ident, PunctDollar, PunctDot, Punctuated, Spanned as _, impl_spanned};

#[derive(Debug, Clone, PartialEq)]
pub struct ValueReference {
	pub(crate) dollar: PunctDollar,
	pub(crate) segments: Punctuated<ValueReferenceSegment, PunctDot>,
}
impl_spanned!(for ValueReference);

impl ValueReference {
	#[must_use]
	#[inline]
	pub fn end_offset(&self) -> usize { self.segments.end_offset() }

	#[must_use]
	#[inline]
	pub fn start_offset(&self) -> usize { self.dollar.start_offset() }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueReferenceSegment {
	pub(crate) ident: Ident,
}
impl_spanned!(for ValueReferenceSegment);

impl ValueReferenceSegment {
	#[must_use]
	#[inline]
	pub fn end_offset(&self) -> usize { self.ident.end_offset() }

	#[must_use]
	#[inline]
	pub fn start_offset(&self) -> usize { self.ident.start_offset() }
}
