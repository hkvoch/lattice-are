#[expect(clippy::arbitrary_source_item_ordering, reason = "Macro re-export ordering.")]
mod macros {
	macro_rules! impl_spanned {
		(for $typ:ty) => {
			impl $crate::Spanned for $typ {
				#[inline]
				fn end_offset(&self) -> usize { self.end_offset() }
				#[inline]
				fn start_offset(&self) -> usize { self.start_offset() }
			}
		};
	}

	pub(crate) use impl_spanned;
}

pub(crate) use macros::*;

pub trait Spanned {
	#[must_use]
	fn end_offset(&self) -> usize;

	#[must_use]
	fn start_offset(&self) -> usize;
}
