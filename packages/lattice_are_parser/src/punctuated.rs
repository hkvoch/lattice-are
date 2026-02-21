use core::borrow::Borrow;
use core::iter::once;
use core::marker::PhantomData;

use crate::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub struct Punctuated<T, P, F = T> {
	_t: PhantomData<T>,
	first: F,
	rest: Vec<(P, T)>,
}

pub type BoxPunctuated<T, P> = Punctuated<T, P, Box<T>>;

impl<T, P, F> Punctuated<T, P, F>
where T: Into<F>
{
	#[inline]
	#[must_use]
	pub fn new<I>(first: T, rest: I) -> Self
	where I: IntoIterator<Item: Into<(P, T)>> {
		Self {
			first: first.into(),
			rest: rest.into_iter().map(Into::into).collect(),
			_t: PhantomData,
		}
	}
}

impl<T, P, F> Punctuated<T, P, F>
where F: Borrow<T>
{
	#[inline]
	#[must_use]
	pub fn first(&self) -> &T { self.first.borrow() }

	#[inline]
	#[must_use]
	pub fn last(&self) -> &T { self.rest.last().map_or(self.first.borrow(), |x| &x.1) }

	#[inline]
	pub fn punctuations(&self) -> impl Iterator<Item = &P> { self.rest.iter().map(|x| &x.0) }

	#[inline]
	pub fn values(&self) -> impl Iterator<Item = &T> { once(self.first.borrow()).chain(self.rest.iter().map(|x| &x.1)) }
}

impl<T, P, F> Spanned for Punctuated<T, P, F>
where
	T: Spanned,
	F: Borrow<T>,
{
	#[inline]
	fn end_offset(&self) -> usize { self.last().end_offset() }

	#[inline]
	fn start_offset(&self) -> usize { self.first().start_offset() }
}
