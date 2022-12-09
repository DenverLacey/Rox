use std::collections::VecDeque;
use std::iter::Iterator;

pub struct VeryPeekable<I: Iterator> {
    inner: I,
    peeked: VecDeque<I::Item>,
}

impl<I: Iterator> Iterator for VeryPeekable<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.peeked.pop_front().or_else(|| self.inner.next())
    }
}

impl<I: Iterator> VeryPeekable<I> {
    pub fn new(inner: I) -> Self {
        Self {
            inner,
            peeked: VecDeque::new(),
        }
    }
}

impl<I: Iterator> VeryPeekable<I> {
    pub fn peek(&mut self, n: usize) -> Option<&<Self as Iterator>::Item> {
        self.peek_mut(n).map(|r| r as &_)
    }

    pub fn peek_mut(&mut self, n: usize) -> Option<&mut <Self as Iterator>::Item> {
        while self.peeked.len() <= n {
            let item = self.inner.next()?;
            self.peeked.push_back(item)
        }

        Some(&mut self.peeked[n])
    }
}

pub trait VeryPeekableIterExt
where
    Self: Iterator + Sized,
{
    fn very_peekable(self) -> VeryPeekable<Self>;
}

impl<I: Iterator + Sized> VeryPeekableIterExt for I {
    fn very_peekable(self) -> VeryPeekable<Self> {
        VeryPeekable::new(self)
    }
}
