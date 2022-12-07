use std::collections::VecDeque;
use std::iter::Iterator;

pub struct Peekable<I: Iterator> {
    inner: I,
    peeked: VecDeque<I::Item>,
}

impl<I: Iterator> Iterator for Peekable<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.peeked.pop_front().or_else(|| self.inner.next())
    }
}

impl<I: Iterator> Peekable<I> {
    pub fn peek(&mut self, n: usize) -> Option<&<Self as Iterator>::Item> {
        while self.peeked.len() <= n {
            let item = self.inner.next()?;
            self.peeked.push_back(item)
        }

        Some(&self.peeked[n])
    }
}

pub trait PeekableIterExt 
where
    Self: Iterator + Sized
{
    fn very_peekable(self) -> Peekable<Self>;
}

impl<I: Iterator + Sized> PeekableIterExt for I {
    fn very_peekable(self) -> Peekable<Self> {
        Peekable { inner: self, peeked: VecDeque::new() }
    }
}

