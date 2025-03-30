use std::marker::PhantomData;

use crate::{FileRange, Location};

pub trait DisplayLocation<L: Location> {
    fn display_location(&self, location: &L) -> String;
}

impl<T, L> DisplayLocation<L> for &'_ T
where
    T: DisplayLocation<L>,
    L: Location,
{
    fn display_location(&self, location: &L) -> String {
        T::display_location(self, location)
    }
}

pub struct SpanDisplay;

impl DisplayLocation<toc_span::Span> for SpanDisplay {
    fn display_location(&self, location: &toc_span::Span) -> String {
        match location.into_parts() {
            Some((file, range)) => {
                let file = file.into_raw().into_raw();
                let (start, end) = (u32::from(range.start()), u32::from(range.end()));
                format!("<file#{file}>:{start}..{end}")
            }
            None => String::from("<unknown>:0..0"),
        }
    }
}

pub struct FileRangeDisplay;

impl DisplayLocation<FileRange> for FileRangeDisplay {
    fn display_location(&self, location: &FileRange) -> String {
        let range = location.0;
        let (start, end) = (u32::from(range.start()), u32::from(range.end()));
        format!("{start}..{end}")
    }
}

pub struct FnDisplay<F: for<'a> Fn(&'a L) -> String, L>(F, PhantomData<L>);

impl<F, L> FnDisplay<F, L>
where
    F: for<'a> Fn(&'a L) -> String,
{
    pub fn new(f: F) -> Self {
        Self(f, PhantomData)
    }
}

impl<F, L> From<F> for FnDisplay<F, L>
where
    F: for<'a> Fn(&'a L) -> String,
{
    fn from(value: F) -> Self {
        Self(value, PhantomData)
    }
}

impl<F, L> DisplayLocation<L> for FnDisplay<F, L>
where
    F: for<'a> Fn(&'a L) -> String,
    L: Location,
{
    fn display_location(&self, location: &L) -> String {
        self.0(location)
    }
}

pub struct DisplayWithLocations<T, M: DisplayLocation<L>, L: Location> {
    pub(crate) item: T,
    pub(crate) mapper: M,
    pub(crate) _location: PhantomData<L>,
}

pub trait WithDisplayLocations<L: Location> {
    /// Displays locations using the specified display location mapper.
    fn display_spans<D: DisplayLocation<L>>(&self, mapper: D) -> DisplayWithLocations<&Self, D, L> {
        DisplayWithLocations {
            item: self,
            mapper,
            _location: PhantomData,
        }
    }
}

impl<T, L: Location> WithDisplayLocations<L> for &'_ T where T: WithDisplayLocations<L> {}
