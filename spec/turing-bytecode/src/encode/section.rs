use std::{marker::PhantomData, num::NonZeroU32};

pub type ManifestAllocator = SectionAllocator<Manifest>;
pub type ManifestSection = Section<Manifest>;
pub type ManifestSlot = SectionSlot<Manifest>;

pub type GlobalsAllocator = SectionAllocator<Global>;
pub type GlobalsSection = Section<Global>;
pub type GlobalsSlot = SectionSlot<Global>;

#[derive(Debug)]
pub struct SectionAllocator<S: SectionInfo> {
    allocs: Vec<SectionAllocation>,
    _section: PhantomData<S>,
}

impl<S: SectionInfo> Default for SectionAllocator<S> {
    fn default() -> Self {
        Self {
            allocs: vec![],
            _section: PhantomData,
        }
    }
}

impl<S: SectionInfo> SectionAllocator<S> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn finish(self) -> Section<S> {
        let mut bytes = vec![];
        let mut offsets = vec![];
        let mut current_offset = 0usize;

        for alloc in &self.allocs {
            let padding_offset = current_offset.next_multiple_of(alloc.align as usize);
            offsets.push(padding_offset as u32);

            if let Some(padding_count) = padding_offset.checked_sub(current_offset) {
                if padding_count > 0 {
                    bytes.extend(std::iter::repeat_n(0x00, padding_count));
                }
            }

            match &alloc.data {
                SectionData::Zeroed(size) => bytes.extend(std::iter::repeat_n(0x00, *size)),
                SectionData::Data(data) => bytes.extend_from_slice(&*data),
            }

            current_offset = padding_offset + alloc.size();
        }

        Section {
            data: bytes.into_boxed_slice(),
            offsets: offsets.into_boxed_slice(),
            _section: PhantomData,
        }
    }
}

impl SectionAllocator<Manifest> {
    /// Allocates a portion region of memory within the manifest section.
    /// The data can either be zeroed, or filled with specific data.
    pub fn alloc(&mut self, data: SectionData, align: u32) -> ManifestSlot {
        assert!(align > 0);
        assert!(align.is_power_of_two() && align <= 4);

        let slot = SectionSlot::from_usize(self.allocs.len());
        self.allocs.push(SectionAllocation { data, align });
        slot
    }
}

impl SectionAllocator<Global> {
    /// Allocates a zeroed region of memory within the global section.
    pub fn alloc(&mut self, size: usize, align: u32) -> GlobalsSlot {
        assert!(align > 0);
        assert!(align.is_power_of_two() && align <= 4);

        let slot = SectionSlot::from_usize(self.allocs.len());
        self.allocs.push(SectionAllocation {
            data: SectionData::Zeroed(size),
            align,
        });
        slot
    }
}

#[derive(Debug)]
pub enum SectionData {
    Zeroed(usize),
    Data(Box<[u8]>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct SectionSlot<S: SectionInfo>(NonZeroU32, PhantomData<S>);

impl<S: SectionInfo> SectionSlot<S> {
    fn from_usize(idx: usize) -> Self {
        NonZeroU32::new(idx as u32)
            .map(|it| Self(it, PhantomData))
            .unwrap()
    }

    pub fn as_usize(self) -> usize {
        self.0.get().saturating_sub(1) as usize
    }
}

#[derive(Debug)]
pub struct Section<S: SectionInfo> {
    data: Box<[u8]>,
    offsets: Box<[u32]>,
    _section: PhantomData<S>,
}

impl<S: SectionInfo> Section<S> {
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    pub fn size(&self) -> usize {
        self.data.len()
    }

    pub fn offset(&self, slot: SectionSlot<S>) -> u32 {
        self.offsets[slot.as_usize()]
    }
}

#[derive(Debug)]
pub struct Global {}
#[derive(Debug)]
pub struct Manifest {}

pub trait SectionInfo: sealed::Sealed {}

impl SectionInfo for Global {}
impl SectionInfo for Manifest {}

#[derive(Debug)]
struct SectionAllocation {
    data: SectionData,
    align: u32,
}

impl SectionAllocation {
    fn size(&self) -> usize {
        match &self.data {
            SectionData::Zeroed(size) => *size,
            SectionData::Data(data) => data.len(),
        }
    }
}

mod sealed {
    use super::*;

    pub trait Sealed {}

    impl Sealed for Global {}
    impl Sealed for Manifest {}
}
