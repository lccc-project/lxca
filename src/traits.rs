use std::hash::{Hash, Hasher};

pub trait DynHash {
    fn dyn_hash(&self, hasher: &mut dyn Hasher);
}

impl<H: Hash> DynHash for H {
    fn dyn_hash(&self, mut hasher: &mut dyn Hasher) {
        <H as Hash>::hash(self, &mut hasher)
    }
}
