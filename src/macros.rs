#[doc(hidden)]
pub use core as _core;

#[macro_export]
macro_rules! nzlit {
    ($e:expr) => {
        const {
            let x = $e;

            $crate::macros::_core::assert!(x != 0);

            unsafe { $crate::macros::_core::num::NonZero::new_unchecked(x) }
        }
    };
}
