use std::mem::ManuallyDrop;

pub const unsafe fn transmute_unchecked<T, U>(val: T) -> U {
    union Transmuter<T, U> {
        input: ManuallyDrop<T>,
        output: ManuallyDrop<U>,
    }

    ManuallyDrop::into_inner(unsafe {
        Transmuter {
            input: ManuallyDrop::new(val),
        }
        .output
    })
}

pub const unsafe fn transmute<T, U>(val: T) -> U {
    const {
        assert!(core::mem::size_of::<T>() == core::mem::size_of::<U>());
    }

    unsafe { transmute_unchecked(val) }
}
