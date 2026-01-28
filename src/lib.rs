#![feature(macro_metavar_expr, iter_advance_by)]

#[macro_use]
#[doc(hidden)]
pub mod macros;

pub mod binfmt;
pub mod collections;
pub mod fmt_helpers;
pub mod ir;
pub mod mem;
pub mod session;
pub mod target;
pub mod traits;

extern crate self as lxca;
