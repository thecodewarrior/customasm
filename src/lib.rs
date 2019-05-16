extern crate getopts;
extern crate num_bigint;
extern crate num_integer;
extern crate num_traits;
extern crate flame;

mod asm;
mod diagn;
mod driver;
mod expr;
mod syntax;
mod util;

pub mod webasm;

#[cfg(test)]
mod test;

pub use self::asm::AssemblerState;
pub use self::diagn::Report;
pub use self::driver::drive;
pub use self::util::FileServer;
pub use self::util::FileServerMock;
pub use self::util::FileServerReal;
