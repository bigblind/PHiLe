//
// error.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 12/06/2017
//

use std; // for error
use std::io;
use std::result;
use std::num::{ ParseIntError, ParseFloatError };
use std::fmt::{ self, Display, Formatter };
use std::cell::{ BorrowError, BorrowMutError };
use lexer::Range;
use util::COLOR;


// Indicates a compiler error. Basically a non-panicking 'unreachable'.
macro_rules! bug {
    () => { bug!("internal inconsistency") };
    ($msg: expr) => { bug!("{}", $msg) };
    ($fmt: expr, $($args: expr),+) => {
        return Err(Error::Unreachable {
            message: format!($fmt, $($args),+),
            file:    file!(),
            line:    line!() as usize,
        })
    };
    ($fmt: expr, $($args: expr),+,) => {
        bug!($fmt, $($args),+)
    };
}

macro_rules! lazy_bug {
    ($msg: expr) => {
        || Error::Unreachable {
            message: $msg.into(),
            file:    file!(),
            line:    line!() as usize,
        }
    }
}


// TODO(H2CO3): storing a stacktrace at the point of the failed
// borrow, strongification, or unreachable code would be nice.
#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Borrow(BorrowError),       // ICE
    BorrowMut(BorrowMutError), // ICE
    Strongify,                 // ICE
    Unreachable {              // ICE
        message: String,
        file:    &'static str,
        line:    usize,
    },
    Syntax {
        message: String,
        range:   Option<Range>,
    },
    Semantic {
        message: String,
        range:   Option<Range>,
    },
}

pub type Result<T> = result::Result<T, Error>;


impl Error {
    pub fn pretty_print<P: AsRef<str>>(&self, wr: &mut io::Write, sources: &[P]) -> io::Result<()> {
        let range = match *self {
            Error::IO(_)                  => None,
            Error::Borrow(_)              => None,
            Error::BorrowMut(_)           => None,
            Error::Strongify              => None,
            Error::Unreachable { .. }     => None,
            Error::Syntax   { range, .. } => range,
            Error::Semantic { range, .. } => range,
        };

        if let Some(r) = range {
            write!(
                wr,
                "\n\n    In file {clr_hgl}{file}{clr_rst}, near {clr_hgl}{range}{clr_rst}:\n",
                file = sources[r.begin.src_idx].as_ref(),
                range = r,
                clr_hgl = COLOR.highlight,
                clr_rst = COLOR.reset,
            )?;
        } else {
            write!(wr, "\n\n")?;
        }

        write!(
            wr,
            "        {clr_err}{error}{clr_rst}\n\n",
            error = self,
            clr_err = COLOR.error,
            clr_rst = COLOR.reset,
        )
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::IO(ref err)        => err.description(),
            Error::Borrow(ref err)    => err.description(),
            Error::BorrowMut(ref err) => err.description(),
            Error::Strongify => "No strong pointer backing weak",
            Error::Unreachable { ref message, .. } => message,
            Error::Syntax { ref message, .. }      => message,
            Error::Semantic { ref message, .. }    => message,
        }
    }

    fn cause(&self) -> Option<&std::error::Error> {
        match *self {
            Error::IO(ref err)        => Some(err),
            Error::Borrow(ref err)    => Some(err),
            Error::BorrowMut(ref err) => Some(err),
            Error::Strongify          => None,
            Error::Unreachable { .. } => None,
            Error::Syntax { .. }      => None,
            Error::Semantic { .. }    => None,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Error::IO(ref err) => write!(f, "I/O error: {}", err),
            Error::Borrow(ref err) => write!(
                f,
                "Internal Compiler Error: {}. This is a bug.",
                err,
            ),
            Error::BorrowMut(ref err) => write!(
                f,
                "Internal Compiler Error: {}. This is a bug.",
                err,
            ),
            Error::Strongify => write!(
                f,
                "Internal Compiler Error: No strong pointer backing weak. This is a bug.",
            ),
            Error::Unreachable { ref message, file, line } => write!(
                f,
                "Internal Compiler Error: Reached unreachable code: {}, in file {}, line {}. This is a bug.",
                message,
                file,
                line,
            ),
            Error::Syntax { ref message, .. } => write!(
                f, "Syntax Error: {}", message
            ),
            Error::Semantic { ref message, .. } => write!(
                f, "Semantic Error: {}", message
            ),
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::IO(error)
    }
}

impl From<BorrowError> for Error {
    fn from(error: BorrowError) -> Self {
        Error::Borrow(error)
    }
}

impl From<BorrowMutError> for Error {
    fn from(error: BorrowMutError) -> Self {
        Error::BorrowMut(error)
    }
}

// TODO(H2CO3): parser should be aware of the range of the ParseIntError
impl From<ParseIntError> for Error {
    fn from(error: ParseIntError) -> Self {
        use std::error::Error;

        self::Error::Syntax {
            message: error.description().to_owned(),
            range:   None,
        }
    }
}

// TODO(H2CO3): parser should be aware of the range of the ParseFloatError
impl From<ParseFloatError> for Error {
    fn from(error: ParseFloatError) -> Self {
        use std::error::Error;

        self::Error::Syntax {
            message: error.description().to_owned(),
            range:   None,
        }
    }
}
