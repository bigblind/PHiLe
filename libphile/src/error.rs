//
// error.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 12/06/2017
//

//! This module defines types for representing possible errors
//! that may be generated during the compilation of some PHiLe
//! source code. It also provides useful macros for reporting
//! errors in a way that is consistent across modules.

use std; // for error
use std::io;
use std::result;
use std::fmt::{ self, Display, Formatter };
use std::cell::{ BorrowError, BorrowMutError };
use util::{ Diagnostic, DiagnosticKind };
use lexer::Range;


/// Internal helper for macros `bug!()` and `lazy_bug!()`.
macro_rules! unreachable_error {
    ($msg: expr) => {
        Error::Unreachable {
            message: $msg,
            file: file!(),
            line: line!() as usize,
        }
    }
}

/// Indicates a compiler error. Makes the current function return
/// an `Error::Unreachable`. This is basically a non-panicking
/// substitute for the standard `unreachable!()` macro.
macro_rules! bug {
    ($msg: expr) => {
        return Err(unreachable_error!($msg.to_owned()))
    };
    ($fmt: expr, $($args: tt)*) => {
        return Err(unreachable_error!(format!($fmt, $($args)*)))
    };
}

/// Similar to `bug!()`, but it yields a closure that returns an
/// `Error::Unreachable`. Useful for handling errors efficiently,
/// lazily, primarily using `Option::ok_or_else()`.
macro_rules! lazy_bug {
    ($msg: expr) => {
        || unreachable_error!($msg.to_owned())
    };
    ($fmt: expr, $($args: tt)*) => {
        || unreachable_error!(format!($fmt, $($args)*))
    };
}


/// An error that may occur while compiling PHiLe source code.
/// This can be either a user-induced error (e.g. syntax error),
/// or an Internal Compiler Error (abbreviated ICE), i.e. a bug.
/// TODO(H2CO3): storing a stacktrace at the point of the failed
/// borrow, strongification, or unreachable code would be nice.
#[derive(Debug)]
pub enum Error {
    /// I/O error, probably coming from the OS, not PHiLe itself
    IO(io::Error),
    /// An immutable dynamic borrow failed. ICE.
    Borrow(BorrowError),
    /// A mutable dynamic borrow failed. ICE.
    BorrowMut(BorrowMutError),
    /// Converting a weak pointer to a strong one failed. ICE.
    Strongify,
    /// Some unreachable code was reached as a result of a bug/unforeseen condition. ICE.
    Unreachable {
        /// Description of the bug.
        message: String,
        /// Source file inside the PHiLe compiler that caused the bug.
        file: &'static str,
        /// Source line index within the offending file.
        line: usize,
    },
    /// A syntactic error was found in the source code.
    Syntax {
        /// Description of the syntax error.
        message: String,
        /// The human-readable source range associated with the error.
        range: Range,
    },
    /// A semantic error was found in the source code.
    Semantic {
        /// Description of the semantic error.
        message: String,
        /// If available, the human-readable source range associated
        /// with the error. End-of-input is represented as `None`.
        range: Option<Range>,
    },
}

/// Convenience type alias for expressing `Result`s of PHiLe `Error`s.
pub type Result<T> = result::Result<T, Error>;


impl Error {
    /// Writes an error object as a nicely-formatted, user-readable
    /// error message, including source file names and locations.
    ///
    /// # Arguments:
    ///
    /// * `wr`: the `io::Write` that the error will be printed to.
    /// * `sources`: a slice of `str`s representing the name of each
    ///   source component (e.g., file).
    pub fn pretty_print<P: AsRef<str>>(&self, wr: &mut io::Write, sources: &[P]) -> io::Result<()> {
        let range = match *self {
            Error::IO(_)                  => None,
            Error::Borrow(_)              => None,
            Error::BorrowMut(_)           => None,
            Error::Strongify              => None,
            Error::Unreachable { .. }     => None,
            Error::Syntax   { range, .. } => range.into(),
            Error::Semantic { range, .. } => range,
        };

        if let Some(r) = range {
            write!(
                wr,
                "\n\n    In file {}, near {}:\n",
                Diagnostic::new(sources[r.start.src_idx].as_ref(), DiagnosticKind::Highlight),
                Diagnostic::new(r, DiagnosticKind::Highlight),
            )?;
        } else {
            write!(wr, "\n\n")?;
        }

        write!(wr, "        {}\n\n", Diagnostic::new(self, DiagnosticKind::Error))
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
