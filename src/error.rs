//
// error.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 12/06/2017
//

use std::io;
use std::error::Error;
use std::fmt::{ self, Debug, Display, Formatter };
use std::cell::{ BorrowError, BorrowMutError };
use lexer::Range;


#[derive(Debug)]
pub enum DerefError {
    Borrow(BorrowError),
    BorrowMut(BorrowMutError),
    Strongify,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub range:   Option<Range>,
}

#[derive(Debug, Clone)]
pub struct SemaError {
    pub message: String,
    pub range:   Option<Range>,
}

pub type DerefResult<T>  = Result<T, DerefError>;
pub type SyntaxResult<T> = Result<T, ParseError>;
pub type SemaResult<T>   = Result<T, SemaError>;


impl Error for DerefError {
    fn description(&self) -> &str {
        match *self {
            DerefError::Borrow(ref err)    => err.description(),
            DerefError::BorrowMut(ref err) => err.description(),
            DerefError::Strongify          => "No RcCell backing WkCell",
        }
    }

    fn cause(&self) -> Option<&Error> {
        match *self {
            DerefError::Borrow(ref err)    => Some(err),
            DerefError::BorrowMut(ref err) => Some(err),
            DerefError::Strongify          => None,
        }
    }
}

impl Display for DerefError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Debug::fmt(self, f) // TODO(H2CO3): I'm lazy af
    }
}

impl From<DerefError> for io::Error {
    fn from(err: DerefError) -> Self {
        io::Error::new(io::ErrorKind::Other, err)
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        &self.message
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Syntax error")?;
        write_range(f, self.range)?;
        write!(f, ": {}", self.message)
    }
}

impl Error for SemaError {
    fn description(&self) -> &str {
        &self.message
    }
}

impl Display for SemaError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Semantic error")?;
        write_range(f, self.range)?;
        write!(f, ": {}", self.message)
    }
}

// TODO(H2CO3): storing a stacktrace at the point of
// the failed borrow or strongification would be nice
impl From<DerefError> for SemaError {
    fn from(err: DerefError) -> SemaError {
        let message = match err {
            DerefError::Borrow(be)    => format!("Cannot borrow RcCell: {}", be.description()),
            DerefError::BorrowMut(be) => format!("Cannot mutably borrow RcCell: {}", be.description()),
            DerefError::Strongify     => "No RcCell backing WkCell".to_owned(),
        };

        SemaError {
            message: message,
            range:   None,
        }
    }
}

fn write_range(f: &mut Formatter, range: Option<Range>) -> fmt::Result {
    match range {
        Some(r) => write!(f, " at {}", r),
        None    => Ok(()),
    }
}
