//! C FFI layer for xmloxide.
//!
//! Provides a C-compatible API for using xmloxide from C/C++ and other
//! languages that support C FFI. All symbols use the `xmloxide_` prefix.
//!
//! # Error Handling
//!
//! Functions that can fail return null pointers (for pointer types) or 0
//! (for `NodeId` values). The last error message is stored in thread-local
//! storage and can be retrieved via [`xmloxide_last_error`].
//!
//! # String Ownership
//!
//! All strings returned by FFI functions are caller-owned C strings that
//! must be freed via [`xmloxide_free_string`](strings::xmloxide_free_string).
//!
//! # Safety
//!
//! All `extern "C"` functions in this module are inherently unsafe because
//! they accept raw pointers from C callers.

// FFI functions require unsafe blocks throughout.
#![allow(unsafe_code, clippy::missing_safety_doc)]

pub mod c14n;
pub mod catalog;
pub mod document;
pub mod serial;
pub mod strings;
pub mod tree;
pub mod validation;
pub mod xinclude;
pub mod xpath;

use std::cell::RefCell;
use std::ffi::CString;
use std::os::raw::c_char;

thread_local! {
    static LAST_ERROR: RefCell<Option<CString>> = const { RefCell::new(None) };
}

/// Stores an error message in thread-local storage.
fn set_last_error(msg: &str) {
    LAST_ERROR.with(|cell| {
        *cell.borrow_mut() = CString::new(msg).ok();
    });
}

/// Clears the thread-local error.
fn clear_last_error() {
    LAST_ERROR.with(|cell| {
        *cell.borrow_mut() = None;
    });
}

/// Returns the last error message, or null if no error occurred.
///
/// The returned string is owned by the library and must NOT be freed
/// by the caller. It is valid until the next FFI call on the same thread.
#[no_mangle]
pub extern "C" fn xmloxide_last_error() -> *const c_char {
    LAST_ERROR.with(|cell| {
        let borrow = cell.borrow();
        match borrow.as_ref() {
            Some(cs) => cs.as_ptr(),
            None => std::ptr::null(),
        }
    })
}
