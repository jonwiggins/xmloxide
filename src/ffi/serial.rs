//! Serialization FFI functions.
#![allow(unsafe_code, clippy::missing_safety_doc)]

use std::os::raw::c_char;

use crate::tree::Document;

use super::strings::to_c_string;
use super::{clear_last_error, set_last_error};

/// Serializes a document to an XML string.
///
/// Returns a caller-owned C string that must be freed with
/// `xmloxide_free_string`. Returns null on failure.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_serialize(doc: *const Document) -> *mut c_char {
    clear_last_error();
    if doc.is_null() {
        set_last_error("null document pointer");
        return std::ptr::null_mut();
    }
    let doc = unsafe { &*doc };
    let output = crate::serial::serialize(doc);
    to_c_string(&output)
}
