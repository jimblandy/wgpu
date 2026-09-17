/*! [`Builder`] utility functions. */

use crate::back;

use alloc::vec::Vec;
use spirv::Word;

fn bytes_to_words(bytes: &[u8]) -> Vec<Word> {
    bytes
        .chunks(4)
        .map(|chars| chars.iter().rev().fold(0u32, |u, c| (u << 8) | *c as u32))
        .collect()
}

pub fn string_to_words(input: &str) -> Vec<Word> {
    let bytes = input.as_bytes();

    debug_str_bytes_to_words(bytes)
}

/// Convert bytes to a vector of SPIR-V words, replacing NUL bytes with `?`.
///
/// (Using the replacement character or NUL symbol would require changing
/// the length of the string, which would complicate chunking of the
/// program source.)
fn debug_str_bytes_to_words(bytes: &[u8]) -> Vec<Word> {
    let sanitized;
    let bytes = if bytes.contains(&0) {
        sanitized = bytes
            .iter()
            .map(|&b| if b == 0 { b'?' } else { b })
            .collect::<Vec<_>>();
        &sanitized[..]
    } else {
        bytes
    };

    let mut words = bytes_to_words(bytes);
    if bytes.len().is_multiple_of(4) {
        // nul-termination
        words.push(0x0u32);
    }

    words
}
