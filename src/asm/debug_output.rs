use asm::BinaryOutput;
use diagn::{Span, RcReport};
use util::CharCounter;
use FileServer;
use std::collections::HashMap;
use core::borrow::Borrow;
use std::cmp::max;

struct DebugOutputItem {
    index: usize,
    width: usize,
    span: Span,
}

pub struct DebugOutput {
    items: Vec<DebugOutputItem>,
}

impl DebugOutput {
    pub fn new() -> DebugOutput {
        DebugOutput { items: Vec::new() }
    }

    pub fn add(&mut self, index: usize, width: usize, span: Span) {
        self.items.push(DebugOutputItem { index, width, span, })
    }

    pub fn generate_str(&mut self, digit_bits: usize, grouping: usize, bits: &BinaryOutput, fileserver: &FileServer) -> String {
        let _flame_guard = flame::start_guard("generate debug string");
        let mut result = String::new();
        let mut files: HashMap<String, CharCounter> = HashMap::new();

        self.items.sort_by_key(|it| it.index);
        let mut display: Vec<(String, String)> = Vec::new();

        let mut hex_width = 0;
        let mut excerpt_width = 0;
        let mut line_num_width = 0;
        for item in &mut self.items {
            let mut hex = String::new();
            let mut index = item.index;
            let mut i = 0;
            while index < item.index + item.width {
                let mut digit: u8 = 0;

                for _ in 0..digit_bits {
                    digit <<= 1;
                    digit |= if bits.read(index) { 1 } else { 0 };
                    index += 1;
                }
                let c = if digit < 10 {
                    ('0' as u8 + digit) as char
                } else {
                    ('a' as u8 + digit - 10) as char
                };

                hex.push(c);
                i += 1;
                if i % grouping == 0 {
                    hex.push(' ');
                }
            }
            hex_width = max(hex_width, hex.len());

            if !files.contains_key((*item.span.file).as_str()) {
                files.insert((*item.span.file).clone(), CharCounter::new(
                    fileserver
                        .get_chars(RcReport::new(), &*item.span.file.clone(), None)
                        .ok()
                        .unwrap()
                ));
            }

            let counter = &files[(*item.span.file).as_str()];
            let mut excerpt = String::new();
            if let Some(location) = item.span.location {
                let (line, col) = counter.get_line_column_at_index(location.0);
                line_num_width = max(line_num_width, format!("{}", line).len());
                let (line_start, line_end) = counter.get_index_range_of_line(line);

                let excerpt_array = counter.get_excerpt(line_start, line_end);
                excerpt.reserve(excerpt_array.len());
                for &c in excerpt_array {
                    if c == '\n' { continue; }
                    excerpt.push(c);
                }

            };
            excerpt_width = max(excerpt_width, excerpt.len());
            display.push((hex, excerpt));
        }

        let address_chars = format!("{:X}", bits.len()).len();
        let mut i = 0;
        for i in 0 .. self.items.len() {
            let item = &self.items[i];
            let display_strings = &display[i];

            result = format!(
                "{0}0x{1:02$X}: {3:<4$}",
                result, item.index, address_chars,
                display_strings.0, hex_width
            );

            let counter = &files[(*item.span.file).as_str()];

            match item.span.location {
                None => {
                    result = format!("{} | ?\n", result)
                },
                Some(location) => {
                    let (line1, col1) = counter.get_line_column_at_index(location.0);
                    let (line2, col2) = counter.get_line_column_at_index(location.1);
                    let filepath = &*item.span.file.clone();
                    // https://codereview.stackexchange.com/questions/98536/extracting-the-last-component-basename-of-a-filesystem-path
                    let mut pieces = filepath.rsplit("/");
                    let filename = match pieces.next() {
                        None => filepath.clone(),
                        Some(f) => f.into()
                    };

                    result = format!(
                        "{} | {filename}:{line1:<line_num_width$} - {excerpt:<excerpt_width$} | {filepath} {line1}:{col1} - {line2}:{col2}\n",
                        result,
                        filename = filename,
                        filepath = *item.span.file,
                        line_num_width = line_num_width,
                        excerpt = display_strings.1,
                        excerpt_width = excerpt_width,
                        line1 = line1 + 1,
                        col1 = col1 + 1,
                        line2 = line2 + 1,
                        col2 = col2 + 1
                    );
                }
            }
        }

        result
    }
}
