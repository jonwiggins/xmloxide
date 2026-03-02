#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::parser::PushParser;

fuzz_target!(|data: &[u8]| {
    // Feed data in a single chunk
    let mut parser = PushParser::new();
    parser.push(data);
    let _ = parser.finish();

    // Feed data byte-by-byte to stress chunk boundary handling
    if data.len() <= 256 {
        let mut parser2 = PushParser::new();
        for byte in data {
            parser2.push(std::slice::from_ref(byte));
        }
        let _ = parser2.finish();
    }

    // Feed data in small random-ish chunks (use data length for variety)
    if !data.is_empty() {
        let chunk_size = (data.len() % 7) + 1;
        let mut parser3 = PushParser::new();
        for chunk in data.chunks(chunk_size) {
            parser3.push(chunk);
        }
        let _ = parser3.finish();
    }
});
