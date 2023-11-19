#[no_mangle]
pub extern "C" fn iprint(x: usize) {
    print!("{}", x);
}

#[no_mangle]
pub extern "C" fn fprint(x: f64) {
    print!("{}", x);
}

#[no_mangle]
pub extern "C" fn cprint(x: u64) {
    print!("{}", x as u8 as char);
}

#[no_mangle]
pub extern "C" fn bprint(x: bool) {
    if x {
        print!("true");
    } else {
        print!("false");
    }
}
