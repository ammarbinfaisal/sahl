use std::process::exit;

#[repr(C)]
pub struct ObjList {
    elems: Vec<usize>,
    boxed: bool,
}

impl ObjList {
    fn new() -> ObjList {
        ObjList {
            elems: Vec::new(),
            boxed: false,
        }
    }

    fn with_capacity(cap: usize) -> ObjList {
        ObjList {
            elems: Vec::with_capacity(cap),
            boxed: false,
        }
    }

    fn push(&mut self, elem: usize) {
        self.elems.push(elem);
    }

    fn get(&self, idx: usize) -> usize {
        self.elems[idx]
    }

    fn set(&mut self, idx: usize, elem: usize) {
        self.elems[idx] = elem;
    }

    fn is_boxed(&self) -> bool {
        self.boxed
    }

    fn set_boxed(&mut self) {
        self.boxed = true;
    }
}

#[repr(C)]
pub enum ObjType {
    String,
    List,
}

#[repr(C)]
pub struct ObjStr {
    s: *mut u8,
    len: usize,
}

#[repr(C)]
pub union ObjData {
    s: *mut ObjStr,
    l: *mut ObjList,
}

#[repr(C)]
pub struct Obj {
    ty: ObjType,
    data: ObjData,
}

#[no_mangle]
pub extern "C" fn make(ty: usize, cap: usize) -> *mut Obj {
    let obj = match ty {
        5 => Obj::list_with_capacity(cap),
        _ => {
            println!("make called with invalid type");
            exit(1);
        }
    };
    Box::into_raw(Box::new(obj))
}

impl Obj {
    fn new_string(s: *const i8, len: usize) -> Obj {
        let s = unsafe { std::ffi::CStr::from_ptr(s) };
        Obj {
            ty: ObjType::String,
            data: ObjData {
                s: Box::into_raw(Box::new(ObjStr {
                    s: s.as_ptr() as *mut u8,
                    len,
                })),
            },
        }
    }

    fn new_list() -> Obj {
        Obj {
            ty: ObjType::List,
            data: ObjData {
                l: Box::into_raw(Box::new(ObjList::new())),
            },
        }
    }

    fn list_with_capacity(cap: usize) -> Obj {
        let mut obj = ObjList::with_capacity(cap);
        for _ in 0..cap {
            obj.push(0);
        }
        Obj {
            ty: ObjType::List,
            data: ObjData {
                l: Box::into_raw(Box::new(obj)),
            },
        }
    }
}

#[no_mangle]
pub extern "C" fn iprint(x: usize) {
    print!("{}", x);
}

#[no_mangle]
pub extern "C" fn fprint(x: f64) {
    print!("{}", x);
}

#[no_mangle]
pub extern "C" fn cprint(x: u8) {
    print!("{}", x as char);
}

#[no_mangle]
pub extern "C" fn bprint(x: bool) {
    if x {
        print!("true");
    } else {
        print!("false");
    }
}

#[no_mangle]
pub extern "C" fn sprint(x: *const Obj) {
    let x = unsafe { &*x };
    match x.ty {
        ObjType::String => {
            let s = unsafe { &*x.data.s };
            let s = unsafe { std::slice::from_raw_parts(s.s, s.len) };
            let s = std::str::from_utf8(s).unwrap();
            print!("{}", s);
        }
        ObjType::List => {
            println!("sprint called on list");
            exit(1);
        }
    }
}

#[no_mangle]
pub extern "C" fn make_string(s: *const i8, len: usize) -> *mut Obj {
    let obj = Obj::new_string(s, len);
    Box::into_raw(Box::new(obj))
}

#[no_mangle]
pub extern "C" fn make_list(size: usize) -> *mut Obj {
    let mut list = ObjList::with_capacity(size);
    for _ in 0..size {
        list.push(0);
    }
    let mut obj = Obj::new_list();
    obj.data.l = Box::into_raw(Box::new(list));
    Box::into_raw(Box::new(obj))
}

#[no_mangle]
pub extern "C" fn append(obj: *mut Obj, val: usize) {
    unsafe {
        let list = &mut *obj;
        let list = &mut *list.data.l;
        list.push(val);
    }
}

#[no_mangle]
pub extern "C" fn listset(obj: *mut Obj, index: usize, val: usize) {
    unsafe {
        let list = &mut *obj;
        let list = &mut *list.data.l;
        list.set(index, val);
    }
}

#[no_mangle]
pub extern "C" fn listget(obj: *mut Obj, index: usize) -> usize {
    unsafe {
        let list = &mut *obj;
        let list = &mut *list.data.l;
        list.get(index)
    }
}

#[no_mangle]
pub extern "C" fn len(obj: *mut Obj) -> usize {
    unsafe {
        let list = &mut *obj;
        match list.ty {
            ObjType::String => {
                let s = &*list.data.s;
                s.len
            }
            ObjType::List => {
                let list = &mut *list.data.l;
                list.elems.len()
            }
        }
    }
}
