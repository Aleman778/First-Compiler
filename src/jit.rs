const PAGE_SIZE: usize = 4096;

pub struct JitCode {
    pub code: *mut u8,
    pub size: usize,
}

#[cfg(target_os="windows")]
pub fn allocate_jit_code(size: usize) -> JitCode {
    use winapi::um::winnt;
    use winapi::ctypes;

    let code: *mut u8;

    unsafe {
        let raw_data: *mut ctypes::c_void;
        raw_data = kernel32::VirtualAlloc(
            std::ptr::null_mut(),
            size as u64,
            winnt::MEM_RESERVE | winnt::MEM_COMMIT,
            winnt::PAGE_READWRITE);

        if raw_data == 0 as *mut ctypes::c_void {
            panic!("error: could not allocate jit code memory");
        }

        code = std::mem::transmute(raw_data);
    };
    
    JitCode {
        code,
        size
    }
}

// #[cfg(any(target_os="linux", target_os="macos"))]
// pub fn allocate_jit_code(size: usize) -> JitCode {
        
//     JitCode {
//         code,
//         size
//     }
// }

#[cfg(target_os="windows")]
pub fn finalize_jit_code(jit: &JitCode) {
    use winapi::um::winnt;
    use winapi::ctypes;
    use winapi::shared::minwindef;

    unsafe {
        let mut old: minwindef::DWORD = 0;
        kernel32::VirtualProtect(
            jit.code as *mut ctypes::c_void,
            jit.size as u64,
            winnt::PAGE_EXECUTE_READ,
            &mut old as minwindef::PDWORD);
    }
}

#[cfg(target_os="windows")]
pub fn free_jit_code(jit: &JitCode) {
    use winapi::um::winnt;
    use winapi::ctypes;
    
    unsafe {
        kernel32::VirtualFree(jit.code as *mut ctypes::c_void, 0, winnt::MEM_RELEASE);
    }
}

pub fn execute_jit_code(jit: &JitCode) -> i32 {
    unsafe {
        let main_function_ptr: extern "C" fn () -> i32;
        main_function_ptr = std::mem::transmute(jit.code);
        main_function_ptr() // TODO(alexander): maybe run with arguments in the future!
    }
}
