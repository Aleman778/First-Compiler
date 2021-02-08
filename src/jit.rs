const PAGE_SIZE: usize = 4096;

pub struct JitCode {
    pub addr: *mut u8,
    pub size: usize,
}

#[cfg(target_os="windows")]
pub fn allocate_jit_code(size: usize) -> JitCode {
    use winapi::um::winnt;
    use winapi::ctypes;

    let addr: *mut u8;

    unsafe {
        let raw_addr: *mut ctypes::c_void;
        raw_addr = kernel32::VirtualAlloc(
            std::ptr::null_mut(),
            size as u64,
            winnt::MEM_RESERVE | winnt::MEM_COMMIT,
            winnt::PAGE_READWRITE);

        if raw_addr == 0 as *mut ctypes::c_void {
            panic!("error: could not allocate jit code memory");
        }

        addr = std::mem::transmute(raw_addr);
    }

    JitCode {
        addr,
        size
    }
}

#[cfg(target_os="windows")]
pub fn finalize_jit_code(jit: &JitCode) {
    use winapi::um::winnt;
    use winapi::ctypes;
    use winapi::shared::minwindef;

    unsafe {
        let mut old: minwindef::DWORD = 0;
        kernel32::VirtualProtect(
            jit.addr as *mut ctypes::c_void,
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
        kernel32::VirtualFree(jit.addr as *mut ctypes::c_void, 0, winnt::MEM_RELEASE);
    }
}

#[cfg(any(target_os="linux", target_os="macos"))]
pub fn allocate_jit_code(size: usize) -> JitCode {
    use libc;

    let addr: *mut u8;

    unsafe {
        let mut raw_addr: *mut libc::c_void = std::mem::uninitialized();

        libc::posix_memalign(&mut raw_addr, 4096, size);
        libc::mprotect(raw_addr, size, libc::PROT_READ | libc::PROT_WRITE);
        libc::memset(raw_addr, 0x0, size);

        addr = std::mem::transmute(raw_addr);
    }

    JitCode {
        addr,
        size
    }
}

#[cfg(any(target_os="linux", target_os="macos"))]
pub fn finalize_jit_code(jit: &JitCode) {
    use libc;
    
    unsafe {
        libc::mprotect(jit.addr, jit.size, libc::PROT_READ | libc::PROT_EXEC);
    }
}

#[cfg(any(target_os="linux", target_os="macos"))]
pub fn free_jit_code(jit: &JitCode) {
    use libc;
    
    unsafe {
        libc::munmap(jit.addr as *mut _, jit.size);
    }
}

pub fn execute_jit_code(jit: &JitCode) -> i32 {
    unsafe {
        let main_function_ptr: extern "C" fn () -> i32;
        main_function_ptr = std::mem::transmute(jit.addr);
        main_function_ptr() // TODO(alexander): maybe run with arguments in the future!
    }
}
