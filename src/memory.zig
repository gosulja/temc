const std = @import("std");
const windows = std.os.windows;

const dword = windows.DWORD;
const pvoid = windows.PVOID;
const sizet = windows.SIZE_T;

pub const jmem = struct {
    mem: []align(std.mem.page_size) u8,
    size: usize,
    cpos: usize,

    pub const Err = error{
        OutOfMem,
        ProtFailed,
        Unexpected,
        InvalidAddress,
    };

    pub fn init(size: usize) Err!jmem {
        const page_size = std.mem.page_size;
        const aligned = std.mem.alignForward(usize, size, page_size);

        // alloc virtual mem
        const m = windows.VirtualAlloc(null, aligned, windows.MEM_COMMIT | windows.MEM_RESERVE, windows.PAGE_EXECUTE_READWRITE) catch |err| switch (err) {
            error.Unexpected => return error.Unexpected,
        };

        return jmem{
            .mem = @alignCast(@as([*]u8, @ptrCast(m))[0..aligned]),
            .size = aligned,
            .cpos = 0,
        };
    }

    pub fn destroy(self: *jmem) void {
        _ = windows.VirtualFree(self.mem.ptr, 0, windows.MEM_RELEASE);
        self.* = undefined;
    }

    pub fn make_exec(self: *jmem) Err!void {
        var old_prot: dword = undefined;

        _ = windows.VirtualProtect(self.mem.ptr, self.size, windows.PAGE_EXECUTE_READ, &old_prot) catch |err| switch (err) {
            error.InvalidAddress => return error.InvalidAddress,
            error.Unexpected => return error.Unexpected,
        };
    }

    pub fn emit(self: *jmem, bytes: []const u8) Err!void {
        if (self.cpos + bytes.len > self.size) {
            return Err.OutOfMem;
        }

        @memcpy(self.mem[self.cpos..][0..bytes.len], bytes);
        self.cpos += bytes.len;
    }

    pub fn emit_byte(self: *jmem, byte: u8) Err!void {
        if (self.cpos + 1 > self.size) {
            return Err.OutOfMem;
        }

        self.mem[self.cpos] = byte;
        self.cpos += 1;
    }

    pub fn get_mem(self: *jmem) [*]const u8 {
        return self.mem.ptr;
    }
};
