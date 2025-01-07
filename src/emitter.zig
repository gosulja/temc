const std = @import("std");
const jmem = @import("memory.zig").jmem;

pub const register = enum(u8) {
    rax = 0,
    rcx = 1,
    rdx = 2,
    rbx = 3,
    rsp = 4,
    rbp = 5,
    rsi = 6,
    rdi = 7,
    r8 = 8,
    r9 = 9,
    r10 = 10,
    r11 = 11,
    r12 = 12,
    r13 = 13,
    r14 = 14,
    r15 = 15,
};

pub const emitter = struct {
    mem: *jmem,

    pub const Err = jmem.Err;

    pub fn init(mem: *jmem) emitter {
        return .{ .mem = mem };
    }

    // rex prefix
    fn emit_rexw(self: *emitter) Err!void {
        try self.mem.emit_byte(0x48);
    }

    // mov immediate to reg
    pub fn mov_imm64(self: *emitter, reg: register, val: i64) Err!void {
        try self.emit_rexw();
        try self.mem.emit_byte(0xB8 + @intFromEnum(reg));
        try self.mem.emit(&std.mem.toBytes(val));
    }

    // reg -> reg mov
    pub fn mov_reg_reg(self: *emitter, dest: register, src: register) Err!void {
        try self.emit_rexw();
        try self.mem.emit_byte(0x89);
        try self.mem.emit_byte(0xC0 | (@intFromEnum(src) << 3) | @intFromEnum(dest));
    }

    // add reg to reg
    pub fn add_reg_reg(self: *emitter, dest: register, src: register) Err!void {
        try self.emit_rexw();
        try self.mem.emit_byte(0x01);
        try self.mem.emit_byte(0xC0 | (@intFromEnum(src) << 3) | @intFromEnum(dest));
    }

    // sub reg to reg
    pub fn sub_reg_reg(self: *emitter, dest: register, src: register) Err!void {
        try self.emit_rexw();
        try self.mem.emit_byte(0x29);
        try self.mem.emit_byte(0xC0 | (@intFromEnum(src) << 3) | @intFromEnum(dest));
    }

    // mul reg to reg
    pub fn mul_reg(self: *emitter, reg: register) Err!void {
        try self.emit_rexw();
        try self.mem.emit_byte(0xF7);
        try self.mem.emit_byte(0xE0 | @intFromEnum(reg));
    }

    pub fn push_reg(self: *emitter, reg: register) Err!void {
        try self.mem.emit_byte(0x50 | @intFromEnum(reg));
    }

    pub fn pop_reg(self: *emitter, reg: register) Err!void {
        try self.mem.emit_byte(0x58 | @intFromEnum(reg));
    }

    pub fn ret(self: *emitter) Err!void {
        try self.mem.emit_byte(0xC3);
    }
};
