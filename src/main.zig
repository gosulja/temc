const std = @import("std");
const jmem = @import("memory.zig").jmem;
const emitter = @import("emitter.zig").emitter;
const register = @import("emitter.zig").register;

pub fn main() !void {
    var j_mem = try jmem.init(4096);
    defer j_mem.destroy();

    var e = emitter.init(&j_mem);

    // compute: (5 + 3) * 2
    try e.mov_imm64(.rax, 5); // mov rax, 5
    try e.push_reg(.rax); // push rax
    try e.mov_imm64(.rax, 3); // mov rax, 3
    try e.pop_reg(.rcx); // pop rcx
    try e.add_reg_reg(.rax, .rcx); // add rax, rcx
    try e.push_reg(.rax); // push rax
    try e.mov_imm64(.rax, 2); // mov rax, 2
    try e.pop_reg(.rcx); // pop rcx
    try e.mul_reg(.rcx); // mul rcx
    try e.ret(); // ret

    try j_mem.make_exec();

    // call the executable memory
    const TemFN = *const fn () callconv(.C) i64;
    const tfn: TemFN = @ptrCast(j_mem.get_mem());

    const result = tfn();
    std.debug.print("result: {}\n", .{result});
}
