const std = @import("std");
const testing = std.testing;

export fn print_int(num: i64) callconv(.C) void {
    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    stdout.print("{d}", .{num}) catch {};
    bw.flush() catch {};
}
