; RUN: opt -p memory-ssa-deconstruction %s -S | FileCheck %s

; CHECK-LABEL: i32 @load_store_ret(
; CHECK-SAME: i32
; CHECK-SAME: ptr
; CHECK-NOT: mem
; CHECK-SAME: )
define i32 @load_store_ret(i32 %0, ptr %1, mem %2) {
  ; CHECK: load i32
  ; CHECK: store i32
  ; CHECK: ret i32
  %v = load mem[%2] i32, ptr %1, align 4
  %4 = store mem[%2] i32 %0, ptr %1, align 4
  ret mem[%4] i32 %v
}

; CHECK-LABEL: i32 @calling(
; CHECK-SAME: i32 %[[ARG0:[[:alnum:]]+]]
; CHECK-SAME: ptr %[[ARG1:[[:alnum:]]+]]
; CHECK-NOT: mem
; CHECK-SAME: )
define i32 @calling(i32 %0, ptr %1, mem %2) {
  ; CHECK: %[[R:.*]] = call i32 @calling(i32 %[[ARG0]], ptr %[[ARG1]])
  %4 = call token (mem, ptr, ...) @llvm.mem.call.p0(mem %2, ptr elementtype(i32 (i32, ptr)) @calling, i32 %0, ptr %1)
  %5 = call mem @llvm.mem.call.mem(token %4)
  %6 = call i32 @llvm.mem.call.result.i32(token %4)
  ; CHECK: ret i32 %[[R]]
  ret mem[%5] i32 %6
}
