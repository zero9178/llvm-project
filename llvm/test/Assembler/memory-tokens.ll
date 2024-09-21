; RUN: opt -o %t.bc < %s
; RUN: opt %t.bc -S | FileCheck %s
; RUN: verify-uselistorder %s

; CHECK-LABEL: @test(
; CHECK-SAME: %[[PTR:[[:alnum:]]+]]
; CHECK-SAME: %[[MEM:[[:alnum:]]+]]
define void @test(ptr %arg0, mem %arg1) {
  ; CHECK: load mem[%[[MEM]]]
  %a = load mem[%arg1] i32, ptr %arg0
  ; CHECK: ret mem[%[[MEM]]] void
  ret mem[%arg1] void
}

; CHECK-LABEL: @test2(
; CHECK-SAME: %[[PTR:[[:alnum:]]+]]
; CHECK-SAME: %[[MEM:[[:alnum:]]+]]
define ptr @test2(ptr %arg0, mem %arg1) {
  ; CHECK: %[[TOKEN:.*]] = call
  ; CHECK-SAME: @llvm.mem.call.p0
  ; CHECK-SAME: mem %[[MEM]]
  ; CHECK-SAME: %[[PTR]]
  ; CHECK-SAME: %[[PTR]]
  %t = call token (mem, ptr, ...) @llvm.mem.call.p0(mem %arg1, ptr elementtype(void (ptr, mem)) %arg0, ptr %arg0)
  ; CHECK: %[[MEM2:.*]] = call mem @llvm.mem.call.mem(token %[[TOKEN]])
  %mem = call mem @llvm.mem.call.mem(token %t)
  ; CHECK: %[[PTR2:.*]] = call ptr @llvm.mem.call.result.p0(token %[[TOKEN]])
  %ptr = call ptr @llvm.mem.call.result.p0(token %t)
  ; CHECK: ret mem[%[[MEM2]]] ptr %[[PTR2]]
  ret mem[%mem] ptr %ptr
}

declare token @llvm.mem.call.p0(mem, ptr, ...)
declare mem @llvm.mem.call.mem(token)
declare ptr @llvm.mem.call.result.p0(token)
