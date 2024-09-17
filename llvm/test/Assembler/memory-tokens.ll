; RUN: opt -o %t.bc < %s
; RUN: opt %t.bc -S | FileCheck %s
; RUN: verify-uselistorder %s

; CHECK-LABEL: @test(
; CHECK-SAME: %[[PTR:[[:alnum:]]+]]
; CHECK-SAME: %[[MEM:[[:alnum:]]+]]
define void @test(ptr %arg0, mem %arg1) {
  ; CHECK: load mem[%[[MEM]]]
  %a = load mem[%arg1] i32, ptr %arg0
  ret void
}
