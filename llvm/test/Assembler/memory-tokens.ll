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
  ; CHECK: ret mem[%[[MEM]]] ptr %[[PTR]]
  ret mem[%arg1] ptr %arg0
}
