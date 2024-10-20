; RUN: opt -p memory-ssa-construction %s -S | FileCheck %s

; CHECK-LABEL: @foo(
; CHECK-SAME: i32
; CHECK-SAME: mem %[[MEM:[[:alnum:]]+]]
; CHECK-SAME: )
define void @foo(i32) {
  ; CHECK: ret mem[%[[MEM]]] void
  ret void
}
