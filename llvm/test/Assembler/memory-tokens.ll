; RUN: opt < %s | opt -S | FileCheck %s
; RUN: verify-uselistorder %s

; CHECK: @test(
define void @test(ptr %arg0, mem %arg1) {
  ret void
}
