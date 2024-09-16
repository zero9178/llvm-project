; RUN: opt < %s | opt -S | FileCheck %s
; RUN: verify-uselistorder %s

define void @test(ptr %arg0) {
  ret void
}
