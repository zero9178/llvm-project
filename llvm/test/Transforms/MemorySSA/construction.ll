; RUN: opt -p memory-ssa-construction %s -S | FileCheck %s

; CHECK-LABEL: define i32 @load_store_ret(
; CHECK-SAME: i32 %[[TMP0:.*]], ptr %[[TMP1:.*]], mem %[[TMP2:.*]]) {
define i32 @load_store_ret(i32 %arg0, ptr %arg1) {
; CHECK: %[[V:.*]] = load mem[%[[TMP2]]] i32, ptr %[[TMP1]], align 4
; CHECK: %[[MEM2:.*]] = store mem[%[[TMP2]]] i32 %[[TMP0]], ptr %[[TMP1]], align 4
; CHECK: ret mem[%[[MEM2]]] i32 %[[V]]
;
  %v = load i32, ptr %arg1
  store i32 %arg0, ptr %arg1
  ret i32 %v
}
