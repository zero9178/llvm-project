; RUN: opt -p memory-ssa-construction %s -S | FileCheck %s

; CHECK-LABEL: declare i32 @unaffected(i32)

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

; CHECK-LABEL: define i32 @calling(
; CHECK-SAME: i32 %[[TMP0:.*]], ptr %[[TMP1:.*]], mem %[[TMP2:.*]]) {
define i32 @calling(i32 %arg0, ptr %arg1) {
  ; CHECK: %[[T:.*]] = call token (mem, ptr, ...) @llvm.mem.call.p0(
  ; CHECK-SAME: mem %[[TMP2]]
  ; CHECK-SAME: ptr elementtype(i32 (i32, ptr)) @calling
  ; CHECK-SAME: i32 %[[TMP0]]
  ; CHECK-SAME: ptr %[[TMP1]]
  ; CHECK: %[[MEM:.*]] = call mem @llvm.mem.call.mem(token %[[T]])
  ; CHECK: %[[R:.*]] = call i32 @llvm.mem.call.result.i32(token %[[T]])
  %r = call i32 @calling(i32 %arg0, ptr %arg1)
  ; CHECK: ret mem[%[[MEM]]] i32 %[[R]]
  ret i32 %r
}

declare i32 @unaffected(i32)
