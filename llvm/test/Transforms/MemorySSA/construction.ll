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

; CHECK-LABEL: define fastcc i32 @calling(
; CHECK-SAME: i32 %[[TMP0:.*]], ptr %[[TMP1:.*]], mem %[[TMP2:.*]]) #[[CALLING_ATTRS:[[:alnum:]]+]] {
define fastcc i32 @calling(i32 %arg0, ptr %arg1) cold {
  ; CHECK: %[[T:.*]] = tail call fastcc token (mem, ptr, ...) @llvm.mem.call.p0(
  ; CHECK-SAME: mem %[[TMP2]]
  ; CHECK-SAME: ptr elementtype(i32 (i32, ptr)) @calling
  ; CHECK-SAME: i32 %[[TMP0]]
  ; CHECK-SAME: ptr readonly %[[TMP1]]
  ; CHECK-SAME: ) #[[CALLING_ATTRS]]
  ; CHECK: %[[MEM:.*]] = call mem @llvm.mem.call.mem(token %[[T]])
  ; CHECK: %[[R:.*]] = call i32 @llvm.mem.call.result.i32(token %[[T]])
  %r = tail call fastcc i32 @calling(i32 %arg0, ptr readonly %arg1) cold
  ; CHECK: ret mem[%[[MEM]]] i32 %[[R]]
  ret i32 %r
}

; CHECK-LABEL: declare i32 @unaffected(i32)
declare i32 @unaffected(i32)

; CHECK: attributes #[[CALLING_ATTRS]] =
; CHECK-SAME: cold
