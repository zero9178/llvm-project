//===- ControlFlowToLLVM.h - ControlFlow to LLVM -----------*- C++ ------*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Define conversions from the ControlFlow dialect to the LLVM IR dialect.
//
//===----------------------------------------------------------------------===//

#ifndef MLIR_CONVERSION_CONTROLFLOWTOLLVM_CONTROLFLOWTOLLVM_H
#define MLIR_CONVERSION_CONTROLFLOWTOLLVM_CONTROLFLOWTOLLVM_H

#include <memory>

namespace mlir {
class DialectRegistry;
class LLVMTypeConverter;
class RewritePatternSet;
class Pass;
class SymbolTableCollection;

#define GEN_PASS_DECL_CONVERTCONTROLFLOWTOLLVMPASS
#include "mlir/Conversion/Passes.h.inc"

namespace cf {

/// Collect the patterns to convert from the ControlFlow dialect to LLVM. The
/// conversion patterns capture the LLVMTypeConverter by reference meaning the
/// references have to remain alive during the entire pattern lifetime.
///
/// Note: This function does not populate the default cf.assert lowering. That
/// is because some platforms have a custom cf.assert lowering. The default
/// lowering can be populated with `populateAssertToLLVMConversionPattern`.
void populateControlFlowToLLVMConversionPatterns(
    const LLVMTypeConverter &converter, RewritePatternSet &patterns);

/// Populate the cf.assert to LLVM conversion pattern. If `abortOnFailure` is
/// set to false, the program execution continues when a condition is
/// unsatisfied.
void populateAssertToLLVMConversionPattern(
    const LLVMTypeConverter &converter, RewritePatternSet &patterns,
    bool abortOnFailure = true, SymbolTableCollection *symbolTables = nullptr);

void registerConvertControlFlowToLLVMInterface(DialectRegistry &registry);

} // namespace cf
} // namespace mlir

#endif // MLIR_CONVERSION_CONTROLFLOWTOLLVM_CONTROLFLOWTOLLVM_H
