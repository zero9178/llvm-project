//===- Construction.cpp - Construct MemorySSA form ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/MemorySSA/Construction.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Analysis/IteratedDominanceFrontier.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/ValueMapper.h"

using namespace llvm;

static void buildMemSSAForm(Function &F, FunctionAnalysisManager &FM) {
  SmallPtrSet<BasicBlock *, 4> definingBlocks;
  definingBlocks.insert(&F.getEntryBlock());
  for (Instruction &inst : instructions(F)) {
    auto *readInst = dyn_cast<MemorySSAReadInstruction>(&inst);
    if (!readInst)
      continue;

    auto *readWriteInst = dyn_cast<MemorySSAReadWriteInstruction>(&inst);
    if (!readWriteInst)
      continue;

    definingBlocks.insert(readWriteInst->getParent());
  }

  auto range = llvm::make_pointer_range(F);
  SmallPtrSet<BasicBlock *, 4> liveInBlocks(range.begin(), range.end());
  auto &DT = FM.getResult<DominatorTreeAnalysis>(F);

  ForwardIDFCalculator calculator(DT);
  calculator.setDefiningBlocks(definingBlocks);
  calculator.setDefiningBlocks(liveInBlocks);

  SmallVector<BasicBlock *> mergePoints;
  calculator.calculate(mergePoints);
  SmallDenseMap<BasicBlock *, llvm::Value *> definition;
  definition[&F.getEntryBlock()] = F.getArg(F.arg_size() - 1);
  for (BasicBlock *Block : mergePoints) {
    auto *phi = PHINode::Create(Type::getMemoryTy(F.getContext()), 0, "",
                                Block->begin());
    definition[Block] = phi;
  }

  for (BasicBlock *Block : llvm::depth_first(&F)) {
    Value *currentDef = definition[Block];
    assert(currentDef && "must have a definition");

    for (Instruction &inst : llvm::make_early_inc_range(*Block)) {
      switch (inst.getOpcode()) {
      case Instruction::Store: {
        auto *oldStore = cast<StoreInst>(&inst);
        auto *newStore = new StoreInst(
            oldStore->getValueOperand(), oldStore->getPointerOperand(),
            currentDef, oldStore->isVolatile(), oldStore->getAlign());
        newStore->insertBefore(oldStore);
        newStore->copyMetadata(*oldStore);
        oldStore->eraseFromParent();
        currentDef = newStore;
        continue;
      }
      case Instruction::Load: {
        auto *loadInst = cast<LoadInst>(&inst);
        loadInst->setMemoryOperand(currentDef);
        continue;
      }
      case Instruction::Ret: {
        auto *oldRet = cast<ReturnInst>(&inst);
        auto *newRet = ReturnInst::Create(
            F.getContext(), oldRet->getReturnValue(), oldRet, currentDef);
        newRet->copyMetadata(*oldRet);
        oldRet->eraseFromParent();
        continue;
      }
      case Instruction::Call: {
        auto *oldCall = cast<CallInst>(&inst);
        if (oldCall->getIntrinsicID() != Intrinsic::not_intrinsic)
          continue;

        // TODO: Copy many of the attributes, metadata etc.
        SmallVector<Value *> args{currentDef, oldCall->getCalledOperand()};
        llvm::append_range(args, oldCall->args());
        auto *token =
            CallInst::Create(Intrinsic::getDeclaration(
                                 F.getParent(), Intrinsic::mem_call,
                                 {
                                     oldCall->getCalledOperand()->getType(),
                                 }),
                             args, "", oldCall);
        token->addParamAttr(1, Attribute::get(oldCall->getContext(),
                                              Attribute::ElementType,
                                              oldCall->getFunctionType()));
        token->setTailCallKind(oldCall->getTailCallKind());
        token->setCallingConv(oldCall->getCallingConv());

        AttributeList OldAttributes = oldCall->getAttributes();
        AttributeList NewAttributes = token->getAttributes().addFnAttributes(
            F.getContext(),
            AttrBuilder(F.getContext(), OldAttributes.getFnAttrs()));
        for (unsigned I : llvm::seq(oldCall->arg_size()))
          NewAttributes = NewAttributes.addParamAttributes(
              F.getContext(), 2 + I,
              AttrBuilder(F.getContext(), OldAttributes.getParamAttrs(I)));
        token->setAttributes(NewAttributes);

        currentDef = CallInst::Create(
            Intrinsic::getDeclaration(F.getParent(), Intrinsic::mem_call_mem),
            token, "", oldCall);

        if (!oldCall->getType()->isVoidTy()) {
          auto *NewCall =
              CallInst::Create(Intrinsic::getDeclaration(
                                   F.getParent(), Intrinsic::mem_call_result,
                                   oldCall->getType()),
                               token, "", oldCall);
          oldCall->replaceAllUsesWith(NewCall);
          NewCall->takeName(oldCall);
        }

        oldCall->eraseFromParent();

        continue;
      }
      default:
        continue;
      }
    }

    //
    for (BasicBlock *Succ : successors(Block))
      definition.insert({Succ, currentDef});
  }
}

PreservedAnalyses ConstructMemorySSA::run(Module &M,
                                          ModuleAnalysisManager &MA) {
  bool changed = false;
  for (Function &F : llvm::make_early_inc_range(M.functions())) {
    if (!F.arg_empty() &&
        std::prev(F.arg_end())->getType() == Type::getMemoryTy(F.getContext()))
      continue;
    if (F.isDeclaration())
      continue;

    changed = true;

    auto vector = llvm::to_vector(F.getFunctionType()->params());
    vector.push_back(Type::getMemoryTy(F.getContext()));
    auto *newFunction =
        Function::Create(FunctionType::get(F.getReturnType(), vector,
                                           F.getFunctionType()->isVarArg()),
                         F.getLinkage(), F.getName(), nullptr);
    ValueToValueMapTy map;
    for (auto [old, newArg] : llvm::zip_first(F.args(), newFunction->args())) {
      map[&old] = &newArg;
      newArg.takeName(&old);
    }

    SmallVector<ReturnInst *, 8> Returns; // Ignore returns cloned.
    CloneFunctionInto(newFunction, &F, map,
                      CloneFunctionChangeType::LocalChangesOnly, Returns, "",
                      nullptr);

    newFunction->takeName(&F);
    F.replaceAllUsesWith(newFunction);
    F.getParent()->getFunctionList().insert(F.getIterator(), newFunction);
    F.eraseFromParent();

    buildMemSSAForm(
        *newFunction,
        MA.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager());
  }
  if (!changed)
    return PreservedAnalyses::all();

  return PreservedAnalyses::none();
}
