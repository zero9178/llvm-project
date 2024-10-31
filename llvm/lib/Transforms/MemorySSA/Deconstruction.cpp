//===- Deconstruction.cpp - Construct MemorySSA form ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/MemorySSA/Deconstruction.h"

#include "llvm/ADT/TypeSwitch.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/ValueMapper.h"

using namespace llvm;

static void deconstructMemSSAForm(Function &F) {
  Value *DummyRepl = std::prev(F.arg_end());

  auto vector = llvm::to_vector(llvm::make_pointer_range(instructions(F)));
  for (Instruction *Inst : llvm::reverse(vector)) {
    TypeSwitch<Instruction *>(Inst)
        .Case<PHINode>([&](PHINode *Phi) {
          if (!Phi->getType()->isMemTy())
            return;
          Phi->replaceAllUsesWith(DummyRepl);
          Phi->eraseFromParent();
        })
        .Case<ReturnInst>([&](ReturnInst *Ret) {
          auto *NewRet =
              ReturnInst::Create(F.getContext(), Ret->getReturnValue(), Ret);
          NewRet->copyMetadata(*Ret);
          Ret->eraseFromParent();
        })
        .Case<LoadInst>(
            [&](LoadInst *Load) { Load->setMemoryOperand(nullptr); })
        .Case<StoreInst>([&](StoreInst *Store) {
          auto *NewStore = new StoreInst(
              Store->getValueOperand(), Store->getPointerOperand(),
              Store->isVolatile(), Store->getAlign());
          NewStore->insertBefore(Store);
          NewStore->copyMetadata(*Store);

          Store->replaceAllUsesWith(DummyRepl);
          Store->eraseFromParent();
        })
        .Case<IntrinsicInst>([&](IntrinsicInst *Intr) {
          if (Intr->getIntrinsicID() != Intrinsic::mem_call)
            return;

          auto *NewCall = CallInst::Create(
              cast<FunctionType>(Intr->getParamElementType(1)),
              Intr->getArgOperand(1),
              llvm::to_vector_of<Value *>(llvm::drop_begin(Intr->args(), 2)),
              "", Intr);
          NewCall->setTailCallKind(Intr->getTailCallKind());
          NewCall->setCallingConv(Intr->getCallingConv());

          AttributeList OldAttributes = Intr->getAttributes();
          AttributeList NewAttributes =
              NewCall->getAttributes().addFnAttributes(
                  F.getContext(),
                  AttrBuilder(F.getContext(), OldAttributes.getFnAttrs()));
          for (unsigned I : llvm::seq<unsigned>(2, Intr->arg_size()))
            NewAttributes = NewAttributes.addParamAttributes(
                F.getContext(), I - 2,
                AttrBuilder(F.getContext(), OldAttributes.getParamAttrs(I)));

          for (User *U : llvm::make_early_inc_range(Intr->users())) {
            auto *SubIntr = dyn_cast<IntrinsicInst>(U);
            if (!SubIntr)
              continue;

            switch (SubIntr->getIntrinsicID()) {
            case Intrinsic::mem_call_result:
              SubIntr->replaceAllUsesWith(NewCall);
              NewCall->takeName(SubIntr);
              NewAttributes = NewAttributes.addRetAttributes(
                  F.getContext(),
                  AttrBuilder(F.getContext(),
                              SubIntr->getAttributes().getRetAttrs()));
              SubIntr->eraseFromParent();
              continue;
            case Intrinsic::mem_call_mem:
              SubIntr->replaceAllUsesWith(DummyRepl);
              SubIntr->eraseFromParent();
              continue;
            default:
              continue;
            }
          }
          NewCall->setAttributes(NewAttributes);

          Intr->eraseFromParent();
        });
  }
}

PreservedAnalyses DeconstructMemorySSA::run(Module &M,
                                            ModuleAnalysisManager &MA) {
  for (Function &F : llvm::make_early_inc_range(M.functions())) {
    if (F.arg_empty())
      continue;
    if (std::prev(F.arg_end())->getType() != Type::getMemoryTy(M.getContext()))
      continue;
    if (F.isIntrinsic())
      continue;

    deconstructMemSSAForm(F);

    auto vector = llvm::to_vector(F.getFunctionType()->params());
    vector.pop_back();
    auto *newFunction =
        Function::Create(FunctionType::get(F.getReturnType(), vector,
                                           F.getFunctionType()->isVarArg()),
                         F.getLinkage(), F.getName(), nullptr);
    ValueToValueMapTy map;
    for (auto [old, newArg] : llvm::zip(F.args(), newFunction->args())) {
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
  }

  return PreservedAnalyses::none();
}
