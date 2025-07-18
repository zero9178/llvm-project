//===--- MultiwayPathsCoveredCheck.cpp - clang-tidy------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MultiwayPathsCoveredCheck.h"
#include "clang/AST/ASTContext.h"

#include <limits>

using namespace clang::ast_matchers;

namespace clang::tidy::hicpp {

void MultiwayPathsCoveredCheck::storeOptions(
    ClangTidyOptions::OptionMap &Opts) {
  Options.store(Opts, "WarnOnMissingElse", WarnOnMissingElse);
}

void MultiwayPathsCoveredCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(
      switchStmt(
          hasCondition(expr(
              // Match on switch statements that have either a bit-field or
              // an integer condition. The ordering in 'anyOf()' is
              // important because the last condition is the most general.
              anyOf(ignoringImpCasts(memberExpr(hasDeclaration(
                        fieldDecl(isBitField()).bind("bitfield")))),
                    ignoringImpCasts(declRefExpr().bind("non-enum-condition"))),
              // 'unless()' must be the last match here and must be bound,
              // otherwise the matcher does not work correctly, because it
              // will not explicitly ignore enum conditions.
              unless(ignoringImpCasts(
                  declRefExpr(hasType(hasCanonicalType(enumType())))
                      .bind("enum-condition"))))))
          .bind("switch"),
      this);

  // This option is noisy, therefore matching is configurable.
  if (WarnOnMissingElse) {
    Finder->addMatcher(ifStmt(hasParent(ifStmt()), unless(hasElse(anything())))
                           .bind("else-if"),
                       this);
  }
}

static std::pair<std::size_t, bool> countCaseLabels(const SwitchStmt *Switch) {
  std::size_t CaseCount = 0;
  bool HasDefault = false;

  const SwitchCase *CurrentCase = Switch->getSwitchCaseList();
  while (CurrentCase) {
    ++CaseCount;
    if (isa<DefaultStmt>(CurrentCase))
      HasDefault = true;

    CurrentCase = CurrentCase->getNextSwitchCase();
  }

  return std::make_pair(CaseCount, HasDefault);
}

/// This function calculate 2 ** Bits and returns
/// numeric_limits<std::size_t>::max() if an overflow occurred.
static std::size_t twoPow(std::size_t Bits) {
  return Bits >= std::numeric_limits<std::size_t>::digits
             ? std::numeric_limits<std::size_t>::max()
             : static_cast<size_t>(1) << Bits;
}

/// Get the number of possible values that can be switched on for the type T.
///
/// \return - 0 if bitcount could not be determined
///         - numeric_limits<std::size_t>::max() when overflow appeared due to
///           more than 64 bits type size.
static std::size_t getNumberOfPossibleValues(QualType T,
                                             const ASTContext &Context) {
  // `isBooleanType` must come first because `bool` is an integral type as well
  // and would not return 2 as result.
  if (T->isBooleanType())
    return 2;
  if (T->isIntegralType(Context))
    return twoPow(Context.getTypeSize(T));
  return 1;
}

void MultiwayPathsCoveredCheck::check(const MatchFinder::MatchResult &Result) {
  if (const auto *ElseIfWithoutElse =
          Result.Nodes.getNodeAs<IfStmt>("else-if")) {
    diag(ElseIfWithoutElse->getBeginLoc(),
         "potentially uncovered codepath; add an ending else statement");
    return;
  }
  const auto *Switch = Result.Nodes.getNodeAs<SwitchStmt>("switch");
  std::size_t SwitchCaseCount = 0;
  bool SwitchHasDefault = false;
  std::tie(SwitchCaseCount, SwitchHasDefault) = countCaseLabels(Switch);

  // Checks the sanity of 'switch' statements that actually do define
  // a default branch but might be degenerated by having no or only one case.
  if (SwitchHasDefault) {
    handleSwitchWithDefault(Switch, SwitchCaseCount);
    return;
  }
  // Checks all 'switch' statements that do not define a default label.
  // Here the heavy lifting happens.
  if (!SwitchHasDefault && SwitchCaseCount > 0) {
    handleSwitchWithoutDefault(Switch, SwitchCaseCount, Result);
    return;
  }
  // Warns for degenerated 'switch' statements that neither define a case nor
  // a default label.
  // FIXME: Evaluate, if emitting a fix-it to simplify that statement is
  // reasonable.
  if (!SwitchHasDefault && SwitchCaseCount == 0) {
    diag(Switch->getBeginLoc(),
         "switch statement without labels has no effect");
    return;
  }
  llvm_unreachable("matched a case, that was not explicitly handled");
}

void MultiwayPathsCoveredCheck::handleSwitchWithDefault(
    const SwitchStmt *Switch, std::size_t CaseCount) {
  assert(CaseCount > 0 && "Switch statement with supposedly one default "
                          "branch did not contain any case labels");
  if (CaseCount == 1 || CaseCount == 2)
    diag(Switch->getBeginLoc(),
         CaseCount == 1
             ? "degenerated switch with default label only"
             : "switch could be better written as an if/else statement");
}

void MultiwayPathsCoveredCheck::handleSwitchWithoutDefault(
    const SwitchStmt *Switch, std::size_t CaseCount,
    const MatchFinder::MatchResult &Result) {
  // The matcher only works because some nodes are explicitly matched and
  // bound but ignored. This is necessary to build the excluding logic for
  // enums and 'switch' statements without a 'default' branch.
  assert(!Result.Nodes.getNodeAs<DeclRefExpr>("enum-condition") &&
         "switch over enum is handled by warnings already, explicitly ignoring "
         "them");
  // Determine the number of case labels. Because 'default' is not present
  // and duplicating case labels is not allowed this number represents
  // the number of codepaths. It can be directly compared to 'MaxPathsPossible'
  // to see if some cases are missing.
  // CaseCount == 0 is caught in DegenerateSwitch. Necessary because the
  // matcher used for here does not match on degenerate 'switch'.
  assert(CaseCount > 0 && "Switch statement without any case found. This case "
                          "should be excluded by the matcher and is handled "
                          "separately.");
  std::size_t MaxPathsPossible = [&]() {
    if (const auto *GeneralCondition =
            Result.Nodes.getNodeAs<DeclRefExpr>("non-enum-condition")) {
      return getNumberOfPossibleValues(GeneralCondition->getType(),
                                       *Result.Context);
    }
    if (const auto *BitfieldDecl =
            Result.Nodes.getNodeAs<FieldDecl>("bitfield")) {
      return twoPow(BitfieldDecl->getBitWidthValue());
    }

    return static_cast<std::size_t>(0);
  }();

  // FIXME: Transform the 'switch' into an 'if' for CaseCount == 1.
  if (CaseCount < MaxPathsPossible)
    diag(Switch->getBeginLoc(),
         CaseCount == 1 ? "switch with only one case; use an if statement"
                        : "potential uncovered code path; add a default label");
}
} // namespace clang::tidy::hicpp
