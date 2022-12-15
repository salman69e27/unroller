#include <string>
#include <set>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/Core/Replacement.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::driver;
using namespace clang::tooling;

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory UnrollerCategory("unroller options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static llvm::cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static llvm::cl::extrahelp MoreHelp("\nUsage: LoopConvert input_file\n");


static bool areSameVariable(const ValueDecl *First, const ValueDecl *Second) {
  return First && Second &&
         First->getCanonicalDecl() == Second->getCanonicalDecl();
}

std::string stmtToString(const Stmt *stmt){
    clang::LangOptions lo;
	std::string out_str;
    llvm::raw_string_ostream outstream(out_str);
    stmt->printPretty(outstream, NULL, PrintingPolicy(lo));
    return out_str;
}

std::set<const ForStmt*> forStmts;

class IncrementForLoopHandler : public MatchFinder::MatchCallback {
public:
  IncrementForLoopHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
	const VarDecl *InitVar = Result.Nodes.getNodeAs<VarDecl>("initVarName");
	const VarDecl *IncVar = Result.Nodes.getNodeAs<VarDecl>("incVarName");
	const VarDecl *CondVar = Result.Nodes.getNodeAs<VarDecl>("condVarName");
	const VarDecl *Var = Result.Nodes.getNodeAs<VarDecl>("var");

	const Stmt *Ref = Result.Nodes.getNodeAs<Stmt>("ref");
	const Stmt *CondRef = Result.Nodes.getNodeAs<Stmt>("condRef");
	const Stmt *IncRef = Result.Nodes.getNodeAs<Stmt>("incRef");

	const Stmt *IncStmt = Result.Nodes.getNodeAs<Stmt>("incStatement");

	const ForStmt *forStmt = Result.Nodes.getNodeAs<ForStmt>("forLoop");


	//Check if the loop is normalized
	if (!areSameVariable(InitVar, IncVar) || !areSameVariable(IncVar, CondVar) || !areSameVariable(CondVar, Var))
		return;

	//We don't want to modify loop variable references in condition or increment statements
	if (Ref == CondRef || Ref == IncRef)
		return;

	const SourceManager *sm = Result.SourceManager;
	std::string varName = Var->getNameAsString();
	//Add 1 to each reference of the loop variable
	Replacement varRepl = Replacement(*sm, Ref, "("+varName+"+1)");
	varRepl.apply(Rewrite);

	//These changes are only done once per loop
	if (forStmts.find(forStmt) == forStmts.end()) {
		Replacement incRepl = Replacement(*sm, IncStmt, varName + "+= 2");
		incRepl.apply(Rewrite);

		const Stmt *FBody = Result.Nodes.getNodeAs<ForStmt>("forLoop")->getBody();
		//Get the source range and manager.
		SourceRange range = FBody->getSourceRange();

		//Use LLVM's lexer to get source text.
		llvm::StringRef bodyStrRef = Lexer::getSourceText(CharSourceRange::getCharRange(range), *sm, LangOptions());
		std::string bodyStr = bodyStrRef.str();

		if (isa<CompoundStmt>(FBody)){
			bodyStr = bodyStr.substr(1);
			Rewrite.InsertText(FBody->getBeginLoc().getLocWithOffset(1), bodyStr, true, true);
		}
		else {
			Rewrite.InsertText(FBody->getBeginLoc(), "{"+bodyStr+"\n", true, true);
		}

		forStmts.insert(forStmt);
	}
  }

private:
  Rewriter &Rewrite;
  MatchFinder Matcher;
};

// Implementation of the ASTConsumer interface for reading an AST produced
// by the Clang parser. It registers a couple of matchers and runs them on
// the AST.
class UnrollerASTConsumer : public ASTConsumer {
public:
  UnrollerASTConsumer(Rewriter &R) : HandlerForFor(R) {
	//Match variable references of loop variables
    Matcher.addMatcher(
        declRefExpr(to(varDecl(hasType(isInteger())).bind("var")),
			hasAncestor(
				forStmt(hasLoopInit(declStmt(hasSingleDecl(
					varDecl(hasInitializer(integerLiteral(equals(0)))).bind("initVarName")))),
					hasIncrement(unaryOperator(
						hasOperatorName("++"),
						hasUnaryOperand(declRefExpr(to(
							varDecl(hasType(isInteger())).bind("incVarName")
						)).bind("incRef"))
					).bind("incStatement")),
					hasCondition(binaryOperator(
						hasOperatorName("<"),
						hasLHS(ignoringParenImpCasts(declRefExpr(to(
							varDecl(hasType(isInteger())).bind("condVarName")
						)).bind("condRef"))),
						hasRHS(expr(hasType(isInteger())))
					))
				).bind("forLoop")
			)
		).bind("ref"),
        &HandlerForFor
	);
  }

  void HandleTranslationUnit(ASTContext &Context) override {
    // Run the matchers when we have the whole TU parsed.
    Matcher.matchAST(Context);
  }

private:
  IncrementForLoopHandler HandlerForFor;
  MatchFinder Matcher;
};


// For each source file provided to the tool, a new FrontendAction is created.
class UnrollAction : public ASTFrontendAction {
public:
  UnrollAction() {}
  void EndSourceFileAction() override {
    TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID())
        .write(llvm::outs());
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return std::make_unique<UnrollerASTConsumer>(TheRewriter);
  }

private:
  Rewriter TheRewriter;
};

int main(int argc, const char **argv) {
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, UnrollerCategory, llvm::cl::OneOrMore, nullptr);
  if (!ExpectedParser) {
    // Fail gracefully for unsupported options.
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }
  CommonOptionsParser& OptionsParser = ExpectedParser.get();
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  return Tool.run(newFrontendActionFactory<UnrollAction>().get());
}
