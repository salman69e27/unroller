CXX = g++

CXXFLAGS = -Wall -Wno-comment $(shell llvm-config --cxxflags)
LDFLAGS = \
		  -lclangTooling \
		  -lclangToolingCore \
		  -lclangFrontendTool \
		  -lclangFrontend \
		  -lclangDriver \
		  -lclangSerialization \
		  -lclangCodeGen \
		  -lclangParse \
		  -lclangSema \
		  -lclangStaticAnalyzerFrontend \
		  -lclangStaticAnalyzerCheckers \
		  -lclangStaticAnalyzerCore \
		  -lclangAnalysis \
		  -lclangARCMigrate \
		  -lclangRewrite \
		  -lclangRewriteFrontend \
		  -lclangEdit \
		  -lclangAST \
		  -lclangASTMatchers \
		  -lclangLex \
		  -lclangBasic \
		  -lclang \
		  $(shell llvm-config --ldflags --libs --system-libs)

all: LoopConvert
LoopConvert: LoopConvert.cpp
	$(CXX) -g -Wall LoopConvert.cpp $(CXXFLAGS) -o bin/LoopConvert $(LDFLAGS)

.PHONY: clean
clean: rm *.o *.exe
