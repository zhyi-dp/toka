#include "toka/CodeGen.h"
#include "toka/Lexer.h"
#include "toka/Parser.h"
#include "toka/Sema.h"
#include "llvm/Support/raw_ostream.h"
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>

void parseSource(const std::string &filename,
                 std::vector<std::unique_ptr<toka::Module>> &astModules,
                 std::set<std::string> &visited,
                 std::vector<std::string> &recursionStack) {
  // Check recursion stack for circular dependency
  for (const auto &f : recursionStack) {
    if (f == filename) {
      std::cerr << "Circular dependency detected: ";
      for (const auto &s : recursionStack)
        std::cerr << s << " -> ";
      std::cerr << filename << "\n";
      exit(1);
    }
  }

  if (visited.count(filename))
    return;
  visited.insert(filename);
  recursionStack.push_back(filename);

  std::ifstream file(filename);
  if (!file.is_open()) {
    // Check if filename is missing extension and try adding .tk
    if (filename.find(".tk") == std::string::npos) {
      std::string withExt = filename + ".tk";
      file.open(withExt);
    }
  }

  if (!file.is_open()) {
    // Try relative to lib/ or ../lib/
    std::string paths[] = {"lib/", "../lib/"};
    bool found = false;
    for (const auto &p : paths) {
      std::string libPath = p + filename;
      if (filename.find(".tk") == std::string::npos)
        libPath += ".tk";
      file.open(libPath);
      if (file.is_open()) {
        found = true;
        break;
      }
    }
    if (!found) {
      std::cerr << filename << ":0:0: error: could not open file\n";
      return;
    }
  }

  std::stringstream buffer;
  buffer << file.rdbuf();
  std::string code = buffer.str();

  llvm::errs() << "Parsing " << filename << "...\n";

  toka::Lexer lexer(code.c_str());
  auto tokens = lexer.tokenize();

  toka::Parser parser(tokens, filename);
  auto module = parser.parseModule();

  // Recursively parse imports
  for (const auto &imp : module->Imports) {
    if (!imp->Items.empty()) {
      // TODO: Handle logic import symbol filtering if we add per-module symbol
      // tables. For now, we just parse the file to register its globals.
    }
    parseSource(imp->PhysicalPath, astModules, visited, recursionStack);
  }

  astModules.push_back(std::move(module));
  recursionStack.pop_back(); // Pop after finishing processing logic for this
                             // module's imports
}

int main(int argc, char **argv) {
  if (argc < 2) {
    llvm::errs() << "Usage: tokac <filename>\n";
    return 1;
  }

  std::vector<std::unique_ptr<toka::Module>> astModules;
  std::set<std::string> visited;

  std::vector<std::string> recursionStack;
  for (int i = 1; i < argc; ++i) {
    parseSource(argv[i], astModules, visited, recursionStack);
  }

  if (astModules.empty())
    return 1;

  llvm::errs() << "Parse Successful. Running Semantic Analysis...\n";

  toka::Sema sema;
  for (const auto &ast : astModules) {
    if (!sema.checkModule(*ast)) {
      return 1;
    }
  }

  llvm::errs() << "Sema Successful. Merging and Generating IR...\n";

  llvm::LLVMContext context;
  toka::CodeGen codegen(context, argv[1]);

  for (const auto &ast : astModules) {
    codegen.generate(*ast);
  }

  if (codegen.hasErrors()) {
    return 1;
  }

  codegen.print(llvm::outs());

  return 0;
}
