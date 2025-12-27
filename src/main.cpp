#include "toka/CodeGen.h"
#include "toka/Lexer.h"
#include "toka/Parser.h"
#include "llvm/Support/raw_ostream.h"
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>

void parseSource(const std::string &filename,
                 std::vector<std::unique_ptr<toka::Module>> &astModules,
                 std::set<std::string> &visited) {
  if (visited.count(filename))
    return;
  visited.insert(filename);

  std::ifstream file(filename);
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
    parseSource(imp->Path, astModules, visited);
  }

  astModules.push_back(std::move(module));
}

int main(int argc, char **argv) {
  if (argc < 2) {
    llvm::errs() << "Usage: tokac <filename>\n";
    return 1;
  }

  std::vector<std::unique_ptr<toka::Module>> astModules;
  std::set<std::string> visited;

  for (int i = 1; i < argc; ++i) {
    parseSource(argv[i], astModules, visited);
  }

  if (astModules.empty())
    return 1;

  llvm::errs() << "Parse Successful. Merging and Generating IR...\n";

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
