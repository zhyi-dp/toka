add_executable(tokac main.cpp Lexer/Lexer.cpp Parser/Parser.cpp 
Sema/Sema.cpp
#CodeGen/CodeGen.cpp  //rename to CodeGen.cpp.single.bak
CodeGen/CodeGen.cpp 
CodeGen/CodeGen_Expr.cpp 
CodeGen/CodeGen_Stmt.cpp 
CodeGen/CodeGen_Memory.cpp 
CodeGen/CodeGen_Decl.cpp 
)

# Link against LLVM libraries
llvm_map_components_to_libnames(llvm_libs Support Core)

target_link_libraries(tokac PUBLIC ${llvm_libs})
