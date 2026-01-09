add_executable(tokac main.cpp Lexer/Lexer.cpp Parser/Parser.cpp 
Sema/Sema.cpp
#CodeGen/CodeGen.cpp 
CodeGen/src/CodeGen/CodeGen.cpp 
CodeGen/src/CodeGen/CodeGen_Expr.cpp 
CodeGen/src/CodeGen/CodeGen_Stmt.cpp 
CodeGen/src/CodeGen/CodeGen_Memory.cpp 
CodeGen/src/CodeGen/CodeGen_Decl.cpp 
)

# Link against LLVM libraries
llvm_map_components_to_libnames(llvm_libs Support Core)

target_link_libraries(tokac PUBLIC ${llvm_libs})
