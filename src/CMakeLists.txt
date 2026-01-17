add_executable(tokac main.cpp Lexer/Lexer.cpp 
Parser/Parser.cpp 
Parser/Parser_Decl.cpp
Parser/Parser_Stmt.cpp
Parser/Parser_Expr.cpp 
Sema/Sema.cpp
Sema/Sema_Type.cpp
Sema/Sema_Expr.cpp
Sema/Sema_Stmt.cpp
Type.cpp
CodeGen/CodeGen.cpp 
CodeGen/CodeGen_Expr.cpp 
CodeGen/CodeGen_Stmt.cpp 
CodeGen/CodeGen_Memory.cpp 
CodeGen/CodeGen_Decl.cpp 
)

# Link against LLVM libraries
llvm_map_components_to_libnames(llvm_libs Support Core)

target_link_libraries(tokac PUBLIC ${llvm_libs})
