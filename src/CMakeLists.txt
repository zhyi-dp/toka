add_executable(tokac main.cpp Lexer/Lexer.cpp Parser/Parser.cpp CodeGen/CodeGen.cpp)

# Link against LLVM libraries
llvm_map_components_to_libnames(llvm_libs Support Core)

target_link_libraries(tokac PUBLIC ${llvm_libs})
