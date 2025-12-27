# Standard Library Implementation Walkthrough

We have successfully bootstrapped the Toka standard library and enhanced the compiler to support it.

## 1. Library Structure
We created a library structure in the `lib/` directory:
- `lib/core/io.tk`: Implements `println` using C's `puts`.

```toka
extern fn puts(^s: char) -> i32;

fn println(^s: char) {
    puts(^s);
    return;
}
```

## 2. Compiler Enhancements
To support the standard library, we implemented several features in the `tokac` compiler:

### A. Recursive Import System
The compiler now supports the `import` keyword. When a file is imported, `tokac` automatically recursively parses the target file from the `lib/` directory.

### B. Extern Functions (FFI)
Support for `extern fn` allows Toka to call C standard library functions (like `puts`). The CodeGen generates appropriate LLVM declarations for these functions.

### C. String Literals & Function Calls
Basic support for `"string literals"` and function calls (with typed arguments) has been implemented in both the Parser and IR CodeGen.

### D. Typed Arguments
Function definitions and external declarations now support basic type annotations (e.g., `s: ^char`), which are mapped to LLVM types (`ptr`).

## 3. Testing
You can now compile a Toka program that uses the library. For example, `test.tk`:

```toka
import "core/io";

fn main() {
    println("Hello from Toka Library!");
    return;
}
```

### Build & Run Instructions
To build and see the IR:
```bash
cd build && make
./src/tokac ../test.tk
```

The resulting LLVM IR will correctly link the `println` call and the `puts` external declaration.
