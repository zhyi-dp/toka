[中文](README_zh.md)

# Toka Programming Language

Toka is a systems programming language created by YiZhonghua in 2025. It is designed to be **secure**, **efficient**, and **syntactically concise**, aiming to resolve the traditional safety-productivity trade-off through its innovative **Attribute Token System**.

## 🌟 Core Philosophy: Attribute Tokens

Toka eliminates hidden memory states by making properties explicit through orthogonal suffix tokens. This allows you to read the "shape" of memory usage at a glance.

| Token | Meaning (Value/Content) | Meaning (Identity/Address) |
| :--- | :--- | :--- |
| `#` | **Writable**: Can modify fields | **Swappable**: Can point elsewhere |
| `?` | **Option**: Can be `none` | **Nullable**: Can be `nullptr` |
| `^` | - | **Unique Pointer** (Ownership) |
| `~` | - | **Shared Pointer** (Ref Counted) |

**Example:**
```scala
auto x# = 10        // Mutable Integer
x# = 11            // OK

auto ^p = new Rect  // Unique Pointer to Rect (default init)
auto ^#p2? = ...    // Mutable (Swappable), Nullable, Unique Pointer
```

## ✅ Project Status (Roadmap)

We are actively building the compiler self-hosting capabilities.

- [x] **Compiler Infrastructure**
    - [x] Lexer (Tokenization)
    - [x] Parser (AST Generation)
    - [x] LLVM IR Code Generation
- [x] **Type System**
    - [x] Primitive Types (`i32`, `f64`, `bool`, etc.)
    - [x] Five-State Shapes (Struct, Tuple, Array)
    - [x] **Algebraic Data Types (ADTs)** (via `shape`)
    - [x] Pattern Matching (`match` statement)
- [x] **Memory Management**
    - [x] Unique Pointers (`^`) with Move Semantics
    - [x] Shared Pointers (`~`) with Reference Counting
    - [x] **Recursive Drop (Deep Drop)**
    - [x] **Soul-Identity Model** (Opaque Pointers)
- [x] **Object Oriented Features**
    - [x] `impl` blocks (Methods)
    - [x] **Trait System** (Interfaces, Default Implementations)
- [x] **Control Flow Expressions**
    - [x] Loops (`while`, `for`, `loop`) as Expressions
    - [x] Value yielding via `pass` and `break`
    - [x] `or` Fallback for loops
    - [x] Targeted labels for `break`/`continue`
- [x] **Modules & Visibility**
    - [x] File-based Modules
    - [x] `import` system (Physical & Logical)
    - [x] `pub` visibility modifier
- [ ] **Semantic Analysis (Sema)** *(In Progress)*
    - [x] Infrastructure Scaffolding
    - [x] **Strict Mutability Enforcement** (`#` Check)
    - [x] Type Checking Pass
    - [x] Ownership & Borrowing Verification (Move Semantics)
    - [x] **Null Safety** (`is` Operator, Strict Null Checks)
    - [x] **Resource Safety Analysis** (Enforced `drop` for resources)
- [ ] **Advanced Features**
    - [ ] Generics / Templates
    - [ ] Concurrency (`Task`, `async`/`await`)
    - [ ] **Standard Library**
        - [x] Basic I/O
        - [x] Memory Management
        - [x] `String` Type

## 🛠 Build & Usage

### Prerequisites
- **C++17** compatible compiler (Clang/GCC)
- **CMake** 3.15+
- **LLVM 17** (Libraries and Headers)

### Building the Compiler
```bash
# 1. Create build directory
mkdir -p build && cd build

# 2. Configure with CMake
cmake ..

# 3. Build
make
```

### Running Toka Programs
Currently, `tokac` compiles `.tk` source files into LLVM IR (`.ll`). You can execute them using the LLVM Interpreter (`lli`) or compile them further with `clang`.

**One-liner to compile and run:**
```bash
./build/src/tokac tests/test_trait.tk > output.ll && lli output.ll
```

## 📄 Example

**Traits & ADTs:**
```scala
import std/io::println

trait @Shape {
    fn area(self) -> i32
}

shape Rect (w: i32, h: i32)

impl Rect@Shape {
    fn area(self) -> i32 {
        return self.w * self.h
    }
}

shape State (
    Running |
    Stopped(i32)
)

fn main() {
    auto r = Rect(w = 10, h = 20)
    auto a = r.area()
    
    auto s = State::Stopped(404)
    match s {
        auto Stopped(code) => println("Stopped with {}", code),
        _ => println("Running...")
    }
}

fn null_safety() {
    auto ^?p = nullptr // Identity is Nullable
    if ^?p is ^p {
        println("Not Null!")
    }
    
    auto obj? = none // Value is Nullable (Option)
    if obj? is obj {
        println("Object exists!")
    }
}
```
