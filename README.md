[中文](README_zh.md)

# Toka Programming Language

Toka is a systems programming language created by YiZhonghua in 2025. It is designed to be **secure**, **efficient**, and **syntactically concise**, aiming to resolve the traditional safety-productivity trade-off through its innovative **Attribute Token System**.

## 🌟 Core Philosophy: Attribute Tokens

Toka eliminates hidden memory states by making properties explicit through orthogonal suffix tokens. This allows you to read the "shape" of memory usage at a glance.

| Token | Meaning (Variable) | Meaning (Pointer) |
| :--- | :--- | :--- |
| `#` | **Writable** (Mutable) | **Swappable** (Re-seatable) |
| `?` | **Nullable** | **Nullable Reference** |
| `^` | - | **Unique Pointer** (Ownership) |
| `~` | - | **Shared Pointer** (Ref Counted) |

**Example:**
```scala
let x# = 10;        // Mutable Integer
x# = 11;            // OK

let ^p = new Rect;  // Unique Pointer to Rect
let ^#p2? = ...;    // Mutable (Swappable), Nullable, Unique Pointer
```

## ✅ Project Status (Roadmap)

We are actively building the compiler self-hosting capabilities.

- [x] **Compiler Infrastructure**
    - [x] Lexer (Tokenization)
    - [x] Parser (AST Generation)
    - [x] LLVM IR Code Generation
- [x] **Type System**
    - [x] Primitive Types (`i32`, `f64`, `bool`, etc.)
    - [x] Structs & Member Access
    - [x] **Algebraic Data Types (ADTs)** (`option`, `enum`)
    - [x] Pattern Matching (`match` statement)
- [x] **Memory Management**
    - [x] Unique Pointers (`^`) with Move Semantics
    - [x] Shared Pointers (`~`) with Reference Counting
- [x] **Object Oriented Features**
    - [x] `impl` blocks (Methods)
    - [x] **Trait System** (Interfaces, Default Implementations)
- [ ] **Semantic Analysis (Sema)** *(In Progress)*
    - [x] Infrastructure Scaffolding
    - [ ] **Strict Mutability Enforcement** (`#` Check)
    - [ ] Type Checking Pass
    - [ ] Ownership & Borrowing Verification
- [ ] **Advanced Features**
    - [ ] Generics / Templates
    - [ ] Concurrency (`Task`, `async`/`await`)
    - [ ] Standard Library

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
trait @Shape {
    fn area(self) -> i32;
}

struct Rect { w: i32, h: i32 }

impl Rect@Shape {
    fn area(self) -> i32 {
        return self.w * self.h;
    }
}

option State {
    Running = (),
    Stopped = (i32)
}

fn main() {
    let r = Rect { w = 10, h = 20 };
    let a = r.area();
    
    let s = State::Stopped(404);
    match s {
        Stopped(code) => printf("Stopped with %d\n", code),
        _ => printf("Running...\n")
    }
}
```
