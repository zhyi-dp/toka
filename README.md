[中文](README_zh.md)

# Toka Programming Language

Toka is a systems programming language created by YiZhonghua in 2025. It is designed to be **secure**, **efficient**, and **syntactically concise**, aiming to resolve the traditional safety-productivity trade-off through its innovative **Attribute Token System**.

## 🌟 Core Philosophy: Attribute Tokens

Toka eliminates hidden memory states by making properties explicit through orthogonal suffix tokens. This allows you to read the "shape" of memory usage at a glance.

### 🚀 Key Innovations

**1. Syntactic RAII (Native Smart Pointers)**
Toka is one of the few languages that elevates **Smart Pointers to Core Syntax**. This means **"Lifecycle Management" is internalized as the language's breath**, rather than just a library feature.
*   **^T (Unique)** and **~T (Shared)** exist as native grammatical constructs, making RAII the zero-visual-cost default pattern.

**2. Soul-Identity Duality**
Toka philosophically separates an object into its **Soul (Content/Value)** and **Identity (Extension/Address)**.
*   **Precise Semantics**: Access the Soul via Borrow (`&`) and manipulate the Identity via Pointer (`*`).
*   **Ambiguity Resolved**: This effectively solves the semantic ambiguity between "reference assignment" and "content modification." In Toka, moving identity (`move`) and modifying content (`mut`) are distinct grammatical dimensions.

**3. Orthogonal Attribute System**
Say goodbye to the keyword soup of `const volatile unsigned`.
*   **Visual Formula**: Through orthogonal suffixes like `#` (Mutable), `?` (Nullable), and `!` (Both), Toka variable definitions look like clear chemical formulas. `T^#?` is instantly recognizable as a "Mutable, Nullable, Unique Pointer to T."

**4. @encap Explicit Encapsulation**
By default, `shape` members are transparent. Toka uses `@encap` for fine-grained access control.
*   **Permission Convergence**: Inside an `impl Type@encap` block, developers must explicitly list (`pub`) which fields are visible to the outside. Unlisted fields become private automatically.
*   **Lifecycle Binding**: The `@encap` block is also the exclusive place to define critical lifecycle methods (like `drop`), ensuring resource safety.

**5. Contract-Based Control Flow**
**"Everything produces a value; everything is a guarantee."**
*   **Branch Balance**: Toka enforces that control flow expressions (like `match`, `for`) must fulfill their value delivery contract on all paths.
*   **Loop Fallback**: The unique `for-or` syntax guarantees that the receiver gets its promised value even if the loop body is never executed (e.g., empty iteration).

### Token System Table

| Token | Meaning (Value/Content) | Meaning (Identity/Address) |
| :--- | :--- | :--- |
| `#` | **Writable**: Can modify fields | **Swappable**: Can point elsewhere |
| `?` | **Nullable**: Value is `none` | **Nullable**: Identity is `nullptr` |
| `!` | **Writable & Nullable** | **Swappable & Nullable** |
| `^` | - | **Unique Pointer** (Ownership) |
| `~` | - | **Shared Pointer** (Ref Counted) |
| `&` | **Borrow**: View into Value | **Reference**: View into Soul |
| `*` | - | **Raw Pointer** (No Ownership) |

**Example:**
```rust
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
- [x] **Semantic Analysis (Sema)** *(Core Completed)*
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
```rust
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

## Inspirations & Lineage

Toka is a modern systems programming language built on the shoulders of giants. It was born not to reinvent the wheel, but to explore the optimal balance between **high performance** and **developer experience**, building upon the wisdom of its predecessors.

We respectfully acknowledge the following pioneers whose design philosophies have shaped the core identity of Toka:

### 1. C++ (Modern C++)
**Core Inspiration: Smart Pointers & RAII (The Origin)**
Toka's memory management mechanism is a direct descendant of Modern C++ (C++11/14+).
* **Syntactic Smart Pointers**: Toka bakes the semantics of `std::unique_ptr` and `std::shared_ptr` directly into its **core grammar**. By using prefix symbols `^T` (Unique) and `~T` (Shared), developers enjoy full RAII semantics without verbose template code. This makes RAII the default, zero-visual-cost pattern.
* **Low-Level Control**: Toka retains Raw Pointers (`*T`) and precise control over memory layout, adhering to the C++ philosophy of "Zero-overhead abstraction."

### 2. Rust
**Core Inspiration: The Borrowing Discipline**
Toka adopts key insights from Rust regarding memory safety but makes different trade-offs in its implementation path.
* **Borrowing Rules**: We have adopted the strict **"Read-Write Mutex" borrowing rules** (within a scope, allowing either multiple immutable references or a single mutable reference) to prevent data races at compile time.
* **Simplified Mental Model**: Unlike Rust, Toka **deliberately discards explicit lifetime annotations** (e.g., `<'a>`). We prefer managing object lifetimes through smart pointer ownership transfer and lexical scope analysis, significantly lowering the learning curve for systems programming.

### 3. Haskell / ML Family
**Core Inspiration: Typeclasses & Orthogonality**
Toka's Trait system spiritually inherits from **Haskell's Typeclasses** (rather than traditional OOP interfaces).
*   **Separation of Data & Behavior**: We champion the complete decoupling of "Data Definition" (`shape`) from "Behavior Implementation" (`impl`). This design grants Toka immense extensibility—you can even implement custom Traits for standard library types.
*   **Algebraic Data Types (ADTs)**: Toka's `shape` perfectly replicates the ML family's capability for precise data modeling.

### 4. Swift / Kotlin / C#
**Core Inspiration: Null Safety**
To address "The Billion Dollar Mistake," Toka adopts the **Explicit Null Safety** design common in modern application-level languages.
* **Type-Level Defense**: Using the suffix modifier (`?`), null pointer checks are enforced at the type system level rather than through defensive programming at runtime.

### 5. Python
**Core Inspiration: Readability & Productivity**
Toka strives to bring the developer experience of scripting languages to systems programming. We admire Python's **minimalist syntax** and "Less is More" philosophy, aiming to make systems code as readable and approachable as high-level scripts.

---

### Special Acknowledgement

**AI-Assisted Engineering**
The implementation of the Toka compiler—specifically the **Type System Refactoring** and **CodeGen Adaptation**—was completed in deep collaboration with **Google Gemini**.

This "Human Architect + AI Pair Programmer" development model drastically accelerated Toka's evolution from a design concept to an industrial-grade implementation. While all architectural decisions, design philosophies, and core logic were led by the human author, AI provided indispensable assistance in code implementation and test verification.