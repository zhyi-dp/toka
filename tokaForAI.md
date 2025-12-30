# Toka Language Specification for AI Developer

This document formalizes the Toka programming language specification based on the design draft. It aims to serve as the reference manual for implementing the Toka compiler.

## 1. Design Philosophy
- **Explicit Mutability & Nullability**: Controlled via orthogonal "Attribute Tokens" (`#`, `?`, `!`) rather than separate keywords.
- **Ownership & Borrowing**: compile-time memory safety similar to Rust but with a unique syntax.
- **Concurrency**: Built-in `Task` and `Channel` primitives with explicit lock types for shared state.

## 2. Lexical Structure

### 2.1 Keywords (Total: < 50)
Compiler parser should treat these as reserved.

**Declaration & Types**
- `let`: Variable declaration start.
- `type`: Type alias.
- `const`: Constant declaration.
- `struct`: Structure definition.
- `trait`: Interface definition.
- `impl`: Implementation block.
- `fn`: Function/Method declaration.
- `new`: Heap allocation (`let ^p = new Person{...}`).
- `dyn`: Dynamic dispatch marker for Traits.
- `move`: Explicit ownership transfer.
- `where`: Type constraints.
- `default`: Use default trait implementation.
- `delete`: Explicitly remove a trait method (contract violation, note implementation warning).
- `final`: Seal implementation or method.
- `Fn`: Synchronous function type.

**Control Flow**
- `if`, `else`, `match`, `case`
- `for`, `while`, `break`, `continue`
- `return`, `yield`

**Concurrency**
- `Task`, `suspend`, `async`, `await`, `cancel`
- `Channel` (Library type, but core concept)

**Modules & Visibility**
- `import`: Import modules.
- `pub`: Public visibility modifier.

**Type & Logic**
- `as` (Cast), `is` (Type check), `in` (Membership/Iteration)
- `self`, `Self`
- `true`, `false`, `none`, `null`
- `defer`: Lazy initialization.
- `main`: Entry point.

### 2.2 Symbols & Operators
- `()`: Grouping, Arguments, Tuples.
- `[]`: Arrays only.
- `{}`: Blocks, Scopes, Object initialization.
- `^`: Pointer prefix (e.g., `^Person`).
- `#`: **Write Token** (Writable / Swappable).
- `?`: **Null Token** (Nullable).
- `!`: **Write + Null Token**.
- `$`: **None Token** (Immutable & Non-null), usually omitted.
- **Literal Defaults**: 
    - Integer literals without suffix: `i32`.
    - Floating point literals without suffix: `f64`.
- `<-`: Dependency / Channel receive (Context dependent).
- `&`: **Reference prefix** (e.g., `let &r = &x#`).
- `++` / `--`: **Increment/Decrement** (Prefix and Postfix).
- `.`: Access.
- `:`: Type annotation.

## 3. Type System & Attribute Tokens

### 3.1 The Attribute Token System (Core Feature)
Toka separates **storage binding** (`let`) from **memory properties** (Tokens).

| Token | Meaning on Object | Meaning on Pointer (`^`) |
| :--- | :--- | :--- |
| `#` | **Writable**: Can modify fields. | **Swappable**: Can change address stored. |
| `?` | **Nullable**: Can be `none`. | **Nullable**: Can be `null`. |
| `!` | **Writable + Nullable**. | **Swappable + Nullable**. |
| `$` | **Immutable + Non-null**. | **Fixed + Non-null**. |

**Syntax Rules:**
- Tokens are suffixes to the variable name (e.g., `let x# = ...`).
- For pointers, tokens can attach to the pointer symbol `^` AND the variable name, creating 16 combinations (e.g., `let ^#p?`).
    - `^#`: Pointer is swappable (can point elsewhere).
    - `p?`: Object pointed to is nullable (can be `none`?). *Correction*: `?` usually implies the wrapper is nullable.
    - *Clarification from doc*: `let ^?ptr#` = Nullable non-swappable pointer, pointing to a Writable non-nullable object.

### 3.2 Basic Types
- `i8`..`i64`, `u8`..`u64`, `f32`, `f64`
- `bool`, `char`, `str` (String)
- `void` (Unit)

### 3.3 Composite Types
- **Tuple**: `(i32, str)`
- **Array**: `[i32; 10]`
- **Struct**:
  ```scala
  struct Person {
      name: str
      age: i32
  }
  ```
  ```
- **Trait**:
  ```scala
  trait Run {
      fn run(self) -> void
  }
  ```
- **Algebraic Data Types (ADTs)**:
    - **Syntax**:
      ```scala
      option Maybe<T, V> {
          Some = (T, V),        // Named tuple variant
          None = (),            // Unit variant
          Data = (value: T, count: V), // Named fields tuple variant
          OTHER = (),           // Pure tag (Enum-like)
          type Inner = (T)      // Type alias (internal helper)
      }
      ```
    - **Usage**:
      - `let opt = Maybe::Some(42, "Hello")`
      - `use Maybe::*` allows `Some(42, "Hello")`
    - **Pattern Matching (`match`)**:
      ```scala
      match opt {
          // Destructuring binding
          let Some(st, sv) => { ... }
          
          // Guard clause
          let Some(st, sv) if sv > 10 => { ... }
          
          // Named field destructuring with partial match
          let Data{ st = .count, .. } => { ... }
          
          // Shorthand for named fields
          Data{ .value, .count } => { ... }
          
          // Unit/Tag match
          None => { ... }
          
          // No destructuring (access raw fields via dot)
          Data as d => { print(d.value) }
          
          // Default
          _ => { ... }
      }
      ```
    - **Control Flow Sugar**:
      - `if let Some(x, y) = opt { ... }`
      - `if opt is Some { print(opt.0) }`

### 3.4 Traits & Implementation
- **Definition**: Trait names are prefixed with `@` in definitions and usage.
  ```scala
  trait @Shape {
      fn area(self) -> i32
  }
  ```
- **Implementation**:
  - **Inherent Methods**: `impl Type { ... }`
  - **Trait Implementation**: `impl Type@Trait { ... }`
  - **Multiple Traits**: `impl Type@{Trait1, Trait2} { ... }` (Planned)
  - **Default/Delete**: Methods can be marked `= default` or `= delete` (Planned).

## 4. Memory Model

### 4.1 Ownership & Move Semantics
- **Copy vs Move**:
    - By default, assignment implies **copy**.
    - If a type is `[#no_bit_copy]`, copy is via `clone`.
    - Variable at last usage (R-value): `move`.
    - Variable as L-value ending scope: `drop`.
- **Optimization & Capture Passing**:
    - **Capture Passing (In-place Capture)**: Function arguments that are **Structs**, **Arrays**, **Tuples**, or marked as **Mutable (`#`)** are passed by **Reference** (pointer) by default.
    - This allows in-place modification within functions without explicit `&` at the call site if the parameter is mutable.
    - Behavior is similar to Java objects: no binary copy for complex types or mutable bindings during call.
    - Immutable primitives are passed by value (copy).

### 4.2 Explicit Mutation Requirement
- To modify any value, the **Write Token (`#`)** is MANDATORY at usage site.
  - `vec#.push(item)`
  - `x# = x + 1`
- This makes mutation explicit and searchable.

### 4.3 Pointers & References
- **Pointer (`^T`)**: Comparison with C++:
    - `^T` ≈ `T* const` (Fixed pointer, immutable data)
    - `^#T` ≈ `T*` (Swappable pointer, immutable data)
    - `^T#` ≈ `T* const` but data is mutable? (Needs strict verification with compiler rules)
    - *Doc Standard*: `let ^p = new T` (Ownership root).
- **Reference**: 
    - Explicitly created using `&` (e.g., `let &ref = &var#`).
    - References are **fixed** (cannot be reseated) and **non-nullable**.
    - Must abide by **Rule 406**: In-place access to the original variable is restricted during the lifetime of a mutable reference.
- **Increment/Decrement**:
    - Supported for any mutable memory location (Variable, Member, Index).
    - **Postfix (`x++`)**: Returns the value **before** the operation.
    - **Prefix (`++x`)**: Returns the value **after** the operation.

### 4.4 Toka Morphology (Point-Value Duality)
Toka uses a unique "morphology" system to manage pointers and handles without verbose dereferencing symbols.

- **Variable Name as Object**: In a scope where a pointer or reference is declared (e.g., `let *ptr = ...`), the name `ptr` refers directly to the **dereferenced object**.
    - `ptr.x` is valid and operates on the object's field.
    - `ptr` used in a context expecting a value will perform an auto-dereference load.
- **Morphology Symbol as Identity/Handle**: To access the **identity** (memory address, ownership handle, or reference count structure), use the definition's morphology symbol as a prefix.
    - `*ptr`: Represents the **raw pointer value** (the address). `printf("%p", *ptr)` prints the address.
    - `^ptr`: Represents the **unique ownership handle**. `let ^q = ^p` moves the handle.
    - `~ptr`: Represents the **shared ownership handle** (the control block + data pointer).
- **Modification of Identity (Reseating)**:
    - `*ptr = new_address`: Changes where the pointer points (reseats the pointer identity).
- **Explicit Pointer Access (`->`)**:
    - While `ptr.x` is the standard, Toka supports `->` for member access via an identity.
    - `(*ptr)->x`: Accesses the member `x` through the raw pointer value. Note that `ptr->x` is invalid because `ptr` is the object itself, not a pointer.

### 4.5 Memory Deallocation
- **Explicit (`del`)**: Used to manually free memory associated with a pointer identity.
    - `del *ptr`: Frees the memory pointed to by the raw pointer identity.
- **RAII**: `^` and `~` handles automatically free memory when their last handle identity goes out of scope.

## 5. Concurrency Model

### 5.1 Tasks
- **Task**: First-class citizen.
- `let t = task_obj.async`: Start immediate.
- `let t = task_obj.suspend`: Create lazy task.
- `t.await`: Block/Yield until result.
- **Return Type**: `await` returns `Enum(Result, AsyncError)`.

### 5.2 Locks & Shared State
- Shared mutable pointers (`^#T#`) MUST specify a lock strategy in declaration.
- **Lock Types**:
    - `mutex` (Mutex), `rwlock` (Read-Write), `spinlock` (Spinlock), `atomic`, `nolock` (Unsafe).
- **Usage**:
    - Lock acquisition is **implicit** via the `#` token scope or `oncelock{}` blocks.
### 5.3 Absolute Type Safety
- **No Implicit Conversion**: Toka strictly prohibits any implicit type conversion (e.g., `i32` to `i64`, `float` to `double`). All conversions must be explicit via the `as` keyword.
- **Binary Operations**: Both operands must have exactly the same type.
- **Assignments**: The right-hand side type must match the left-hand side type exactly.
- **Function Calls**: Argument types must match parameter types exactly.

## 6. Implementation Guidelines for Compiler
1.  **Parser**:
    - Handle `Write Token` suffixing carefully. `ident` vs `ident#`.
    - Distinguish `^Type` from `XOR` operator (if any). context sensitive.
2.  **Semantic Analysis**:
    - **Mutability Check**: Enforce that any LHS of assignment or `&mut` self method call has `#` token.
    - **Trait Check**: `impl` blocks must satisfy trait contracts (warn on `delete`).
    - **Thread Safety**: Verify that any `Token` marked shared variable has a Lock strategy.
3.  **CodeGen**:
    - "Immutable Pass-by-Value" -> "Pass-by-Reference" optimization.
    - **Type Aliases**: Resolution must be recursive via a compiler-wide mapping.
    - **In-place Initialization**: If a complex type (Struct/Tuple) is initialized with a slightly different type (e.g., different sized integers), CodeGen must perform **Member-wise conversion** (creating temporaries and casting each field) rather than a simple bit-cast to ensure data integrity.
    - Auto-generation of `drop` glue at scope end.
    - `async/await` state machine generation.

## 7. Example Valid Code
```scala
struct Data { v: i32 }

fn process(d: Data) -> void {
    // d is immutable here
    println(d.v)
}

fn main() {
    let x# = 10         // Mutable int
    x# = 11             // Mutation with token
    
    let ^p = new Data{v = 0} // Heap alloc
    // p.v = 1 // Error: p points to immutable Data
    
    let ^p2# = new Data{v = 0}
    p2#.v = 1     // OK: p2 points to mutable Data
    
    process(p2)     // Implicit optimization to clean reference
}
```

## 8. Build and Usage

### 8.1 Building the Compiler
To build the `tokac` compiler:

```bash
mkdir -p build && cd build
cmake ..
make
```

### 8.2 Running Toka Programs
The compiler generates LLVM IR (.ll) which can be executed using `lli` (LLVM interpreter):

```bash
# 1. Compile .tk to .ll
./build/src/tokac tests/your_file.tk > your_file.ll

# 2. Run with lli
lli your_file.ll
```

Or as a one-line command:
```bash
./build/src/tokac tests/your_file.tk > tests/your_file.ll && lli tests/your_file.ll
```
