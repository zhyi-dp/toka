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
- `<-`: Dependency / Channel receive (Context dependent).
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
- **Trait**:
  ```scala
  trait Run {
      fn run(self) -> void
  }
  ```

## 4. Memory Model

### 4.1 Ownership & Move Semantics
- **Copy vs Move**:
    - By default, assignment implies **copy**.
    - If a type is `[#no_bit_copy]`, copy is via `clone`.
    - Variable at last usage (R-value): `move`.
    - Variable as L-value ending scope: `drop`.
    - **Optimization**: Immutable variables passed by value are optimized to **Reference** passing under the hood.

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
- **Reference**: Implicitly created during function calls for immutable args.

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
    - `mut` (Mutex), `rw` (Read-Write), `spin` (Spinlock), `atomic`, `nolock` (Unsafe).
- **Usage**:
    - Lock acquisition is **implicit** via the `#` token scope or `oncelock{}` blocks.
    - `let shared_data#mut = ...`

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
    - Auto-generation of `drop` glue at scope end.
    - `async/await` state machine generation.

## 7. Example Valid Code
```scala
struct Data { let: i32 }

fn process(d: Data) -> void {
    // d is immutable here
    println(d.let)
}

fn main() {
    let x# = 10         // Mutable int
    x# = 11             // Mutation with token
    
    let ^p = new Data{let: 0} // Heap alloc
    // p.let = 1 // Error: p points to immutable Data
    
    let ^p2# = new Data{let: 0}
    p2#.let = 1     // OK: p2 points to mutable Data
    
    process(p2)     // Implicit optimization to clean reference
}
```
