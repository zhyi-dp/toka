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
- `auto`: Variable declaration start.
- `type`: Type alias.
- `const`: Constant declaration.
- `shape`: Unified structure/layout definition.
- `trait`: Interface definition.
- `impl`: Implementation block.
- `fn`: Function/Method declaration.
- `new`: Heap allocation (`auto ^p = new Person{...}`).
- `dyn`: Dynamic dispatch marker for Traits.
- `move`: Explicit ownership transfer.
- `where`: Type constraints.
- `default`: Use default trait implementation.
- `delete`: Explicitly remove a trait method (contract violation, note implementation warning).
- `final`: Seal implementation or method.
- `Fn`: Synchronous function type.

**Control Flow**
- `if`, `else`, `match`, `pass`
- `for`, `while`, `loop`, `break`, `continue`
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
- `()`: Grouping, Arguments, Tuples, Object initialization.
- `[]`: Arrays only.
- `{}`: Blocks, Scopes.
- `^`: Pointer prefix (e.g., `^Person`).
- `#`: **Write Token** (Writable Content / Swappable Address).
- `?`: **Null Token** (Nullable: `none` for objects, `null` for pointers).
- `!`: **Write + Null Token** (Writable/Swappable + Nullable).
- `$`: **None Token** (Immutable & Non-null), usually omitted.
- **Literal Defaults**: 
    - Integer literals without suffix: `i32`.
    - Floating point literals without suffix: `f64`.
- `<-`: Dependency / Channel receive (Context dependent).
- `&`: **Reference prefix** (e.g., `auto &r = &x#`).
- `++` / `--`: **Increment/Decrement** (Prefix and Postfix).
- `.`: Access.
- `:`: Type annotation.
- `;`: **Optional Statement Terminator**.

### 2.3 Semicolon Rules (Optional Semicolons)
Toka supports semicolon-less syntax. A semicolon is required ONLY to separate multiple statements on the same line.
- **Statement Termination**: A newline acts as a terminator unless the previous token is an operator or delimiter that expects continuation.
- **Continuation Rule**: If a line ends with an operator (e.g., `+`, `-`, `*`, `/`, `=`, `,`, `.`, `->`, `::`, `(`, `[`, `{`), the next line is considered a continuation of the same statement.
- **Example**:
  ```scala
  auto x = 10    // OK: Newline terminates
  auto y = 1 +   // OK: '+' at end means continuation
           2
  ```

## 3. Type System & Attribute Tokens

### 3.1 The Attribute Token System (Core Feature)
Toka separates **storage binding** (`auto`) from **memory properties** (Tokens).

| Token | Meaning on Object | Meaning on Pointer (`^`, `*`, `~`) |
| :--- | :--- | :--- |
| `#` | **Writable** (Content) | **Swappable** (Identity/Address) |
| `?` | **Nullable Slot** (Content) | **Nullable Slot** (Identity/Address) |
| `!` | **Writable + Nullable**. | **Swappable + Nullable**. |
| `$` | **Immutable + Non-null**. | **Fixed + Non-null**. |

**Syntax Rules:**
- Tokens are suffixes to the variable name (e.g., `auto x# = ...`).
- For pointers, tokens can attach to the pointer symbol `^` AND the variable name, creating 16 combinations (e.g., `auto ^#p?`).
    - `^#`: Pointer address is swappable (can point elsewhere).
    - `p?`: Object content is nullable (can be `none`).
    - *Clarification*: `auto ^?ptr#` = Nullable non-swappable pointer, pointing to a Writable non-nullable object.

#### 3.1.1 Reseat vs. Mutation
Toka distinguishes between changing **where** a pointer points and **what** it points to.
- **Reseat (Identity Change)**: `p = ...` or `*p = null`. 
    - Requires **Morphology `#` or `!`** (e.g., `^#` or `^!`).
    - If the source is `null`, Morphology must also permit a null slot (`?` or `!`).
- **Mutation (Content Change)**: `p.field = ...`.
    - Requires **Value `#` or `!`** (e.g., `p#` or `p!`).
    - If the source is `none`, Value must also permit a none slot (`?` or `!`).

### 3.2 Basic Types
- `i8`..`i64`, `u8`..`u64`, `f32`, `f64`
- `bool`, `char`, `str` (String)
- `void` (Unit)

### 3.3 The Five-State Shape System
Toka replaces separate `struct`, `enum`, and `option` keywords with a single **`shape`** keyword. A shape defines a physical memory layout and can represent five distinct forms.

#### 3.3.1 Struct Shape (Named Fields)
Standard record type with named fields.
```scala
shape Line(x: i32, y: i32)
```
**Initialization**: `auto l = Line(x=10, y=20)`

#### 3.3.2 Tuple Shape (Positional Fields)
Fields are accessed by index or destructuring.
```scala
shape Point(i32, i64)
```
**Initialization**: `auto p = Point(10, 20)`

#### 3.3.3 Array Shape (Fixed Size)
A fixed-length contiguous sequence.
```scala
shape Nums[i32; 5]
```
**Initialization**: `auto n = Nums[1, 2, 3, 4, 5]` (uses standard array index expr)

#### 3.3.4 Tagged Union Shape (Enum / Option)
Also known as an ADT. Each variant has a **Tag** (physical value) and an optional **Payload**.
```scala
shape Maybe(
    One(i64) = 1 | 
    None = 0
)
```
- **Physical Layout (Scheme A: Natural Alignment)**:
    - **Tag**: 1 byte (Offset 0).
    - **Padding**: Variable (to align Payload).
    - **Payload**: Starts at the next naturally aligned offset (e.g., Offset 8 for `i64`).
    - **Total Size**: Determined by max alignment requirements (e.g., 16 bytes for `Maybe(One(i64))`).
- **Access Rule**: MUST be accessed via `match` or `if auto`. No direct member access.

#### 3.3.5 Bare Union Shape
A C-style union where all members share the same memory. No tag is stored.
```scala
shape Bytes4(
    as u32 |
    as (u8, u8, u8, u8) |
    as (head: u16, mid: u8, tail: u8)
)
```
- **Physical Layout**: Size is `max(sizeof(members))`. Alignment is `max(alignof(members))`.
- **Access Rule**: MUST specify the interpretation using `as`.
- **Example**: `auto (&r, &g#, &b, &a) = val as shape(u8, u8, u8, u8)`.

---

### 3.4 Pattern Matching & Expressions

#### 3.4.1 The `match` Expression
`match` is an expression that yields a value via `pass`.

**Binding Sovereignty (Physical Contract):**
- `auto One(v)`: **Copy**. Creates a new stack variable and copies the payload.
- `auto One(&v)`: **Reference (Read-only)**. `v` is an alias to the original payload memory.
- `auto One(&v#)`: **Mutable Reference**. `v` provides write access to the original payload memory.
- `auto One(v#)`: **Mutable Copy**. Copies payload to a new mutable stack variable.

**Syntax Example:**
```scala
auto res = Maybe::One(12)
match res {
    auto Maybe::One(&v) => {
        println("got One ref={}", v)
    }
    Maybe::None => println("got None")
}
```

#### 3.4.2 The `pass` Keyword & Type Softening
`pass` delivers a value from a block.
- **Rules**: Must be the last effective action in a block. If one branch matches `T` and another matches `none`, the resulting type is "softened" to `T!` (Nullable).
- **Example**:
  ```scala
  auto val = match res {
      auto Maybe::One(v) => pass v
      Maybe::None => pass none
  }
  // val is inferred as i64!
  ```

#### 3.4.3 Expression-based `if`
```scala
auto x = if val > 0 { pass val } else { pass 1 }
```
- If the target variable (e.g., `x`) expects a value, all branches MUST end with `pass`.
- `pass none` is required for optional branches to satisfy the type requirement.

#### 3.4.2 Loop Expressions & `or` Fallback
When a `for` or `while` loop is used as an expression, it must provide a fallback value in case the loop completes normally (without a `break`). This is done using the `or` block.
```scala
auto val = for auto x in list {
    if x > 10 {
        break x  // Loop terminates, val becomes x
    }
} or {
    pass -1      // Loop finished normally, val becomes -1
}
```
*Note*: `loop` does not require an `or` block because it is either infinite or must be terminated by a `break` with a value.

#### 3.4.3 Targeted Control Flow (Labels)
Loops can be targeted by `break` and `continue` using the name of the variable receiving the loop's value as a label.
```scala
auto result = loop {
    for auto i in [1, 2, 3] {
        if i == 2 {
            break to result 42 // Hits the 'loop' directly
        }
    }
}
```
If no label is provided, `break` and `continue` target the innermost loop.

### 3.5 Traits & Implementation
- **Definition**: Trait names are prefixed with `@` in definitions and usage.
  ```scala
  trait @Draw {
      fn draw(self) -> i32
  }
  ```
- **Implementation**:
  - **Inherent Methods**: `impl Type { ... }`
  - **Trait Implementation**: `impl Line@Draw { ... }`
  - **Multiple Traits**: `impl Type@{Trait1, Trait2} { ... }` (Planned)
  - **Default/Delete**: Methods can be marked `= default` or `= delete` (Planned).
  
### 3.6 Modules System
- **Module Identity**: File path based. E.g., `tests/lib.tk` is module `lib` (or fully qualified via path).
- **Import Syntax**:
  - `import path/to/file`: Imports the module. Symbols are accessed via namespace `file::symbol`.
  - `import path/to/file::*`: Imports the module and brings all `pub` symbols into current scope.
  - `import path/to/file::Symbol`: Imports specific symbol.
  - `import path/to/file::{A, B}`: Destructuring import.
- **Visibility**:
  - `pub`: Makes a top-level declaration (fn, struct, type, trait) visible to importers.
  - **Private by default**: Without `pub`, symbols are only visible within the declaring file.
- **Resolution**:
 [Physical Path] --resolve--> [Module] --filter--> [Visibility] --bind--> [Scope]

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
    - *Doc Standard*: `auto ^p = new T` (Ownership root).
- **Reference**: 
    - Explicitly created using `&` (e.g., `auto &ref = &var#`).
    - References are **fixed** (cannot be reseated) and **non-nullable**.
    - Must abide by **Rule 406**: In-place access to the original variable is restricted during the lifetime of a mutable reference.
- **Increment/Decrement**:
    - Supported for any mutable memory location (Variable, Member, Index).
    - **Postfix (`x++`)**: Returns the value **before** the operation.
    - **Prefix (`++x`)**: Returns the value **after** the operation.

### 4.4 Toka Morphology (Point-Value Duality)
Toka uses a unique "morphology" system to manage pointers and handles without verbose dereferencing symbols.

- **Variable Name as Object**: In a scope where a pointer or reference is declared (e.g., `auto *ptr = ...`), the name `ptr` refers directly to the **dereferenced object**.
    - `ptr.x` is valid and operates on the object's field.
    - `ptr` used in a context expecting a value will perform an auto-dereference load.
- **Morphology Symbol as Identity/Handle**: To access the **identity** (memory address, ownership handle, or reference count structure), use the definition's morphology symbol as a prefix.
    - `*ptr`: Represents the **raw pointer value** (the address). `printf("%p", *ptr)` prints the address.
    - `^ptr`: Represents the **unique ownership handle**. `auto ^q = ^p` moves the handle.
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
- `auto t = task_obj.async`: Start immediate.
- `auto t = task_obj.suspend`: Create lazy task.
- `t.await`: Block/Yield until result.
- **Return Type**: `await` returns `Enum(Result, AsyncError)`.

### 5.2 Locks & Shared State
- Shared mutable pointers (`^#T#`) MUST specify a lock strategy in declaration.
- **Lock Types**:
    - `mutex` (Mutex), `rwlock` (Read-Write), `spinlock` (Spinlock), `atomic`, `nolock` (Unsafe).
- **Usage**:
    - Lock acquisition is **implicit** via the `#` token scope or `oncelock{}` blocks.
### 3. Permissions & Attributes System (Detailed)

Toka uses a **Dual-Location Attribute System** distinguishing between the *Pointer/Container* and the *Object Value*.

#### A. Pointer Attributes (Prefix on Morphology Token `^`, `*`, `~`, `&`)
Control the **Reference/Pointer itself** (the container).
-   **Suffixes on Morphology**: `#`, `?`, `!`.
    -   `#` (`^#`): **Rebindable**. The pointer variable can be reassigned (`p = new`).
    -   `?` (`^?`): **Nullable**. The pointer can be null.
-   **Transfer Rule**:
    -   **Move/Copy**: **Free Redefinition**. The new owner can define their pointer as Rebindable or Nullable regardless of the source, as it only affects the local container.
    -   **Share/Borrow**: **Covariant**. Cannot grant more permission than the source (e.g., cannot borrow a nullable pointer as non-null without check).

#### B. Value Attributes (Suffix on Identifier)
Control the **Object Value** (the soul).
-   **Suffixes on Identifier**: `#`, `?`.
    -   `#` (`name#`): **Value Mutable**. The content (`*p`, `p.field`) can be modified.
    -   `?` (`name?`): **Value Option** (Logical None possibility).
-   **Transfer Rule**:
    -   **ALL Transfers (Move/Copy/Share/Borrow)**: **Strict No-Upgrade**.
        -   **Immutable -> Mutable**: **FORBIDDEN**. You cannot gain write access if the source was read-only, even if you Move it.
        -   **Mutable -> Immutable**: **ALLOWED**. You can voluntarily drop write access.

#### Implementation Requirements
1.  **Parser Update**: Extract attributes from both Morphology Token (Prefix) and Identifier Token (Suffix).
2.  **SymbolInfo**: Track `IsRebindable`, `IsValueMutable`, `IsPointerNullable`, `IsValueNullable` separately.
3.  **Sema Logic**:
    -   **Reassignment (`p = ...`)**: Check `IsRebindable`.
    -   **Modification (`p.f = ...`)**: Check `IsValueMutable`.
    -   **Transfer Check**:
        -   Target `IsValueMutable` must be <= Source `IsValueMutable` (True <= True, False <= True/False). **False <= True is OK (Downgrade). True <= False is ERROR (Upgrade).**

### 5. Nullability Handling (The `is` Operator)
**Sole Mechanism**: Toka uses the `is` operator as the **only** method for null checks and unwrapping. 
-   **Discarded/Forbidden**: `if auto`, `match` for nulls, `== null` checks, implicit smart casts.
-   **Syntax**: `if Source is Target { ... }`
    -   **Source Expression**: `^?p` (matches the declaration pattern of the nullable variable).
    -   **Target Expression**: `^p` (matches the desired non-nullable pattern).
    -   **Actual Code**: `if ^?p is ^p { ... }`.
    -   **Semantics**: Checks if variable `p` (declared as `auto ^?p`) is not null. Returns true if valid.
-   **Capabilities**:
    -   **Unwrap**: Converts `?` or `!` to non-null (`#` or default).
    -   **Deep Conversion**: `if ~!ptr? is ~ptr`.

| Source Declaration | Check/Unwrap Syntax | Resulting Binding |
| :--- | :--- | :--- |
|    auto ^?p = null | `if ^?p is ^p { printf("Not Null!\n") }` | `p`: Unique Non-null (Check only for now) |
| `auto obj! = ...` | `if obj! is obj { ... }` | `obj`: Object Non-null |
| `auto ~!ptr? = ...` | `if ~!ptr? is ~ptr { ... }` | `ptr`: Shared Non-null |

#### C. Control Flow Analysis
Sema must verify that:
-   **Return Check**: All control paths in a non-void function must explicitly return a value.
-   **Unreachable Code**: Detect code after returns or infinite loops (optional but good).

#### D. Parser Debt (Immediate Fixes Needed)
- [x] **Logical Operators**: Implement parsing for `&&` (And) and `||` (Or) in `parseExpr`.
- [x] **Attribute Extraction in Parser**:
    -   Update `parseVariableDecl` to read `IsSwappablePtr` and `HasNull` from the Morphology Token (e.g., `Caret`).
    -   Also updated `UnaryExpr` to propagate these attributes for `is` operator checks.
- [x] **Refined Null Check (`is`)**: Implemented `if ^?p is ^p` syntax support via `is` operator and relaxed Sema checking for this construct.

### 4. Smart Pointer Implementation Guidelines
## 6. Implementation Guidelines for Compiler
1.  **Parser**:
    - Handle `Write Token` suffixing carefully. `ident` vs `ident#`.
    - Distinguish `^Type` from `XOR` operator (if any). context sensitive.
2.  **Semantic Analysis**:
    - **Mutability Check**: Enforce that any LHS of assignment or `&mut` self method call has `#` token.
    - **Trait Check**: `impl` blocks must satisfy trait contracts (warn on `delete`).
    - **Thread Safety**: Verify that any `Token` marked shared variable has a Lock strategy.
    - **Smart Pointer Ownership**:
        - **Unique (`^`)**: Enforce **Move Semantics**. Assignment `auto ^q = ^p` implies move; `p` effectively becomes invalid (though currently implemented as runtime nulling, Sema should eventually track this).
        - **Shared (`~`)**: Enforce type compatibility. Shared pointers can only be initialized from other Shared pointers (copy/incref) or fresh allocations (`new`).
    - **Type Inference**: Correctly propagate `IsUnique`/`IsShared` attributes during type inference (e.g., `auto ~x = ...`).
    - **Permissions & Attributes**:
        - **Object Value Permission**: Cannot upgrade. If the source object is immutable, the destination object cannot become mutable, even on move.
        - **Pointer Permission**: Can be redefined on move. The mutability/nullability of the pointer itself (not the pointed-to value) can change when moved to a new variable.
3.  **CodeGen**:
    - "Immutable Pass-by-Value" -> "Pass-by-Reference" optimization.
    - **Type Aliases**: Resolution must be recursive via a compiler-wide mapping.
    - **Struct vs Pointer Type Resolution**: When resolving types for `Unique` (`^`) or `Shared` (`~`) variables, `resolveType` MUST be called with explicit flags (`IsUnique` or `IsShared`) to return the correct **pointer/handle type** instead of the underlying **Soul struct type**. Failure to do this leads to LLVM IR type mismatches (allocating struct instead of pointer) and crashes during cleanup (e.g., `CreateIsNotNull` on a struct).
    - **Smart Pointer Layout**:
        - **Unique (`^`)**: Implemented as a raw LLVM pointer (`ptr`). Cleanup uses `free` on non-null pointers.
        - **Shared (`~`)**: Implemented as a struct `{ ptr, ptr }` containing the data pointer and a reference count pointer.
    - **Scope Cleanup**: `BlockStmt` must generate cleanup code for all stack-allocated variables.
        - **Shared**: Decrement ref-count; release both data and ref-count memory if zero.
        - **Unique**: Check for null; `free` if not null.
    - **In-place Initialization**: If a complex type (Struct/Tuple) is initialized with a slightly different type (e.g., different sized integers), CodeGen must perform **Member-wise conversion** (creating temporaries and casting each field) rather than a simple bit-cast to ensure data integrity.
    - **CodeGen Safety & Dead Code (Dead Instruction Probes)**:
        - Before emitting ANY instruction, CodeGen MUST check if the current `BasicBlock` is terminated: `if (!m_Builder.GetInsertBlock() || m_Builder.GetInsertBlock()->getTerminator()) return nullptr`.
        - This prevents LLVM assertion failures when generating code for branches that follow a `break`, `continue`, or `return`.
        - In `BinaryExpr`, check liveness after each operand evaluation, as the evaluation itself might contain a terminator (e.g., a function call that includes a non-local break).
        - **Result Allocas**: Ensure `resultAddr` allocas for `IfExpr`, `WhileExpr`, etc., are created at the VERY BEGINNING of the expression generation, before any branches, to ensure they are accessible from all paths.
    - Auto-generation of `drop` glue at scope end.
    - `async/await` state machine generation.

## 7. Example Valid Code
```scala
shape Data (v: i32)

fn process(d: Data) -> void {
    // d is immutable here
    println("{}", d.v)
}

fn main() {
    auto x# = 10         // Mutable int
    x# = 11             // Mutation with token
    
    auto ^p = new Data(v = 0) // Heap alloc
    // p.v = 1 // Error: p points to immutable Data
    
    auto ^p2# = new Data(v = 0)
    p2#.v = 1     // OK: p2 points to mutable Data
    
    process(p2)     // Implicit optimization to clean reference
}
```

### 7.4 Implementation Notes: Unified Morphology
In the implementation of `Parser` and `Sema`, symbols like `*`, `^`, `~`, `&` are all unified as `UnaryExpr` nodes. This allows for a consistent handling of Toka's morphology system. `Sema` distinguishes between them using the operator kind and the `SymbolInfo` structure, ensuring that `*p` (Identity) and `&p` (Reference) are correctly resolved.

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
