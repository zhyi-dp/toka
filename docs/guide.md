# Toka Language Guide

Welcome to the Toka Programming Language. Toka is a statically typed system programming language designed for safety, performance, and modern ergonomics, with a unique approach to object morphology and memory management.

---

## 1. Getting Started

### Hello World
A minimal Toka program looks like this:

```toka
import std/io::println

fn main() -> i32 {
    println("Hello, Toka!")
    return 0
}
```

### The `main` function
The entry point of every Toka program is the `main` function, which typically returns an `i32` exit code.

---

## 2. Variables and Types

### Variable Declaration
Toka uses `auto` for variable declarations, with support for type inference.

```toka
auto x = 10         // i32 by default
auto y: f64 = 3.14  // explicit type
```

### Mutability
By default, variables are immutable. Use the `#` suffix to declare a mutable variable.

```toka
auto val = 5
// val = 6          // Error: val is immutable

auto count# = 0
count# = 1          // OK
```

### Primitive Types
- **Integers**: `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`
- **Floating Point**: `f32`, `f64`
- **Others**: `bool`, `char`, `str`, `void`

---

## 3. Data Structures

### Shapes (Structs)
`shape` is Toka's primary way to define structured data.

```toka
shape Rect (
    width: i32,
    height: i32
)

fn main() {
    auto r = Rect(width = 10, height = 20)
    println("Area: {}", r.width * r.height)
}
```

### Algebraic Data Types (ADTs)
Shapes also support sum types (enums), which can be matched using `match`.

```toka
shape State (
    On |
    Off |
    ErrCode(i32)
)

fn process(s: State) {
    match s {
        On => println("Active")
        Off => println("Inactive")
        auto ErrCode(code) => println("Error: {}", code)
    }
}
```

### Encapsulation and Visibility
Toka uses a granular encapsulation system via the `@encap` block.

#### Default Visibility
By default, all fields in a `shape` are **public**. You can access them freely from any module.

#### Enabling Encapsulation
To restrict access, define an `@encap` block (`impl Shape@encap`).
**Once declared, all fields become private by default.** You must explicitly expose fields using keywords like `pub`.

```toka
shape Config (
    api_key: str,
    port: i32
)

// Activates encapsulation. 'port' is exposed, 'api_key' becomes private.
impl Config@encap {
    pub port            // Public everywhere
    // api_key is omitted, so it is private
}
```

#### Advanced Rules
- **Exclusion**: You can make everything public *except* specific fields.
  ```toka
  impl Data@encap {
      pub * ! secret_key  // All public except secret_key
  }
  ```
- **Path-Based**: `pub(path/to/module)` grants access to specific modules.

#### Trait Visibility
Trait methods can now be explicitly marked as `pub` to ensuring they are callable from outside the defining module.

```toka
trait @Drawable {
    pub fn draw(self)
}
```

---

## 4. Object Morphology & Memory

Toka introduces "Morphology" operators to handle object identity, ownership, and nullability.

| Operator | Meaning | Example |
| :--- | :--- | :--- |
| `^` | **Ownership/Move**: Indicates an owning pointer or a move operation. | `auto ^r2 = ^r1` |
| `#` | **Mutable**: Used in types or variables to allow modification. | `auto &x# = &y#` |
| `?` | **Optional/Nullable**: Indicates a type can be `null`. | `auto p: Point? = null` |
| `*` | **Identity/Raw Pointer**: Accesses the underlying pointer/address. | `println("Addr: {}", *ptr)` |

### Borrowing
Borrowed references are created using `&`.

```toka
auto x# = 10
{
    auto &y = &x       // Immutable borrow
    println("y: {}", y)
}                      // Borrow ends here

auto &z# = &x#         // Mutable borrow
z# = 20                // Modifies x
```

### Allocation and Deallocation
Use `new` to allocate on the heap and `del` to release.

```toka
auto *ptr# = new Point(x = 1, y = 2)
// ... use ptr ...
del *ptr               // Manual deallocation of the identity
```

---

## 5. Functions and Methods

### Function Definition
Functions use the `fn` keyword.

```toka
fn add(a: i32, b: i32) -> i32 {
    return a + b
}
```

### Implementation Blocks (`impl`)
Methods are defined within `impl` blocks for a specific shape.

```toka
impl Rect {
    fn area(self) -> i32 {
        return self.width * self.height
    }
}
```

### Traits
Traits define shared behavior (similar to interfaces).

```toka
trait @Shape {
    fn area(self) -> i32
}

impl Rect@Shape {
    fn area(self) -> i32 {
        return self.width * self.height
    }
}
```

---

## 6. Control Flow

### If / Else
```toka
if x > 0 {
    println("Positive")
} else {
    println("Non-positive")
}
```

### Loops
Toka supports `while`, `loop` (infinite loop), and `for`.

```toka
while x > 0 {
    x# = x - 1
}

loop {
    if done() { break }
}
```

---

## 7. Modules and Imports

Use `import` to bring symbols from other modules into scope.

```toka
import std/io::println
import std/memory as mem

fn main() {
    println("Using imports")
}
```

---

## 8. Philosophy: Safety First
Toka is designed to prevent common memory errors through:
1. **Strict Move Semantics**: Prevents "Use-after-move".
2. **Borrow Checker**: Ensures references do not outlive their data.
3. **Null Safety**: Types are non-nullable by default unless marked with `?`.
4. **Explicit Identity**: Differentiating between the object value and its pointer identity via Morphology.
