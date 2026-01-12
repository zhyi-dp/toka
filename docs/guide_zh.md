# Toka 语言指南

欢迎使用 Toka 编程语言。Toka 是一种静态类型的系统编程语言，旨在实现安全、高性能和现代化的开发体验，并拥有一种独特的高级对象形态学（Morphology）和内存管理机制。

---

## 1. 快速入门

### Hello World
一个最简单的 Toka 程序如下：

```toka
import std/io::println

fn main() -> i32 {
    println("Hello, Toka!")
    return 0
}
```

### `main` 函数
每个 Toka 程序的入口点都是 `main` 函数，它通常返回一个 `i32` 类型的退出状态码。

---

## 2. 变量与类型

### 变量声明
Toka 使用 `auto` 关键字声明变量，并支持类型推导。

```toka
auto x = 10         // 默认为 i32
auto y: f64 = 3.14  // 显式指定类型
```

### 可变性
默认情况下，变量是不可变的。使用 `#` 后缀可以声明一个可变变量。

```toka
auto val = 5
// val = 6          // 错误：val 是不可变的

auto count# = 0     // 声明可变变量
count# = 1          // 写入: 使用 '#' 后缀进行修改
println("{}", count)// 读取: 使用不带后缀的名称 (语法更清晰)
```

### 基础类型
- **整数**: `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`
- **浮点数**: `f32`, `f64`
- **其他**: `bool`, `char`, `void`
- **文本**: `str` (字符串字面量/底层), `std/string::String` (推荐，可增长字符串)

---

## 3. 数据结构

### Shapes (结构体)
`shape` 是 Toka 定义结构化数据的主要方式。

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

### 封装与可见性 (Encapsulation and Visibility)
Toka 通过 `@encap` 块提供细粒度的封装控制系统。

#### 默认可见性
默认情况下，`shape` 的所有字段都是 **公开（Public）** 的。你可以在任何地方访问它们。

#### 启用封装
要限制访问权限，你需要定义一个 `@encap` 块 (`impl Shape@encap`)。
**一旦定义了该块，所有字段默认变为私有。** 你必须在块内显式地列出需要公开的字段。

```toka
shape Config (
    api_key: str,
    port: i32
)

// 启用封装。'port' 被公开，'api_key' 变为私有。
impl Config@encap {
    pub port            // 全局公开
    // api_key 未被列出，因此变为私有
}
```

#### 高级规则
- **排除法**: 你可以将所有字段设为公有，但排除特定字段。
  ```toka
  impl Data@encap {
      pub * ! secret_key  // 除 secret_key 外全部公开
  }
  ```
- **基于路径**: `pub(path/to/module)` 授予特定模块访问权限。

#### Trait 可见性
Trait 方法现在可以显式标记为 `pub`，以确保它们可以从定义模块的外部被调用。

```toka
trait @Drawable {
    pub fn draw(self)
}
```

---

## 4. 对象形态学 (Morphology) 与内存

Toka 引入了“形态学”操作符来处理对象标识、所有权和空值安全。

| 操作符 | 含义 | 示例 |
| :--- | :--- | :--- |
| `#` | **可变**: 用于类型或变量，允许修改。 | `auto &x# = &y#` |
| `?` | **可空**: 值使用 `none`，身份使用 `nullptr`。 | `auto x? = none` <br> `auto ^?p = nullptr` |
| `!` | **可写且可空**: 既可修改，又可为 `none`/`nullptr`。 | `auto x! = none` |
| `^` | **所有权/移动**: 表示拥有所有权的指针或移动操作。 | `auto ^r2 = ^r1` |
| `~` | **共享指针**: 引用计数的共享所有权。 | `auto ~s2 = ~s1` |
| `&` | **借用/引用**: 对值或 Soul 的临时视图 (非所有权)。 | `auto &y = &x` |
| `*` | **标识/原始指针**: 访问底层指针或地址。 | `println("Addr: {}", *ptr)` |

### 借用 (Borrowing)
使用 `&` 创建借用引用。

```toka
auto x# = 10
{
    auto &y = &x       // 不可变借用
    println("y: {}", y)
}                      // 借用在此结束

auto &z# = &x#         // 可变借用
z# = 20                // 修改 x 的值
```

### 匹配 (Match)
`match` 表达式用于模式匹配，可以解构枚举、结构体等。

```toka
enum Status {
    Running,
    Stopped(i32),
    Paused { reason: str }
}

fn process_status(s: Status) {
    match s {
        auto Stopped(code) => println("Stopped with {}", code), // 结构体变体: 需要 'auto' 进行解构
        Status::Running => println("Running..."),               // 单元变体或通配符: 通常不需要 'auto'
        Status::Paused { reason } => println("Paused because: {}", reason)
    }
}
```

### 内存管理 (Memory Management)

#### 智能指针 (自动管理)
使用 `new` 创建独占指针 (`^`) 或共享指针 (`~`)。当它们超出作用域时，会自动执行释放 (Drop) 操作。

```toka
{
    auto ^ptr = new Point(x = 1, y = 2) 
    // ^ptr 是唯一的独占所有者
} // 作用域结束：^ptr 自动执行 drop 并释放内存
```

#### 原始指针 (手动管理)
对于手动内存管理（不安全），请使用 `std/memory` 中的 `alloc` 和 `free`。Toka 不会自动清理原始指针 (`*`)。

```toka
import std/memory

auto *raw = memory::alloc(sizeof(Point))
// ...
memory::free(*raw)
```

### 自动资源管理 (深度释放/Deep Drop)
Toka 支持 **递归释放 (Recursive Drop)**。当一个容器结构体被释放（例如超出作用域）时，Toka 会自动调用所有需要释放的成员的 `drop` 方法。

```toka
impl Resource {
    fn drop(self#) {
        println("Resource dropped")
        // 成员变量如 'self.inner_ptr' 会在此函数体执行完毕后自动释放。
    }
}
```

**安全规则:** 如果你的 Shape 包含资源（如原始指针或其他带有 `drop` 的 Shape），编译器会**强制**你为该 Shape 实现 `drop` 以确保权责分明。

---

## 5. 函数与方法

### 函数定义
使用 `fn` 关键字定义函数。

```toka
fn add(a: i32, b: i32) -> i32 {
    return a + b
}
```

### 实现块 (`impl`)
方法通常定义在针对特定 Shape 的 `impl` 块中。

```toka
impl Rect {
    fn area(self) -> i32 {
        return self.width * self.height
    }
}
```

### Trait (特征)
Trait 定义了共享的行为（类似于其他语言中的接口）。

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

## 6. 控制流

### If / Else
```toka
if x > 0 {
    println("Positive")
} else {
    println("Non-positive")
}
```

### 循环
Toka 支持 `while`、`loop`（无限循环）和 `for`。

```toka
while x > 0 {
    x# = x - 1
}

loop {
    if done() { break }
}
```

---

## 7. 模块与导入

使用 `import` 将其他模块的符号引入当前作用域。

```toka
import std/io::println
import std/memory as mem

fn main() {
    println("Using imports")
}
```

---

## 8. 语言设计哲学：安全至上
Toka 旨在通过以下机制防止常见的内存错误：
1. **严格的移动语义 (Strict Move Semantics)**: 防止“移动后使用”错误。
2. **借用检查器 (Borrow Checker)**: 确保引用不会比其指向的数据存活得更久。
3. **空安全 (Null Safety)**: 除非标记为 `?`，否则类型默认为非空。
4. **显式标识 (Explicit Identity)**: 通过形态学操作符区分对象值与其指针标识。
5. **资源安全 (Resource Safety)**: 强制对持有资源的类型执行清理逻辑 (递归释放)。
