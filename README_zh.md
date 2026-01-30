[English](README.md)

# Toka 编程语言

Toka 是一门由易中华 (YiZhonghua) 于 2025 年创造的系统级编程语言。它旨在实现 **安全**、**高效** 和 **语法简洁**，并通过其创新的 **属性标记系统 (Attribute Token System)** 解决了传统编程中安全性与生产力之间的权衡难题。

## 🌟 核心理念：属性标记 (Attribute Tokens)

Toka 通过正交的后缀标记让内存属性显式化，消除了隐藏的内存状态。这使得你一眼就能读懂内存的使用“形状”。

### 🚀 独特创新 (Key Innovations)

**1. 原生语法的 RAII (Syntactic RAII)**
Toka 是极少数将**智能指针提升为核心语法符号**的语言。这意味着**“生命周期管理”被内化为语言的呼吸**，而非仅仅是标准库提供的功能。
*   **^T (独占)** 和 **~T (共享)** 直接作为原生类型存在，让 RAII 成为零视觉负担的默认模式。

**2. 灵-身二元论 (Soul-Identity Duality)**
Toka 哲学性地将对象拆分为 **Soul (内涵/值)** 与 **Identity (外延/身份)**。
*   **精确语义**：通过借用 (`&`) 访问 Soul，通过指针 (`*`) 操作 Identity。
*   **消除歧义**：这彻底解决了传统语言中“引用赋值”与“内容修改”的语义模糊。在 Toka 中，移动身份 (`move`) 和修改内容 (`mut`) 是完全不同的语法维度。

**3. 正交属性系统 (Orthogonal Attributes)**
告别 `const volatile unsigned` 的关键字堆砌。
*   **视觉化化学式**：通过 `#` (可变), `?` (可空), `!` (可变且可空) 等正交后缀，Toka 让变量定义像化学式一样清晰。`T^#?` 一眼即知是“可变、可空、独占的 T 指针”。

**4. @encap 显式封装 (Explicit Encapsulation)**
Toka 默认 `shape` 的成员是透明可访问的，但通过 `@encap` 实现了细粒度的访问控制。
*   **权限收束**：通过 `impl Type@encap` 块，开发者必须显式列出 (`pub`) 哪些字段对外部可见。未列出的字段自动私有化。
*   **生命周期绑定**：`@encap` 块也是定义构造、析构 (`drop`) 等生命周期关键方法的唯一合法场所，从而确保资源的安全性。

**5. 契约式控制流 (Contract-Based Control Flow)**
**“一切皆有值，一切皆保证”**。
*   **分支平衡**：Toka 强制控制流表达式（如 `match`, `for`）在所有路径上都必须履行值的交付契约。
*   **循环回退**：独特的 `for-or` 语法保证了即使循环未执行（空迭代），接收者也能得到承诺的值保证。

### 符号系统表

| 标记 (Token) | 内容上的含义 (Value/Content) | 身份上的含义 (Identity/Address) |
| :--- | :--- | :--- |
| `#` | **可写**: 可修改字段/内容 | **可交换**: 可重定向(Reseat) |
| `?` | **可空**: 值为 `none` | **可空**: 身份为 `nullptr` |
| `!` | **可写且可空** | **可交换且可空** |
| `^` | - | **独占指针** (所有权) |
| `~` | - | **共享指针** (引用计数) |
| `&` | **借用**: 值的临时视图 | **引用**: Soul 的临时视图 |
| `*` | - | **原始指针** (无所有权) |

**示例:**
```rust
auto x# = 10        // 可变整数 (Mutable Integer)
x# = 11            // 允许修改 (OK)

auto ^p = new Rect  // Rect 的独占指针 (默认初始化)
auto ^#p2? = ...    // 可交换(指向可变)、可空、独占指针
```

## ✅ 项目状态 (路线图)

我们正在积极构建编译器的自举 (self-hosting) 能力。

- [x] **编译器基础设施**
    - [x] 词法分析器 (Lexer)
    - [x] 语法分析器 (Parser / AST Generation)
    - [x] LLVM IR 代码生成 (Code Generation)
- [x] **类型系统**
    - [x] 基础类型 (`i32`, `f64`, `bool` 等)
    - [x] 五态 Shape 系统 (Struct, Tuple, Array)
    - [x] **代数数据类型 (ADTs)** (通过 `shape`)
    - [x] 模式匹配 (`match` 语句)
- [x] **内存管理 (Memory Management)**
    - [x] 独占指针 (`^`) 与移动语义 (Move Semantics)
    - [x] 共享指针 (`~`) 与引用计数 (Reference Counting)
    - [x] **递归式释放 (Recursive Drop)** (Deep Drop)
    - [x] **Soul-Identity 内存模型** (不透明指针支持)
- [x] **面向对象特性**
    - [x] `impl` 块 (方法)
    - [x] **Trait 系统** (接口、默认实现)
- [x] **控制流表达式 (Control Flow Expressions)**
    - [x] 循环 (`while`, `for`, `loop`) 作为表达式
    - [x] 通过 `pass` 和 `break` 产生值
    - [x] 循环的 `or` 回退块
    - [x] `break`/`continue` 的定向标签 (Target Labels)
- [x] **模块与可见性 (Modules & Visibility)**
    - [x] 文件级模块 (File-based Modules)
    - [x] `import` 导入系统 (物理路径与逻辑导入)
    - [x] `pub` 可见性修饰符
- [x] **语义分析 (Sema)** *(核心完成 / Core Completed)*
    - [x] 基础设施脚手架 (Infrastructure Scaffolding)
    - [x] **严格的可变性强制检查** (`#` 检查)
    - [x] 类型检查 (Type Checking Pass)
    - [x] 所有权与借用验证 (Ownership & Borrowing Verification)
    - [x] **空安全 (Null Safety)** (`is` 操作符、严格判空)
    - [x] **资源安全分析 (Resource Safety)** (强制含资源 Shape 实现 `drop`)
- [x] **高级特性**
    - [x] **泛型 / 模板 (Generics)** (类型与函数)
    - [x] **自动析构合成 (Automatic Drop Synthesis)** (递归 Deep Drop)
    - [ ] 并发 (`Task`, `async`/`await`)
    - [ ] **标准库 (Standard Library)**
        - [x] 基础 I/O
        - [x] 内存管理
        - [x] `String` 类型

## 🛠 构建与使用

### 前置要求
- **C++17** 兼容的编译器 (Clang/GCC)
- **CMake** 3.15+
- **LLVM 17** (Libraries and Headers)

### 构建编译器
```bash
# 1. 创建构建目录
mkdir -p build && cd build

# 2. 通过 CMake 配置
cmake ..

# 3. 编译
make
```

### 运行 Toka 程序
目前，`tokac` 将 `.tk` 源文件编译为 LLVM IR (`.ll`)。你可以使用 LLVM 解释器 (`lli`) 执行它们，或者使用 `clang` 进一步编译。

**一键编译并运行:**
```bash
./build/src/tokac tests/test_trait.tk > output.ll && lli output.ll
```

## 📄 示例

**Traits 与 ADTs:**
```rust
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
        auto Stopped(code) => println("Stopped with {}", code)
        _ => println("Running...")
    }
}

fn null_safety() {
    auto ^?p = nullptr // 身份可空 (Identity is Nullable)
    
    // 1. 安全解包 (通过 'is')
    if ^?p is ^p {
        println("Not Null: {}", *p) // 只有在指针不为空时执行
    }
    
    // 2. 直接断言 (空则 Panic)
    // - 前缀 '??' 用于身份 (Pointer) 断言
    // - 后缀 '??' 用于灵魂 (Value) 断言
    auto ^must_ptr = ??p        
    auto val = some_val??
}
```

## 致敬与灵感来源 (Inspirations & Lineage)

Toka 是一门站在巨人肩膀上的现代系统级编程语言。它的诞生并非为了重新发明轮子，而是为了在前辈们的智慧基础上，探索**高性能**与**开发体验**之间的最佳平衡。

我们诚挚地致敬以下编程语言先驱，它们的设计哲学共同塑造了 Toka 的核心面貌：

### 1. C++ (Modern C++)
**核心致敬：智能指针与 RAII (The Origin)**
Toka 的内存管理机制直接继承自现代 C++ (C++11/14+) 的深厚积淀。
* **语法级智能指针**：Toka 不再将智能指针视为库功能的包装，而是将其直接**固化为核心语法**。使用前缀符号 `^T` (Unique) 和 `~T` (Shared)，让 RAII 成为默认且零视觉负担的编程模式，彻底告别冗长的模板代码 (如 `std::unique_ptr<T>`)。
* **底层掌控力**：Toka 保留了原始指针 (`*T`) 和对内存布局的精确控制，秉承 C++ "不为未使用的特性付费 (Zero-overhead abstraction)" 的实用主义精神。

### 2. Rust
**核心致敬：借用纪律 (The Borrowing Discipline)**
Toka 吸收了 Rust 在内存安全领域的关键洞见，但在实现路径上做出了不同的取舍。
* **借用规则**：我们采纳了 Rust 严格的 **“读写互斥”借用规则**（同一作用域内，要么有多个不可变引用，要么只有一个可变引用），以此在编译期杜绝数据竞争。
* **简化心智模型**：与 Rust 不同的是，Toka **刻意舍弃了显式的生命周期注解**（Explicit Lifetimes, 如 `<'a>`）。我们倾向于通过智能指针的所有权转移和词法作用域分析来管理生命周期，从而**显著**降低系统编程的学习曲线。

### 3. Haskell / ML Family
**核心致敬：类型类与正交设计 (Typeclasses & Orthogonality)**
Toka 的 Trait 系统在精神上继承自 **Haskell 的 Typeclasses**（而非传统的 OOP 接口）。
*   **数据与行为分离**：我们推崇将“数据定义”(`shape`)与“行为实现”(`impl`)完全剥离。这种设计赋予了 Toka 极强的扩展性——你甚至可以为标准库中的类型实现自定义的 Trait。
*   **代数数据类型 (ADTs)**：Toka 的 `shape` 完美复刻了 ML 家族对数据结构的精确建模能力。

### 4. Swift / Kotlin / C#
**核心致敬：空安全 (Null Safety)**
为了彻底解决“十亿美元的错误 (The Billion Dollar Mistake)”，Toka 采用了现代应用级语言通用的 **显式空安全** 设计。
* **类型级防御**：通过后缀修饰符 (`?`)，我们将空指针检查强制提升到了类型系统层面，而非依赖运行时的防御性编程。

### 5. Python
**核心致敬：可读性与开发效率 (Readability & Productivity)**
Toka 致力于将脚本语言的开发体验带入系统编程。我们借鉴了 Python **极简的语法风格**和“少即是多”的设计哲学，力求让系统级代码也拥有脚本般的清晰度与亲和力。

---

### 特别致敬 (Special Acknowledgement)

**AI 辅助工程 (AI-Assisted Engineering)**
Toka 语言的编译器实现——特别是类型系统的重构 (Type System Refactoring) 与代码生成适配 (CodeGen Adaptation)——是在 **Google Gemini** 的深度协作下完成的。

这种“人类架构师 + AI 结对编程”的开发模式，极大地加速了 Toka 从设计概念走向工业级实现的进程。所有的架构决策、设计哲学及核心逻辑均由人类作者主导，而 AI 则在代码实现与测试验证中提供了不可或缺的助力。