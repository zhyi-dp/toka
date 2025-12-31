[English](README.md)

# Toka 编程语言

Toka 是一门由易中华 (YiZhonghua) 于 2025 年创造的系统级编程语言。它旨在实现 **安全**、**高效** 和 **语法简洁**，并通过其创新的 **属性标记系统 (Attribute Token System)** 解了传统编程中安全性与生产力之间的权衡难题。

## 🌟 核心理念：属性标记 (Attribute Tokens)

Toka 通过正交的后缀标记让内存属性显式化，消除了隐藏的内存状态。这使得你一眼就能读懂内存的使用“形状”。

| 标记 (Token) | 内容上的含义 (Value/Content) | 身份上的含义 (Identity/Address) |
| :--- | :--- | :--- |
| `#` | **可写**: 可修改字段/内容 | **可交换**: 可重定向(Reseat) |
| `?` | **可选**: 可为 `none` | **可空**: 可为 `null` |
| `^` | - | **独占指针** (所有权) |
| `~` | - | **共享指针** (引用计数) |

**示例:**
```scala
let x# = 10;        // 可变整数 (Mutable Integer)
x# = 11;            // 允许修改 (OK)

let ^p = new Rect;  // Rect 的独占指针 (Unique Pointer)
let ^#p2? = ...;    // 可交换(指向可变)、可空、独占指针
```

## ✅ 项目状态 (路线图)

我们正在积极构建编译器的自举 (self-hosting) 能力。

- [x] **编译器基础设施**
    - [x] 词法分析器 (Lexer)
    - [x] 语法分析器 (Parser / AST Generation)
    - [x] LLVM IR 代码生成 (Code Generation)
- [x] **类型系统**
    - [x] 基础类型 (`i32`, `f64`, `bool` 等)
    - [x] 结构体 (Structs) 与成员访问
    - [x] **代数数据类型 (ADTs)** (`option`, `enum`)
    - [x] 模式匹配 (`match` 语句)
- [x] **内存管理 (Memory Management)**
    - [x] 独占指针 (`^`) 与移动语义 (Move Semantics)
    - [x] 共享指针 (`~`) 与引用计数 (Reference Counting)
- [x] **面向对象特性**
    - [x] `impl` 块 (方法)
    - [x] **Trait 系统** (接口、默认实现)
- [ ] **语义分析 (Sema)** *(进行中)*
    - [x] 基础设施脚手架 (Infrastructure Scaffolding)
    - [x] **严格的可变性强制检查** (`#` 检查)
    - [x] 类型检查 (Type Checking Pass)
    - [x] 所有权与借用验证 (Ownership & Borrowing Verification)
    - [x] **空安全 (Null Safety)** (`is` 操作符、严格判空)
- [ ] **高级特性**
    - [ ] 泛型 / 模板 (Generics)
    - [ ] 并发 (`Task`, `async`/`await`)
    - [ ] 标准库 (Standard Library)

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
}

fn null_safety() {
    let ^?p = null; // 身份可空 (Identity is Nullable)
    if ^?p is ^p {
        printf("Not Null!\n"); // 只有在指针不为空时执行
    }
    
    let obj! = none; // 内容可空 (Value is Nullable)
    if obj! is obj {
        printf("Object exists!\n");
    }
}
```
