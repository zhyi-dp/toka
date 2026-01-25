- 你是贤弟，我是大哥
- 默认使用英文（代码注释以及文档以你你自己的思考过程等），但对话框你需要给我传达的信息请使用中文，纯讨论时请使用中文
- 不要主动生成任何.txt扩展名文件，不要主动创建任何无扩展名的文件
- make 编译器方法：make -C build -j8 如果成功会生成可执行编译器 build/src/tokac
- pass 批量测试脚本：正向测试是tool/test_pass.sh 反向测试是tool/test_fail.sh
- 单个测试脚本：tool/test_single.sh tests/pass/xxx.tk
- 正向测试 tk 都放在 tests/pass/下
- 反向测试 tk 都放在 tests/fail/下
- 生成 IR 到.ll 文件：build/src/tokac tests/pass/xxx.tk > x.ll
- 执行.ll 文件：lli x.ll
- 每次完成阶段性的工作并且 pass 测试全部通过之前，请提交一次 commit，然后给出中文总结，并等待新的指示
- git commit -m 信息不能包含可能被 bash/zsh 等截断内容的符号
- 每次commit前运行 tool/test_pass.sh， 将测试结果以 [pass 71/79]的格式放在 commit 信息的最开始
- 如果遇到编译器代码出现大括号不匹配的问题，可以使用 tool/check_braces.py 工具辅助定位
- toka编译器的编译错误信息必须出自DiagnosticDefs.def，若有新的错误信息类型，请维护DiagnosticDefs.def先
- toka没有`let`关键字，而是`auto`关键字; toka的shape定义使用圆括号而不是大括号
- toka的指针形态符号(&^~*)用法跟其他语言有很大不同，你必须切记：在toka中 如果是一个指针，那他在任何地方必须携带指针形态符号，如果去掉指针形态符号就是解指针/解引用，记住了吗！！！！！
- toka与C++的解引用语法恰恰相反，必须时刻谨记：戴帽子 (*p)：这是指针本体（Handle/Pointer）。操作它是为了重绑定（Rebind），即改变指针指向的目标地址。摘帽子 (p)：这才是解引用（Soul/Value）。操作它是为了读写内存内容。
- toka中除了 函数返回签名指针形态符号在类型名前 外，指针形态符号始终在变量名前 而不是类型名前，如 ^p:Node 而不是 p:^Node
- toka 中的权限/属性符号  #$?!同样总是在变量名后或者指针形态符号之后（返回签名除外）
- toka 中对指针的成员访问 推荐:解引用后点访问 如 ptr.a; 也可以 指针形态->访问, 如 ^ptr->a 
- toka中，大括号仅用于作用域边界 类型级别名字的集合 和 字符串格式化输出的占位，其他情况不使用大括号。shape的定义以及实例初始化列表形式如 auto x = Point(x=1,y=2)均是圆括号，不要使用大括号
- toka 代码中如需使用 println 函数，必须使用 import std/io::println
- 为了自动化测试方便，目前我们所有的测试程序的 main 必须返回i32，正常时返回 0
- toka 使用统一的 `shape` 关键字定义所有结构化数据，通过括号内的结构自动判定实体种类。
| 语法特征 | 判定 Kind | 访问约束 |
| :--- | :--- | :--- |
| `shape Name(x: T, y: U)` | **Struct** | 直接通过 `.name` 访问 |
| `shape Name(T, U)` | **Tuple** | 通过 `.0`, `.1` 访问 |
| `shape Name(A \| B(T) \| C = N)` | **Enum (Tagged)** | **强制** 必须通过 `match` 或 `if auto` 访问 |
| `shape Name(as T \| as name: T)` | **Union** | `as` 重解释 (Bare) 或 `.name` (Named) |
| `alias Name = [T; N]` | **Array** | 通过 `[index]` 访问 |
- toka 的函数传参行为为 捕获（隐式引用传递），不涉及对实参的复制或转移。foo(^p) 不会消耗掉^p