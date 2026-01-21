#include <cassert>
#include <iostream>
#include <memory>
#include <string>

// Toka: shape Node(val: i32, ~?next: Node)
struct Node {
  int val;
  // 灵魂层后缀默认为 $ (Immutable)，所以使用 const Node
  std::shared_ptr<const Node> next;

  Node(int v, std::shared_ptr<const Node> n) : val(v), next(n) {}

  // Toka: fn unwrap(~?self: Node) -> &Node
  const Node &unwrap() const {
    if (this)
      return *this; // 模拟 if is 提升
    assert(false && "unwrap null");
    return *this;
  }
};

// Toka: fn unwrap(~?p: Node) -> &Node
const Node &unwrap_global(std::shared_ptr<const Node> p) {
  if (p)
    return *p;
  assert(false && "unwrap null");
  return *p;
}

int main() {
  auto count = 100000;
  // Toka: auto ~#head = new Node(val=0, ~?next=nullptr)
  // 前缀 # 对应 C++ 的非 const 变量，允许重绑定
  auto head = std::make_shared<const Node>(1, nullptr);

  // std::cout << "head: " << head->val << std::endl;
  // assert(head->val == 0);

  // Push 1 到 5
  for (int i = 1; i <= count; ++i) {
    // Toka: auto ~next = ~head (Copy 语义，RC +1)
    auto next = head;
    // Toka: ~#head = new Node(...) (Reseat，旧值 RC-1, 新值接管)
    head = std::make_shared<const Node>(1, next);

    // std::cout << "[" << i << "]pushed: " << head->val << std::endl;
    // assert(head->val == i);
  }

  // --- 遍历逻辑 ---
  // Toka: auto ~#cursor = ~head
  auto cursor = head;
  int i = count + 1;
  size_t sum = 0;
  while (true) {
    // assert(i >= 0 && "check i >= 0");
    //  std::cout << "[" << i << "]val: " << cursor->val << std::endl;
    //  assert(cursor->val == i && "check cursor.val");

    if (i == 1)
      break;

    sum += cursor->val;

    i -= 1;

    // Toka: if cursor.~?next is cursor.~next
    // 你强调的“路径提升”：直接检查并重绑定，不产生中间临时句柄
    if (cursor->next != nullptr) {
      // 这里等效于 Toka 的 ~#cursor = cursor.~next
      // C++ 的 operator= 会处理：1. 新值 IncRef, 2. 旧值 DecRef
      // 没有任何“中间人”变量产生的额外 +1/-1
      cursor = cursor->next;
    } else {
      break;
    }
  }

  std::cout << "Sum: " << sum << std::endl;

  return 0;
}