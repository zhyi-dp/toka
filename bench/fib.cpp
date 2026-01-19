#include <iostream>
long long fib(long long n) {
  if (n < 2) {
    return n;
  }
  return fib(n - 1) + fib(n - 2);
}

int main() {
  long long start_n = 45;
  long long res = fib(start_n);
  if (res == 0xDEADBEEF) {
    std::cout << "never see me" << std::endl;
  }
  return 0;
}
