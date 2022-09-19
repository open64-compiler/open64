#include <utility>
#include <tuple>

int foo() {
  int val1;
  int val2;
  std::tie(val1, val2) = std::pair<int, int>(3,4);
  return val2;
}
