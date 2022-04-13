#include <iostream>

template <typename T>
struct Bot {
  T space;
};

struct Space1 {
  int x;
  Space1 sample() {
    return {0};
  }
};

struct Space2 {
  int x;
  int y;
  Space2 sample() {
    return {1, 2};
  }
};

template <typename T>
T sample2(T & space) {
  return space.sample();
}

template <>
Space2 sample2<Space2>(Space2 & space) {
  return {3,4};
}

int main() {

  Bot<Space1> b1;
  auto v1 = b1.space.sample();
  std::cout << v1.x << std::endl;
  auto v1b = sample2(v1);
  std::cout << v1b.x << std::endl;

  Bot<Space2> b2;
  auto v2 = b2.space.sample();
  std::cout << v2.x << " " << v2.y << std::endl;
  auto v2b = sample2(v2);
  std::cout << v2b.x << " " << v2b.y << std::endl;

  return 0;
}

