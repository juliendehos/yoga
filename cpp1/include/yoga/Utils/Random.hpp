#pragma once

#include <random>

class Random {
  private:
    std::mt19937 _engine;

  public:
    // Random() = default;
    Random() : _engine(std::random_device{}()) {}

    Random(const Random &) = delete;

    void seed(uint64_t s) {
      _engine.seed(s);
    }

    int uniformInt(int a, int b) {
      std::uniform_int_distribution<int> dist(a, b);
      return dist(_engine);
    }

    double uniformDouble(double a, double b) {
      std::uniform_real_distribution<double> dist(a, b);
      return dist(_engine);
    }

    double normalDouble(double m, double s) {
      std::normal_distribution<double> dist(m, s);
      return dist(_engine);
    }

};

