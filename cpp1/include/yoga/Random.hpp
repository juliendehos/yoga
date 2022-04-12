#pragma once

#include <optional>
#include <random>

class Random {
  private:
    std::mt19937 _engine;

  public:
    Random(std::optional<uint64_t> s) : _engine(std::random_device{}()) {
      if (s)
        _engine.seed(*s);
    }

    Random(const Random &) = delete;

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

