#include "Random.hpp"

Random::Random() : _engine(std::random_device{}()) {}

void Random::seed(uint64_t s) {
    _engine.seed(s);
}

int Random::uniformInt(int a, int b) {
    std::uniform_int_distribution<int> dist(a, b);
    return dist(_engine);
}

double Random::uniformDouble(double a, double b) {
    std::uniform_real_distribution<double> dist(a, b);
    return dist(_engine);
}

double Random::normalDouble(double m, double s) {
    std::normal_distribution<double> dist(m, s);
    return dist(_engine);
}

