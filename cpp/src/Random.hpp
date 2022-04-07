#pragma once

#include <random>

class Random {
    private:
        std::mt19937 _engine;

    public:
        // Random() = default;
        Random();
        Random(const Random &) = delete;

        void seed(uint64_t s);

        int uniformInt(int a, int b);
        double uniformDouble(double a, double b);
        double normalDouble(double m, double s);
};

