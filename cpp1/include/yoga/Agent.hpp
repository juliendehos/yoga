#pragma once

#include "Env/EnvCitycat.hpp"

class Agent {
  public:
    virtual EnvCitycat::Action decide(const EnvCitycat & env) = 0;
};

