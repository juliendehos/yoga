#pragma once

#include "Env/EnvCitycat.hpp"

class Agent {
  public:
    virtual void learn(const EnvCitycat & env) = 0;
    virtual EnvCitycat::Action genmove(const EnvCitycat & env) = 0;
};

