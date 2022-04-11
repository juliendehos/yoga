#pragma once

#include "../Agent.hpp"

class AgentFirst : public Agent {
  public:

    void learn(const EnvCitycat & env) {}

    EnvCitycat::Action genmove(const EnvCitycat & env) override {
      return env.actions().front();
    }

};

