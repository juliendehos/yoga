#pragma once

#include "../Agent.hpp"

class AgentFirst : public Agent {
  public:
    EnvCitycat::Action decide(const EnvCitycat & env) override {
      return env.actions().front();
    }
};

