#pragma once

#include "../Agent.hpp"
#include "../Utils/Random.hpp"

class AgentRandom : public Agent {
  private:
    Random _random;

  public:
    AgentRandom(std::optional<uint64_t> s) : _random(s) {}

    void learn(const EnvCitycat & env) {}

    EnvCitycat::Action genmove(const EnvCitycat & env) override {
      auto actions = env.actions();
      int k = _random.uniformInt(0, actions.size()); 
      return actions[k];
    }

};

