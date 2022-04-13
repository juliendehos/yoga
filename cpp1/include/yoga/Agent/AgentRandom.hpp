#pragma once

#include <iostream>

#include "../Agent.hpp"
#include "../Random.hpp"

class AgentRandom : public Agent {
  private:
    Random _random;

  public:
    AgentRandom(const Space & observationSpace, const Space & actionSpace, std::optional<uint64_t> s) :
      Agent(observationSpace, actionSpace),
      _random(s)
    {}

    void learn(const Env & env) {
    }

    Point genAction(const Point & observations) {
      int nd = _actionSpace.nd();
      int nb = _actionSpace.nb();
      Point action(nd, nb);
      for (int i=0; i<nd; i++) {
        const int x0 = _actionSpace._discreteMin[i];
        const int x1 = _actionSpace._discreteMax[i];
        action._discrete[i] = _random.uniformInt(x0, x1);
      }
      for (int i=0; i<nb; i++) {
        const double x0 = _actionSpace._boxMin[i];
        const double x1 = _actionSpace._boxMax[i];
        action._box[i] = _random.uniformDouble(x0, x1);
      }
      return action;
    }

};

