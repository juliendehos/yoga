#pragma once

#include "../Agent.hpp"

class AgentFirst : public Agent {
  public:
    AgentFirst(const Space & observationSpace, const Space & actionSpace) :
      Agent(observationSpace, actionSpace) 
    {}

    void learn(const Env & env) {
    }

    Point genAction(const Point & observations) {
      return {};
      // return actions._discreteMin;
    }

};

