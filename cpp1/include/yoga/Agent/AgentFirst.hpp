#pragma once

#include "../Agent.hpp"

class AgentFirst : public Agent {
  public:
    AgentFirst(const Space & observationSpace, const Space & actionSpace) :
      Agent(observationSpace, actionSpace) 
    {}

    void learn(const Env & env) override {
    }

    Point genAction(const Point & observations) override {
      // Point p(_actionSpace.nd(), _actionSpace.nb());
      // p._discrete[0] = 1;
      // return p;

      return {_actionSpace._discreteMin, _actionSpace._boxMin};
    }

};

