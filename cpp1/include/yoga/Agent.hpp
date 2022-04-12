#pragma once

#include "Env.hpp"

class Agent {
  protected:
    const Space _observationSpace;
    const Space _actionSpace;

  public:
    Agent(const Space & observationSpace, const Space & actionSpace) :
      _observationSpace(observationSpace),
      _actionSpace(actionSpace)
    {}

    virtual void learn(const Env & env) = 0;

    virtual Point genAction(const Point & observationPoint) = 0;

};

