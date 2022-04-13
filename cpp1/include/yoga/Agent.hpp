#pragma once

#include <cassert>

#include "Env.hpp"

class Agent {
  protected:
    const Space _observationSpace;
    const Space _actionSpace;

  public:
    Agent(const Space & observationSpace, const Space & actionSpace) :
      _observationSpace(observationSpace),
      _actionSpace(actionSpace)
    {
      assert(actionSpace._discreteMin.size() == actionSpace._discreteMax.size());
      assert(actionSpace._boxMin.size() == actionSpace._boxMax.size());
    }

    virtual void learn(const Env & env) = 0;

    virtual Point genAction(const Point & observationPoint) = 0;

};

