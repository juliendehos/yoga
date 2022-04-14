#pragma once

#include "Env.hpp"

template <typename T>
class Agent {
  public:
    virtual void learn(const Env<T> & env) = 0;

    virtual typename T::Action genAction(
        const typename T::ObservationSpace & os,
        const typename T::Observation & o,
        const typename T::ActionSpace & as) = 0;

};

