#pragma once

#include <iostream>

#include "../Agent.hpp"
#include "../Random.hpp"

template <typename T>
class AgentRandom : public Agent<T> {
  private:
    Random _random;

  public:
    AgentRandom(std::optional<uint64_t> s) :
      _random(s)
    {}

    void learn(const Env<T> & env) override {
    }

    typename T::Action genAction(
        const typename T::ObservationSpace & os,
        const typename T::Observation & o,
        const typename T::ActionSpace & as) override {
      return as.sample(_random);
    }

};

