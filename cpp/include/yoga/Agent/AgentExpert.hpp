#pragma once

#include <iostream>

#include "../Agent.hpp"

#include "../Env/EnvCitycat.hpp"

class AgentExpertCitycat : public Agent<Citycat> {
  public:
    void learn(const Env<Citycat> & env) override {
    }

    Citycat::Action genAction(
        const Citycat::ObservationSpace & os,
        const Citycat::Observation & o,
        const Citycat::ActionSpace & as) override {

      const auto front1 = o._front[0];
      if (front1 == Citycat::Cell::Food or front1 == Citycat::Cell::Empty)
        return Citycat::Action::Front;
      else
        return Citycat::Action::Left;
    }

};


