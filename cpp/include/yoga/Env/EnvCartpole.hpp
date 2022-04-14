#pragma once

#include <algorithm>
#include <cassert>
#include <deque>
#include <vector>

#include "../Env.hpp"
#include "../Random.hpp"

struct Cartpole {

    enum class Action {Left, Nothing, Right};

    struct ActionSpace {
      const std::vector<Action> _actions 
        {Action::Left, Action::Nothing, Action::Right};

      Action sample(Random & random) const {
        return random.uniformChoice(_actions);
      }
    };

    struct Observation {
      // TODO
    };

    struct ObservationSpace {
      // TODO
    };

};

class EnvCartpole : public Env<Cartpole> {
  public:

    void reset() override {
    }

    void step(const Cartpole::Action & action) {
    }

};


