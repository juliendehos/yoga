#pragma once

#include <vector>

#include "../Utils/Random.hpp"

class EnvCitycat {

  public:
    enum class Action {Left, Right, Straight};

    enum class Cell {Empty, Cat, Dog, Food, Wall} ;

    struct Observations {
      Cell _left;
      Cell _right;
      std::vector<Cell> _front;
    };

  protected:
    Random _random;
    int _catX;
    int _catY;
    int _catDx;
    int _catDy;
    double _vitality;

    double _score;
    bool _done;
    std::vector<Action> _actions;
    Observations _observations;

  public:
    void reset() {
      // TODO
    }

    EnvCitycat(uint64_t seed) {
      // TODO
      reset();
    }

    EnvCitycat() {
      // TODO
      reset();
    }

    void step(const Action & action) {
      // TODO
    }

    bool done() const {
      return _done;
    }

    double score() const {
      return _score;
    }

    const Observations & observations() const {
      return _observations;
    }

    const std::vector<Action> & actions() const {
      return _actions;
    }

};

#include <iostream>

class EnvCitycatConsole : public EnvCitycat {
  private:
    std::ostream & _os;

  public:
    EnvCitycatConsole(std::ostream & os) : _os(os) {}

    EnvCitycatConsole(std::ostream & os, uint64_t seed) : EnvCitycat(seed), _os(os) {}

    void render() {
      _os << "todo" << std::endl;
      // TODO
    }
};

