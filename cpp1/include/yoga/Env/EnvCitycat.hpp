#pragma once

#include "../Utils/Random.hpp"

#include <cassert>
#include <vector>

// TODO test

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
    int _catI;
    int _catJ;
    int _catDi;
    int _catDj;
    int _vitality;

    const int _vitality0;
    const int _ni;
    const int _nj;
    std::vector<Cell> _board;

    Cell board(int i, int j) const {
      assert(0<=i and i<_ni);
      assert(0<=j and j<_nj);
      return _board[i*_nj + j];
    }

    Cell & board(int i, int j) {
      assert(0<=i and i<_ni);
      assert(0<=j and j<_nj);
      return _board[i*_nj + j];
    }

    double _score;
    bool _done;
    std::vector<Action> _actions;
    Observations _observations;

  public:
    void reset() {
      _vitality = _vitality0;
      _score = 0;
      _done = false;
      // TODO
      // catI
      // catJ
      // catDi
      // catDj
      // board
      // actions
      // observations
    }

    EnvCitycat(int ni, int nj, int vitality0, std::optional<uint64_t> s) :
      _random(s),
      _vitality0(vitality0),
      _ni(ni), 
      _nj(nj), 
      _board(ni*nj)
    {
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

class RenderConsole {
    virtual void render(std::ostream & os) const = 0;
};

// TODO RenderSdl

class EnvCitycatConsole : public EnvCitycat, public RenderConsole {
  public:
    EnvCitycatConsole(int ni, int nj, int vitality0, std::optional<uint64_t> s) :
      EnvCitycat(ni, nj, vitality0, s) {}

    void render(std::ostream & os) const override {
      os << "todo" << std::endl;
      // TODO
    }
};

