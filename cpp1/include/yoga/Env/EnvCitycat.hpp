#pragma once

#include "../Utils/Random.hpp"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <vector>

// TODO test
// TODO RenderSdl

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
    const int _ni;
    const int _nj;
    const int _objectCapacity;
    const int _startingVitality;

  private:
    std::vector<Cell> _board;
    Random _random;
    double _score;
    bool _done;
    std::vector<Action> _actions;
    Observations _observations;

    int _catI;
    int _catJ;
    int _catDi;
    int _catDj;
    int _vitality;

  private:

    void updateObservations() {
      _observations._left = Cell::Dog;
      _observations._right = Cell::Food;
      _observations._front = {Cell::Empty, Cell::Wall};
    }

    Cell & board(int i, int j) {
      assert(0<=i and i<_ni);
      assert(0<=j and j<_nj);
      return _board[i*_nj + j];
    }

  protected:
    Cell board(int i, int j) const {
      assert(0<=i and i<_ni);
      assert(0<=j and j<_nj);
      return _board[i*_nj + j];
    }

  public:
    void reset() {
      _vitality = _startingVitality;
      _score = 0;
      _done = false;

      _catI = _ni/2 + _random.uniformInt(-2, 2);
      _catJ = _nj/2 + _random.uniformInt(-2, 2);
      _catDi = -1;
      _catDj = 0;

      // board
      std::fill(_board.begin(), _board.end(), Cell::Empty);
      for (int i=0; i<_ni; i++) {
        board(i, 0) = Cell::Wall;
        board(i, _nj-1) = Cell::Wall;
      }
      for (int j=0; j<_nj; j++) {
        board(0, j) = Cell::Wall;
        board(_ni-1, j) = Cell::Wall;
      }
      board(_catI, _catJ) = Cell::Cat;

      // TODO add food
      // TODO add dog

      updateObservations();
    }

    EnvCitycat(int ni, int nj, int startingVitality, std::optional<uint64_t> s) :
      _ni(ni+2), 
      _nj(nj+2), 
      _objectCapacity((_ni+_nj)/2),
      _startingVitality(startingVitality),
      _board(_ni*_nj),
      _random(s),
      _actions({Action::Left, Action::Right, Action::Straight})
    {
      _observations._front.reserve(3);
      reset();
    }

    void step(const Action & action) {
      _vitality -= 1;
      _score += 1;

      int di, dj;
      if (action == Action::Left) {
        di = -_catDj;
        dj = _catDi;
      }
      else if (action == Action::Right) {
        di = _catDj;
        dj = -_catDi;
      }
      else {
        di = _catDi;
        dj = _catDj;
      }

      int i = _catI + di;
      int j = _catJ + dj;
      Cell & c = board(i, j);

      if (c == Cell::Dog) {
        _score -= 5;
        _done = true;
        return;
      }

      if (c == Cell::Food) {
        _score += 10;
        _vitality += 5;
      }

      if (c == Cell::Wall) {
        _score -= 2;
      }
      else {
        board(_catI, _catJ) = Cell::Empty;
        board(i, j) = Cell::Cat;
        _catI = i;
        _catJ = j;
        _catDi = di;
        _catDj = dj;
      }

      if (_score > 100) {
        _done = true;
      }
      else {
      // TODO add food
      // TODO add dog
      }
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

class RenderConsole {
    virtual void render(std::ostream & os) const = 0;
};

std::map<EnvCitycat::Cell, char> fmtCell = {
  {EnvCitycat::Cell::Empty, '.'},
  {EnvCitycat::Cell::Cat, 'C'},
  {EnvCitycat::Cell::Dog, 'D'},
  {EnvCitycat::Cell::Food, 'F'},
  {EnvCitycat::Cell::Wall, '#'}
};

std::map<EnvCitycat::Action, std::string> fmtAction = {
  {EnvCitycat::Action::Left, "left"},
  {EnvCitycat::Action::Right, "right"},
  {EnvCitycat::Action::Straight, "straight"}
};

class EnvCitycatConsole : public EnvCitycat, public RenderConsole {
  public:
    EnvCitycatConsole(int ni, int nj, int startingVitality, std::optional<uint64_t> s) :
      EnvCitycat(ni, nj, startingVitality, s) {}

    void render(std::ostream & os) const override {
      for (int i=0; i<_ni; i++) {
        for (int j=0; j<_nj; j++) {
          os << fmtCell[board(i, j)];
        }
        os << std::endl;
      }
      os << "score: " << score() << std::endl;
      os << "done: " << done() << std::endl;

      os << "actions:";
      for (auto a : actions())
        os << " " << fmtAction[a];
      os << std::endl;

      os << "observations:" << std::endl;
      auto o = observations();
      os << "  - left: " << fmtCell[o._left] << std::endl;
      os << "  - front: ";
      for (auto c : o._front)
        os << fmtCell[c];
      os << std::endl;
      os << "  - right: " << fmtCell[o._right] << std::endl;

      os << std::endl;
    }

};

