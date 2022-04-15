#pragma once

#include <algorithm>
#include <cassert>
#include <deque>
#include <vector>

#include "../Env.hpp"
#include "../Random.hpp"

struct Citycat {
    enum class Cell {Empty, Cat, Dog, Food, Wall} ;

    enum class Action {Left, Front, Right};

    struct ActionSpace {
      const std::vector<Action> _actions 
        {Action::Left, Action::Front, Action::Right};

      Action sample(Random & random) const {
        return random.uniformChoice(_actions);
      }
    };

    struct Observation {
      Cell _left;
      Cell _right;
      std::vector<Cell> _front;
      double _vitality;
    };

    struct ObservationSpace {
      // TODO
    };

};

class EnvCitycat : public Env<Citycat> {
  public:

    using ItemDeque = std::deque<std::pair<int, int>>;

    static constexpr double MAX_SCORE = 100; 

  protected:
    const int _ni;
    const int _nj;
    const double _startingVitality;
    const unsigned _itemCapacity;

    std::vector<Citycat::Cell> _board;
    Random _random;
    std::optional<Citycat::Action> _lastAction;

    int _catI;
    int _catJ;
    int _catDi;
    int _catDj;

    double _vitality;
    ItemDeque _foods;
    ItemDeque _dogs;

  private:

    int cellToInt(Citycat::Cell c) const {
      switch(c) {
        case Citycat::Cell::Cat:  return 1;
        case Citycat::Cell::Dog:  return 2;
        case Citycat::Cell::Food: return 3;
        case Citycat::Cell::Wall: return 4;
        default:                  return 0;
      }
    }

    std::pair<int, int> actionToDij(Citycat::Action action) const {
      switch(action) {
        case Citycat::Action::Left: 
          return { -_catDj, _catDi };
        case Citycat::Action::Right: 
          return { _catDj, -_catDi };
        default: 
          return { _catDi, _catDj };
      }
    }

    void updateObservations() {

      auto [diLeft, djLeft] = actionToDij(Citycat::Action::Left);
      _observation._left = board(_catI+diLeft, _catJ+djLeft);

      auto [diRight, djRight] = actionToDij(Citycat::Action::Right);
      _observation._right = board(_catI+diRight, _catJ+djRight);

      _observation._front.clear();
      for (int k=1; k<=3; k++) {
        auto c = board(_catI + k*_catDi, _catJ + k*_catDj);
        _observation._front.push_back(c);
        if (c == Citycat::Cell::Wall)
          break;
      }

      _observation._vitality = _vitality;
    }

    Citycat::Cell & board(int i, int j) {
      assert(0<=i and i<_ni);
      assert(0<=j and j<_nj);
      return _board[i*_nj + j];
    }

    void addItem(ItemDeque & items, Citycat::Cell cell) {
      // remove the oldest item if the queue is full
      if (items.size() >= _itemCapacity) {
        auto [i,j] = items.front();
        assert(board(i, j) == cell);
        board(i, j) = Citycat::Cell::Empty;
        items.pop_front();
      }
      // add one new item using rejection sampling
      while (true) {
        int i = _random.uniformInt(1, _ni-2);
        int j = _random.uniformInt(1, _nj-2);
        if (board(i, j) == Citycat::Cell::Empty) {
          board(i, j) = cell;
          items.push_back({i, j});
          break;
        }
      }
    }

  protected:
    Citycat::Cell board(int i, int j) const {
      assert(0<=i and i<_ni);
      assert(0<=j and j<_nj);
      return _board[i*_nj + j];
    }

  public:
    void reset() override {
      _vitality = _startingVitality;
      _score = 0;
      _done = false;
      _lastAction.reset();

      _catI = _ni/2 + _random.uniformInt(-2, 2);
      _catJ = _nj/2 + _random.uniformInt(-2, 2);
      _catDi = -1;
      _catDj = 0;

      // board
      std::fill(_board.begin(), _board.end(), Citycat::Cell::Empty);
      for (int i=0; i<_ni; i++) {
        board(i, 0) = Citycat::Cell::Wall;
        board(i, _nj-1) = Citycat::Cell::Wall;
      }
      for (int j=0; j<_nj; j++) {
        board(0, j) = Citycat::Cell::Wall;
        board(_ni-1, j) = Citycat::Cell::Wall;
      }
      board(_catI, _catJ) = Citycat::Cell::Cat;

      _foods.clear();
      _dogs.clear();
      for (unsigned i=0; i<_itemCapacity/2; i++) {
        addItem(_foods, Citycat::Cell::Food);
        addItem(_dogs, Citycat::Cell::Dog);
      }

      updateObservations();
    }

    EnvCitycat(int ni, int nj, double startingVitality, std::optional<uint64_t> s) :
      _ni(ni+2), 
      _nj(nj+2), 
      _startingVitality(startingVitality),
      _itemCapacity((_ni+_nj)/2),
      _board(_ni*_nj),
      _random(s)
    {
      reset();
    }

    void step(const Citycat::Action & action) {
      assert(not _done);

      _vitality -= 1;
      _score += 1;
      _lastAction = std::make_optional(action);

      auto [di, dj] = actionToDij(action);

      int i = _catI + di;
      int j = _catJ + dj;
      Citycat::Cell & c = board(i, j);

      if (c == Citycat::Cell::Dog) {
        _score -= 5;
        _done = true;
        return;
      }

      if (c == Citycat::Cell::Food) {
        _score += 10;
        _vitality += 5;
        auto it = std::find(_foods.begin(), _foods.end(), std::make_pair(i, j));
        _foods.erase(it);
      }

      _catDi = di;
      _catDj = dj;

      if (c == Citycat::Cell::Wall) {
        _score -= 2;
      }
      else {
        board(_catI, _catJ) = Citycat::Cell::Empty;
        board(i, j) = Citycat::Cell::Cat;
        _catI = i;
        _catJ = j;
      }

      if (_score >= MAX_SCORE or _vitality <= 0) {
        _done = true;
        return;
      }

      addItem(_foods, Citycat::Cell::Food);
      addItem(_dogs, Citycat::Cell::Dog);
      updateObservations();
    }

};

