#pragma once

#include "../Utils/Random.hpp"

#include <algorithm>
#include <cassert>
#include <deque>
#include <vector>

class EnvCitycat {
  public:
    enum class Action {Left, Right, Front};

    enum class Cell {Empty, Cat, Dog, Food, Wall} ;

    struct Observations {
      Cell _left;
      Cell _right;
      std::vector<Cell> _front;
      int _i;
      int _j;
      int _di;
      int _dj;
    };

    using ItemDeque = std::deque<std::pair<int, int>>;

  protected:
    const int _ni;
    const int _nj;
    const int _startingVitality;
    const unsigned _itemCapacity;

  private:
    std::vector<Cell> _board;
    Random _random;
    double _score;
    bool _done;
    std::vector<Action> _actions;
    Observations _observations;
    std::optional<Action> _lastAction;

    int _catI;
    int _catJ;
    int _catDi;
    int _catDj;
    int _vitality;
    ItemDeque _foods;
    ItemDeque _dogs;

  private:

    std::pair<int, int> actionToDij(Action action) const {
      switch(action) {
        case Action::Left: 
          return { -_catDj, _catDi };
        case Action::Right: 
          return { _catDj, -_catDi };
        default: 
          return { _catDi, _catDj };
      }
    }

    void updateObservations() {

      auto [diLeft, djLeft] = actionToDij(Action::Left);
      _observations._left = board(_catI+diLeft, _catJ+djLeft);

      auto [diRight, djRight] = actionToDij(Action::Right);
      _observations._right = board(_catI+diRight, _catJ+djRight);

      // TODO
      _observations._front = {Cell::Empty, Cell::Wall};

      _observations._i = _catI;
      _observations._j = _catJ;
      _observations._di = _catDi;
      _observations._dj = _catDj;
    }

    Cell & board(int i, int j) {
      assert(0<=i and i<_ni);
      assert(0<=j and j<_nj);
      return _board[i*_nj + j];
    }

    void addItem(ItemDeque & items, Cell cell) {
      // remove the oldest item if the queue is full
      if (items.size() >= _itemCapacity) {
        auto [i,j] = items.front();
        assert(board(i, j) == cell);
        board(i, j) = Cell::Empty;
        items.pop_front();
      }
      // add one new item using rejection sampling
      while (true) {
        int i = _random.uniformInt(1, _ni-2);
        int j = _random.uniformInt(1, _nj-2);
        if (board(i, j) == Cell::Empty) {
          board(i, j) = cell;
          items.push_back({i, j});
          break;
        }
      }
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
      _lastAction.reset();

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

      _foods.clear();
      _dogs.clear();
      for (unsigned i=0; i<_itemCapacity/2; i++) {
        addItem(_foods, Cell::Food);
        addItem(_dogs, Cell::Dog);
      }

      updateObservations();
    }

    EnvCitycat(int ni, int nj, int startingVitality, std::optional<uint64_t> s) :
      _ni(ni+2), 
      _nj(nj+2), 
      _startingVitality(startingVitality),
      _itemCapacity((_ni+_nj)/2),
      _board(_ni*_nj),
      _random(s),
      _actions({Action::Left, Action::Right, Action::Front})
    {
      _observations._front.reserve(3);
      reset();
    }

    void step(const Action & action) {
      assert(not _done);

      _vitality -= 1;
      _score += 1;
      _lastAction = std::make_optional(action);

      auto [di, dj] = actionToDij(action);

      int i = _catI + di;
      int j = _catJ + dj;
      Cell & c = board(i, j);

      if (c == Cell::Dog) {
        _score -= 5;
        _done = true;
        updateObservations();
        return;
      }

      if (c == Cell::Food) {
        _score += 10;
        _vitality += 5;
        auto it = std::find(_foods.begin(), _foods.end(), std::make_pair(i, j));
        _foods.erase(it);
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
        addItem(_foods, Cell::Food);
        addItem(_dogs, Cell::Dog);
      }

      updateObservations();
    }

    bool done() const {
      return _done;
    }

    double score() const {
      return _score;
    }

    std::optional<Action> lastAction() const {
      return _lastAction;
    }

    const Observations & observations() const {
      return _observations;
    }

    const std::vector<Action> & actions() const {
      return _actions;
    }

};

