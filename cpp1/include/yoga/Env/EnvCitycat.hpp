#pragma once

#include <algorithm>
#include <cassert>
#include <deque>
#include <vector>

#include "../Env.hpp"
#include "../Random.hpp"

class EnvCitycat : public Env {
  public:
    enum class Move {Left, Front, Right};

    enum class Cell {Empty, Cat, Dog, Food, Wall} ;

    using ItemDeque = std::deque<std::pair<int, int>>;

    const double MAX_VITALITY = 1000; 

  protected:
    const int _ni;
    const int _nj;
    const double _startingVitality;
    const unsigned _itemCapacity;

    std::vector<Cell> _board;
    Random _random;
    std::optional<Move> _lastMove;

    int _catI;
    int _catJ;
    int _catDi;
    int _catDj;

    double _vitality;
    ItemDeque _foods;
    ItemDeque _dogs;

  private:

    Move actionToMove(const Point & action) const {
      assert(action._discrete.size() == 1);
      int v = action._discrete[0];
      if (v==0) return Move::Left;
      if (v==1) return Move::Front;
      if (v==2) return Move::Right;
      assert(false);
    }

    int cellToInt(EnvCitycat::Cell c) const {
      switch(c) {
        case Cell::Cat:   return 1;
        case Cell::Dog:   return 2;
        case Cell::Food:  return 3;
        case Cell::Wall:  return 4;
        default:          return 0;
      }
    }

    std::pair<int, int> moveToDij(Move move) const {
      switch(move) {
        case Move::Left: 
          return { -_catDj, _catDi };
        case Move::Right: 
          return { _catDj, -_catDi };
        default: 
          return { _catDi, _catDj };
      }
    }

    void updateObservations() {

      auto [diLeft, djLeft] = moveToDij(Move::Left);
      _observationPoint._discrete[0] = cellToInt(board(_catI+diLeft, _catJ+djLeft));

      // TODO
      /*
      auto [diRight, djRight] = actionToDij(Action::Right);
      _observations._right = board(_catI+diRight, _catJ+djRight);

      _observations._front.clear();
      for (int k=1; k<=3; k++) {
        auto c = board(_catI + k*_catDi, _catJ + k*_catDj);
        _observations._front.push_back(c);
        if (c == Cell::Wall)
          break;
      }
      */

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
    void reset() override {
      _vitality = _startingVitality;
      _score = 0;
      _done = false;
      _lastMove.reset();

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

    EnvCitycat(int ni, int nj, double startingVitality, std::optional<uint64_t> s) :
      Env({{0}, {2}, {}, {}}, {{0,0,0,0,0}, {2,2,2,2,2}, {0}, {MAX_VITALITY}}),
      _ni(ni+2), 
      _nj(nj+2), 
      _startingVitality(std::min(startingVitality, MAX_VITALITY)),
      _itemCapacity((_ni+_nj)/2),
      _board(_ni*_nj),
      _random(s)
    {
      _observationPoint._discrete.reserve(5);
      _observationPoint._box.reserve(1);
      reset();
    }

    void step(const Point & action) {
      assert(not _done);

      Move move = actionToMove(action);

      _vitality -= 1;
      _score += 1;
      _lastMove = std::make_optional(move);

      auto [di, dj] = moveToDij(move);

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
        _vitality = std::min(_vitality+5, MAX_VITALITY);
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

      if (_score > 100 or _vitality <= 0) {
        _done = true;
      }
      else {
        addItem(_foods, Cell::Food);
        addItem(_dogs, Cell::Dog);
      }

      updateObservations();
    }

};

