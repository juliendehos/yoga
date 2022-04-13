#pragma once

#include <map>

#include "../RenderConsole.hpp"
#include "EnvCitycat.hpp"

std::map<EnvCitycat::Cell, char> fmtCell = {
  {EnvCitycat::Cell::Empty, '.'},
  {EnvCitycat::Cell::Cat, 'C'},
  {EnvCitycat::Cell::Dog, 'D'},
  {EnvCitycat::Cell::Food, 'F'},
  {EnvCitycat::Cell::Wall, '#'}
};

std::map<EnvCitycat::Move, std::string> fmtMove = {
  {EnvCitycat::Move::Left, "left"},
  {EnvCitycat::Move::Right, "right"},
  {EnvCitycat::Move::Front, "front"}
};

class EnvCitycatConsole : public EnvCitycat, public RenderConsole {
  public:
    EnvCitycatConsole(int ni, int nj, double startingVitality, std::optional<uint64_t> s) :
      EnvCitycat(ni, nj, startingVitality, s) {}

    void render(std::ostream & os) const override {
      for (int i=0; i<_ni; i++) {
        for (int j=0; j<_nj; j++) {
          os << fmtCell[board(i, j)];
        }
        os << std::endl;
      }
      os << "score: " << _score << std::endl;
      os << "done: " << _done << std::endl;
      os << "lastMove: ";
      if (_lastMove) 
        os << fmtMove[*_lastMove] << std::endl;

      /*
      os << "actions:";
      for (auto & a : _actions)
        os << " " << fmtMove[a];
      os << std::endl;

      os << "observations:" << std::endl;
      auto o = _observations;
      os << "  - cat: " 
        << o._i << "," << o._j << " " 
        << o._di << "," << o._dj << std::endl;
      os << "  - left: " << fmtCell[o._left] << std::endl;
      os << "  - front: ";
      for (auto c : o._front)
        os << fmtCell[c];
      os << std::endl;
      os << "  - right: " << fmtCell[o._right] << std::endl;
      */

      os << std::endl;
    }

};


