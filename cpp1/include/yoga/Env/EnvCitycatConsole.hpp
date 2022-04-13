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
      os << "position: " << _catI << " " << _catJ << std::endl;
      os << "direction: " << _catDi << " " << _catDj << std::endl;

      os << "actionSpace:\n";
      printSpace(std::cout, _actionSpace);

      os << "observationSpace:\n";
      printSpace(std::cout, _observationSpace);

      os << "observationPoint:\n";
      printPoint(std::cout, _observationPoint);

      os << std::endl;
    }

};


