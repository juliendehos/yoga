#pragma once

#include <map>

#include "../RenderConsole.hpp"
#include "EnvCitycat.hpp"

std::map<Citycat::Cell, char> fmtCell = {
  {Citycat::Cell::Empty, '.'},
  {Citycat::Cell::Cat, 'C'},
  {Citycat::Cell::Dog, 'D'},
  {Citycat::Cell::Food, 'F'},
  {Citycat::Cell::Wall, '#'}
};

std::map<Citycat::Action, std::string> fmtAction = {
  {Citycat::Action::Left, "left"},
  {Citycat::Action::Right, "right"},
  {Citycat::Action::Front, "front"}
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
      os << "lastAction: ";
      if (_lastAction) 
        os << fmtAction[*_lastAction] << std::endl;
      os << "position: " << _catI << " " << _catJ << std::endl;
      os << "direction: " << _catDi << " " << _catDj << std::endl;

      os << "actionSpace:\n";
      // TODO printSpace(std::cout, _actionSpace);

      os << "observationSpace:\n";
      // TODO printSpace(std::cout, _observationSpace);

      os << "observationPoint:\n";
      // TODO printPoint(std::cout, _observationPoint);

      os << std::endl;
    }

};


