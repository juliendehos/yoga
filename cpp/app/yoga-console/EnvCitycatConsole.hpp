#pragma once

#include <map>

#include "RenderConsole.hpp"
#include <yoga/Env/EnvCitycat.hpp>

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

      os << "actionSpace:";
      for (auto a : _actionSpace._actions)
        os << " " << fmtAction[a];
      os << std::endl;

      os << "observationSpace:";
      os << " TODO" << std::endl;
      // TODO 

      os << "observation:" << std::endl;
      auto o = _observation;
      os << "  - left: " << fmtCell[o._left] << std::endl;
      os << "  - front: ";
      for (auto c : o._front)
        os << fmtCell[c];
      os << std::endl;
      os << "  - right: " << fmtCell[o._right] << std::endl;
      os << "  - vitality: " << o._vitality << std::endl;

      os << std::endl;
    }

};


