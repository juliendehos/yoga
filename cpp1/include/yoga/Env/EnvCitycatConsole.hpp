#pragma once

#include "../RenderConsole.hpp"
#include "EnvCitycat.hpp"

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
  {EnvCitycat::Action::Front, "front"}
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
      os << "lastAction: ";
      auto la = lastAction();
      if (la) 
        os << fmtAction[*la] << std::endl;

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


