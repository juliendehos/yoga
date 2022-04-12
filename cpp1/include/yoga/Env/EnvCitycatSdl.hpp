#pragma once

#include "../RenderSdl.hpp"
#include "EnvCitycat.hpp"

class EnvCitycatSdl : public EnvCitycat, public RenderSdl {
  public:
    EnvCitycatSdl(int ni, int nj, int startingVitality, std::optional<uint64_t> s) :
      EnvCitycat(ni, nj, startingVitality, s) {}

    void render() const override {
    }

};
 
