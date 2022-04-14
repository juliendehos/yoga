#pragma once

#include "../RenderSdl.hpp"
#include "EnvCartpole.hpp"

class EnvCartpoleSdl : public EnvCartpole, public RenderSdl {
  public:
    void render() const override {
    }

};
 

