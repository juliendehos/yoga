#pragma once

#include "RenderSdl.hpp"
#include <yoga/Env/EnvCartpole.hpp>

class EnvCartpoleSdl : public EnvCartpole, public RenderSdl {
  public:
    void render() const override {
    }

};
 

