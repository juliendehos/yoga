#pragma once

#include <iostream>

class RenderConsole {
    virtual void render(std::ostream & os) const = 0;
};

