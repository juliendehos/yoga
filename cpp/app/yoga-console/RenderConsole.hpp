#pragma once

#include <iostream>

#include <yoga/Env.hpp>

class RenderConsole {
    virtual void render(std::ostream & os) const = 0;
};

/*
template<typename T>
void printVector(std::ostream & os, const std::vector<T> & v, const std::string & prefix) {
  os << prefix;
  for (auto x : v)
    os << " " << x;
  os << std::endl;
}

void printPoint(std::ostream & os, const Point & p) {
  printVector(os, p._discrete, "- discrete:");
  printVector(os, p._box, "- box:");
}

void printSpace(std::ostream & os, const Space & s) {
  printVector(os, s._discreteMin, "- discreteMin:");
  printVector(os, s._discreteMax, "- discreteMax:");
  printVector(os, s._boxMin, "- boxMin:");
  printVector(os, s._boxMax, "- boxMax:");
}
*/

