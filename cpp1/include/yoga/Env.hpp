#pragma once

#include <memory>
#include <vector>

struct Point {
  std::vector<int> _discrete;
  std::vector<double> _box;

  Point(const std::vector<int> & d, const std::vector<double> & b) :
    _discrete(d),
    _box(b)
  {}

  Point(int nd, int nb) {
    _discrete.resize(nd);
    _box.resize(nb);
  }

};

struct Space {
  std::vector<int> _discreteMin;
  std::vector<int> _discreteMax;
  std::vector<double> _boxMin;
  std::vector<double> _boxMax;

  int nd() const {
    return _discreteMin.size();
  }

  int nb() const {
    return _boxMin.size();
  }

};

class Env {

  protected:
    const Space _actionSpace;
    const Space _observationSpace;
    Point _observationPoint;
    double _score;
    bool _done;

  public:
    Env(const Space & actionSpace, const Space & observationSpace) :
      _actionSpace(actionSpace),
      _observationSpace(observationSpace),
      _observationPoint(_observationSpace.nd(), _observationSpace.nb())
    {}

    virtual void reset() = 0;

    virtual void step(const Point & action) = 0;
  
    bool done() const {
      return _done;
    }

    double score() const {
      return _score;
    }

    const Point & observationPoint() const {
      return _observationPoint;
    }

    const Space & observationSpace() const {
      return _observationSpace;
    }

    const Space & actionSpace() const {
      return _actionSpace;
    }

};

