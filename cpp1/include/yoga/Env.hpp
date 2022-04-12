#pragma once

#include <Eigen/Dense>

#include <memory>
#include <vector>

struct Point {
  Eigen::VectorXi _discrete;
  Eigen::VectorXf _box;
};

struct Space {
  Eigen::VectorXi _discreteMin;
  Eigen::VectorXi _discreteMax;
  Eigen::VectorXf _boxMin;
  Eigen::VectorXf _boxMax;
};

class Env {

  protected:
    bool _done;
    double _score;
    Space _actionSpace;
    Point _observationPoint;
    Space _observationSpace;

  public:
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

