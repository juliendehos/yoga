#pragma once

template <typename T>
class Env {

  protected:
    double _score;
    bool _done;
    typename T::ObservationSpace _observationSpace;
    typename T::Observation _observation;
    typename T::ActionSpace _actionSpace;

  public:
    bool done() const {
      return _done;
    }

    double score() const {
      return _score;
    }

    const typename T::ObservationSpace & observationSpace() const {
      return _observationSpace;
    }

    const typename T::Observation & observation() const {
      return _observation;
    }

    const typename T::ActionSpace & actionSpace() const {
      return _actionSpace;
    }

    virtual void reset() = 0;

    virtual void step(const typename T::Action & action) = 0;

};

