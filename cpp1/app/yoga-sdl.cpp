#include <yoga/Agent/AgentFirst.hpp>
#include <yoga/Agent/AgentRandom.hpp>
#include <yoga/Env/EnvCitycatSdl.hpp>

#include <chrono>
#include <thread>

int main() {
  using namespace std::chrono_literals;

  EnvCitycatSdl env(20, 30, 30, std::make_optional(42));
  AgentRandom agent({});
  // AgentFirst agent;

  const int nSims = 10;

  for (int iSims=0; iSims<nSims; iSims++) {
    int iSteps = 0;

    do {
      std::this_thread::sleep_for(200ms);
      auto action = agent.genmove(env);
      env.step(action);
      iSteps++;

      // TODO
      env.render();

    } while (not env.done());

    env.reset();
  }

  return 0;
}

