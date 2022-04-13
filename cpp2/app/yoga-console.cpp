#include <yoga/Agent/AgentRandom.hpp>
#include <yoga/Env/EnvCitycatConsole.hpp>

#include <chrono>
#include <thread>

int main() {
  using namespace std::chrono_literals;

  EnvCitycatConsole env(20, 30, 30, std::make_optional(42));
  AgentRandom<Citycat> agent({});

  const int nSims = 10;

  for (int iSims=0; iSims<nSims; iSims++) {
    int iSteps = 0;

    do {
      std::this_thread::sleep_for(200ms);
      auto action = agent.genAction(
          env.observationSpace(),
          env.observation(),
          env.actionSpace());
      env.step(action);
      iSteps++;

      std::cout << "iSims: " << iSims << std::endl;
      std::cout << "iSteps: " << iSteps << std::endl;
      env.render(std::cout);

    } while (not env.done());

    env.reset();
  }

  return 0;
}

