#include <yoga/Agent/AgentFirst.hpp>
#include <yoga/Agent/AgentRandom.hpp>
#include <yoga/Env/EnvCitycat.hpp>

#include <chrono>
#include <thread>

int main() {
  using namespace std::chrono_literals;

  const int nbSteps = 10;
  EnvCitycatConsole env(20, 30, 30, std::make_optional(42));
  AgentRandom agent({});
  // AgentFirst agent;

  for (int i=0; i<nbSteps; i++) {
    std::this_thread::sleep_for(1s);
    auto action = agent.genmove(env);
    env.step(action);
    env.render(std::cout);
    if (env.done())
      env.reset();
  }

  return 0;
}

