#include <yoga/Agent/AgentFirst.hpp>
#include <yoga/Env/EnvCitycat.hpp>

#include <iostream>

int main() {

  EnvCitycatConsole env(std::cout);
  AgentFirst agent;

  for (int i=0; i<100; i++) {
    auto action = agent.decide(env);
    env.step(action);
    env.render();
    if (env.done())
      env.reset();
  }

  return 0;
}

