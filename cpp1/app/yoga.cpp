#include <yoga/Agent/AgentRandom.hpp>
#include <yoga/Env/EnvCitycat.hpp>

#include <iostream>

int main() {

  EnvCitycatConsole env(20, 30, 30, std::make_optional(42));
  AgentRandom agent({});

  for (int i=0; i<100; i++) {
    auto action = agent.genmove(env);
    env.step(action);
    env.render(std::cout);
    if (env.done())
      env.reset();
  }

  return 0;
}

