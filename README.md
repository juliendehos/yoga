
*alpha-experimental-early stage*

<https://juliendehos.github.io/yoga>

# cpp

## build

```
nix-shell
cmake -S . -B build
cmake --build build
```

## todo

  - C++20 modules ?


# haskell

## build

```
nix-shell
cabal build
```

# references

<https://gym.openai.com>

<https://github.com/openai/gym>


```py
import gym

env = gym.make("CartPole-v1")
observation, info = env.reset(seed=42, return_info=True)

for _ in range(1000):
    action = env.action_space.sample()
    observation, reward, done, info = env.step(action)

    if done:
        observation, info = env.reset(return_info=True)

env.close()
```

