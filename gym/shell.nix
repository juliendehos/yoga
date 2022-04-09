with import <nixpkgs> {};
let
  pythonEnv = python3.withPackages (ps: [
    ps.gym
    ps.numpy
    ps.pytorchWithoutCuda
    ps.pygame
    ps.pyglet
  ]);
in mkShell {
  packages = [
    pythonEnv
  ];
}

