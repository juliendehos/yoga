let
  config = { allowUnfree = true; };
  pkgs = import <nixpkgs> { inherit config; };

in with pkgs; stdenv.mkDerivation {
  name = "yoga";
  src = ./.;

  buildInputs = [
    cmake
    libtorch-bin
    mkl
    SDL2
    SDL2_gfx
    SDL2_ttf
  ];

}

