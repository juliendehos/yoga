let
  config = { allowUnfree = true; };
  pkgs = import <nixpkgs> { inherit config; };

in with pkgs; stdenv.mkDerivation {
  name = "citycat";
  src = ./.;

  buildInputs = [
    cmake
    libtorch-bin
    mkl
  ];

}

