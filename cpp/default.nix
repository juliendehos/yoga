let
  # config = { allowUnfree = true; };
  # pkgs = import <nixpkgs> { inherit config; };
  pkgs = import <nixpkgs> {};

  #flags = with pkgs; ''
  #  export CXXFLAGS="-I${libtorch-bin.dev}/include -I${libtorch-bin.dev}/include/torch/csrc/api/include"
  #  export LDFLAGS="-L${libtorch-bin}/lib -lc10 -ltorch_cpu"
  #'';

in with pkgs; stdenv.mkDerivation {
  name = "citycat";
  src = ./.;
  #preConfigure = flags;
  #shellHook = flags;

  buildInputs = [
    cmake
  ];

}

