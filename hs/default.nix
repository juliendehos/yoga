
let
  
  grenade-src = fetchTarball {
    url = https://github.com/HuwCampbell/grenade/archive/5206c95.tar.gz;
    sha256 = "0228fq028h7gf2vhq2g31phzx4cbls8f9jsyl35m9dj2hzb1yqn0";
  };

  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: with pkgs.haskell.lib; {
          grenade = dontCheck (self.callCabal2nix "grenade" grenade-src {});
        };
      };
    };
  };

  # channel = (fetchTarball "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz");
  channel = <nixpkgs>;

  pkgs = import channel { inherit config; };

  drv = pkgs.haskellPackages.callCabal2nix "citycat" ./. {};

in

  if pkgs.lib.inNixShell then drv.env else drv

