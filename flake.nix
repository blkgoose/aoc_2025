{
  description = "Advent of Code";

  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/*";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          packages = { };

          devShell = { hlsCheck.enable = false; };
          autoWire = [ "packages" "apps" "checks" ];
        };

        devShells.default = pkgs.mkShell {
          name = "aoc-dev-shell";
          inputsFrom = [ config.haskellProjects.default.outputs.devShell ];
          nativeBuildInputs = [ ];
        };
      };
    };
}
