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

        devShells.default = let
          cabal-watch-tests = pkgs.writeShellScriptBin "cabal-watch-tests" ''
            while true; do 
              ${pkgs.inotify-tools}/bin/inotifywait -e modify -r ./; 
              clear
              cabal test
            done
          '';
        in pkgs.mkShell {
          name = "aoc-dev-shell";
          inputsFrom = [ config.haskellProjects.default.outputs.devShell ];
          nativeBuildInputs = [ cabal-watch-tests ];
        };
      };
    };
}
