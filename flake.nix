{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat";
      flake = false;
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cardano-node = {
      url = "github:IntersectMBO/cardano-node/10.4.1";
      flake = false; # otherwise, +2k dependencies we don’t really use
    };
    nix-bundle-exe = {
      url = "github:3noch/nix-bundle-exe";
      flake = false;
    };
  };

  outputs = inputs: let
    inherit (inputs.nixpkgs) lib;
  in
    inputs.flake-parts.lib.mkFlake {inherit inputs;} ({config, ...}: {
      imports = [
        inputs.treefmt-nix.flakeModule
        inputs.devshell.flakeModule
      ];

      flake.internal =
        lib.genAttrs config.systems (
          targetSystem: import ./nix/internal.nix {inherit inputs targetSystem;}
        )
        // lib.genAttrs ["x86_64-windows"] (
          targetSystem: import ./nix/internal.nix {inherit inputs targetSystem;}
        );

      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      perSystem = {system, ...}: let
        internal = inputs.self.internal.${system};
      in {
        packages =
          {
            default = internal.defaultPackage;
          }
          // (lib.optionalAttrs (system == "x86_64-linux") {
            default-x86_64-windows = inputs.self.internal.x86_64-windows.defaultPackage;
          });

        devshells.default = internal.devShell.new;

        treefmt = {pkgs, ...}: {
          projectRootFile = "flake.nix";
          programs = {
            alejandra.enable = true; # Nix
            ormolu.enable = true; # Haskell
            cabal-fmt.enable = true;
            prettier.enable = true; # Markdown, JSON, …
            shfmt.enable = true;
            ruff-check.enable = true; # Python
            ruff-format.enable = true; # Python
            yamlfmt.enable = pkgs.system != "x86_64-darwin"; # a treefmt-nix+yamlfmt bug on Intel Macs
          };
        };
      };

      flake.hydraJobs = let
        allJobs = {
          testgen-hs = lib.genAttrs (config.systems ++ ["x86_64-windows"]) (
            targetSystem: inputs.self.internal.${targetSystem}.hydraPackage
          );
          inherit (inputs.self) checks devShells;
        };
      in
        allJobs
        // {
          required = inputs.nixpkgs.legacyPackages.x86_64-linux.releaseTools.aggregate {
            name = "github-required";
            meta.description = "All jobs required to pass CI";
            constituents =
              lib.collect lib.isDerivation allJobs;
          };
        };
    });
}
