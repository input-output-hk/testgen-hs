{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat.url = "github:input-output-hk/flake-compat";
    flake-compat.flake = false;
    cardano-node.url = "github:IntersectMBO/cardano-node/10.1.2";
    cardano-node.flake = false; # otherwise, +2k dependencies we don’t really use
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} ({config, ...}: {
      imports = [
        inputs.treefmt-nix.flakeModule
      ];

      flake.internal =
        inputs.nixpkgs.lib.genAttrs config.systems (
          targetSystem: import ./nix/internal.nix {inherit inputs targetSystem;}
        )
        // inputs.nixpkgs.lib.genAttrs ["x86_64-windows"] (
          targetSystem: import ./nix/internal.nix {inherit inputs targetSystem;}
        );

      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      perSystem = {
        config,
        system,
        pkgs,
        ...
      }: {
        packages = let
          internal = inputs.self.internal.${system};
        in
          {
            default = internal.defaultPackage;
          }
          // (inputs.nixpkgs.lib.optionalAttrs (system == "x86_64-linux") {
            default-x86_64-windows = inputs.self.internal.x86_64-windows.defaultPackage;
          });

        treefmt = {pkgs, ...}: {
          projectRootFile = "flake.nix";
          programs.alejandra.enable = true; # Nix
          programs.ormolu.enable = true; # Haskell
          programs.cabal-fmt.enable = true;
          programs.shfmt.enable = true;
        };
      };
    });
}
