{
  inputs,
  targetSystem,
}:
# For now, let's keep all UNIX definitions together, until they diverge more in the future.
assert __elem targetSystem ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" "x86_64-windows"]; let
  buildSystem =
    if targetSystem != "x86_64-windows"
    then targetSystem
    else "x86_64-linux";
  pkgs = inputs.nixpkgs.legacyPackages.${buildSystem};
  inherit (pkgs) lib;
in rec {
  defaultPackage = testgen-hs;
  hydraPackage = testgen-hs;

  cardano-node-flake = let
    unpatched = inputs.cardano-node;
  in
    (import inputs.flake-compat {
      src =
        if targetSystem != "aarch64-darwin"
        then unpatched
        else {
          outPath = toString (pkgs.runCommand "source" {} ''
            cp -r ${unpatched} $out
            chmod -R +w $out
            cd $out
            echo ${lib.escapeShellArg (builtins.toJSON [targetSystem])} $out/nix/supported-systems.nix
          '');
          inherit (unpatched) rev shortRev lastModified lastModifiedDate;
        };
    })
    .defaultNix;

  cardano-node-packages =
    {
      x86_64-linux = cardano-node-flake.hydraJobs.x86_64-linux.musl;
      x86_64-darwin = cardano-node-flake.packages.x86_64-darwin;
      aarch64-darwin = cardano-node-flake.packages.aarch64-darwin;
    }
    .${targetSystem};

  inherit (cardano-node-packages) cardano-node cardano-cli;

  testgen-hs = let
    patched-flake = let
      unpatched = inputs.cardano-node;
    in
      (import inputs.flake-compat {
        src = {
          outPath = toString (pkgs.runCommandNoCC "source" {} ''
            cp -r ${unpatched} $out
            chmod -R +w $out
            cd $out
            echo ${lib.escapeShellArg (builtins.toJSON [targetSystem])} $out/nix/supported-systems.nix
            cp -r ${../testgen-hs} ./testgen-hs
            sed -r '/^packages:/ a\  testgen-hs' -i cabal.project
            sed -r 's/other-modules:\s*/                    , /g' -i cardano-submit-api/cardano-submit-api.cabal
          '');
          inherit (unpatched) rev shortRev lastModified lastModifiedDate;
        };
      })
      .defaultNix;
  in
    {
      x86_64-linux = patched-flake.hydraJobs.x86_64-linux.musl.testgen-hs;
      x86_64-darwin = patched-flake.packages.x86_64-darwin.testgen-hs;
      aarch64-darwin = patched-flake.packages.aarch64-darwin.testgen-hs;
      x86_64-windows = patched-flake.legacyPackages.x86_64-linux.hydraJobs.windows.testgen-hs;
    }
    .${targetSystem};
}
