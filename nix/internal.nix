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

  cardano-node-flake = (import inputs.flake-compat {src = inputs.cardano-node;}).defaultNix;

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

            patch -p1 -i ${./cardano-node--apply-patches.diff}
            cp  ${./cardano-ledger-core--Arbitrary-PoolMetadata.diff} nix/cardano-ledger-core--Arbitrary-PoolMetadata.diff
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

  nix-bundle-exe = import inputs.nix-bundle-exe {inherit pkgs;};

  nix-bundle-exe--same-dir = let
    patched = pkgs.runCommand "nix-bundle-exe-same-dir" {} ''
      cp -R ${inputs.nix-bundle-exe} $out
      chmod -R +w $out
      sed -r 's+@executable_path/\$relative_bin_to_lib/\$lib_dir+@executable_path+g' -i $out/bundle-macos.sh
    '';
  in
    import patched {
      inherit pkgs;
      bin_dir = ".";
      exe_dir = "_unused_";
      lib_dir = ".";
    };

  hydraPackage = let
    downloadableFromHydra = ''
      # Make it downloadable from Hydra:
      mkdir -p $out/nix-support
      echo "file binary-dist \"$target\"" >$out/nix-support/hydra-build-products
    '';
    darwinLike = (nix-bundle-exe--same-dir defaultPackage).overrideAttrs (drv: {
      buildCommand =
        drv.buildCommand
        + ''
          mkdir testgen-hs
          mv $out/* testgen-hs/
          target=$out/testgen-hs-${defaultPackage.version}-${targetSystem}.tar.bz2
          tar --dereference -cjf "$target" testgen-hs
          ${downloadableFromHydra}
        '';
    });
    linuxLike = {useZip ? false}:
      pkgs.runCommandNoCC "bundle" {} ''
        mkdir -p $out
        mkdir -p testgen-hs
        cp -R ${defaultPackage}/bin/. testgen-hs/
        ${
          if useZip
          then ''
            target=$out/testgen-hs-${defaultPackage.version}-${targetSystem}.zip
            ${lib.getExe pkgs.zip} -q -r "$target" testgen-hs
          ''
          else ''
            target=$out/testgen-hs-${defaultPackage.version}-${targetSystem}.tar.bz2
            tar --dereference -cjf "$target" testgen-hs
          ''
        }
        ${downloadableFromHydra}
      '';
  in
    {
      aarch64-darwin = darwinLike;
      x86_64-darwin = darwinLike;
      x86_64-linux = linuxLike {};
      x86_64-windows = linuxLike {useZip = true;};
    }
    .${targetSystem};
}
