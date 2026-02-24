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

  cardano-node-flake = let
    unpatched = inputs.cardano-node;
  in
    (import inputs.flake-compat {
      src =
        if targetSystem != "aarch64-linux"
        then unpatched
        else {
          outPath = toString (pkgs.runCommand "source" {} ''
            cp -r ${unpatched} $out
            chmod -R +w $out
            cd $out
            ${lib.optionalString (targetSystem == "aarch64-linux") ''
              echo ${lib.escapeShellArg (builtins.toJSON [targetSystem])} >$out/nix/supported-systems.nix
              sed -r 's/"-fexternal-interpreter"//g' -i $out/nix/haskell.nix
            ''}
          '');
          inherit (unpatched) rev shortRev lastModified lastModifiedDate;
        };
    })
    .defaultNix;

  cardano-node-packages =
    {
      x86_64-linux = cardano-node-flake.hydraJobs.x86_64-linux.musl;
      aarch64-linux = cardano-node-flake.packages.aarch64-linux;
      x86_64-darwin = cardano-node-flake.packages.x86_64-darwin;
      aarch64-darwin = cardano-node-flake.packages.aarch64-darwin;
    }
    .${
      targetSystem
    };

  inherit (cardano-node-packages) cardano-node cardano-cli;

  testgen-hs = let
    patched-flake = let
      unpatched = inputs.cardano-node;
      cardano-ledger-src = let
        dep-id = cardano-node-flake.project.${buildSystem}.hsPkgs.cardano-ledger-core.identifier;
        dep-tag = "${dep-id.name}-${dep-id.version}";
      in
        pkgs.fetchFromGitHub {
          name = "cardano-ledger--${dep-tag}";
          owner = "IntersectMBO";
          repo = "cardano-ledger";
          #rev = "a9e78ae63cf8870f0ce6ce76bd7029b82ddb47e1"; # the one for cardano-node 10.4.1, tag: cardano-ledger-core-1.17.0.0
          rev = dep-tag; # the one for cardano-node 10.4.1
          hash = "sha256-pD22f9VzNApynPhVYv0T7fsOZdbvYr1vlOxhKRhMSYk=";
        };
    in
      (import inputs.flake-compat {
        src = {
          outPath = toString (pkgs.runCommandNoCC "source" {} ''
            cp -r ${unpatched} $out
            chmod -R +w $out
            cd $out
            ${lib.optionalString (targetSystem == "aarch64-linux") ''
              echo ${lib.escapeShellArg (builtins.toJSON [targetSystem])} >$out/nix/supported-systems.nix
              sed -r 's/"-fexternal-interpreter"//g' -i $out/nix/haskell.nix
            ''}
            cp -r ${../testgen-hs} ./testgen-hs
            sed -r '/^packages:/ a\  testgen-hs' -i cabal.project
            sed -r 's/other-modules:\s*/                    , /g' -i cardano-submit-api/cardano-submit-api.cabal

            patch -p1 -i ${./cardano-node--apply-patches.diff}
            cp  ${./cardano-ledger-core--Arbitrary-PoolMetadata.diff} nix/cardano-ledger-core--Arbitrary-PoolMetadata.diff
            cp  ${./cardano-ledger-test--expose-helpers.diff} nix/cardano-ledger-test--expose-helpers.diff
            cp  ${
              if targetSystem == "x86_64-windows"
              then ./cardano-ledger-test--windows-fix.diff
              else pkgs.emptyFile
            } nix/cardano-ledger-test--windows-fix.diff
            cp  ${./cardano-api--expose-internal.diff} nix/cardano-api--expose-internal.diff

            patch -p1 -i ${./cardano-node--expose-cardano-ledger-test.diff}
            sed -r 's,CARDANO_LEDGER_SOURCE,${cardano-ledger-src},g' -i nix/haskell.nix
          '');
          inherit (unpatched) rev shortRev lastModified lastModifiedDate;
        };
      })
      .defaultNix;
  in
    {
      x86_64-linux = patched-flake.hydraJobs.x86_64-linux.musl.testgen-hs;
      aarch64-linux = patched-flake.packages.aarch64-linux.testgen-hs;
      x86_64-darwin = patched-flake.packages.x86_64-darwin.testgen-hs;
      aarch64-darwin = patched-flake.packages.aarch64-darwin.testgen-hs;
      x86_64-windows = patched-flake.legacyPackages.x86_64-linux.hydraJobs.windows.testgen-hs;
    }
    .${
      targetSystem
    };

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
        ${
          if targetSystem == "aarch64-linux"
          then ''
            cp -R ${nix-bundle-exe defaultPackage}/. testgen-hs/
            chmod -R +w testgen-hs/
            mv testgen-hs/bin/testgen-hs testgen-hs/testgen-hs
            rmdir testgen-hs/bin
            sed -r 's/dirname/echo/' -i testgen-hs/testgen-hs
          ''
          else ''
            cp -R ${defaultPackage}/bin/. testgen-hs/
          ''
        }
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
      aarch64-linux = linuxLike {};
      x86_64-windows = linuxLike {useZip = true;};
    }
    .${
      targetSystem
    };
}
