{
  inputs,
  targetSystem,
}:
# For now, let's keep all UNIX definitions together, until they diverge more in the future.
assert builtins.elem targetSystem ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" "x86_64-windows"]; let
  buildSystem =
    if targetSystem != "x86_64-windows"
    then targetSystem
    else "x86_64-linux";
  pkgs = inputs.nixpkgs.legacyPackages.${buildSystem};
  inherit (pkgs) lib;
  cardano-node-src = let
    unpatched = inputs.cardano-node;
  in
    if targetSystem != "aarch64-linux"
    then unpatched
    else
      pkgs.runCommand "source" {} ''
        cp -r ${unpatched} $out
        chmod -R +w $out
        cd $out
        ${lib.optionalString (targetSystem == "aarch64-linux") ''
          echo ${lib.escapeShellArg (builtins.toJSON [targetSystem])} >$out/nix/supported-systems.nix
          sed -r 's/"-fexternal-interpreter"//g' -i $out/nix/haskell.nix
        ''}
      '';
  cardano-node-src-for-flake =
    if targetSystem != "aarch64-linux"
    then cardano-node-src
    else {
      outPath = toString cardano-node-src;
      inherit (inputs.cardano-node) rev shortRev lastModified lastModifiedDate;
    };
  cardano-node-flake' = (import inputs.flake-compat {src = cardano-node-src-for-flake;}).defaultNix;
  cardano-ledger-src = let
    dep-id = cardano-node-flake'.project.${buildSystem}.hsPkgs.cardano-ledger-core.identifier;
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
  patched-cardano-ledger-src = pkgs.runCommandNoCC "cardano-ledger-src-patched" {} ''
    cp -r ${cardano-ledger-src} $out
    chmod -R +w $out
    patch -p1 -d $out/libs/cardano-ledger-core -i ${./cardano-ledger-core--Arbitrary-PoolMetadata.diff}
    patch -p1 -d $out/libs/cardano-ledger-test -i ${./cardano-ledger-test--expose-helpers.diff}
    ${lib.optionalString (targetSystem == "x86_64-windows") ''
      patch -p1 -d $out/libs/cardano-ledger-test -i ${./cardano-ledger-test--windows-fix.diff}
    ''}
  '';
  cardano-api-src = cardano-node-flake'.project.${buildSystem}.hsPkgs.cardano-api.src;
  patched-cardano-api-src = pkgs.applyPatches {
    name = "cardano-api-src-patched";
    src = cardano-api-src;
    patches = [./cardano-api--expose-internal.diff];
  };
  patched-cardano-node-src = {withOurCode ? true}:
    pkgs.runCommandNoCC "cardano-node-src-patched" {} ''
      cp -r ${cardano-node-src} $out
      chmod -R +w $out
      cd $out
      ${lib.optionalString (targetSystem == "aarch64-linux") ''
        echo ${lib.escapeShellArg (builtins.toJSON [targetSystem])} >$out/nix/supported-systems.nix
        sed -r 's/"-fexternal-interpreter"//g' -i $out/nix/haskell.nix
      ''}
      ${
        if withOurCode
        then ''
          cp -r ${../testgen-hs} ./testgen-hs
        ''
        else ''
          mkdir -p ./testgen-hs
        ''
      }
      sed -r '/^packages:/ a\  testgen-hs' -i cabal.project

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

      patch -p1 -i ${./cardano-node--export-cardano-submit-api.diff}
    '';
  patched-cardano-node-flake' =
    (import inputs.flake-compat {
      src = {
        outPath = toString (patched-cardano-node-src {withOurCode = true;});
        inherit (inputs.cardano-node) rev shortRev lastModified lastModifiedDate;
      };
    })
    .defaultNix;
in rec {
  defaultPackage = testgen-hs;
  cardano-node-flake = cardano-node-flake';

  cardano-node-packages =
    {
      x86_64-linux = cardano-node-flake.hydraJobs.x86_64-linux.musl;
      inherit (cardano-node-flake.packages) aarch64-linux x86_64-darwin aarch64-darwin;
    }
    .${
      targetSystem
    };

  inherit (cardano-node-packages) cardano-node cardano-cli;

  devShell = let
    cardano-node-devshell = cardano-node-flake.devShells.${buildSystem}.default;
    cabal-project-base = cardano-node-flake.project.${buildSystem}.args.cabalProject;
    cabal-project-template = pkgs.writeText "cabal.project" cabal-project-base;
    cabal-project-extra-packages = [
      "${patched-cardano-api-src}"
      "${patched-cardano-ledger-src}/libs/cardano-ledger-core"
      "${patched-cardano-ledger-src}/libs/cardano-ledger-test"
      "${patched-cardano-ledger-src}/libs/constrained-generators"
      "@REPO_ROOT@/testgen-hs"
    ];
    cabal-project-extra-packages-json = builtins.toJSON cabal-project-extra-packages;
    cabal-project-rewrite-script = pkgs.substituteAll {
      src = ./rewrite_cabal_project.py;
      cabal_project_template = toString cabal-project-template;
      patched_node_src = toString (patched-cardano-node-src {withOurCode = false;});
      extra_packages_json = cabal-project-extra-packages-json;
    };
  in
    pkgs.mkShell {
      inputsFrom = [cardano-node-devshell];
      shellHook = ''
        ${lib.getExe pkgs.python3} ${cabal-project-rewrite-script}
      '';
    };

  testgen-hs = let
    patched-flake = patched-cardano-node-flake';
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
