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
      rev = dep-tag;
      hash = "sha256-RvnNYY76OhRuC/uP5Lr+HLEKWyMHCWxx+10HlPrH6mQ=";
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
  constrained-generators-src = patched-cardano-node-flake'.project.${buildSystem}.hsPkgs.constrained-generators.src;
  patched-constrained-generators-src = constrained-generators-src;
  cardano-api-src = cardano-node-flake'.project.${buildSystem}.hsPkgs.cardano-api.src;
  cardano-api-id = cardano-node-flake'.project.${buildSystem}.hsPkgs.cardano-api.identifier;
  # Extract the latest revised .cabal from the local CHaP index.
  # CHaP revisions relax version bounds to match the package set;
  # without this, the on-disk .cabal carries the original tight bounds
  # and cabal-install's solver rejects valid dependency plans.
  #
  # GNU tar overwrites earlier entries with later ones, so a plain
  # extract of a duplicate member yields the last revision — exactly
  # the latest CHaP revision we need.
  cardano-api-revised-cabal = let
    member = "${cardano-api-id.name}/${cardano-api-id.version}/${cardano-api-id.name}.cabal";
  in
    pkgs.runCommandNoCC "cardano-api-revised-cabal" {} ''
      tar xf ${cardano-node-flake'.inputs.CHaP}/01-index.tar.gz '${member}'
      cp '${member}' $out
    '';
  patched-cardano-api-src = pkgs.applyPatches {
    name = "cardano-api-src-patched";
    src = cardano-api-src;
    prePatch = ''
      cp ${cardano-api-revised-cabal} cardano-api.cabal
    '';
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
    cardano-node-inputs = lib.filter lib.isDerivation (
      (cardano-node-devshell.buildInputs or [])
      ++ (cardano-node-devshell.nativeBuildInputs or [])
      ++ (cardano-node-devshell.propagatedBuildInputs or [])
      ++ (cardano-node-devshell.propagatedNativeBuildInputs or [])
    );
    cardano-node-env = pkgs.buildEnv {
      name = "cardano-node-devshell-env";
      paths = cardano-node-inputs;
      ignoreCollisions = true;
    };
    cardano-node-ghc-libdir = cardano-node-devshell.NIX_GHC_LIBDIR or "";

    # Extract an individual package from the cardano-node devshell inputs
    # so we can expose it as a devshell command with its own menu entry.
    findInput = pred: label:
      lib.findFirst pred (throw "devshell input '${label}' not found") cardano-node-inputs;

    # numtide/devshell does not run stdenv setup hooks, so env vars that
    # mkShell would set (e.g. PKG_CONFIG_PATH) are missing.
    # PKG_CONFIG_PATH is critical: without it the cabal solver cannot
    # verify pkgconfig-depends and dependency resolution fails.
    # We extract it by running the same stdenv setup with the same inputs.
    devshell-pkg-config-path =
      pkgs.runCommand "devshell-pkg-config-path" {
        inherit (cardano-node-devshell) buildInputs nativeBuildInputs;
        propagatedBuildInputs = cardano-node-devshell.propagatedBuildInputs or [];
        propagatedNativeBuildInputs = cardano-node-devshell.propagatedNativeBuildInputs or [];
      } ''
        source $stdenv/setup 2>/dev/null || true
        mkdir -p $out
        echo -n "''${PKG_CONFIG_PATH:-}" >$out/PKG_CONFIG_PATH
      '';

    cabal-project-base = cardano-node-flake.project.${buildSystem}.args.cabalProject;
    cabal-project-template = pkgs.writeText "cabal.project" cabal-project-base;
    cabal-project-extra-packages = [
      "${patched-cardano-api-src}"
      "${patched-cardano-ledger-src}/libs/cardano-ledger-core"
      "${patched-cardano-ledger-src}/libs/cardano-ledger-test"
      "${patched-constrained-generators-src}"
      "@REPO_ROOT@/testgen-hs"
    ];
    cabal-project-extra-packages-json = builtins.toJSON cabal-project-extra-packages;
    # cardano-ledger-core uses the deprecated PV3.fromGHC (should be
    # fromHaskellRatio). The upstream cabal.project sets -Werror globally,
    # which promotes this deprecation warning into a build error. The nix
    # build is unaffected because haskell.nix doesn't pass -Werror from
    # the project file, but the devshell's cabal does.
    cabal-project-extra-suffix = ''
      package cardano-ledger-core
        ghc-options: -Wwarn=deprecations
    '';
    cabal-project-rewrite-script = pkgs.substituteAll {
      src = ./rewrite_cabal_project.py;
      cabal_project_template = toString cabal-project-template;
      patched_node_src = toString (patched-cardano-node-src {withOurCode = false;});
      extra_packages_json = cabal-project-extra-packages-json;
      extra_project_suffix = cabal-project-extra-suffix;
      local_chap_path = toString cardano-node-flake'.inputs.CHaP;
      repo_root = "@REPO_ROOT@";
    };

    # Pre-generate cabal.project at Nix build time so the devshell
    # startup doesn't need to launch Python (~260 ms saved).
    # The only remaining placeholder is @REPO_ROOT@, which is
    # sed-replaced with $PRJ_ROOT at shell entry.
    cabal-project-generated =
      pkgs.runCommand "cabal-project-generated" {
        nativeBuildInputs = [pkgs.python3];
      } ''
        python3 ${cabal-project-rewrite-script}
        cp cabal.project $out
      '';
  in {
    old = let
      chap-store-path = toString cardano-node-flake'.inputs.CHaP;
    in
      pkgs.mkShell {
        inputsFrom = [cardano-node-devshell];
        shellHook = ''
          export CABAL_DIR="$PWD/.cabal"
          _new=$(sed "s|@REPO_ROOT@|$PWD|g" ${cabal-project-generated})
          if [ ! -e "$PWD/cabal.project" ] || [ "$(cat "$PWD/cabal.project")" != "$_new" ]; then
            printf '%s\n' "$_new" > "$PWD/cabal.project"
          fi

          _chap_marker="$CABAL_DIR/.chap-store-path"
          if [ ! -e "$_chap_marker" ] || [ "$(cat "$_chap_marker")" != "${chap-store-path}" ]; then
            mkdir -p "$CABAL_DIR/packages/cardano-haskell-packages"
            cabal update cardano-haskell-packages 2>/dev/null
            printf '%s' '${chap-store-path}' >"$_chap_marker"
          fi

          if [ ! -e "$CABAL_DIR/packages/hackage.haskell.org/01-index.tar" ]; then
            echo "First-time setup: downloading Hackage package index…"
            cabal update hackage.haskell.org
          fi
        '';
      };
    new = {config, ...}: {
      name = "testgen-hs-devshell";
      env =
        lib.optional (cardano-node-ghc-libdir != "") {
          name = "NIX_GHC_LIBDIR";
          value = cardano-node-ghc-libdir;
        }
        ++ [
          {
            name = "CABAL_DIR";
            eval = "$PRJ_ROOT/.cabal";
          }
          {
            name = "PKG_CONFIG_PATH";
            eval = "$(cat ${devshell-pkg-config-path}/PKG_CONFIG_PATH)\${PKG_CONFIG_PATH:+:\$PKG_CONFIG_PATH}";
          }
        ];
      commands = [
        {
          name = "ghc";
          package = findInput (p: lib.hasPrefix "ghc-shell-for-packages" (p.name or "")) "ghc";
          category = "haskell";
        }
        {
          name = "cabal";
          package = findInput (p: (p.pname or "") == "cabal-install-exe-cabal") "cabal";
          category = "haskell";
        }
        {
          name = "haskell-language-server";
          package = findInput (p: (p.pname or "") == "haskell-language-server-exe-haskell-language-server") "hls";
          category = "haskell";
        }
      ];
      devshell = {
        packages = [cardano-node-env];
        startup.rewrite-cabal-project.text = let
          chap-store-path = toString cardano-node-flake'.inputs.CHaP;
        in ''
          _new=$(sed "s|@REPO_ROOT@|$PRJ_ROOT|g" ${cabal-project-generated})
          if [ ! -e "$PRJ_ROOT/cabal.project" ] || [ "$(cat "$PRJ_ROOT/cabal.project")" != "$_new" ]; then
            printf '%s\n' "$_new" > "$PRJ_ROOT/cabal.project"
          fi
          # Re-index the local CHaP only when the underlying Nix store
          # path changes (i.e. after a flake.lock update).  A marker
          # file records the store path that was last indexed.
          _chap_marker="$CABAL_DIR/.chap-store-path"
          if [ ! -e "$_chap_marker" ] || [ "$(cat "$_chap_marker")" != "${chap-store-path}" ]; then
            mkdir -p "$CABAL_DIR/packages/cardano-haskell-packages"
            cabal update cardano-haskell-packages 2>/dev/null
            printf '%s' '${chap-store-path}' >"$_chap_marker"
          fi

          # On first use of this project-local CABAL_DIR, also fetch the
          # Hackage index so that 'cabal build' works out of the box.
          if [ ! -e "$CABAL_DIR/packages/hackage.haskell.org/01-index.tar" ]; then
            echo "First-time setup: downloading Hackage package index…"
            cabal update hackage.haskell.org
          fi

        '';
        motd = ''

          {202}🔨 Welcome to ${config.name}{reset}
          $(menu)

          You can now run:
            · {bold}cabal build testgen-hs{reset}
            · {bold}cabal run testgen-hs -- --help{reset}
        '';
      };
    };
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
