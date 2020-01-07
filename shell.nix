let
  importNixPkgs = rev:
    import (fetchNixPkgs rev) {
      config.allowBroken = true;
    };

  fetchNixPkgs = rev:
    builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";

  pkgs = importNixPkgs "0fb7ae83ade88abd3af3f6969796909499b2bc2a";

  gitignore-src =
    pkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "dcd0b0e878009ef7b1425f8ca3e5b883f12459d8";
      sha256 = "sha256:0ni9hl6j4l94mfwsdmrg5l0ffibfgd8amspda7vc5fb3wm8blxpy";
    };
  inherit (import gitignore-src { inherit (pkgs) lib; }) gitignoreSource;

  src = gitignoreSource ./.;

  nix-pre-commit-hooks-src =
    builtins.fetchTarball "https://github.com/hercules-ci/nix-pre-commit-hooks/archive/c28ce2cd8a8ce1b4e2761f61da10dd79cbd5b8aa.tar.gz";
  nix-pre-commit-hooks = import nix-pre-commit-hooks-src;
  pre-commit-check =
    nix-pre-commit-hooks.run {
      settings = {
        ormolu.defaultExtensions = [
          "AutoDeriveTypeable"
          "BangPatterns"
          "BinaryLiterals"
          "ConstraintKinds"
          "DataKinds"
          "DefaultSignatures"
          "DeriveDataTypeable"
          "DeriveFoldable"
          "DeriveFunctor"
          "DeriveGeneric"
          "DeriveTraversable"
          "DuplicateRecordFields"
          "DoAndIfThenElse"
          "EmptyDataDecls"
          "ExistentialQuantification"
          "FlexibleContexts"
          "FlexibleInstances"
          "FunctionalDependencies"
          "GADTs"
          "GeneralizedNewtypeDeriving"
          "InstanceSigs"
          "KindSignatures"
          "LambdaCase"
          "MultiParamTypeClasses"
          "MultiWayIf"
          "NamedFieldPuns"
          "NoImplicitPrelude"
          "OverloadedLabels"
          "OverloadedStrings"
          "QuasiQuotes"
          "PartialTypeSignatures"
          "PatternGuards"
          "PolyKinds"
          "RankNTypes"
          "RecordWildCards"
          "ScopedTypeVariables"
          "StandaloneDeriving"
          "TupleSections"
          "TypeApplications"
          "TypeFamilies"
          "TypeSynonymInstances"
          "ViewPatterns"
        ];
      };
      src = src;
      hooks =
        {
          hlint.enable = true;
          nixpkgs-fmt.enable = true;
          ormolu.enable = true;
          shellcheck.enable = true;
        };
    };

  ghcVersion = "ghc865";
  hpkgs = pkgs.haskell.packages.${ghcVersion};

  projectDrv = hpkgs.override {
    overrides = hpkgsNew: hpkgsOld: with pkgs.haskell.lib; {
      telegram-bot-simple = hpkgsNew.callCabal2nix "telegram-bot-simple" src {};
      cron = dontCheck hpkgsOld.cron;
    };
  };

  projectShell = projectDrv.shellFor {
    packages = p: [ p.telegram-bot-simple ];
    buildInputs = with pkgs; [ projectDrv.ghcid gmp zlib ];
    shellHook = pre-commit-check.shellHook;
  };
in
if pkgs.lib.inNixShell then projectShell else projectDrv.telegram-bot-simple
