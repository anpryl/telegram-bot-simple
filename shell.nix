let
  importNixpkgs = rev:
    import (fetchNixpkgs rev) {
      config.allowBroken = true;
    };

  fetchNixpkgs = rev:
    builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";

  pkgs = importNixpkgs "93498b15264bb796d4ff3a08971b96847cdd9391";

  ghcVersion = "ghc8103";

  hpkgs = pkgs.haskell.packages.${ghcVersion};

  gitignore-src =
    pkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "dcd0b0e878009ef7b1425f8ca3e5b883f12459d8";
      sha256 = "sha256:0ni9hl6j4l94mfwsdmrg5l0ffibfgd8amspda7vc5fb3wm8blxpy";
    };
  gitignore-drv = import gitignore-src { inherit (pkgs) lib; };
  gitignoreSource = gitignore-drv.gitignoreSource;

  fourmolu-src = pkgs.fetchFromGitHub {
    owner = "parsonsmatt";
    repo = "fourmolu";
    rev = "f4e3b56d644b9bbf1eedee72616475028ea6e15a";
    sha256 = "1r8qjdmcr2wk5ycvik02wn8ip3021zw4whaxcyz2pj8y190za7fn";
  };

  fourmolu = hpkgs.callCabal2nix "fourmolu" fourmolu-src {};

  ormoluAlias = (
    pkgs.writeScriptBin "ormolu" ''
      #!${pkgs.stdenv.shell}
      ${fourmolu}/bin/fourmolu $@ 2> /dev/null
    ''
  );

  src = gitignoreSource ./.;

  nix-pre-commit-hooks-src = pkgs.fetchFromGitHub {
    owner = "anpryl";
    repo = "pre-commit-hooks.nix";
    rev = "9d19a769e15a361a408ab3e9cb1449a7c3fb2281";
    sha256 = "1dcvwq4icmnbnz1nk93xc86gwaprcbnzml4smfyldjr0flpg8lcz";
  };
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
      tools =
        {
          ormolu = ormoluAlias;
        };
    };

  projectDrv = hpkgs.override {
    overrides = hpkgsNew: hpkgsOld: with pkgs.haskell.lib; {
      telegram-bot-simple = hpkgsNew.callCabal2nix "telegram-bot-simple" src {};
      cron = dontCheck hpkgsOld.cron;
    };
  };

  projectShell = projectDrv.shellFor {
    packages = p: [ p.telegram-bot-simple ];
    buildInputs = with pkgs; [ projectDrv.ghcid gmp zlib ormoluAlias ];
    shellHook = pre-commit-check.shellHook;
  };
in
if pkgs.lib.inNixShell then projectShell else projectDrv.telegram-bot-simple
