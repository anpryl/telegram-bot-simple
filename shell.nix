let importNixPkgs = rev:
      import (fetchNixPkgs rev) {};
    fetchNixPkgs = rev:
      builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    pkgs = importNixPkgs "55a82ffcd3090086a935d127c6fb5cd7eabe1f8a";
in pkgs.haskell.lib.buildStackProject {
  name = "telegram-bot-simple";
  buildInputs = with pkgs; [ gmp zlib haskellPackages.ghcid haskellPackages.stylish-haskell ];
}
