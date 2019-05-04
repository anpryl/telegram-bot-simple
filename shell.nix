let importNixPkgs = rev:
      import (fetchNixPkgs rev) {};
    fetchNixPkgs = rev:
      builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    pkgs = importNixPkgs "069bf7aee30faf7b3ed773cfae2154d761b2d6c2";
in pkgs.haskell.lib.buildStackProject {
  name = "telegram-bot-simple";
  buildInputs = with pkgs; [ gmp zlib ];
}
