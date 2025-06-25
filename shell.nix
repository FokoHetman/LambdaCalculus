{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs;[ haskell-language-server ghcid /*cabal-install*/];
  shellHook = ''
    nvim src/Main.hs +":vsplit | term cd src; ghcid Main.hs" +":split | term ghci src/Main.hs"
    exit
    '';
}
