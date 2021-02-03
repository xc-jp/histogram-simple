let
  pkgs = import ./pkgs.nix;
  hsPkgs = pkgs.hsPkgs;
in
hsPkgs.shellFor {
  withHoogle = true;
  tools = {
    cabal = "latest";
    ghcid = "latest";
    ormolu = "latest";
    haskell-language-server = "latest";
  };
  exactDeps = true;
}
