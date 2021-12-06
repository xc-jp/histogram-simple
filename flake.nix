{
  description = "histogram-simple";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: _: {
          hsPkgs =
            self.haskell-nix.project' rec {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell = {
                tools = {
                  cabal = { };
                  ghcid = { };
                  haskell-language-server = { };
                  hlint = { };
                };
                buildInputs =
                  let
                    ormolu = pkgs.haskell-nix.tool compiler-nix-name "ormolu" "latest";
                    ormolu-wrapped = pkgs.writeShellScriptBin "ormolu" ''
                      ${ormolu}/bin/ormolu --ghc-opt=-XImportQualifiedPost $@
                    '';
                  in
                  [ ormolu-wrapped ];
              };
            };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNix.overlay
            overlay
          ];
        };
      in
      (pkgs.hsPkgs.flake { }) // { defaultPackage = pkgs.hsPkgs.hsPkgs.histogram-simple.components.library; }
    );
}
