{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, naersk }: {
    overlays.default = final: prev: rec{
      unit =
        let
          inherit (final) lib;
          inherit (prev.haskell.lib.compose)
            overrideCabal generateOptparseApplicativeCompletion;
          compiler = final.haskell.packages.ghc92;

          fixCyclicReference = overrideCabal (_: {
            enableSeparateBinOutput = false;
          });

          overrides = [
            (overrideCabal (old: {
              passthru = rec {
                shell = unit.env.overrideAttrs
                  (old: {
                    nativeBuildInputs = old.nativeBuildInputs or [ ] ++
                      [
                        (compiler.haskell-language-server.overrideScope (_:_: {
                          ormolu = fixCyclicReference compiler.ormolu;
                        }))
                      ];
                  });
                cache = final.symlinkJoin {
                  name = "unit-cache";
                  paths = shell.nativeBuildInputs ++ [ final.cabal2nix ];
                };
              };
            }))
          ];

        in
        lib.pipe (compiler.developPackage { root = self; }) overrides;
    };

  } // flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default ];
      };
    in
    {
      packages = rec {
        unit = pkgs.unit;
        default = unit;
        cache = unit.passthru.cache;
      };
      devShells.default = pkgs.unit.passthru.shell;
    });
}
