{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }: {
    overlays.default = final: prev: rec{
      unit =
        let
          inherit (final) lib;
          inherit (prev.haskell.lib.compose) overrideCabal;
          compiler = final.haskell.packages.ghc92;

          fixCyclicReference = overrideCabal (_: {
            enableSeparateBinOutput = false;
          });

          overrides = [
            (overrideCabal (old: {
              passthru = rec {
                shell = final.mkShell {
                  inputsFrom = [ unit ];
                  packages = [
                    (compiler.haskell-language-server.overrideScope (_:_: {
                      ormolu = fixCyclicReference compiler.ormolu;
                    }))
                  ];

                  shellHook = ''
                    alias unit="cabal run -- unit"
                  '';
                };

                cache = final.symlinkJoin {
                  name = "unit-cache";
                  paths = shell.nativeBuildInputs ++ [ final.cabal2nix ];
                };
              };
            }))
            (compiler.generateOptparseApplicativeCompletions [ "unit" ])
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
