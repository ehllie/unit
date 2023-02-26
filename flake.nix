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
    overlays.default = final: prev:
      let
        naersk' = final.callPackage naersk { };
        unit = naersk'.buildPackage {
          src = self;
          passthru = rec {
            cache = shell;

            shell = final.mkShell {
              inputsFrom = [ unit ];

              packages = final.lib.attrValues {
                inherit (final)
                  rust-analyzer
                  clippy
                  rustfmt;
              };

              shellHook = ''
                export PATH=$PWD/target/debug:$PATH
              '';
            };
          };
        };
      in
      { inherit unit; };

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
