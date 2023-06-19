{
  description = "symengine/symengine.hs: SymEngine symbolic mathematics engine for Haskell";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    with builtins;
    let
      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "test"
          "symengine.cabal"
          "README.md"
          "LICENSE"
        ];
      };

      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          (self: super: {
            symengine = super.symengine.overrideAttrs (attrs: rec {
              version = "0.10.1";
              src = self.fetchFromGitHub {
                owner = attrs.pname;
                repo = attrs.pname;
                rev = "v${version}";
                sha256 = "sha256-qTu0vS9K6rrr/0SXKpGC9P1QSN/AN7hyO/4DrGvhxWM=";
              };
              cmakeFlags = (attrs.cmakeFlags or [ ]) ++ [
                "-DCMAKE_BUILD_TYPE=Debug"
                "-DBUILD_SHARED_LIBS=ON"
              ];
            });

            haskell = super.haskell // {
              packageOverrides = hself: hsuper: {
                symengine = (hself.callCabal2nix "symengine" src {
                  inherit (self) symengine;
                  mpc = self.libmpc;
                });
              };
            };
          })
        ];

        config.allowBroken = true;
      };


    in
    {
      packages = {
        default = pkgs.haskellPackages.symengine;
        symengine = pkgs.haskellPackages.symengine;
        haskell = pkgs.haskell.packages;
      };

      devShells.default = haskellPackages.shellFor {
        packages = ps: with ps; [ symengine ];
        withHoogle = true;
        nativeBuildInputs = with pkgs; with ps; [
          # Building and testing
          cabal-install
          # Language servers
          haskell-language-server
          nil
          # Formatters
          fourmolu
          cabal-fmt
          nixpkgs-fmt
          # Previewing markdown files
          python3Packages.grip
        ];
        shellHook = ''
          LD_LIBRARY_PATH=${pkgs.symengine}/lib:${pkgs.flint}/lib:${pkgs.libmpc}/lib:${pkgs.mpfr}/lib:$LD_LIBRARY_PATH
          SYMENGINE_PATH=${pkgs.symengine}
        '';
      };
      # The formatter to use for .nix files (but not .hs files)
      # Allows us to run `nix fmt` to reformat nix files.
      formatter = pkgs.nixpkgs-fmt;
    }
  );
}
