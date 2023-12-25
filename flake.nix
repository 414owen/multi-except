{
  description = "multi-except development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { flake-utils, nixpkgs, self }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc96";
        pkgs = import nixpkgs { inherit system; };
        hsPkgs = pkgs.haskell.packages.${compiler};
      in rec {
        devShell = packages.default.env.overrideAttrs(old: {
          nativeBuildInputs = old.nativeBuildInputs ++ [
            hsPkgs.haskell-language-server
          ];
        });
        packages = rec {
          default = multi-except;
          multi-except = hsPkgs.callPackage ./default.nix {};
        };
      }
    );
}
