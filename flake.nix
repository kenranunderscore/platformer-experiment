{
  description = "A very basic flake";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ (final: prev: { hs = prev.haskell.packages.ghc96; }) ];
      };
      platformer =
        pkgs.hs.callCabal2nix "platformer" (pkgs.lib.cleanSource ./.) { };
    in {
      devShells.${system}.default = pkgs.hs.shellFor {
        packages = _: [ platformer ];
        nativeBuildInputs = [
          pkgs.haskellPackages.cabal-fmt
          pkgs.cabal-install
          pkgs.hs.haskell-language-server
        ];
      };
    };
}
