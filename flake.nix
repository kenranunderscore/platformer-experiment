{
  description = "A very basic flake";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (final: prev: { haskellPackages = prev.haskell.packages.ghc96; })
          self.overlays.default
        ];
      };
    in {
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        packages = p: [ p.platformer ];
        nativeBuildInputs = [
          pkgs.haskellPackages.cabal-fmt
          pkgs.cabal-install
          pkgs.haskellPackages.haskell-language-server
        ];
      };

      packages.${system}.default = pkgs.haskellPackages.platformer;

      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
            (hfinal: hprev: {
              platformer =
                hfinal.callCabal2nix "platformer" (final.lib.cleanSource ./.)
                { };
            });
        });
      };
    };
}
