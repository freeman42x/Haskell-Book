with import <nixpkgs> { };

((haskellPackages.override {
  overrides = self: super: {
    QuickCheck = self.callPackage ./QuickCheck.nix { };
  };
}).callPackage ./. { }).env
