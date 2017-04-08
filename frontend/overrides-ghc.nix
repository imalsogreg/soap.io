{ reflex-platform, ... }:
let

  dc = reflex-platform.lib.dontCheck;
  c2n = reflex-platform.cabal2nixResult;

in
reflex-platform.ghc.override {
  overrides = self: super: { 
    groundhog-ghcjs = self.callPackage (c2n ../deps/groundhog-ghcjs) {};
    groundhog = self.callPackage (c2n ../deps/groundhog/groundhog) {};
    groundhog-th = self.callPackage (c2n ../deps/groundhog/groundhog-th) {};
    common = self.callPackage (c2n ../common) {};
    servant = dc (self.callPackage (c2n ../deps/servant/servant) {});
    servant-reflex = self.callPackage (c2n ../deps/servant-reflex) {};
  };
}
