{ reflex-platform, ... }:
let

  dontCheck = reflex-platform.lib.dontCheck;
  cabal2nixResult = reflex-platform.cabal2nixResult;

in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
    common = self.callPackage ../common { groundhog-ghcjs = self.groundhog-ghcjs; };
    servant-reflex = self.callPackage (cabal2nixResult ../deps/servant-reflex) {};
    servant= self.callPackage (cabal2nixResult ../deps/servant/servant) {};
    groundhog = self.callPackage (cabal2nixResult ../deps/groundhog/groundhog) {};
    groundhog-postgresql = null;
    groundhog-th = null;
    groundhog-ghcjs = self.callPackage (cabal2nixResult ../deps/groundhog-ghcjs) {};
  };
}
