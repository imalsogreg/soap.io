{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});
  dc  = reflex-platform.lib.dontCheck;
  c2n = reflex-platform.cabal2nixResult;
  # snap-pkgs = ["snap-core" "snap-server" "io-streams" "io-streams-haproxy" "xmlhtml" "heist"];
  # snap-deps = "../deps/servant-reflex/deps/servant-snap/deps/snap/deps/"

in
reflex-platform.ghc.override {
  overrides = self: super: { 
    common = self.callPackage (c2n ../common) {};
    groundhog = dc (self.callPackage (c2n ../deps/groundhog/groundhog) {});
    groundhog-th = dc (self.callPackage (c2n ../deps/groundhog/groundhog-th) {});
    groundhog-postgresql = dc (self.callPackage (c2n ../deps/groundhog/groundhog-postgresql) {});
    xmlhtml = dc (self.callPackage (c2n ../deps/servant-reflex/deps/servant-snap/deps/snap/deps/xmlhtml) {});
    heist = dc (self.callPackage (c2n ../deps/servant-reflex/deps/servant-snap/deps/snap/deps/heist) {});
    snap-core = dc (self.callPackage (c2n ../deps/servant-reflex/deps/servant-snap/deps/snap/deps/snap-core) {});
    snap = dc (self.callPackage (c2n ../deps/servant-reflex/deps/servant-snap/deps/snap) {});
    snap-server = dc (self.callPackage (c2n ../deps/servant-reflex/deps/servant-snap/deps/snap/deps/snap-server) {});
    io-streams-haproxy = dc (self.callPackage (c2n ../deps/servant-reflex/deps/servant-snap/deps/snap/deps/io-streams-haproxy) {});
    servant = dc (self.callPackage (c2n ../deps/servant/servant) {});
    servant-snap = dc (self.callPackage (c2n ../deps/servant-reflex/deps/servant-snap) {});
    # servant-reflex = self.callPackage (cabal2nixResult ../deps/servant-reflex) {};
    vector = dc super.vector;
    Glob = dc super.Glob;
    lens = dc super.lens;
  };
}
