#! /usr/bin/env sh

../deps/reflex-platform/work-on ./overrides.nix ./. --command "cabal configure --builddir jsbuild --ghcjs && cabal build --builddir=jsbuild && cp jsbuild/build/frontend/frontend.jsexe/* ../static" --fallback
