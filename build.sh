#!/usr/bin/env sh

deps/reflex-platform/work-on backend/overrides-ghc.nix ./backend --command "cd backend && cabal build && cd .."
deps/reflex-platform/work-on frontend/overrides.nix ./frontend --command "cd frontend && cabal configure --ghcjs && cabal build && cd .."
