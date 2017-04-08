{ mkDerivation, compiler ? "ghc", aeson, base, groundhog-th, groundhog, groundhog-ghcjs, groundhog-postgresql, http-api-data, servant
, stdenv, text
}:
mkDerivation {
  pname = "common";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base groundhog http-api-data servant text
  ] ++ (if compiler == "ghcjs" then [groundhog-ghcjs] else [groundhog-ghcjs]);
#  ] ++ (if compiler == "ghcjs" then [groundhog-ghcjs] else [groundhog-th groundhog-postgresql]);
  license = stdenv.lib.licenses.bsd3;
}
