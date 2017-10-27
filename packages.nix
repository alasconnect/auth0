{ mkDerivation, aeson, base, bytestring, containers, exceptions
, hspec, http-conduit, http-types, mtl, stdenv, tagged, text
, transformers
}:
mkDerivation {
  pname = "auth0";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers exceptions http-conduit http-types
    mtl tagged text transformers
  ];
  testHaskellDepends = [ base bytestring hspec ];
  homepage = "https://github.com/alasconnect/auth0";
  description = "Auth0 API";
  license = stdenv.lib.licenses.asl20;
}
