{
  description = "Parsecable class, and utilities; base version without file parsing";

  inputs = {
    nixpkgs.url       = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    build-utils.url   = github:sixears/flake-build-utils/r1.0.0.13;

    has-callstack.url = github:sixears/has-callstack/r1.0.1.19;
    monaderror-io.url = github:sixears/monaderror-io/r1.2.5.20;
    more-unicode.url  = github:sixears/more-unicode/r0.0.17.12;
  };

  outputs = { self, nixpkgs, build-utils
            , has-callstack, monaderror-io, more-unicode }:
    build-utils.lib.hOutputs self nixpkgs "parsec-plus-base" {
      deps = {
        inherit has-callstack monaderror-io more-unicode;
      };
      ghc = p: p.ghc8107; # for tfmt
      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, base-unicode-symbols, data-textual, deepseq, lens
                    , mtl, parsec, text-printer
                    }:
        mkDerivation {
          pname = "parsec-plus-base";
          version = "1.0.5.23";
          src = ./.;
          libraryHaskellDepends = [
            base base-unicode-symbols data-textual deepseq lens mtl parsec
            text-printer
          ] ++ mapPkg [ has-callstack monaderror-io more-unicode ];
          testHaskellDepends = [ base ];
          description = "Parsecable class, and utilities; base version without file parsing";
          license = lib.licenses.mit;
        };
    };
}
