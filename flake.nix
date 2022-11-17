{
  description = "Parsecable class, and utilities; base version without file parsing";

  inputs = {
    nixpkgs.url       = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url   = "github:sixears/flake-build-utils/r1.0.0.11";

    has-callstack.url = "github:sixears/has-callstack/r1.0.1.11";
    monaderror-io.url = "github:sixears/monaderror-io/r1.2.5.10";
    more-unicode.url  = "github:sixears/more-unicode/r0.0.17.8";
  };

  outputs = { self, nixpkgs, build-utils
            , has-callstack, monaderror-io, more-unicode }:
    build-utils.lib.hOutputs self nixpkgs "parsec-plus-base" {
      deps = {
        inherit has-callstack monaderror-io more-unicode;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
