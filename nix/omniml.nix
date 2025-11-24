{
  lib,
  ocamlPackages,
}:
with ocamlPackages;
  buildDunePackage rec {
    pname = "omniml";
    version = "dev";

    src = lib.cleanSource ../.;

    nativeBuildInputs = [
      menhir
    ];

    propagatedBuildInputs = [
      core
      core_unix
      base_quickcheck
      async
      ppx_jane
      grace
      fmt
      menhir
      ppx_quick_test
    ];
  }
