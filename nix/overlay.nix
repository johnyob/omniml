final: prev:
with prev; {
  ocamlPackages = final.ocaml-ng.ocamlPackages_5_2;

  ocaml-ng =
    ocaml-ng
    // (with ocaml-ng; {
      ocamlPackages_5_2 = ocamlPackages_5_2.overrideScope (
        _: prev:
          with prev; {
            grace = buildDunePackage {
              pname = "grace";
              version = "0.4.1";

              minimalOCamlVersion = "4.14";

              src = fetchFromGitHub {
                owner = "johnyob";
                repo = "grace";
                rev = "9ebd80819fe6963ca9af25ff380a26987885ae36";
                hash = "sha256-dhFECxv7Tzp1MR7WwZ+2W6w89exbxO4fvTQO0aTwmVk=";
              };
              propagatedBuildInputs = [fmt dedent iter yojson];
              checkInputs = [core core_unix ppx_jane dedent];
            };
          }
      );
    });
}
