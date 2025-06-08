{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "org-fc";
  version = "20240531";
  src = pkgs.fetchFromGitHub {
    owner = "l3kn";
    repo = "org-fc";
    rev = "cc191458a991138bdba53328690a569b8b563502";
    hash = "sha256-wzMSgS4iZfpKOICqQQuQYNPb2h7i4tTWsMs7mVmgBt8=";
  };
  recipe = pkgs.writeText "recipe" ''
    (org-fc
     :files (:defaults "awk")
     :repo "l3kn/org-fc"
     :fetcher github)
  '';
  packageRequires = with epkgs; [
    pkgs.gawk
    hydra
    pkgs.findutils
  ];
  meta = {
    homepage = "";
    license = lib.licenses.gpl3Plus;
  };
}
