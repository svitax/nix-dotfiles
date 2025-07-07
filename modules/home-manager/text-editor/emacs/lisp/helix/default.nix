{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "helix";
  version = "20250702.1106";
  src = pkgs.fetchFromGitHub {
    owner = "anuvyklack";
    repo = "helix.el";
    rev = "8e4db3865b881efc0b5e1490810fae60d7e2169f";
    hash = "sha256-+9j0Ap22WBAFmfWmGnFB3jQM+TQa8FvvY0Nf0b/HkJI=";
  };
  recipe = pkgs.writeText "recipe" ''
    (helix :repo "anuvyklack/helix.el" :fetcher github)
  '';
  packageRequires = with epkgs; [
    s
    dash
    pcre2el
    avy
    evil-matchit
  ];
  meta = {
    homepage = "";
    license = lib.licenses.gpl3Plus;
  };
}
