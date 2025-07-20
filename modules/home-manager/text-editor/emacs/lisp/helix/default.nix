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
    rev = "8619a79340a9cabc31de2b19714bdf4aa8fe5233";
    hash = "sha256-6DwQCJelHgA6oXSyxgXcxQFVYGyMEa0K+fCZJvjz7m8=";
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
