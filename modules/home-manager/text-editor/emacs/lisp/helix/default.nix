{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "helix";
  version = "20250622.1135";
  src = pkgs.fetchFromGitHub {
    owner = "anuvyklack";
    repo = "helix.el";
    rev = "7af64a8b47063f3499e9baf95d61f624837878fe";
    hash = "sha256-eCQq+0oKhjucaF9/EYdmYswZd+4aZtTaSORDAY6bnDI=";
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
