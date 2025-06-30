{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "helix";
  version = "20250628.1430";
  src = pkgs.fetchFromGitHub {
    owner = "anuvyklack";
    repo = "helix.el";
    rev = "9aeb3279ce210fd29da09ad337b7788d49fed35c";
    hash = "sha256-HxjThE3tu6/8XnXL7YgYybdfGeuAgEhILzK/p213dEQ=";
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
