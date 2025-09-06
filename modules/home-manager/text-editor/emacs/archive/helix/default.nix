{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "helix";
  version = "20250903.1042";
  src = pkgs.fetchFromGitHub {
    owner = "anuvyklack";
    repo = "helix.el";
    rev = "164da93b1439e7c2eaa34a1bf9760d7173611d29";
    hash = "sha256-WoMZ6XicDlMDpytGafm0cqyPoHsCB4ny3N4ytg18Zrw=";
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
    homepage = "https://github.com/anuvyklack/helix.el";
    license = lib.licenses.gpl3Plus;
  };
}
