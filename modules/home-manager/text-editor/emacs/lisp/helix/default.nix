{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "helix";
  version = "20250622.1829";
  src = pkgs.fetchFromGitHub {
    owner = "anuvyklack";
    repo = "helix.el";
    rev = "c417a5230713e36b5014a0a5203d76d810f8a7b3";
    hash = "sha256-amX7awgyhxopuZe5a+KB5yD9wuDSfANNfibRfa9PzYg=";
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
