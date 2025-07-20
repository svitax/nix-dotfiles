{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "too-wide-minibuffer-mode";
  version = "20250525.1932";
  src = pkgs.fetchFromGitHub {
    owner = "hron";
    repo = "too-wide-minibuffer-mode";
    rev = "8e33c87bb28c447d00fca9390efbf75d110893cf";
    hash = "sha256-+zGp4KvQc12GAUDe/9SPx2TzJJR1U5sMeoAc3pWWPlw=";
  };
  recipe = pkgs.writeText "recipe" ''
    (too-wide-minibuffer-mode
     :repo "hron/too-wide-minibuffer-mode"
     :fetcher github)
  '';
  packageRequires = with epkgs; [
  ];
  meta = {
    homepage = "https://github.com/hron/too-wide-minibuffer-mode";
    license = lib.licenses.gpl3;
  };
}
