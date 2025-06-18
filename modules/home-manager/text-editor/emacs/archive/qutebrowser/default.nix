{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "qutebrowser";
  version = "20250618.1340";
  src = pkgs.fetchFromGitHub {
    owner = "lrustand";
    repo = "qutebrowser.el";
    rev = "137f066dac06e1abdbf3285561ce2e1f3059b491";
    hash = "sha256-kXBZxE5/WR/FDGacVpN3M998E+wt/XmsEhrPldmM4jY=";
  };
  recipe = pkgs.writeText "recipe" ''
    (qutebrowser
     :repo "lrustand/qutebrowser"
     :fetcher github
     :files (:defaults "*.py"))
  '';
  packageRequires = with epkgs; [
    consult
    exwm
    doom-modeline
    evil
    password-store
    password-store-otp
  ];
  meta = {
    homepage = "https://github.com/lrustand/qutebrowser.el/";
    license = lib.licenses.gpl3Plus;
  };
}
