{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "gptel-quick";
  version = "20250601.1010";
  src = pkgs.fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-quick";
    rev = "495b5e0b5348dbced1448bd12cbf8847e30b5175";
    hash = "sha256-xMrzeWG5L+MpGAhPFlV8KV+xa7CWC1D48osRrioGlsw=";
  };
  recipe = pkgs.writeText "recipe" ''
    (gptel-quick
     :repo "karthink/gptel-quick"
     :fetcher github)
  '';
  packageRequires = with epkgs; [
    gptel
  ];
  meta = {
    homepage = "";
    license = lib.licenses.gpl3Plus;
  };
}
