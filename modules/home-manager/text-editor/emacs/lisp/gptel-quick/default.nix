{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "gptel-quick";
  version = "20251105.1158";
  src = pkgs.fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-quick";
    rev = "018ff2be8f860a1e8fe3966eec418ad635620c38";
    hash = "sha256-7a5+YQifwtVYHP6qQXS1yxA42bVGXmErirra0TrSSQ0=";
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
    homepage = "https://github.com/karthink/gptel-quick";
    license = lib.licenses.gpl3Plus;
  };
}
