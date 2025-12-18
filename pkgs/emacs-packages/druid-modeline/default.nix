{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "druid-modeline";
  version = "20250610.1031";
  src = ./.;
  recipe = pkgs.writeText "recipe" ''
    (druid-modeline :fetcher git :url "localhost")
  '';
  packageRequires = with epkgs; [ ];
  meta = {
    homepage = "";
    license = lib.licenses.gpl3Plus;
  };
}
