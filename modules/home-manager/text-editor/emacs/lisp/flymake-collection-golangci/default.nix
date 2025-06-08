# {
#   lib,
#   pkgs,
# }:
# pkgs.emacsPackages.trivialBuild {
#   pname = "flymake-collection-golangci";
#   version = "1.0";
#   src = ./.;
#   buildInputs = with pkgs.emacsPackages; [ flymake-collection ];
# }

{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "flymake-collection-golangci";
  version = "20250522";
  src = ./.;
  recipe = pkgs.writeText "recipe" ''
    (flymake-collection-golangci :fetcher git :url "localhost")
  '';
  packageRequires = with epkgs; [
    flymake-collection
  ];
  meta = {
    homepage = "";
    license = lib.licenses.gpl3Plus;
  };
}
