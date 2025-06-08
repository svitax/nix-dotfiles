{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "auto-side-windows";
  version = "20250531.1108";
  src = pkgs.fetchFromGitHub {
    owner = "MArpogaus";
    repo = "auto-side-windows";
    rev = "d461116428c26edd68b8cda361dc31a39b6ad7e8";
    hash = "sha256-GqibKikPH+dEzrR1LjrjZLGdTBsIg1YNnfHphoX/CuY=";
  };
  recipe = pkgs.writeText "recipe" ''
    (auto-side-windows
     :repo "MArpogaus/auto-side-windows"
     :fetcher github)
  '';
  packageRequires = with epkgs; [
  ];
  meta = {
    homepage = "https://github.com/MArpogaus/auto-side-windows";
    license = lib.licenses.gpl3;
  };
}
