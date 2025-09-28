{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "nix-update";
  version = "20250817.1556";
  src = pkgs.fetchFromGitHub {
    owner = "jwiegley";
    repo = "nix-update-el";
    rev = "d67f4f7ba8c8ec43144600f5f970c5fd958fc2f7";
    sha256 = "151mfjfg16v2xp327cmq3wwdzbmjyv68w26z7br5wvqqkfwn8pi0";
  };
  patches = [
    ./nix-prefetch-git-with-nix-run.patch
  ];
  recipe = pkgs.writeText "recipe" ''
    (nix-update
     :repo "jwiegley/nix-update-el"
     :fetcher github)
  '';
  packageRequires = with epkgs; [ ];
  meta = {
    homepage = "https://github.com/jwiegley/nix-update-el";
    license = lib.licenses.gpl2Plus;
  };
}
