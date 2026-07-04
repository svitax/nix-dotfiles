{
  lib,
  pkgs,
  epkgs,
}:
# Rev/hash prefetched via nix-prefetch-github on 2026-06-13 (branch main).
epkgs.melpaBuild {
  pname = "agent-shell-macext";
  version = "0.1.0-unstable-2026-06-13";
  src = pkgs.fetchFromGitHub {
    owner = "cxa";
    repo = "agent-shell-macext";
    rev = "ae3a1603e1d7a138f7eddfcd967e097dcca8b612";
    hash = "sha256-JUmLUh9ofn083xVHIaWiKCqZPGlMiYXmhSQjqQk91Ek=";
  };
  recipe = pkgs.writeText "recipe" ''
    (agent-shell-macext :fetcher github :repo "cxa/agent-shell-macext")
  '';
  packageRequires = with epkgs; [
    agent-shell
  ];
  meta = {
    description = "macOS native extensions (notifications, file copy) for agent-shell";
    homepage = "https://github.com/cxa/agent-shell-macext";
    license = lib.licenses.gpl3Plus;
    platforms = lib.platforms.darwin;
  };
}
