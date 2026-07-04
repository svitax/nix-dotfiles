{
  lib,
  pkgs,
  epkgs,
}:
# Rev/hash prefetched via nix-prefetch-github on 2026-06-13 (branch main).
epkgs.melpaBuild {
  pname = "agent-shell-ediff";
  version = "0.0.1-unstable-2026-06-13";
  src = pkgs.fetchFromGitHub {
    owner = "cassandracomar";
    repo = "agent-shell-ediff";
    rev = "cf70687f373decd23642cb662eeb117ea8ac4631";
    hash = "sha256-+ZFw4iCWVoPr5TnD4A3qZJ6k0PYpwtKWemLJ2OwRsEc=";
  };
  recipe = pkgs.writeText "recipe" ''
    (agent-shell-ediff :fetcher github :repo "cassandracomar/agent-shell-ediff")
  '';
  packageRequires = with epkgs; [
    agent-shell
  ];
  meta = {
    description = "ediff-based diff review for agent-shell";
    homepage = "https://github.com/cassandracomar/agent-shell-ediff";
    license = lib.licenses.gpl3Plus;
  };
}
