{
  lib,
  pkgs,
  epkgs,
}:
epkgs.melpaBuild {
  pname = "claude-code-ide";
  version = "0.1.0-unstable-2026-06-14";
  src = pkgs.fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "a9485f766ea69f6cb3a3f08dea20d44fd6596673";
    hash = "sha256-6kaTPI2CCsdxxiCpZ7qqciv/HJCQRsJ8084+SqW8Idc=";
  };
  recipe = pkgs.writeText "recipe" ''
    (claude-code-ide :fetcher github :repo "manzaltu/claude-code-ide.el")
  '';
  packageRequires = with epkgs; [
    vterm
    websocket
    transient
    web-server
  ];
  meta = {
    description = "Claude Code IDE integration for Emacs";
    homepage = "https://github.com/manzaltu/claude-code-ide.el";
    license = lib.licenses.gpl3Plus;
  };
}
