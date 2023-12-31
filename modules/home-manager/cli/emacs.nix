{ config, pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = (pkgs.emacsWithPackagesFromUsePackage {
      config = ../.config/emacs/config.el;
      defaultInitFile = true;
      alwaysEnsure = true;
      extraEmacsPackages = epkgs:
        with epkgs; [
          treesit-grammars.with-all-grammars
          emacsql-sqlite
          nerd-icons
        ];
    });
  };
}
