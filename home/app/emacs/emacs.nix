{ pkgs, config, ... }:
let
  elisp = src: file:
    pkgs.runCommand "${file}.el" { } ''
      mkdir -p $out/share/emacs/site-lisp
      cp -r ${src}/* $out/share/emacs/site-lisp/
    '';
in {
  programs.info = { enable = true; };
  services.emacs = {
    enable = true;
    defaultEditor = true;
  };
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      package = pkgs.emacs-unstable.override { withNativeCompilation = true; };
      extraEmacsPackages = epkgs:
        with epkgs; [
          treesit-grammars.with-all-grammars
          emacsql-sqlite
          all-the-icons
        ];
      override = epkgs:
        epkgs // {
	  # Mar 30 2024
          mood-line = elisp (pkgs.fetchFromGitLab {
            owner = "jessieh";
            repo = "mood-line";
	    rev = "a15d166249f04b047a6136856e5be109357762d3";
	    hash = "sha256-Y9n0p3jO5Ol/uUigrRNfrfxD5aeeb98NjNSDtroRffc=";
          }) "mood-line";
	};
    };
  };
  # programs.emacs = {
  #   enable = true;
  #   package = pkgs.emacs-unstable.override { withNativeCompilation = true; };
  #   extraPackages = epkgs:
  #     with epkgs; [
  #       treesit-grammars.with-all-grammars
  #       emacsql-sqlite
  #       all-the-icons
  #     ];
  # };
  xdg.configFile.emacs = {
    source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/nix-dotfiles/home/app/emacs/";
    recursive = true;
  };
}
