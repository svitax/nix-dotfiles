{ config, pkgs, ... }:
let
  elisp = src: file:
    pkgs.runCommand "${file}.el" { } ''
      mkdir -p $out/share/emacs/site-lisp
      cp -r ${src}/* $out/share/emacs/site-lisp/
    '';
in {
  services.emacs = {
    enable = true;
    defaultEditor = true;
    # client.enable = true;
    startWithUserSession = "graphical";
  };
  systemd.user.services.emacs.Unit = {
    After = [ "graphical-session-pre.target" ];
    PartOf = [ "graphical-session.target" ];
  };
  home.packages = with pkgs; [ nixfmt nil statix ];
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ../.config/emacs/init.el;
      # defaultInitFile = true;
      # NOTE: emacs-pgtk doesn't build with emacs-jupyter
      package = pkgs.emacs-unstable.override { withNativeCompilation = true; };
      alwaysEnsure = true;
      extraEmacsPackages = epkgs:
        with epkgs; [
          treesit-grammars.with-all-grammars
          emacsql-sqlite
          all-the-icons
        ];
      override = epkgs:
        epkgs // {
          smart-tab-over = elisp (pkgs.fetchFromGitHub {
            owner = "alphapapa";
            repo = "smart-tab-over";
            rev = "24730ffba49979a26a5d1136b17407306b8013ab";
            hash = "sha256-KN4EjDbVJSpirSBkZKd0tOmcCrNZ9s8snNPMpwFQxlE=";
          }) "smart-tab-over";
          denote-explore = elisp (pkgs.fetchFromGitHub {
            owner = "pprevos";
            repo = "denote-explore";
            rev = "96cb16bb7394aea0f53b9da9a93cc50521c676dc";
            hash = "sha256-bW4E3boiSlAk3dYvLDzMViNY69RFPv/ks7TTYWJC8Io=";
          }) "denote-explore";
          app-launcher = elisp (pkgs.fetchFromGitHub {
            owner = "SebastienWae";
            repo = "app-launcher";
            rev = "d5015e394b0a666a8c7c4d4bdf786266e773b145";
            hash = "sha256-d0d5rkuxK/zKpSCa1UTdpV7o+RDDsEeab56rI7xUJ1E=";
          }) "app-launcher";
          bookmark-plus = elisp (pkgs.fetchFromGitHub {
            owner = "emacsmirror";
            repo = "bookmark-plus";
            rev = "f371815649ccac6cb610f6a5b32463cd91302af8";
            hash = "sha256-X6gyfWo9yuPGA2X4SFF2NKSaIxEEI1cGPCW1+iLmhNM=";
          }) "bookmark-plus";
          justify-kp = elisp (pkgs.fetchFromGitHub {
            owner = "Fuco1";
            repo = "justify-kp";
            rev = "33a186e297c0359547820088669486afd7b5fddb";
            hash = "sha256-4zT6cED3wQkLCXhi1mZd+LREISS6XFtktNN1CkItZ5I=";
          }) "justify-kp";
        };
    };
  };
  home.file.".config/emacs" = {
    recursive = true;
    source = ../.config/emacs;
  };
  # home.file.".config/emacs/init.el".source = ../.config/emacs/init.el;
  # home.file.".config/emacs/early-init.el".source =
  #   ../.config/emacs/early-init.el;
  # home.file.".config/emacs/modules/core-lib.el".source =
  #   ../.config/emacs/modules/core-lib.el;
  # home.file.".config/emacs/modules/elisp-cider-overlays.el".source =
  #   ../.config/emacs/modules/elisp-cider-overlays.el;
  # home.file.".config/emacs/modules/elisp-fontification.el".source =
  #   ../.config/emacs/modules/elisp-fontification.el;
  # home.file.".config/emacs/modules/elisp-indentation.el".source =
  #   ../.config/emacs/modules/elisp-indentation.el;
  # home.file.".config/emacs/modules/on.el".source =
  #   ../.config/emacs/modules/on.el;
  # home.file.".config/emacs/modules/biblio-gscholar.el".source =
  #   ../.config/emacs/modules/biblio-gscholar.el;
  # home.file.".config/emacs/modules/splash-screen.el".source =
  #   ../.config/emacs/modules/splash-screen.el;

  # home.file.".emacs.d/eshell/alias".source = ../.config/emacs/eshell/aliases;
  # home.file.".emacs.d/xemacs_color.svg".source =
  #   ../.config/emacs/xemacs_color.svg;
  # home.file.".emacs.d/emacs_gnu_color.svg".source =
  #   ../.config/emacs/emacs_gnu_color.svg;
}
