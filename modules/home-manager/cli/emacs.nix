{ config, pkgs, ... }:
let
  elisp = src: file:
    pkgs.runCommand "${file}.el" { } ''
      mkdir -p $out/share/emacs/site-lisp
      cp -r ${src}/* $out/share/emacs/site-lisp/
    '';
in {
  programs.emacs = {
    enable = true;
    package = (pkgs.emacsWithPackagesFromUsePackage {
      config = ../.config/emacs/init.el;
      # defaultInitFile = true;
      package = pkgs.emacs29-pgtk.override { withNativeCompilation = true; };
      alwaysEnsure = true;
      extraEmacsPackages = epkgs:
        with epkgs; [
          treesit-grammars.with-all-grammars
          emacsql-sqlite
          nerd-icons
        ];
      override = epkgs:
        epkgs // {
          doom-nano-modeline = elisp (pkgs.fetchFromGitHub {
            owner = "ronisbr";
            repo = "doom-nano-modeline";
            rev = "196fb7db2a177fd6c827a1e775cfada05c33da77";
            sha256 = "sha256-fvYraJKFdbaCELrfeSzsTvvOA5yhsLVRLe5AdXkt0ig=";
          }) "doom-nano-modeline";
        };
    });
  };
  home.file.".emacs.d/init.el".source = ../.config/emacs/init.el;
  home.file.".emacs.d/early-init.el".source = ../.config/emacs/early-init.el;
}
