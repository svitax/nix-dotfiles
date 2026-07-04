{
  inputs,
  ...
}:
{
  flake.modules.darwin.emacs-darwin = {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];
  };

  flake.modules.homeManager.emacs-darwin =
    { pkgs, ... }:
    let
      emacsIcon = pkgs.fetchurl {
        url = "https://github.com/d12frosted/homebrew-emacs-plus/raw/master/community/icons/savchenkovaleriy-big-sur-curvy-3d/icon.icns";
        hash = "sha256-1phSRxzHEbK8vj7MdyYjYOKphb3JH0Wes4Y5yIOL7+4=";
      };
      myEmacs = pkgs.emacs-git.overrideAttrs (old: {
        patches = old.patches ++ [
          (pkgs.fetchpatch {
            url = "https://github.com/d12frosted/homebrew-emacs-plus/raw/master/patches/emacs-31/system-appearance.patch";
            hash = "sha256-Uyg1A9te0oh+nXM7qq+A8sgQ5mjngumIvaWFWgsevrQ=";
          })
          (pkgs.fetchpatch {
            url = "https://github.com/d12frosted/homebrew-emacs-plus/raw/master/patches/emacs-31/round-undecorated-frame.patch";
            hash = "sha256-JpR7ZyT8KfrdRIiYCMXPC0zmJ4zwT0YIaiHfUMjEFR0=";
          })
          (pkgs.fetchpatch {
            url = "https://github.com/d12frosted/homebrew-emacs-plus/raw/master/patches/emacs-31/fix-ns-x-colors.patch";
            hash = "sha256-nl0+JqjTiNOgALaX1YJ2lkXKk61Ze0ETdE3rpLiai54=";
          })
        ];
        postPatch =
          old.postPatch
          + ''
            cp -f ${emacsIcon} nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns
          '';
      });
    in
    {
      programs.emacs = {
        enable = true;
        package = myEmacs;
        extraPackages = epkgs:
          with epkgs; [
            modus-themes
            fontaine
            cursory
            lin
            rainbow-mode
            keycast
            visual-fill-column
            orderless
            vertico
            marginalia
            embark
            embark-consult
            avy
            link-hint
            consult
            consult-eglot
            consult-compile-multi
            move-text
            flymake-collection
            apheleia
            hl-todo
            dape
            devdocs
            diff-hl
            compile-multi
            envrc
            inheritenv
            jinx
            vterm
            treesit-grammars.with-all-grammars
            nix-mode
            nix-ts-mode
            nix-update
            ess
            pydoc
            dts-mode
            git-modes
            pdf-tools
            saveplace-pdf-view
            nov
            denote
            consult-denote
            biblio
            citar
            citar-denote
            citar-embark
            tmr
            notmuch
            consult-notmuch
            ol-notmuch
            shr-tag-pre-highlight
            elpher
            gptel
            gptel-quick
            nerd-icons
            nerd-icons-completion
            nerd-icons-dired
            nerd-icons-ibuffer
            nerd-icons-grep
            compile-multi-nerd-icons
            markdown-mode
            agent-shell
            agent-shell-macext
            agent-shell-ediff
            claude-code-ide
            exec-path-from-shell
          ];
      };

      services.emacs = {
        enable = true;
        defaultEditor = true;
      };

      home.packages = [ pkgs.coreutils-prefixed ];

      home.file.".emacs.d/init.el".source = ./init.el;
    };
}
