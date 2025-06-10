{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.text-editor.emacs;

  emacsBin = "${cfg.package}/bin/emacsclient";

  # TODO find a way to not have to define epkgs here and just inherit from my programs.emacs
  epkgs = pkgs.emacsPackagesFor cfg.package;

  lispDir = ../emacs/lisp;
  lispPackages = builtins.attrValues (
    builtins.mapAttrs (
      _name: type:
      if type == "directory" && builtins.pathExists (lispDir + "/${_name}/default.nix") then
        import (lispDir + "/${_name}") {
          inherit lib pkgs;
          inherit epkgs;
        }
      else
        null
    ) (builtins.readDir lispDir)
  );
in
{
  options.text-editor.emacs = {
    enable = mkEnableOption "Enable Emacs.";

    package = mkOption {
      type = types.package;
      default = pkgs.emacs-unstable;
      defaultText = lib.literalExample "pkgs.emacs-unstable-pgtk";
      description = "The Emacs derivation to use.";
    };

    enableGitDiff = mkOption {
      type = types.bool;
      default = true;
      description = "Enable ediff as default git diff tool.";
    };

    defaultEditor = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to use Emacs as default editor";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        nixpkgs.overlays = [
          inputs.emacs-overlay.overlay
          inputs.emacs-overlay.overlays.package
          # outputs.overlays.additions
        ];

        home.packages = with pkgs; [
          difftastic # for difftastic.el
          unzip # for nov.el
          emacs-lsp-booster
          single-file-cli
          chromium # need chromium to use single-file-cli
          # monolith
          # `jinx' cant seem to find aspell dictionaries. hunspell
          # seems to work better, but we need to override libenchant
          # to not use aspell or hspell.
          hunspellDicts.en-us
          hunspellDicts.pt-br
          hunspellDicts.es-ar
          (enchant.override {
            withHspell = false;
            withAspell = false;
          })
        ];

        programs.emacs = {
          enable = true;
          # package = pkgs.emacs-unstable;
          inherit (cfg) package;
          extraPackages =
            _:
            with epkgs;
            [
              # evil
              # meow
              fontaine
              modus-themes
              ef-themes
              doric-themes
              pulsar
              cursory
              highlight-numbers
              envrc
              inheritenv
              add-node-modules-path
              marginalia
              orderless
              vertico
              consult
              consult-dir
              embark
              embark-consult
              corfu
              cape
              tempel
              tempel-collection
              eglot-tempel
              # projection
              substitute
              goto-chg
              dired-preview
              # popper
              avy
              ace-link
              logos
              mowie
              move-text
              expand-region
              multiple-cursors
              # vundo
              flymake-collection
              apheleia
              treesit-grammars.with-all-grammars
              helpful
              difftastic
              magit
              git-gutter
              git-gutter-fringe
              vterm
              mistty
              jupyter
              nix-ts-mode
              # templ-ts-mode
              dts-mode
              jinx
              anki-editor
              biblio
              # biblio-openlibrary
              # biblio-gbooks
              citar
              citar-embark
              denote
              consult-denote
              citar-denote
              # org-remark
              markdown-mode
              pdf-tools
              saveplace-pdf-view
              nov
              shr-tag-pre-highlight
              elpher
              # erc-hl-nicks
              # leetcode
              # gptel
              nerd-icons
              nerd-icons-completion
              nerd-icons-corfu
              nerd-icons-dired
              nerd-icons-ibuffer
            ]
            ++ lispPackages;
        };

        services.emacs = with pkgs; {
          enable = true;
          client.enable = true;
          startWithUserSession = "graphical";
        };

        # xdg.mimeApps = {
        #   enable = true;
        #   defaultApplications = {
        #     "application/text" = "emacsclient.desktop";
        #     "application/textedit" = "emacsclient.desktop";
        #     "text/anytext" = "emacsclient.desktop";
        #     "text/plain" = "emacsclient.desktop";
        #   };
        # };

        programs.info.enable = true;
        programs.man.enable = true;

        programs.git.extraConfig = {
          difftool.diff.cmd = ''
            ${emacsBin} --eval '(ediff-files "'$LOCAL'" "'$REMOTE'")'
          '';
          mergetool.ediff.cmd = ''
            ${emacsBin} --eval '(ediff-merge-files-with-ancestor "'$LOCAL'" "'$REMOTE'" '"$BASE'" nil "'$MERGED'")'
          '';
        };

        xdg.enable = true;
        xdg.configFile."emacs" = {
          source = ../emacs;
          recursive = true;
        };
      }
      (lib.mkIf cfg.defaultEditor { services.emacs.defaultEditor = true; })
      (lib.mkIf cfg.enableGitDiff { programs.git.extraConfig.diff.tool = "ediff"; })
    ]
  );
}
