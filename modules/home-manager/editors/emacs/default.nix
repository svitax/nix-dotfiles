{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.modules.editors.emacs;

  emacsBin = "${cfg.package}/bin/emacsclient";

  # TODO find a way to not have to define epkgs here and just inherit from my programs.emacs
  epkgs = pkgs.emacsPackagesFor cfg.package;
in
{
  options.modules.editors.emacs = {
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
          unzip # for nov.el
          single-file-cli # for +web-archive-url
          # chromium # need chromium to use single-file-cli
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
            _: with epkgs; [
              fontaine
              # modus-themes
              pulsar
              cursory
              highlight-numbers
              druid-modeline
              keycast
              visual-fill-column
              envrc
              inheritenv
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
              consult-eglot
              dape
              avy
              link-hint
              logos
              scratch-plus
              move-text
              expand-region
              multiple-cursors
              symbol-overlay
              # vundo
              flymake-collection
              hl-todo
              consult-todo
              apheleia
              treesit-grammars.with-all-grammars
              helpful
              devdocs
              magit
              diff-hl
              compile-multi
              consult-compile-multi
              # native-complete
              vterm
              ess
              pydoc
              nix-mode
              nix-ts-mode
              nix-update
              templ-ts-mode
              dts-mode
              git-modes
              jinx
              biblio
              # biblio-openlibrary
              # biblio-gbooks
              citar
              citar-embark
              denote
              consult-denote
              citar-denote
              org-remark
              org-noter
              anki-editor
              pdf-tools
              saveplace-pdf-view
              nov
              tmr
              shr-tag-pre-highlight
              elpher
              # leetcode
              gptel
              gptel-quick
              nerd-icons
              nerd-icons-completion
              nerd-icons-corfu
              nerd-icons-dired
              nerd-icons-ibuffer
              nerd-icons-grep
              compile-multi-nerd-icons
            ];
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

        programs.git.settings = {
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
      (lib.mkIf cfg.enableGitDiff { programs.git.settings.diff.tool = "ediff"; })
    ]
  );
}
