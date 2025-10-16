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

  localInfoFiles = (
    pkgs.runCommand "local-info-files"
      {
        src = ./info;
      }
      ''
        mkdir -p $out/share/info
        cp $src/*.info $out/share/info/

        for file in $out/share/info/*.info; do
          ${pkgs.texinfo}/bin/install-info "$file" $out/share/info/dir
        done
      ''
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
          single-file-cli # for +web-archive-url
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
          localInfoFiles
        ];

        programs.emacs = {
          enable = true;
          # package = pkgs.emacs-unstable;
          inherit (cfg) package;
          extraPackages =
            _:
            with epkgs;
            [
              fontaine
              modus-themes
              # ef-themes
              # doric-themes
              pulsar
              cursory
              highlight-numbers
              keycast
              envrc
              inheritenv
              # add-node-modules-path
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
              # substitute
              goto-chg
              # dired-preview
              avy
              ace-link
              link-hint
              logos
              move-text
              expand-region
              multiple-cursors
              # vundo
              flymake-collection
              apheleia
              treesit-grammars.with-all-grammars
              helpful
              devdocs
              # difftastic
              magit
              git-gutter
              git-gutter-fringe
              eldoc-diffstat
              # native-complete
              # vterm
              mistty
              ess
              pydoc
              nix-mode
              nix-ts-mode
              # templ-ts-mode
              dts-mode
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
              markdown-mode
              pdf-tools
              saveplace-pdf-view
              nov
              tmr
              # empv
              mpv
              shr-tag-pre-highlight
              elpher
              # erc-hl-nicks
              # leetcode
              gptel
              nerd-icons
              nerd-icons-completion
              nerd-icons-corfu
              nerd-icons-dired
              nerd-icons-ibuffer
              nerd-icons-grep
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
