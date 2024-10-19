{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types;
  inherit (inputs.self) outputs;

  cfg = config.editor.emacs;

  emacsBin = "${cfg.package}/bin/emacsclient";

  elPackage = name: src:
    pkgs.runCommand "${name}-epkg" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp -r ${src}/* $out/share/emacs/site-lisp/
    '';
in {
  options.editor.emacs = {
    enable = mkEnableOption "Emacs";

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

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      # TODO: extract git, ripgrep, and fd config
      # TODO: extract go and gopls config?
      home.packages = with pkgs; [
        unzip # for nov.el
        emacs-lsp-booster
        #
        go
        gopls
        golangci-lint
        pyright
      ];

      nixpkgs.overlays = [
        inputs.emacs-overlay.overlay
        inputs.emacs-overlay.overlays.package
        outputs.overlays.additions
      ];

      editor.emacs.package = pkgs.emacsWithPackagesFromUsePackage {
        config = ./init.el;
        package = pkgs.emacs-unstable.override {
          withTreeSitter = true;
          withNativeCompilation = true;
        };
        extraEmacsPackages = epkgs:
          with epkgs; [
            treesit-grammars.with-all-grammars
            pdf-tools
            # emacsql-sqlite
            all-the-icons
            #
            persid
            corg
            parsebib
            citar
            macrursors
            visual-replace
            projel
            eglot-booster
            org-block-capf
            org-src-context
          ];
        override = epkgs:
          epkgs
          // {
            persid = elPackage "persid" (pkgs.fetchFromGitHub {
              owner = "rougier";
              repo = "persid";
              rev = "6980ce24a63d9c3a6b25c7ac5db6621d9daa2c4c";
              hash = "sha256-GPzvfTnMjltq2ljj3/0HTE37J/piO90TLbMTGti3x1U=";
            });
            corg = elPackage "corg" (pkgs.fetchFromGitHub {
              owner = "isamert";
              repo = "corg.el";
              rev = "16d2b80fb2ff4daf0ab9bf89e5c9f649431337fc";
              hash = "sha256-BX78yKSOj4TOPoXkLdyhrjges42h0+Wb8PUYR4XANR4=";
            });
            # BUG: because someone decided to break parsebib (and citar) after 2 years for no good fucking reason and emacs-overlay hasn't picked up the fix yet
            parsebib = elPackage "parsebib" (pkgs.fetchFromGitHub {
              owner = "joostkremers";
              repo = "parsebib";
              rev = "73fe6204a3f204b45316fc25f17ad035a00acc16";
              hash = "sha256-FTCyZ+EmXy1nIDsGrrOQcRcJig+pDA2S/+U/76cp/Wc=";
            });
            citar = elPackage "citar" (pkgs.fetchFromGitHub {
              owner = "emacs-citar";
              repo = "citar";
              rev = "0f1786b7fee58452a3225e4b9b7c94176fff9b5a";
              hash = "sha256-saOtIu44FAKOfAo59eG5LG6G6ORqYePcco/1iSI3Uww=";
            });
            macrursors = elPackage "macrursors" (pkgs.fetchFromGitHub {
              owner = "corytertel";
              repo = "macrursors";
              rev = "926d93a4f7b3edb047b79a50f8cfd6072227e94e";
              hash = "sha256-ytYBMTqcKEMng8s1Zy5Sn33Kh2iP7fyvpJqSRDDx+UU=";
            });
            visual-replace = elPackage "visual-replace" (pkgs.fetchFromGitHub {
              owner = "szermatt";
              repo = "visual-replace";
              rev = "7675534be785f66e548c9783c828c47172c8e36a";
              hash = "sha256-f0qQ4I4je1vArdDxexxVgWHHqDBvEoxlDg08Tx4SA94=";
            });
            projel = elPackage "projel" (pkgs.fetchFromGitHub {
              owner = "KarimAziev";
              repo = "projel";
              rev = "0de8115e5df0c2eb76491142d052c826a9ff9dc5";
              hash = "sha256-M47q+DKgTO1BBIM3jYrFjgbCVXi7gBRmq/lZr15bmfY=";
            });
            eglot-booster = elPackage "eglot-booster" (pkgs.fetchFromGitHub {
              owner = "jdtsmith";
              repo = "eglot-booster";
              rev = "e19dd7ea81bada84c66e8bdd121408d9c0761fe6";
              hash = "sha256-vF34ZoUUj8RENyH9OeKGSPk34G6KXZhEZozQKEcRNhs=";
            });
            org-block-capf = elPackage "org-block-capf" (pkgs.fetchFromGitHub {
              owner = "xenodium";
              repo = "org-block-capf";
              rev = "080cfd2ed630a6739633b07a8ab6b896a1b5ef4a";
              hash = "sha256-iQPuKkIrAfpFf6G9E1kCOOCoB2riSuuRrtpfeBe20uc=";
            });
            org-src-context = elPackage "org-src-context" (pkgs.fetchFromGitHub {
              owner = "karthink";
              repo = "org-src-context";
              rev = "625fc800950ed16dbf77c666e5129087b2315e2a";
              hash = "sha256-znfBXCWpooZTOMuP4ap2wjUsSpaz41NS2h9YSdgZacQ=";
            });
          };
      };

      # Install our Emacs package
      # home.packages = [ cfg.package ];

      programs.info.enable = true;

      services.emacs = {
        enable = true;
        package = cfg.package;
        # idk what this does
        socketActivation.enable = true;
        client.enable = true;
      };

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
    (lib.mkIf cfg.defaultEditor {services.emacs.defaultEditor = true;})
    (lib.mkIf cfg.enableGitDiff {programs.git.extraConfig.diff.tool = "ediff";})
  ]);
}
