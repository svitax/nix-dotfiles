{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.editor.emacs;

  emacsBin = "${cfg.package}/bin/emacsclient";

  elisp = src: file:
    pkgs.runCommand "${file}.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp -r ${src}/* $out/share/emacs/site-lisp/
    '';
	elPackage = name: src:
		pkgs.runCommand "${name}-epkg" { } ''
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
      home.packages = with pkgs; [
        git
        ripgrep
        fd
      ];
      
      nixpkgs.overlays = [
        inputs.emacs-overlay.overlay
        inputs.emacs-overlay.overlays.package
      ];

      editor.emacs.package = pkgs.emacsWithPackagesFromUsePackage {
        config = ./init.el;
        package = pkgs.emacs-unstable-pgtk.override {
          withTreeSitter = true;
          withNativeCompilation = true;
        };
        extraEmacsPackages = epkgs:
          with epkgs; [
            treesit-grammars.with-all-grammars
            emacsql-sqlite
            all-the-icons
            #
            macrursors
            smart-tab-over
            visual-replace
          ];
        override = epkgs: epkgs // {
          macrursors = elPackage "macrursors" (pkgs.fetchFromGitHub {
            owner = "corytertel";
            repo = "macrursors";
            rev = "926d93a4f7b3edb047b79a50f8cfd6072227e94e";
            hash = "sha256-ytYBMTqcKEMng8s1Zy5Sn33Kh2iP7fyvpJqSRDDx+UU=";           
          });
          smart-tab-over = elPackage "smart-tab-over" (pkgs.fetchFromGitHub {
            owner = "alphapapa";
            repo = "smart-tab-over";
            rev = "24730ffba49979a26a5d1136b17407306b8013ab";
            hash = "sha256-KN4EjDbVJSpirSBkZKd0tOmcCrNZ9s8snNPMpwFQxlE=";
          });
          visual-replace = elPackage "visual-replace" (pkgs.fetchFromGitHub {
            owner = "szermatt";
            repo = "visual-replace";
            rev = "7675534be785f66e548c9783c828c47172c8e36a";
            hash = "sha256-f0qQ4I4je1vArdDxexxVgWHHqDBvEoxlDg08Tx4SA94=";
          });
        };
      };

      # Install our Emacs package
      # home.packages = [ cfg.package ];

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
      xdg.configFile."emacs/init.el".source = ./init.el;
      xdg.configFile."emacs/early-init.el".source = ./early-init.el;
      xdg.configFile."emacs/plugins" = {
        source = ./plugins;
        recursive = true;
      };
    }
    (lib.mkIf cfg.defaultEditor {services.emacs.defaultEditor = true;})
    (lib.mkIf cfg.enableGitDiff {programs.git.extraConfig.diff.tool = "ediff";})
  ]);
}
