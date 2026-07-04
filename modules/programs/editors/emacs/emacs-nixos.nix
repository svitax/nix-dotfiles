{
  inputs,
  ...
}:
{
  flake.modules.homeManager.emacs-nixos =
    { pkgs, ... }:
    {
      nixpkgs.overlays = [
        inputs.emacs-overlay.overlay
        inputs.emacs-overlay.overlays.package
      ];

      home.packages = with pkgs; [
        unzip
        single-file-cli
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
        package = pkgs.emacs-unstable;
        extraPackages = _: with pkgs.emacsPackages; [
          fontaine pulsar lin cursory rainbow-mode highlight-numbers
          druid-modeline keycast visual-fill-column envrc inheritenv
          marginalia orderless vertico consult consult-dir
          embark embark-consult corfu cape tempel tempel-collection
          eglot-tempel consult-eglot dape ace-window avy link-hint
          logos scratch-plus move-text expand-region multiple-cursors
          symbol-overlay flymake-collection hl-todo consult-todo apheleia
          treesit-grammars.with-all-grammars helpful devdocs magit diff-hl
          compile-multi consult-compile-multi vterm ess pydoc
          nix-mode nix-ts-mode nix-update templ-ts-mode dts-mode git-modes
          jinx biblio citar citar-embark denote consult-denote citar-denote
          org-remark org-noter anki-editor pdf-tools saveplace-pdf-view nov
          tmr notmuch consult-notmuch ol-notmuch shr-tag-pre-highlight
          elpher gptel gptel-quick nerd-icons nerd-icons-completion
          nerd-icons-corfu nerd-icons-dired nerd-icons-ibuffer
          nerd-icons-grep compile-multi-nerd-icons
        ];
      };

      services.emacs = {
        enable = true;
        client.enable = true;
        startWithUserSession = "graphical";
        defaultEditor = true;
      };

      programs.git.settings = {
        difftool.diff.cmd = "emacsclient --eval '(ediff-files \"'$LOCAL'\" \"'$REMOTE'\")' ";
        mergetool.ediff.cmd = "emacsclient --eval '(ediff-merge-files-with-ancestor \"'$LOCAL'\" \"'$REMOTE'\" \"'$BASE'\" nil \"'$MERGED'\")' ";
        diff.tool = "ediff";
      };

      xdg.enable = true;
      xdg.configFile."emacs" = {
        source = ./nixos-config;
        recursive = true;
      };
      xdg.mimeApps = {
        enable = true;
        defaultApplications = {
          "application/text" = "emacsclient.desktop";
          "application/textedit" = "emacsclient.desktop";
          "text/anytext" = "emacsclient.desktop";
          "text/plain" = "emacsclient.desktop";
          "x-scheme-handler/mailto" = "emacsclient-mail.desktop";
        };
      };
    };
}
