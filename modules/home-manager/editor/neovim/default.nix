{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types;
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (inputs.self) outputs;

  cfg = config.editor.neovim;
in {
  options.editor.neovim = {
    enable = mkEnableOption "Neovim";

    enableGitDiff = mkOption {
      type = types.bool;
      default = true;
      description = "Enable vimdiff as default git diff tool.";
    };

    defaultEditor = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to use Neovim as default editor";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      # TODO: extract git, ripgrep, and fd config
      # TODO: extract lua, nix, to lang/
      home.packages = with pkgs; [
        git
        ripgrep
        fd
        fzf
        zf
        #
        go
        gopls
        golangci-lint
        lua-language-server
        stylua
        selene
        alejandra
        nil
        statix
        pyright
      ];

      programs.neovim = {
        enable = true;
        viAlias = true;
        vimAlias = true;
        vimdiffAlias = true;
      };

      xdg.enable = true;
      xdg.configFile."nvim" = {
        source =
          mkOutOfStoreSymlink
          "${config.home.homeDirectory}/nix-dotfiles/modules/home-manager/editor/neovim";
        recursive = true;
      };
    }
    (lib.mkIf cfg.defaultEditor {programs.neovim.defaultEditor = true;})
    (lib.mkIf cfg.enableGitDiff {
      programs.git.extraConfig.diff.tool = "vimdiff";
    })
  ]);
}
