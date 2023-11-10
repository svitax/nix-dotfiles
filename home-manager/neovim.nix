{
  pkgs,
  lib,
  ...
}: {
  # home.file.".config/nvim".source = ./config/nvim;
  programs.neovim-ide = {
    enable = true;
    settings = {
      vim = {
        viAlias = false;
        vimAlias = true;
        preventJunkFiles = true;
        customPlugins = with pkgs.vimPlugins; [
          multiple-cursors
          vim-mergetool
          vim-repeat
        ];
        # nightly is quite broken with a lot of plugins, treesitter included
        #neovim.package = pkgs.neovim-nightly;
        lsp = {
          enable = true;
          folds = true;
          formatOnSave = false;
          lightbulb.enable = true;
          lspsaga.enable = false;
          nvimCodeActionMenu.enable = true;
          trouble.enable = true;
          lspSignature.enable = false;
          nix = {
            enable = true;
            type = "nil";
          };
          scala = {
            enable = false;
          };
          ts = true;
          smithy.enable = true;
          rust.enable = false;
          dhall = false;
          elm = true;
          haskell = false;
          sql = false;
          python = false;
          clang = false;
          go = false;
        };
        plantuml.enable = true;
        fx.automaton.enable = true;
        visuals = {
          enable = true;
          nvimWebDevicons.enable = true;
          lspkind.enable = true;
          indentBlankline = {
            enable = true;
            fillChar = "";
            eolChar = "";
            showCurrContext = true;
          };
          cursorWordline = {
            enable = true;
            lineTimeout = 0;
          };
        };
        statusline.lualine = {
          enable = true;
          theme = "onedark";
        };
        theme = {
          enable = true;
          name = "onedark";
          style = "deep";
          transparency = true;
        };
        autopairs.enable = true;
        autocomplete.enable = true;
        filetree.nvimTreeLua = {
          enable = true;
          hideDotFiles = false;
          hideFiles = ["node_modules" ".cache"];
          openOnSetup = false;
        };
        neoclip.enable = true;
        dial.enable = true;
        hop.enable = true;
        notifications.enable = true;
        todo.enable = true;
        tabline.nvimBufferline.enable = true;
        treesitter = {
          enable = true;
          autotagHtml = true;
          context.enable = true;
        };
        keys = {
          enable = true;
          whichKey.enable = true;
        };
        comments = {
          enable = true;
          type = "nerdcommenter";
        };
        shortcuts = {
          enable = true;
        };
        surround = {
          enable = true;
        };
        telescope = {
          enable = true;
        };
        markdown = {
          enable = true;
          glow.enable = true;
        };
        chatgpt = {
          enable = false;
        };
        git = {
          enable = true;
          gitsigns.enable = true;
        };
        spider = {
          enable = false;
          skipInsignificantPunctuation = true;
        };
        mind = {
          enable = false;
        };
      };
    };
  };
  # programs.neovim = {
  #   enable = true;
  #   defaultEditor = true;
  # };
}
