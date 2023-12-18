{ inputs }:
final: prev:
with final.pkgs.lib;
let
  pkgs = final;

  # Use this to create a plugin from an input

  mkNeovim = pkgs.callPackage ./mkNeovim.nix { };

  all-plugins = with pkgs.vimPlugins; [
    # plugins from nixpkgs go in here.
    # https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=vimPlugins
    nvim-cmp
    cmp-cmdline
    cmp-nvim-lsp
    cmp-path
    cmp-buffer
    cmp-dap
    cmp_luasnip
    cmp-under-comparator
    cmp-git
    nvim-colorizer-lua
    comment-nvim
    conform-nvim
    nvim-dap
    nvim-dap-ui
    nvim-dap-python
    # nvim-dap-repl-highlights
    dropbar-nvim
    flash-nvim
    fzf-lua
    gitsigns-nvim # https://github.com/lewis6991/gitsigns.nvim/
    # hlchunk-nvim
    inc-rename-nvim
    nvim-lint
    lsp_lines-nvim
    nvim-lspconfig
    fidget-nvim
    neodev-nvim # adds support for Neovim's Lua API to lua-language-server | https://github.com/folke/neodev.nvim/
    neoconf-nvim
    nvim-web-devicons
    lualine-nvim # Status line | https://github.com/nvim-lualine/lualine.nvim/
    luasnip
    mini-nvim
    neogit # https://github.com/TimUntersberger/neogit/
    nightfox-nvim
    noice-nvim
    # obsidian-nvim
    octo-nvim
    oil-nvim
    # nvim-projector
    # projector-loader-vscode
    rest-nvim
    # smart-tab-nvim
    statuscol-nvim # Status column | https://github.com/luukvbaal/statuscol.nvim/
    nvim-surround # https://github.com/kylechui/nvim-surround/
    nvim-treesitter.withAllGrammars
    nvim-treesitter-textobjects # https://github.com/nvim-treesitter/nvim-treesitter-textobjects/
    nvim-treesitter-endwise
    # tree-sitter-just
    todo-comments-nvim
    # nvim-puppeteer
    # hmts-nvim
    trouble-nvim
    nvim-ufo
    promise-async
    # ultimate-autopair-nvim
    # wf-nvim
    # windex-nvim
    sqlite-lua
    plenary-nvim
    # bleeding-edge plugins from flake inputs
    # (mkNvimPlugin inputs.wf-nvim "wf.nvim") # (example) keymap hints | https://github.com/Cassin01/wf.nvim
    # ^ bleeding-edge plugins from flake inputs
  ];

  extraPackages = with pkgs; [
    # language servers, etc.
    lua-language-server
    nil # nix LSP
  ];
in {
  # This is the neovim derivation
  # returned by the overlay
  nvim-fennec = mkNeovim {
    plugins = all-plugins;
    inherit (inputs) neovim-src;
    inherit extraPackages;
  };

  # You can add as many derivations as you like.
}
