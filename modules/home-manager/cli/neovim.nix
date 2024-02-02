{ pkgs, config, ... }: {
  programs.neovim = {
    enable = true;
    withPython3 = true;
    # defaultEditor = true;
    plugins = with pkgs; [ vimPlugins.nvim-treesitter.withAllGrammars ];
    # extraPackages = with pkgs; [];
    extraLuaPackages = ps: [ ps.magick ];
    extraPython3Packages = p:
      with p; [
        # Neovim needs access to these packages for molten-nvim
        pynvim
        jupyter-client
        cairosvg
        plotly
        # kaleido
        pnglatex
        pyperclip
        nbformat
        # Neovim needs this to open ipython repls
        ipython
      ];
  };
  home.packages = with pkgs; [
    # Globally install language servers and jupytext
    lua-language-server
    stylua
    selene
    nixfmt
    nil
    statix
    vscode-langservers-extracted # html/css/json/eslint
  ];
  xdg.configFile.nvim = {
    # TODO: figure out a way to not hard code a path to my nix-dotfiles directory
    source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/nix-dotfiles/modules/home-manager/.config/nvim";
    recursive = true;
  };
  # home.file.".config/nvim/" = {
  #   source = config.lib.file.mkOutOfStoreSymlink "/home/svitax/nix-dotfiles/home-manager/config/nvim";
  #   recursive = true;
  # };
}
