{
  pkgs,
  lib,
  config,
  ...
}: {
  programs.neovim = {
    enable = true;
    withPython3 = true;
    # defaultEditor = true;
    plugins = with pkgs; [
      vimPlugins.nvim-treesitter.withAllGrammars
    ];
    extraLuaPackages = ps: [ps.magick];
    extraPython3Packages = p:
      with p; [
        pynvim
        jupyter-client
        cairosvg
        plotly
        # kaleido
        pnglatex
        pyperclip
        ipython
        nbformat
      ];
  };
  home.packages = with pkgs; [
    # Globally install language servers and jupytext
    lua-language-server
    stylua
    selene
    alejandra
    nil
    taplo
  ];
  xdg.configFile.nvim = {
    # TODO: figure out a way to not hard code a path to my nix-dotfiles directory
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nix-dotfiles/home-manager/config/nvim";
    recursive = true;
  };
  # home.file.".config/nvim/" = {
  #   source = config.lib.file.mkOutOfStoreSymlink "/home/svitax/nix-dotfiles/home-manager/config/nvim";
  #   recursive = true;
  # };
}
