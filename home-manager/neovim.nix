{
  pkgs,
  lib,
  config,
  ...
}: {
  programs.neovim = {
    enable = true;
    # defaultEditor = true;
    plugins = with pkgs; [
    	vimPlugins.nvim-treesitter.withAllGrammars
    ];
    extraPackages = with pkgs; [
      lua-language-server
    ];
  };
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
