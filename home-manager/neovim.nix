{
  pkgs,
  lib,
  config,
  ...
}: {
  xdg.configFile.nvim = {
    # TODO: figure out a way to not hard code a path to my nix-dotfiles directory
    source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nix-dotfiles/home-manager/config/nvim";
    recursive = true;
  };
  # home.file.".config/nvim/" = {
  #   source = config.lib.file.mkOutOfStoreSymlink "/home/svitax/nix-dotfiles/home-manager/config/nvim";
  #   recursive = true;
  # };
  # programs.neovim = {
  #   enable = true;
  #   defaultEditor = true;
  # };
}
