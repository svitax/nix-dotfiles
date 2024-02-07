{ config, lib, pkgs, ... }:
let inherit (config.colorScheme) colors;
in {
  # TODO: migrate swaylock.nix from hm modules to home.file
  programs.swaylock = {
    enable = true;
    settings = { color = "#" + colors.base00; };
  };
}
