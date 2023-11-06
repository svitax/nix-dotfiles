{ pkgs, lib, config, ... }:

{
  programs.git = {
    enable = true;
    userName = "Igor Barale";
    userEmail = "svitaxiom@gmail.com";
    extraConfig = {
      init.defaultBranch = "master";
    };
  };
}
