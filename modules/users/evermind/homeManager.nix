{
  inputs,
  ...
}:
let
  username = "evermind";
in
{
  flake.modules.homeManager.${username} =
    { pkgs, ... }:
    {
      imports = with inputs.self.modules.homeManager; [
        # Shell
        bash
        direnv
        git
        starship
        zoxide
        timewarrior
        mail
        # Editors
        emacs-nixos
        # Desktop
        qutebrowser
        librewolf
        anki
        discord
        mpv
        yt-dlp
        xfce-hm
        # Documentation
        info
        # Services
        dropbox
      ];

      home.username = username;
      home.homeDirectory = "/home/${username}";
      home.stateVersion = "24.05";

      home.packages = with pkgs; [
        runelite
      ];

      programs.home-manager.enable = true;
      nixpkgs.config.allowUnfreePredicate = _: true;
    };
}
