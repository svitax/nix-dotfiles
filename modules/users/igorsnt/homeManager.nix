{
  inputs,
  ...
}:
let
  username = "igorsnt";
in
{
  flake.modules.homeManager.${username} =
    { pkgs, ... }:
    {
      imports = with inputs.self.modules.homeManager; [
        emacs-darwin
        ghostty
        toolbox
      ];
      home.username = username;
      home.homeDirectory = "/Users/${username}";
      home.stateVersion = "25.11";
      home.packages = with pkgs; [
        git
        gh
        enchant
        pandoc
        terminal-notifier
        aporetic
        claude-agent-acp
        (writeShellScriptBin "docker" ''exec finch "$@"'')
      ];
      programs.home-manager.enable = true;
    };
}
