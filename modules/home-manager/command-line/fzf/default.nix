{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.shell.fzf;
in
{
  options.shell.fzf = {
    enable = mkEnableOption "Fzf";
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        programs.fzf = {
          enable = true;
          enableFishIntegration = true;
          enableBashIntegration = true;
          enableZshIntegration = true;
          defaultOptions = [
            "--multi"
            "--bind 'alt-a:select-all,alt-d:kill-word,alt-t:toggle-all,ctrl-j:accept,ctrl-k:kill-line,ctrl-n:down,ctrl-e:up,up:previous-history,down:next-history'"
            "--height 90%"
            "--reverse"
            "--tiebreak end"
          ];
        };
      }
    ]
  );
}
