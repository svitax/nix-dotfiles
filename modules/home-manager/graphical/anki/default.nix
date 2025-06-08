{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.graphical.anki;
in
{
  options.graphical.anki = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # anki is out of date (idk why?) so we use anki-bin
      anki-bin
      # Decks/flashcards can be synced using a self hosted instance of
      # anki-sync-server or an AnkiWeb account
    ];
  };
}
