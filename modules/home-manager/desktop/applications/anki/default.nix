{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.desktop.applications.anki;
in
{
  options.modules.desktop.applications.anki = {
    enable = mkEnableOption "Anki";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # Decks/flashcards can be synced using a self hosted instance of
      # anki-sync-server or an AnkiWeb account
      (pkgs.anki.withAddons [
        pkgs.ankiAddons.anki-connect
        pkgs.ankiAddons.review-heatmap
        # Make Anki work better with declarative settings. See script for
        # specific changes.
        (pkgs.anki-utils.buildAnkiAddon (finalAttrs: {
          pname = "home-manager";
          version = "1.0";
          src = pkgs.writeTextDir "__init__.py" ''
            import aqt
            from aqt.qt import QWidget, QMessageBox
            from anki.hooks import wrap
            from typing import Any

            def make_config_differences_str(initial_config: dict[str, Any],
                                            new_config: dict[str, Any]) -> str:
              details = ""
              for key, val in new_config.items():
                  initial_val = initial_config.get(key)
                  if val != initial_val:
                    details += f"{key} changed from `{initial_val}` to `{val}`\n"
              return details

            def dialog_did_open(dialog_manager: aqt.DialogManager,
                                dialog_name: str,
                                dialog_instance: QWidget) -> None:
              if dialog_name != "Preferences":
                return

              # Make sure defaults are loaded before copying the initial configs
              dialog_instance.update_global()
              dialog_instance.update_profile()
              initial_meta = aqt.mw.pm.meta.copy()
              initial_profile_conf = aqt.mw.pm.profile.copy()

              def on_preferences_save() -> None:
                aqt.mw.pm.save = lambda: None

                details = make_config_differences_str(initial_meta, aqt.mw.pm.meta)
                details += make_config_differences_str(initial_profile_conf,
                                                       aqt.mw.pm.profile)
                if not details:
                  return

                message_box = QMessageBox(
                  QMessageBox.Icon.Warning,
                  "NixOS Info",
                  ("Anki settings are currently being managed by Home Manager.<br>"
                   "Changes to certain settings won't be saved.")
                )
                message_box.setDetailedText(details)
                message_box.exec()

              aqt.mw.pm.save = on_preferences_save

            def state_will_change(new_state: aqt.main.MainWindowState,
                                  old_state: aqt.main.MainWindowState):
              if new_state != "profileManager":
                return

              QMessageBox.warning(
                aqt.mw,
                "NixOS Info",
                ("Profiles cannot be changed or added while settings are managed with "
                 "Home Manager.")
              )


            # Ensure Anki doesn't try to save to the read-only DB settings file.
            aqt.mw.pm.save = lambda: None

            # Tell the user when they try to change settings that won't be persisted.
            aqt.gui_hooks.dialog_manager_did_open_dialog.append(dialog_did_open)

            # Show warning when users try to switch or customize profiles.
            aqt.gui_hooks.state_will_change.append(state_will_change)
          '';
        }))
      ])
    ];
  };
}
