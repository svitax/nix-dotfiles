{
  inputs,
  ...
}:
{
  flake.modules.darwin.kanata =
    { ... }:
    {
      imports = [ inputs.kanata-darwin.darwinModules.default ];
      services.kanata = {
        enable = true;
        configSource = ./kanata.kbd;
        sudoers = true;
        kanata-bar = {
          enable = true;
          settings.kanata_bar.pam_touchid = "auto";
          settings.kanata_bar.autostart_kanata = true;
          settings.kanata_bar.autorestart_kanata = true;
        };
      };
    };
}
