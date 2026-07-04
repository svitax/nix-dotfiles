{
  ...
}:
{
  flake.modules.homeManager.dropbox =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.maestral ];
      systemd.user.services.maestral = {
        Unit.Description = "Maestral daemon (Dropbox)";
        Install.WantedBy = [ "default.target" ];
        Service = {
          ExecStart = "${pkgs.maestral}/bin/maestral start --foreground";
          ExecStop = "${pkgs.maestral}/bin/maestral stop";
          Restart = "on-failure";
        };
      };
    };
}
