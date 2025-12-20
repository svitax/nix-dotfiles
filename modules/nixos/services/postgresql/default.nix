{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.services.postgresql;
in

{
  options.modules.services.postgresql = {
    enable = mkEnableOption "PostgreSQL";
  };

  config = lib.mkIf cfg.enable {
    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_14;
      settings = {
        wal_level = "replica";
        archive_mode = "on";
        archive_command = "test ! -f /srv/pgsql/archive/wal/%f && cp %p /srv/pgsql/archive/wal/%f";

        shared_preload_libraries = "pg_stat_statements";
        # log_min_duration_statement = 10000;
        # statement_timeout = 60*1000;

        shared_buffers = "4GB";
        effective_cache_size = "12GB";
        maintenance_work_mem = "1GB";
        work_mem = "4GB";
        default_statistics_target = 250;

        max_worker_processes = "40";
        max_parallel_workers_per_gather = "10";
        max_parallel_workers = "20";

        autovacuum = true;
        track_counts = true;
      };
    };

    # you fuckers silently broke my updates with a one-liner in the release notes!!!
    systemd.tmpfiles.rules = [
      "d /srv/pgsql/archive/wal 0750 postgres postgres -"
    ];
    systemd.services.postgresql.serviceConfig.ReadWritePaths = [
      "/srv/pgsql/archive/"
    ];

    # backup weekly
    services.postgresqlBackup = {
      enable = true;
      backupAll = true;
      location = "/srv/pgsql/backups/";
      startAt = "Mon *-*-* 02:00:00";
    };
  };
}
