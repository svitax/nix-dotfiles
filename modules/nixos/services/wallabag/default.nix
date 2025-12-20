{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkEnableOption
    mkOption
    types
    mdDoc
    ;

  wallabag = pkgs.wallabag.overrideAttrs (attrs: {
    patches =
      builtins.filter (patch: builtins.baseNameOf patch != "wallabag-data.patch") attrs.patches
      ++ [
        # Out of the box, Wallabag wants to write to various subdirectories of the project directory.
        # Let’s replace references to such paths with designated systemd locations
        # so that the project source can remain immutable.
        ./wallabag-data.patch
      ];
  });

  # Based on https://github.com/wallabag/wallabag/blob/2.6.6/app/config/parameters.yml.dist
  settings = {
    # database_driver = "${cfg.database_type}";
    database_driver = "pdo_pgsql";
    database_host = null;
    database_port = 5432;
    database_name = "wallabag";
    database_user = "wallabag";
    database_password = null;
    database_path = null;
    database_table_prefix = "wallabag_";
    database_socket = "/run/postgresql/.s.PGSQL.${toString config.services.postgresql.settings.port}";
    database_charset = "utf8";

    # domain_name = "";
    domain_name = "http://wallabag.localhost";
    server_name = "Wallabag";

    # Needs an explicit command since Symfony version used by Wallabag does not yet support the `native` transport
    # and the `sendmail` transport does not respect `sendmail_path` configured in `php.ini`.
    mailer_dsn = "sendmail://default?command=/run/wrappers/bin/sendmail%%20-t%%20-i";

    locale = "en";

    # A secret key that's used to generate certain security-related tokens.
    "env(SECRET_FILE)" = "%env(string:STATE_DIRECTORY)%/secret.txt";
    secret = "%env(file:resolve:SECRET_FILE)%";

    # two factor stuff
    # twofactor_auth = false;
    # twofactor_sender = "";
    twofactor_auth = true;
    twofactor_sender = "wallabag@localhost";

    # fosuser stuff
    fosuser_registration = false;
    fosuser_confirmation = false;

    # how long the access token should live in seconds for the API
    fos_oauth_server_access_token_lifetime = 3600;
    # how long the refresh token should life in seconds for the API
    fos_oauth_server_refresh_token_lifetime = 1209600;

    # from_email = "";
    from_email = "wallabag@localhost";

    # RabbitMQ processing
    rabbitmq_host = null;
    rabbitmq_port = null;
    rabbitmq_user = null;
    rabbitmq_password = null;
    rabbitmq_prefetch_count = null;

    # Redis processing
    redis_scheme = null;
    redis_host = null;
    redis_port = null;
    redis_path = null;
    redis_password = null;

    # sentry logging
    sentry_dsn = null;
  } // cfg.parameters;

  php = cfg.php.package.withExtensions (
    { enabled, all }:
    enabled
    ++ (with all; [
      imagick
      tidy
    ])
  );

  commonServiceConfig = {
    CacheDirectory = "wallabag";
    # Stores sessions.
    CacheDirectoryMode = "700";
    ConfigurationDirectory = "wallabag";
    LogsDirectory = "wallabag";
    StateDirectory = "wallabag";
    # Stores site-credentials-secret-key.txt.
    StateDirectoryMode = "700";
  };

  cfg = config.modules.services.wallabag;
in
{
  options.modules.services.wallabag = {
    enable = mkEnableOption "Wallabag read-it later service";

    package = mkOption {
      type = types.package;
      default = pkgs.wallabag;
    };

    php.package = mkOption {
      type = types.package;
      default = pkgs.php;
    };

    parameters = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = "Parameters to override from the default. See <https://doc.wallabag.org/en/admin/parameters.html> for values.";
    };

    database_type = mkOption {
      type = types.enum [
        "pdo_sqlite3"
        "pdo_pgsql"
      ];
      default = if config.services.postgresql.enable then "pdo_pgsql" else "pdo_sqlite3";
      defaultText = ''
        if config.services.postgresql.enable
        then "pdo_pgsql"
        else "pdo_sqlite3"
      '';
      description = mdDoc ''
        The database engine name. Can be pdo_sqlite3 or pdo_pgsql.
      '';
    };

    domain = mkOption {
      type = types.str;
      description = "Bare domain name for Wallabag";
    };

    virtualHost.enable = mkEnableOption (mdDoc "Define nginx virtualhost for Wallabag");
  };

  config = lib.mkIf cfg.enable {
    environment.etc."wallabag/parameters.yml" = {
      source = pkgs.writeTextFile {
        name = "wallabag-config";
        text = builtins.toJSON {
          parameters = settings;
        };
      };
    };

    services.nginx = lib.mkIf cfg.virtualHost.enable {
      enable = true;

      virtualHosts = {
        "wallabag.localhost" = {
          root = "${wallabag}/web";

          extraConfig = ''
            add_header X-Frame-Options SAMEORIGIN;
            add_header X-Content-Type-Options nosniff;
            add_header X-XSS-Protection "1; mode=block";
          '';

          locations."/" = {
            extraConfig = ''
              try_files $uri /app.php$is_args$args;
            '';
          };

          locations."/assets".root = "${wallabag}/app/web";

          locations."~ ^/app\\.php(/|$)" = {
            extraConfig = ''
              fastcgi_pass unix:${config.services.phpfpm.pools.wallabag.socket};
              include ${config.services.nginx.package}/conf/fastcgi.conf;
              fastcgi_param PATH_INFO $fastcgi_path_info;
              fastcgi_param PATH_TRANSLATED $document_root$fastcgi_path_info;
              fastcgi_param SCRIPT_FILENAME ${wallabag}/web/$fastcgi_script_name;
              fastcgi_param DOCUMENT_ROOT ${wallabag}/web;
              fastcgi_read_timeout 120;
              internal;
            '';
          };

          locations."~ /(?!app)\\.php$" = {
            extraConfig = ''
              return 404;
            '';
          };
        };
      };
    };

    # networking.hosts = {
    #   "127.0.0.1" = [ "wallabag.localhost" ];
    # };

    services.phpfpm.pools.wallabag = {
      user = config.users.users.wallabag.name;
      phpPackage = php;
      settings = {
        "catch_workers_output" = true;

        "listen.owner" = config.services.nginx.user;
        "listen.group" = "root";
        "pm" = "dynamic";
        "pm.max_children" = 5;
        "pm.start_servers" = 2;
        "pm.min_spare_servers" = 1;
        "pm.max_spare_servers" = 3;
        "php_admin_value[error_log]" = "/var/log/wallabag/error.log";
        "php_admin_value[access_log]" = "/var/log/wallabag/access.log";
        "php_admin_flag[log_errors]" = true;
      };
      phpOptions = ''
        ; Set up $_ENV superglobal.
        ; http://php.net/request-order
        variables_order = "EGPCS"
        # Wallabag will crash on start-up.
        # https://github.com/wallabag/wallabag/issues/6042
        # error_reporting = E_ALL & ~E_USER_DEPRECATED & ~E_DEPRECATED
      '';
      settings = {
        # Accept settings from the systemd service.
        clear_env = false;
      };
    };

    users.users.wallabag = {
      isSystemUser = true;
      group = "wallabag";
    };
    users.groups.wallabag = { };
    services.redis.servers.wallabag = {
      enable = true;
      user = "wallabag";
    };
    services.rabbitmq.enable = false;

    systemd.services.phpfpm-wallabag.serviceConfig = commonServiceConfig;

    systemd.services.wallabag-install = {
      description = "Wallabag install service";
      wantedBy = [ "multi-user.target" ];
      before = [ "phpfpm-wallabag.service" ];
      after = [ "postgresql.service" ];
      path = with pkgs; [
        coreutils
        php
        phpPackages.composer
      ];

      serviceConfig = {
        User = "wallabag";
        Type = "oneshot";
      } // commonServiceConfig;

      script = ''
        if [ ! -f "$STATE_DIRECTORY/installed" ]; then
          php ${wallabag}/bin/console --env=prod wallabag:install
          touch "$STATE_DIRECTORY/installed"
        else
          php ${wallabag}/bin/console --env=prod doctrine:migrations:migrate --no-interaction
        fi
        php ${wallabag}/bin/console --env=prod cache:clear
      '';
    };
  };
}
