# NOTE: untested — I haven't verified that wallabag actually works with this config.
# Ported from svitax-nix-dotfiles but never deployed.
{
  ...
}:
{
  flake.modules.nixos.wallabag =
    { config, pkgs, lib, ... }:
    let
      wallabag = pkgs.wallabag.overrideAttrs (attrs: {
        patches =
          builtins.filter (patch: builtins.baseNameOf patch != "wallabag-data.patch") attrs.patches
          ++ [ ./wallabag-data.patch ];
      });

      php = pkgs.php.withExtensions (
        { enabled, all }:
        enabled ++ (with all; [ imagick tidy ])
      );

      settings = {
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

        domain_name = "http://wallabag.localhost";
        server_name = "Wallabag";

        mailer_dsn = "sendmail://default?command=/run/wrappers/bin/sendmail%%20-t%%20-i";

        locale = "en";

        "env(SECRET_FILE)" = "%env(string:STATE_DIRECTORY)%/secret.txt";
        secret = "%env(file:resolve:SECRET_FILE)%";

        twofactor_auth = true;
        twofactor_sender = "wallabag@localhost";

        fosuser_registration = false;
        fosuser_confirmation = false;

        fos_oauth_server_access_token_lifetime = 3600;
        fos_oauth_server_refresh_token_lifetime = 1209600;

        from_email = "wallabag@localhost";

        rabbitmq_host = null;
        rabbitmq_port = null;
        rabbitmq_user = null;
        rabbitmq_password = null;
        rabbitmq_prefetch_count = null;

        redis_scheme = null;
        redis_host = null;
        redis_port = null;
        redis_path = null;
        redis_password = null;

        sentry_dsn = null;
      };

      commonServiceConfig = {
        CacheDirectory = "wallabag";
        CacheDirectoryMode = "700";
        ConfigurationDirectory = "wallabag";
        LogsDirectory = "wallabag";
        StateDirectory = "wallabag";
        StateDirectoryMode = "700";
      };
    in
    {
      environment.etc."wallabag/parameters.yml".source = pkgs.writeTextFile {
        name = "wallabag-config";
        text = builtins.toJSON { parameters = settings; };
      };

      services.nginx = {
        enable = true;
        virtualHosts."wallabag.localhost" = {
          root = "${wallabag}/web";
          extraConfig = ''
            add_header X-Frame-Options SAMEORIGIN;
            add_header X-Content-Type-Options nosniff;
            add_header X-XSS-Protection "1; mode=block";
          '';
          locations."/" = {
            extraConfig = ''try_files $uri /app.php$is_args$args;'';
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
            extraConfig = ''return 404;'';
          };
        };
      };

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
          clear_env = false;
        };
        phpOptions = ''
          variables_order = "EGPCS"
        '';
      };

      users.users.wallabag = {
        isSystemUser = true;
        group = "wallabag";
      };
      users.groups.wallabag = {};

      services.redis.servers.wallabag = {
        enable = true;
        user = "wallabag";
      };

      systemd.services.phpfpm-wallabag.serviceConfig = commonServiceConfig;

      systemd.services.wallabag-install = {
        description = "Wallabag install service";
        wantedBy = [ "multi-user.target" ];
        before = [ "phpfpm-wallabag.service" ];
        after = [ "postgresql.service" ];
        path = with pkgs; [ coreutils php phpPackages.composer ];
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
