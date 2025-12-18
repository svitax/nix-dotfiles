let
  modulesDir = ./.;

  # Recursively find all default.nix files in subdirectories
  findModules =
    dir:
    let
      entries = builtins.readDir dir;
      processEntry =
        name: type:
        let
          path = dir + "/${name}";
        in
        if type == "directory" then
          if builtins.pathExists (path + "/default.nix") then [ path ] else findModules path
        else
          [ ];
    in
    builtins.concatLists (builtins.attrValues (builtins.mapAttrs processEntry entries));
in
{
  allModules = {
    imports = findModules modulesDir;

    #   audio = {
    #     # NOTE: pipewire
    #   };

    #   boot = {
    #     # NOTE: grub theme https://github.com/PROxZIMA/boo-grub
    #     # NOTE: plymouth (for graphical boot splash) https://github.com/PROxZIMA/proxzima-plymouth
    #     systemd-boot = ./boot/systemd-boot;
    #   };

    #   desktop-manager = {
    #     # NOTE: remove gnome completely
    #     # gnome = ./window-manager/gnome;
    #     xfce = ./desktop-manager/xfce;
    #     kde = ./desktop-manager/kde;
    #   };

    #   disk = {
    #     # NOTE: disko https://github.com/nix-community/disko
    #   };

    #   display-manager = {
    #     # lightdm = ./window-manager/lightdm;
    #     # NOTE: sddm theme https://github.com/stepanzubkov/where-is-my-sddm-theme
    #     # NOTE: sddm theme https://github.com/Zhaith-Izaliel/sddm-sugar-candy-nix
    #   };

    #   display-server = {
    #     # wayland = ./window-manager/wayland;
    #   };

    #   graphics = {
    #     # NOTE: opengl
    #   };

    #   homelab = {
    #     # NOTE: nextcloud or owncloud https://github.com/owncloud/ocis
    #     # NOTE: freshrss https://github.com/FreshRSS/FreshRSS
    #     # NOTE: photoprism (photo server) https://github.com/photoprism/photoprism or immich (photo and video server) https://github.com/immich-app/immich
    #     # NOTE: jellyfin (media server) https://github.com/jellyfin/jellyfin
    #     # NOTE: jellyseerr (media server request manager) https://github.com/Fallenbagel/jellyseerr
    #     # NOTE: TRaSH-Guides https://github.com/TRaSH-Guides/Guides
    #     # NOTE: recyclarr (sync TRaSH Guides to sonarr and radarr) https://github.com/recyclarr/recyclarr
    #     # NOTE: lidarr (music indexer) https://github.com/Lidarr/Lidarr
    #     # NOTE: radarr (movie indexer) https://github.com/Radarr/Radarr
    #     # NOTE: sonarr (tv show indexer) https://github.com/Sonarr/Sonarr
    #     # NOTE: bazarr (subtitle indexer) https://github.com/morpheus65535/bazarr
    #     # NOTE: prowlarr https://github.com/Prowlarr/Prowlarr
    #     # NOTE: readarr https://github.com/Readarr/Readarr
    #     # NOTE: unpackerr (extract downloads) https://github.com/Unpackerr/unpackerr
    #     # NOTE: glance (homelab and feeds dashboard) https://github.com/glanceapp/glance or homer (homelab dashboard) https://github.com/bastienwirtz/homer
    #     # NOTE: tubearchivist (youtube media server) https://github.com/tubearchivist/tubearchivist
    #     # NOTE: podgrab (podcast manager/downloader/archiver) https://github.com/akhilrex/podgrab
    #     # NOTE: audiobookshelf (podcast and audiobook server) https://github.com/advplyr/audiobookshelf
    #     # NOTE: plapp
    #     # NOTE: plane (jira alternative) https://github.com/makeplane/plane
    #     # NOTE: feishin (music player) https://github.com/jeffvli/feishin
    #     # NOTE: wallabag (read-it-later) https://github.com/wallabag/wallabag
    #     wallabag = ./homelab/wallabag;
    #     shiori = ./homelab/shiori;
    #     # NOTE: paperless-ngx (physical document archiver) https://github.com/paperless-ngx/paperless-ngx
    #     # NOTE: kaizoku (manga downloader) https://github.com/oae/kaizoku
    #     # NOTE: radicale (caldav and carddav) https://github.com/Kozea/Radicale
    #     # NOTE: maybe (personal finance) https://github.com/maybe-finance/maybe
    #     postgresql = ./homelab/postgresql;
    #   };

    #   network = {
    #     # NOTE: blocky https://nixos.wiki/wiki/Blocky and https://github.com/0xERR0R/blocky
    #     # NOTE: pi-hole https://github.com/pi-hole/pi-hole
    #     # NOTE: wireguard or tailscale
    #     stevenblack = ./network/stevenblack;
    #   };

    #   nix-settings = ./nix-settings;

    #   security = {
    #     # NOTE: gpg
    #     # NOTE: ssh
    #     # NOTE: shoji-nix https://github.com/AdoPi/shoji-nix
    #     # NOTE: polkit
    #   };

    #   services-mananger = {
    #     # NOTE: systemd
    #   };

    #   shell = {
    #     # bash = ./shell/bash;
    #     # fish = ./shell/fish;
    #     # zsh = ./shell/zsh;
    #   };

    #   storage = {
    #     onedrive = ./storage/onedrive;
    #   };

    #   style = {
    #     fonts = ./style/fonts;
    #   };

    #   virtualization = {
    #     # NOTE: docker
    #     # NOTE: flatpak
    #     # NOTE: virt-mananger
    #     # NOTE: virtual box
    #     # NOTE: distrobox
    #   };

    #   window-manager = {
    #     # NOTE: sway
    #   };

  };
}
