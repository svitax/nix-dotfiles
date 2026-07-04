# Removing Home-Manager

A design exploration for migrating this flake away from home-manager (HM).

## Why Consider Removing HM

- One less large dependency and evaluation overhead
- Full control over what happens during activation (no black box)
- Simpler mental model: system configuration manages everything
- nix-darwin and NixOS already have native options that overlap with HM

## The Critical Gap

Neither nix-darwin nor NixOS has a native equivalent to `home.file` for placing
per-user symlinks into `$HOME`. This is the core reason HM exists. Without it,
you need an alternative mechanism for deploying dotfiles.

## What nix-darwin Already Provides Natively

| Task | Mechanism |
|------|-----------|
| Install packages | `environment.systemPackages` |
| Direnv (full featured) | `programs.direnv` |
| Emacs daemon | `services.emacs` (launchd agent) |
| GnuPG agent | `programs.gnupg` |
| SSH config | `programs.ssh` |
| Shell integration | `programs.bash.interactiveShellInit` |
| Environment variables | `environment.variables` |
| Shell aliases | `environment.shellAliases` |
| Place files in /etc | `environment.etc.*` |
| Activation scripts | `system.activationScripts.postActivation.text` |
| Launch agents | `launchd.user.agents.*` |

## What You Lose

| Capability | Without HM |
|------------|-----------|
| `home.file`, `xdg.configFile` | Must use activation scripts or wrappers |
| Structured config generation (git, starship) | DIY with `pkgs.formats.*` |
| Per-user packages (not system-wide) | Not available in nix-darwin |
| Generation tracking / rollback | Must implement or accept stale symlinks |
| `xdg.mimeApps` | Place file manually |
| Cross-platform `services.emacs` | Separate nix-darwin / NixOS code |

## Migration Approaches

### Approach 1: Hybrid (Recommended First Step)

Keep HM but strip it down to ONLY file placement (`home.file`,
`xdg.configFile`). Move everything else to native options:

```nix
# Before (HM):
programs.direnv = { enable = true; nix-direnv.enable = true; };
programs.starship = { enable = true; settings = { ... }; };

# After (native nix-darwin + HM for files only):
programs.direnv = { enable = true; silent = true; nix-direnv.enable = true; };
programs.bash.interactiveShellInit = ''eval "$(starship init bash)"'';
home-manager.users.igorsnt = {
  home.file.".config/starship.toml".source = starshipToml;
};
```

### Approach 2: wrapper-manager

Encode config INTO the executable via flags or env vars. No dotfiles needed.

```nix
# Starship config via env var (no file in $HOME):
wrappers.bash = {
  basePackage = pkgs.bash;
  env.STARSHIP_CONFIG.value = starshipToml;
  env.GIT_CONFIG_GLOBAL.value = gitConfig;
};
```

Works for: git (`GIT_CONFIG_GLOBAL`), starship (`STARSHIP_CONFIG`), emacs
(`--init-directory`), direnv (`DIRENV_CONFIG`).

Does NOT work for: programs that only read `~/.config/foo/` with no override.

Reference: https://github.com/viperML/wrapper-manager

### Approach 3: Activation Script + Derivation

Build a store derivation containing your dotfile tree, symlink during activation:

```nix
let
  dotfiles = pkgs.runCommand "my-dotfiles" {} ''
    mkdir -p $out/.config/git
    ln -s ${gitConfig} $out/.config/git/config
    mkdir -p $out/.emacs.d
    cp -r ${./emacs-config}/* $out/.emacs.d/
  '';
in {
  system.activationScripts.postActivation.text = ''
    USER_HOME="/Users/igorsnt"
    # Symlink each file from the derivation into $HOME
    find ${dotfiles} -type l -o -type f | while read src; do
      rel="''${src#${dotfiles}/}"
      target="$USER_HOME/$rel"
      mkdir -p "$(dirname "$target")"
      ln -sf "$src" "$target"
    done
  '';
}
```

Tradeoff: runs as root on nix-darwin, no generation tracking, must handle stale
link cleanup yourself.

### Approach 4: nix-maid (Linux Only)

Uses systemd-tmpfiles for file placement. Lightweight, fast, atomic.
Currently Linux-only — no macOS support.

Reference: https://github.com/viperML/nix-maid

## Generating Config Files Without HM

HM's `programs.starship.settings` converts Nix attrsets to TOML. Without HM:

```nix
let
  format = pkgs.formats.toml {};
  starshipToml = format.generate "starship.toml" {
    add_newline = false;
    character.success_symbol = "[>](bold green)";
  };

  gitFormat = pkgs.formats.ini {};
  gitConfig = gitFormat.generate "gitconfig" {
    user = { name = "svitax"; email = "svitaxiom@gmail.com"; };
    diff = { tool = "ediff"; };
  };
in ...
```

## Current HM Usage in This Repo

### hermes (darwin)

- `programs.emacs` + `home.file.".emacs.d"` — emacs build + config placement
- `programs.ghostty` — just `enable = true; package = null`
- `programs.toolbox` — amzn-community module (HM-specific, blocker)
- `home.packages` — git, gh, enchant, claude-agent-acp

### erasmus (NixOS)

- `programs.emacs` + `xdg.configFile."emacs"` — full emacs build + config
- `programs.git`, `programs.starship`, `programs.direnv`, `programs.zoxide`
- `programs.bash` + `home.file` (dircolors)
- `programs.mpv`, `programs.yt-dlp`, `programs.firefox` (librewolf)
- `services.emacs` (systemd user service)
- `xdg.mimeApps` (default applications)
- `systemd.user.services` (mbsync timer, maestral)
- Multiple `home.file` placements (mail configs, qutebrowser, etc.)

### Blockers

- `programs.toolbox` (amzn-community) requires HM — no alternative exists
- `programs.emacs.extraPackages` pattern has no native equivalent
- Heavy `home.file` usage for dotfile placement

## Recommended Migration Path

1. Move to native `programs.direnv` on nix-darwin (it's richer than HM's)
2. Move shell integrations (starship, zoxide) to `interactiveShellInit`
3. Generate config files with `pkgs.formats.*` instead of HM attrsets
4. Keep HM for file placement only (the hybrid approach)
5. Evaluate wrapper-manager for programs that support config via env/flags
6. Revisit full removal once toolbox has a non-HM alternative
