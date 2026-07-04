# The Dendritic Pattern with Flake Parts

Reference: https://github.com/Doc-Steve/dendritic-design-with-flake-parts

## What It Is

A way to structure Nix flakes so that each "feature" (a program, a service, a
user, a host) is a self-contained file that registers itself into the flake's
module system. The flake.nix becomes trivial — all logic lives in the module
tree, auto-discovered at evaluation time.

## The Two Key Libraries

**flake-parts** (`github:hercules-ci/flake-parts`) — Replaces the manual
`outputs = { nixosConfigurations = ...; }` with a module system for the flake
itself. Each file can set `flake.darwinConfigurations`, `flake.modules.darwin.*`,
`perSystem`, etc. It's the NixOS module system, but for flake outputs.

**import-tree** (`github:vic/import-tree`) — Recursively walks a directory and
imports every `.nix` file as a flake-parts module. Drop a file in `modules/`,
it's auto-discovered. No import lists to maintain.

## The Minimal flake.nix

```nix
{
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    import-tree.url = "github:vic/import-tree";
    # ... other inputs
  };
}
```

That's it. Everything else is in `modules/`.

## How Modules Register Themselves

Every `.nix` file under `modules/` is a flake-parts module. It receives
`{ inputs, lib, config, ... }` (the flake-parts module arguments, NOT the
NixOS/darwin module arguments). It sets attributes on the flake:

```nix
# modules/programs/git/git.nix
{ ... }:
{
  flake.modules.homeManager.git = {
    programs.git = {
      enable = true;
      settings.user.name = "me";
    };
  };
}
```

This registers a home-manager module named "git" at
`self.modules.homeManager.git`. Any user that wants git just imports it.

### The three namespaces

| Namespace | What it holds | Consumed by |
|-----------|--------------|-------------|
| `flake.modules.darwin.<name>` | darwin system modules | `darwinSystem { modules = [...] }` |
| `flake.modules.nixos.<name>` | NixOS system modules | `nixosSystem { modules = [...] }` |
| `flake.modules.homeManager.<name>` | home-manager modules | `home-manager.users.<name>.imports = [...]` |

These are custom flake-level options defined in a `lib.nix` helper. They're just
attrsets — flake-parts doesn't enforce the separation, it's convention.

## The lib.nix Helper

Defines `mkDarwin` and `mkNixos` — factory functions that look up a host's
module by name and build the system configuration:

```nix
# modules/nix/flake-parts/lib.nix
{ inputs, lib, ... }:
{
  options.flake.lib = lib.mkOption {
    type = lib.types.attrsOf lib.types.unspecified;
    default = {};
  };

  config.flake.lib = {
    mkDarwin = system: name: {
      ${name} = inputs.nix-darwin.lib.darwinSystem {
        modules = [
          inputs.self.modules.darwin.${name}
          { nixpkgs.hostPlatform = lib.mkDefault system; }
        ];
      };
    };
    mkNixos = system: name: {
      ${name} = inputs.nixpkgs.lib.nixosSystem {
        modules = [
          inputs.self.modules.nixos.${name}
          { nixpkgs.hostPlatform = lib.mkDefault system; }
        ];
      };
    };
  };
}
```

## How a Host is Defined

Each host has a directory with (typically) two files:

**flake-parts.nix** — registers the host as a buildable output:
```nix
{ inputs, ... }:
{
  flake.darwinConfigurations = inputs.self.lib.mkDarwin "aarch64-darwin" "myhost";
}
```

**configuration.nix** — defines what the host is made of (composes features):
```nix
{ inputs, ... }:
{
  flake.modules.darwin.myhost = {
    imports = with inputs.self.modules.darwin; [
      nix-settings
      homebrew
      kanata
      home-manager
      myuser
    ];
    networking.hostName = "myhost";
    system.stateVersion = 6;
  };
}
```

The host module imports other `flake.modules.darwin.*` entries as features.
This is the composition point — the host picks what it wants.

## How a User is Defined

**configuration.nix** — system-level user account + HM wiring:
```nix
{ inputs, ... }:
let username = "alice"; in
{
  flake.modules.darwin.${username} = { ... }: {
    users.users.${username}.home = "/Users/${username}";
    home-manager.users.${username} = {
      imports = [ inputs.self.modules.homeManager.${username} ];
    };
  };
}
```

**homeManager.nix** — the user's portable identity (what programs they use):
```nix
{ inputs, ... }:
let username = "alice"; in
{
  flake.modules.homeManager.${username} = { pkgs, ... }: {
    imports = with inputs.self.modules.homeManager; [
      emacs
      git
      starship
    ];
    home.username = username;
    home.homeDirectory = "/Users/${username}";
    home.stateVersion = "25.11";
  };
}
```

## Composition by Import (Not by Enable)

The key philosophical difference from traditional NixOS module patterns:

**Traditional:** Import everything, toggle with `enable`:
```nix
imports = [ allModules ];
modules.editors.emacs.enable = true;
modules.shell.git.enable = true;
```

**Dendritic:** Import only what you want. If imported, it's active:
```nix
imports = with inputs.self.modules.homeManager; [ emacs git ];
```

No `mkEnableOption`, no `mkIf cfg.enable`. Each module is a raw config block.
The composition happens at the import site.

## When a Module Needs to Work on Multiple Platforms

Define the shared config once, register it for both:

```nix
# modules/programs/cli-tools/cli-tools.nix
let
  shared = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [ git tmux ripgrep ];
  };
in
{
  flake.modules.nixos.cli-tools = { imports = [ shared ]; };
  flake.modules.darwin.cli-tools = { imports = [ shared ]; };
}
```

## Directory Structure Conventions

Organized by **concern**, not by platform. Each module declares its platform
in code via `flake.modules.{darwin,nixos,homeManager}.*` — the filesystem
never encodes platform.

```
modules/
  nix/                    ← flake-level plumbing
    flake-parts/            lib.nix (mkDarwin/mkNixos), overlays.nix
    home-manager.nix        darwin + nixos HM wiring (co-located)
    nix-settings.nix        darwin + nixos nix config (co-located)
  hosts/<name>/           ← one dir per machine (composition points)
  users/<name>/           ← one dir per user identity
  programs/               ← user-facing programs
    shell/                  CLI tools (bash, git, direnv, starship, toolbox, etc.)
    editors/                emacs
    browsers/               qutebrowser, librewolf, chromium
    media/                  mpv, yt-dlp
    terminal/               ghostty
    apps/                   misc GUI apps (anki, discord)
    documentation/          info, man
  services/               ← background services (kanata, ollama, dropbox, etc.)
  desktop/                ← desktop environments (xfce, kde)
  security/               ← gnupg, pass
  system/                 ← OS infrastructure (boot, fonts, homebrew)
packages/                 ← custom derivations (callPackage'd via overlay)
templates/                ← nix flake init templates
```

Cross-platform modules (nix-settings, home-manager, xfce system+user prefs)
register multiple `flake.modules.*` entries from a single file.

## How Home-Manager Gets Wired In

Home-manager needs to be imported as a darwin/NixOS module first. A single
co-located wiring module registers both platform variants:

```nix
# modules/nix/home-manager.nix
{ inputs, ... }:
let
  shared = {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.backupFileExtension = "bak";
  };
in
{
  flake.modules.darwin.home-manager = {
    imports = [ inputs.home-manager.darwinModules.home-manager shared ];
  };
  flake.modules.nixos.home-manager = {
    imports = [ inputs.home-manager.nixosModules.home-manager shared ];
  };
}
```

The host imports this, then user modules can use `home-manager.users.<name>`.

## The Flow (How It All Connects)

```
flake.nix
  → import-tree ./modules (discovers all .nix files)
    → lib.nix defines mkDarwin/mkNixos
    → hosts/myhost/flake-parts.nix calls mkDarwin "myhost"
      → looks up flake.modules.darwin.myhost
        → hosts/myhost/configuration.nix defines that module
          → imports: [nix-settings, homebrew, home-manager, myuser]
            → each is a flake.modules.darwin.* entry defined elsewhere
            → myuser wires home-manager.users.myuser
              → imports flake.modules.homeManager.myuser
                → users/myuser/homeManager.nix lists [emacs, git, ...]
                  → each is a flake.modules.homeManager.* entry
```

## What You Don't Need Home-Manager For

Home-manager is optional. Without it, programs are managed via:
- `environment.systemPackages` (system-wide packages)
- `users.users.<name>.packages` (per-user packages on NixOS)
- Raw dotfile management via `environment.etc` or `system.activationScripts`
- Direct program configuration via darwin/NixOS options (`programs.git`, etc.)

The tradeoff: HM gives you declarative per-program config (`programs.git.settings`,
`programs.mpv.config`, `programs.starship.settings`). Without HM, you'd manage
those configs as raw files placed by the system activation.

## Key Insight

The dendritic pattern is NOT about flake-parts or import-tree specifically.
It's about **features as composable, self-registering units** and
**hosts as pure composition points**. The libraries just make it ergonomic.
You could implement the same pattern with manual imports and a custom module
option — you'd just have more boilerplate.
