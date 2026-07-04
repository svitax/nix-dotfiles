# TODO

## Emacs Convergence

Merge `emacs-darwin.nix` and `emacs-nixos.nix` into a single `emacs.nix` that:
- Uses `pkgs.stdenv.isDarwin` / `pkgs.stdenv.isLinux` in the Nix module for
  build differences (macOS patches, package selection, config file placement)
- Deploys one `init.el` everywhere, with `system-type` checks for runtime
  differences (exec-path-from-shell, spell backend, keybindings)
- Uses `emacs-unstable` on both platforms, with macOS patches on darwin
- Consolidates the package set: start from the darwin minimal set, incorporate
  the useful bits from the nixos config
- Registers one `flake.modules.homeManager.emacs` consumed by both users

## Emacs — remaining tasks

- **dictd (#17):** `dictionary-server "localhost"` is set in `init.el`, but no
  dictd service runs on hermes. Set up dictd on darwin and port the dictionary
  Nix config from svitax.
- **gptel backend (#18):** still the Ollama placeholder in `init.el`. Replace
  with the Amazon-internal LLM backend.
- **templ-ts-mode on nixos:** removed from the darwin config (broken on
  emacs-git 32: `go-ts-mode--iota-query-supported-p` is void). `emacs-nixos.nix`
  and `nixos-config/init.el` still reference it, and the shared package def at
  `packages/emacs-packages/templ-ts-mode/` is kept for that consumer. Decide
  whether to drop it on nixos too (may not be broken on the nixos emacs).
- **kanata cleanup:** the `kanata-darwin` flake input is imported by no host but
  is still referenced by `modules/services/kanata/kanata.nix`. To remove the
  input, also remove the kanata service module (or keep both for future use).

## Done (2026-06-16)

- Added darwin fonts module (`nerd-fonts.symbols-only`, aporetic, mplus) and
  wired it into hermes — fixes nerd-icons tofu boxes.
- dired: use `gls` (`coreutils-prefixed`) on macOS + restored GNU listing
  switches; resolves the `--dired`/`--group-directories-first` BSD-ls errors.
- vertico no longer shrinks when narrowing: `resize-mini-windows` → `grow-only`.
- Fixed `+man-copy-name-as-kill` (removed the empty-body `when-let*`).
- Removed `templ-ts-mode` from the darwin config.
- Removed the dead `lisp/` dir (`prot-common`/`prot-minibuffer`, unused after
  the port) and its `home.file` deployment.
- Retired the completed svitax-port handoffs under `.sisyphus/handoff/`.
