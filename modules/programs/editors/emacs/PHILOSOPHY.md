# Emacs Philosophy

This document states the principles that govern the Emacs configuration in
this repo. Its purpose is **consistency**: any change should be checkable
against these principles. It is a statement of philosophy, not a list of
what is installed. When in doubt, prefer the built-in option and re-read the
dependency bar below.

## Core Principle: Built-in Emacs First

This configuration leans on **built-in Emacs functionality** with the fewest
third-party packages and the fewest tweaks that still give a comfortable
daily setup. Modern Emacs ships capable completion, project, version-control,
and editing facilities. We reach for the built-in option first and stay there
unless it is genuinely inadequate.

## The Dependency Bar

The bar for adding a dependency is high. A package — or a vendored library —
earns its place only when:

1. There is **no built-in equivalent**, or the built-in equivalent is
   **genuinely inadequate** for daily use; and
2. It does not introduce new problems of its own (bloat, a large dependency
   tree, or poor performance); and
3. The reason it earns its place is understood, and written down where it is
   configured when that reason is not obvious.

A built-in facility that is merely *adequate* always wins over a third-party
package that is only *nicer*. "Nicer", "more popular", or "more featureful"
are not qualifying reasons on their own — inadequacy of the built-in is.

## Vendoring

Vendoring plain Elisp into the config is acceptable on exactly the same terms
as any other dependency: only when it has been determined to be genuinely
useful and necessary. A small library that augments a built-in subsystem is
preferable to a package that replaces that subsystem wholesale.

## Minimal Tweaks

Prefer the smallest change that achieves the goal. Avoid speculative
configuration, defensive settings for situations that do not arise, and
options carried over from elsewhere without a reason that applies here. Every
setting should trace to an actual need.

## Structure

- Keep the configuration in a **single `init.el`** rather than splitting it
  across many files.
- The **unit of organization is the `use-package` form**: one per feature,
  whether built-in or external. Built-in features are declared with the same
  form using its no-load option — `:no-require` today — so they read the same
  as external ones and group naturally alongside them. The principle is that
  built-ins get a first-class declaration too, not that the keyword name is
  fixed.
- Each form is **self-contained** — its options, keybindings, hooks, and the
  comments explaining them all live together in that one place.
- Group the forms into **clearly labeled sections** with header-comment
  banners, ordered from foundational settings (frame, startup, core editing)
  outward to specific features.

## Configuration Mechanisms

Within that structure, configuration uses a small set of deliberately chosen
forms. Each is picked over a more obvious alternative for a specific reason.
Name the concrete form below, but treat **the reason as the durable
principle**: if a future form satisfies the same reason better, prefer it.

A cross-cutting reason for preferring the explicit forms (`add-hook`,
`bind-keys`, `with-eval-after-load`) over the equivalent use-package keywords
(`:hook`, `:bind`, `:commands`) is that the keywords also generate
**autoloads** — implicit deferred loading. This configuration **eager-loads**
instead. Keeping the package set light and tight (see the dependency bar)
keeps startup cost low enough that deferral is not worth its complexity, and
eager loading keeps behaviour identical whether or not a persistent daemon is
running. The explicit forms keep loading under explicit, eager control rather
than having deferral introduced as a side effect of a keyword.

- **Options → `setopt`.** It runs an option's custom setter, so the value
  takes effect the way the option's author intended; plain `setq` does not.
  Principle: use the setter-aware form.
- **Keybindings → `bind-keys` / `bind-key`.** Chosen over raw `define-key` /
  `global-set-key`, and over use-package's `:bind` keyword, for three reasons:
  uniform declarative grouping (`:map`), binding introspection via
  `describe-personal-keybindings`, and override semantics via `bind-key*`
  (personal bindings that win over minor-mode maps). Principle: keys go through
  one declarative, introspectable form — never scattered imperative calls.
- **Hooks → `add-hook`.** Chosen over use-package's `:hook` because it names
  the real `*-hook` variable, accepts lambdas and the depth/local arguments,
  and avoids `:hook`'s implicit mode→hook-name suffixing. Principle: add to
  hooks with the explicit primitive that exposes full control, not sugar that
  hides it.
- **Deferred / cross-feature config → `with-eval-after-load`.** Chosen over an
  eager `require` or a use-package `:after` dependency because it runs only
  once the feature actually loads, without forcing eager loading or coupling
  load order. Principle: defer configuration to load time without imposing an
  ordering.
- **Buffer-local settings → `setq-local`.** Chosen over global `setopt` / `setq`
  when a value should apply only in certain buffers (typically set from a mode
  hook), so mode-specific settings do not leak. Principle: scope a setting to
  the narrowest place it applies.

## Conventions

- **Comment generously, and comment the _why_, not the _what_.** The reasoning
  behind a setting is what keeps the configuration coherent and maintainable
  over time; a future reader should understand a choice without external
  context. The explanatory comments are a first-class part of the
  configuration, not decoration: they carry the reasoning, so they are kept
  and kept accurate, never dropped or hollowed out.
- Keep a **single configuration that works across platforms**, gating the few
  genuine platform differences with explicit platform predicates rather than
  divergent configs. The concrete predicates today are `system-type` at runtime
  and `stdenv.isDarwin` / `stdenv.isLinux` in Nix; the durable principle is to
  branch on an explicit predicate, so prefer whatever the current correct one is.
