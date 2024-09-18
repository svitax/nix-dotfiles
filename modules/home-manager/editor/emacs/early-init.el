;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Disable packages.el ;;;;

;; Emacs initializes its built-in `package.el' package manager before init,
;; unless we preempt it in early init.
(setopt package-enable-at-startup nil)

;; Later on, `use-package' will attempt to initialize `package.el' via the same
;; `:ensure' keyword that my Nix configuration reads. `:ensure' it still relevant
;; to Nix's `emacsWithPackagesFromUsePackage', but we want Emacs itself to ignore
;; it.
(setopt use-package-ensure-function 'ignore)

;; That should be sufficient, but we can further protect against leaks
;; by clearing out the package archives.
(setopt package-archives nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Native compilation cache ;;;;

;; More effort goes into keeping `.config/emacs' clean with custom XDG
;; use-package keywords through `use-package-xdg' by rossabaker, but
;; the native compilation cache needs to happen in `early-init.el'.
(require 'xdg)
(startup-redirect-eln-cache
 (expand-file-name "emacs/eln-cache/" (xdg-cache-home)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Disable unused UI elements ;;;;

;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No menu-bar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No tool-bar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; No tooltip by default.
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; No alarms by default
(setq ring-bell-function 'ignore)
