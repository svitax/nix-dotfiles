;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Even though straight.el already byte-compiles all installed packages, setting it to `t' doesn't hurt.
(setq comp-deferred-compilation t)

;; Remove all package.el loading at startup time, we are using straight.el and use-package.
;; Also avoid package.el adding `custom-set-variables' to init.el.
;; (setq package-enable-at-startup nil
;;       package--init-file-ensured t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(font . "JetBrains Mono Nerd Font") default-frame-alist)
(push '(cursor . "#ebdbb2") default-frame-alist)
(push '(undecorated . t) default-frame-alist)

;; Faster to disable these here (before they've been initialized)
(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)
(defun sx-disable-scroll-bars (frame)
  "Remove all possible scroll-bars, both vertical and horizontal."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'sx-disable-scroll-bars)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; Set `load-prefer-newer' to non-nil value, to avoid Emacs using older byte-compiled files. Using newer files, we force emacs to "byte-compile" the files that it is trying to use.
(setq load-prefer-newer noninteractive)

;; Change location of the native compilation cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))
