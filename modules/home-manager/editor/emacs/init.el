;;; init.el --- This is my init.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is where my Emacs config starts.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; init.el gc values (faster loading) ;;;;

;; Do not load outdated byte code files.
(setq load-prefer-newer t)

;; Raise the maximum chunk of output we choose to read from a sub-process. Should vastly speed up performance with language-servers.
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; Default of 800 was too low.
;; Avoid Lisp nesting exceeding.
(setq max-lisp-eval-depth 10000)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (or that the user needs to stop relying on case
;; insensitivity).
(setq auto-mode-case-fold nil)

;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides chaging packages upstream?
(setq ad-redefinition-action 'accept)

;; Set momentary title.
(when (display-graphic-p)
  (setq frame-title-format "loading..."))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Set up package ;;;;

;; Additional load paths.
(eval-when-compile
  ;; "plugins/" contains downloaded packages or plugins I've written.
  (add-to-list 'load-path (concat user-emacs-directory "plugins"))
  ;; "lisp/" is configuration and glue code.
  (add-to-list 'load-path (concat user-emacs-directory "lisp")))

;; use-package-enable-imenu-support must be set before requiring use-package
(setq use-package-enable-imenu-support t)
(require 'use-package)

;; Done loading core init.el.
(defun my/non-core-init ()
  "Load non-core initialisation."
  (message "Emacs ready in %s with %d garbage collections."
	   (format "%.2f seconds" (float-time
				   (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'my/non-core-init)

;; Load no-litter early.
(use-package no-litter)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package extensions ;;

;; TODO: gcmh
;; TODO: validate https://github.com/xenodium/dotsies/blob/main/emacs/features/fe-packages-extensions.el#L56

;; TODO: (:modify-syntax keyword)
;; use-package keyword to easily add characters like -, _, !, :, & as word
;; constituents, $ as paired delimiter, etc

;; TODO: (:pretty-symbols keyword)

(use-package define-repeat-map)

;;;;;;;;;;;;
;;;; ui ;;;;

(when (display-graphic-p)
  ;; No title.
  (setq-default frame-title-format nil)
  ;; Hide the cursor in inactive windows.
  (setq cursor-in-non-selected-windows nil)
  ;; Avoid native dialogs
  (setq use-dialog-box nil))

;; Don't use continuation character
(setq-default fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist))

;; TODO: replace doom-modeline with mood-line? or a custom builtin one? moody? minions?
(use-package doom-modeline
  :ensure t
  :config
  (setq-default mode-line-buffer-identification "%b")
  (setq doom-modeline-mode-alist nil)
  (doom-modeline-def-modeline 'my-modeline
    '(matches buffer-info remote-host buffer-position)
    ;; TODO: checker segment was renamed to check in #62890ef
    '(misc-info time irc debug input-method major-mode process check))
  (add-hook 'doom-modeline-mode-hook
	    (lambda nil
	      (doom-modeline-set-modeline 'my-modeline 'default)))
  (doom-modeline-mode 1)
  (column-number-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-highlight-modified-buffer-name nil)
  (doom-modeline-irc-buffers t)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-time-icon nil))

(use-package spacious-padding
  :ensure t
  :disabled t
  :init (spacious-padding-mode)
  :custom
  (spacious-padding-subtle-mode-line '( :mode-line-active 'default
					:mode-line-inactive 'vertical-border))
  (spacious-padding-widths '( :internal-border-width 16
			      :header-line-width 4
			      :mode-line-width 2
			      :tab-width 2
			      :right-divider-width 24
			      :scroll-bar-width 8)))

(use-package frame
  :custom
  (frame-resize-pixelwise t)
  ;; Consistent window title
  (frame-title-format '("Emacs"))
  ;; Vim-like scroll behaviour
  (scroll-conservatively 101)
  (scroll-step 1)
  (scroll-margin 3))

(use-package fontaine
  :ensure t
  :custom
  (x-underline-at-descent-line nil)
  (fontaine-latest-state-file (var "fontaine-latest-state.eld"))
  (text-scale-remap-header-line t)
  (fontaine-presets '((regular)
		      (t
		       :default-family "Iosevka Comfy"
		       ;; font height is 1/10pt.
		       :default-height 160)))
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1))

(use-package ef-themes :ensure t)

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-common-palette-overrides `((fringe unspecified)
					   (fg-line-number-active fg-main)
					   (bg-line-number-inactive bg-main)
					   (fg-line-number-inactive fg-dim))))

(use-package themes-extras
  :init
  (add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces)
  (add-hook 'ef-themes-post-load-hook #'my/ef-themes-custom-faces)
  :config
  (modus-themes-select 'modus-operandi))

(use-package pulsar
  :ensure t
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 20)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-yellow)
  :config
  (pulsar-global-mode 1)
  (dolist (func '(my/scroll-up-half
		  my/scroll-down-half
		  recenter
		  embark-dwim
		  embark-find-definition
		  better-jumper-jump-forward
		  better-jumper-jump-backward
		  better-jumper-set-jump))
    (add-to-list 'pulsar-pulse-functions func)))

;; TODO: goggles for kill-ring-save
(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config (custom-set-faces
	   '(goggles-added ((t (:inherit pulsar-green))))
	   '(goggles-changed ((t (:inherit pulsar-yellow))))
	   '(goggles-removed ((t (:inherit pulsar-red))))))

(use-package lin
  :ensure t
  :hook (after-init . lin-global-mode)
  :custom (lin-face 'lin-magenta))

(use-package cursory
  :ensure t
  :custom
  (cursory-latest-state-file (var "cursory-latest-state.eld"))
  (cursory-presets '((box
		      :blink-cursor-interval 0.6)
		     (box-no-blink
		      :blink-cursor-mode -1)
		     (bar
		      :cursor-type (bar . 2)
		      :blink-cursor-interval 0.5)
		     (bar-no-other-window
		      :inherit bar
		      :cursor-in-non-selected-windows nil)
		     (bar-thick
		      :cursor-type (bar . 4)
		      :blink-cursor-interval 0.5
		      :blink-cursor-blinks 50)
		     (underscore
		      :cursor-type (hbar . 3)
		      :blink-cursor-blinks 50)
		     (underscore-thick
		      :cursor-type (hbar . 8)
		      :blink-cursor-interval 0.3
		      :blink-cursor-blinks 50
		      :cursor-in-non-selected-windows (hbar . 3))
		     (underscore-thick-no-blink
		      :blink-cursor-mode -1
		      :cursor-type (hbar . 8)
		      :cursor-in-non-selected-windows (hbar . 3))
		     (t
		      :cursor-type box
		      :cursor-in-non-selected-windows hollow
		      :blink-cursor-mode 1
		      :blink-cursor-blinks 10
		      :blink-cursor-interval 0.2
		      :blink-cursor-delay 0.2)))
  :init
  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))
  (add-hook 'kill-emacs-hook #'cursory-store-latest-preset))

(use-package editorconfig
  :ensure t
  :custom (editorconfig-lisp-use-default-indent t))

;; TODO: indents are kinda wonky with this
;; (use-package indent
;;   :hook (after-change-major-mode . +set-indent-offset))

(use-package hide-whitespace
  :hook (change-major-mode-after-body . +set-trailing-whitespace))

(use-package display-line-numbers
  :init (global-display-line-numbers-mode))

(use-package dot-fringe)

;; TODO: better variable pitch mode for org-mode/text-mode
(use-package face-remap
  :hook ((notmuch-show-mode elfeed-show-mode) . my/enable-variable-pitch)
  :init (defun my/enable-variable-pitch ()
	  (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
	    (variable-pitch-mode 1))))

;; visible-mark https://git.sr.ht/~iank/visible-mark/tree/c1852e13b6b61982738b56977a452ec9026faf1b
;; highlight-thing: ideally i want to highlight lsp or treesitter symbols
;; is idle-highlight-mode faster or easier to plug into? https://codeberg.org/ideasman42/emacs-idle-highlight-mode
(use-package highlight-thing
  :ensure t
  :config (custom-set-faces
	   '(highlight-thing ((t (:inherit pulsar-yellow))))))

;;;;;;;;;;;;;;;;;
;;;; scratch ;;;;

(use-package persistent-scratch
  :ensure t
  :config
  ;; Show persistent scratch ASAP
  (persistent-scratch-setup-default)
  (run-with-timer 5 nil (lambda ()
			  (with-current-buffer "*scratch*"
			    ;; idk if lisp-interaction-mode is enough
			    ;; (emacs-lisp-mode)
			    (emacs-lock-mode 'kill)))))

;;;;;;;;;;;;;;;;;;;;;
;;;; maintenance ;;;;

;; Find errors in init.el by bisecting the file
(use-package bug-hunter :ensure t)

;; BUG: doesn't work on nixos
(use-package esup :ensure t)

;; Peek into macros by expanding them inline.
(use-package macrostep :ensure t)

;;;;;;;;;;;;;;
;;;; core ;;;;

(use-package server
  :custom
  (server-client-instructions nil))

(use-package envrc
  :ensure t
  :custom
  (envrc-show-summary-in-minibuffer nil)
  :init (envrc-global-mode))

;; inheritenv
;; buffer-env https://github.com/astoff/buffer-env

(use-package with-editor
  :ensure t
  :hook ((eshell-mode . with-editor-export-editor)
	 (term-exec . with-editor-export-editor)
	 (shell-mode . with-editor-export-editor)
	 (vterm-mode . with-editor-export-editor)))

;; exec-path-from-shell

;;;;;;;;;;;;;;;
;;;; files ;;;;

(use-package files
  :custom
  (use-short-answers t)
  (y-or-n-p-use-read-key t)
  (find-file-suppress-same-file-warnings t)
  ;; Automatically kill running processes on exit
  (confirm-kill-processes nil)
  ;; Always display opened files using canonical location (not symlink)
  (find-file-visit-truename t)
  ;; Disable backup
  (backup-inhibited t)
  (make-backup-files nil)
  ;; Disable autosave
  (auto-save-default nil)
  ;; Open that large file.
  (large-file-warning-threshold nil))

(use-package files-extras
  :config (add-to-list 'find-file-not-found-functions
		       #'+files-create-non-existent-directory))

(use-package autorevert
  :init (global-auto-revert-mode)
  :custom
  ;; Be quiet about reverts
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

(use-package autorevert-extras
  :config
  ;; global-auto-revert-mode can slow things down. try to enable it per active window.
  (add-to-list 'window-state-change-functions #'+window-state-state-change))

;; Avoid creating lock files (ie. .#some-file.el)
(setq create-lockfiles nil)

;; TODO: recentf silent? don't show "cleaning up the recentf list" messages
;; TODO: vertico truncate so recentf file names in /nix/store/ don't get too big
;; TODO: maybe don't include /nix/store/ items from recentf in recentf-extras
;; Keep track of recently opened files. Also feeds into the list of recent
;; directories used by consult-dir.
(use-package recentf
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 300)
  (recentf-save-file (var "recentf-save.el"))
  :config
  ;; Exclude anything in runtime folders
  (add-to-list 'recentf-exclude
	       (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
					     "/run"))))
  ;; Exclude anything in nix store
  (add-to-list 'recentf-exclude (concat "^" (regexp-quote "/nix/store")))
  ;; Text properties inflate the size of recentf's files, and there is no
  ;; purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)
  ;; The most sensible time to clean up your recent files list is when you quit
  ;; Emacs (unless this is a long-running daemon session).
  (setq recent-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup)

  (recentf-mode +1))

(use-package recentf-extras
  :config
  ;; Make filenames more portable and readable
  (add-to-list 'recentf-filename-handlers #'+recentf--file-truename-fn)
  ;; Bump file in recentf file list when it is switched or written to.
  (add-hook 'on-switch-window-hook #'+recentf--touch-buffer-h)
  (add-hook 'write-file-functions #'+recentf--touch-buffer-h)
  ;; TODO: Update recentf list when renaming files in dired https://emacswiki.org/emacs/RecentFiles
  (add-hook 'dired-mode-hook #'+recentf--add-dired-directory-h))

(use-package saveplace
  :custom (save-place-file (var "save-place.el"))
  :init (save-place-mode))

(use-package cus-edit
  :custom (custom-file (file-name-concat user-cache-directory "etc/custom.el")))

;;  TODO: find a place to put these variables
;; 	(echo-keystrokes 0.01)
;; 	(cycle-spacing-actions '(delete-all-space just-one-space restore))
;; 	(words-include-escapes t)
;; 	(view-read-only t) ;; TODO: view-mode
;; 	(vc-follow-symlinks t)
;; 	(word-wrap t)

(use-package savehist
  :init (savehist-mode)
  :custom
  (savehist-file (var "savehist.el"))
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 300)
  (savehist-ignored-variables '(file-name-history))
  (savehist-additional-variables '(kill-ring register-alist
				   mark-ring global-mark-ring
				   search-ring regexp-search-ring
				   vertico-repeat-history))
  (history-length 1000)
  (history-delete-duplicates t))

(use-package savehist-extras
  :config
  (add-hook 'savehist-save-hook '+savehist-unpropertize-variables-h)
  (add-hook 'savehist-save-hook '+savehist-remove-unprintable-registers-h))

;;;;;;;;;;;;;;;;;;;;
;;;; minibuffer ;;;;

(use-package minibuffer
  :bind ( :map minibuffer-local-completion-map
	       ("<backtab>" . minibuffer-force-complete))
  :hook
  ;; TODO: where does force-truncate-lines come from? oantolin?
  ;; (completion-list-mode . force-truncate-lines)
  (minibuffer-setup . cursor-intangible-mode)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (tab-always-indent 'complete)
  (read-file-name-completion-ignore-case t)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (resize-mini-windows t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :init
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode))

(use-package minibuffer-extras
  :config (advice-add #'completing-read-multiple :filter-args #'+crm-indicator))

;;;;;;;;;;;;;;;;;;;;
;;;; marginalia ;;;;

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
	      ("M-a" . marginalia-cycle))
  :custom (marginalia-max-relative-age 0)
  :init (marginalia-mode))

(use-package marginalia-extras)

;;;;;;;;;;;;;;;;;;;
;;;; orderless ;;;;

(use-package orderless :ensure t)

;;;;;;;;;;;;;;;;;
;;;; vertico ;;;;

(use-package vertico
  :ensure t
  :bind ( :map vertico-map
	       ("C-M-n" . vertico-next-group)
	       ("C-M-p" . vertico-previous-group))
  :custom
  (vertico-count 10 "Items displayed, defaults to 10")
  (vertico-cycle t)
  (vertico-resize 'grow-only)
  :init (vertico-mode))

(use-package vertico-extras
  :bind ( :map vertico-map
	       ("H-m" . +vertico-multiform-monocle)
	       ("C-l" . +vertico-multiform-unobtrusive)
	       ("C-j" . +vertico-really-exit)))

(use-package vertico-directory
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind ( :map vertico-map
	       ("DEL" . vertico-directory-delete-char)
	       ("M-DEL" . vertico-directory-delete-word)
	       ("RET" . vertico-directory-enter)))

(use-package vertico-grid
  :after vertico
  :custom
  (vertico-grid-separator "    ")
  (vertico-grid-lookahead 50))

(use-package vertico-quick
  :after vertico
  :bind ( :map vertico-map
	       ("M-<tab>" . vertico-quick-insert)
	       ("C-t" . vertico-quick-jump)
	       ;; ("C-t" . vertico-quick-exit)
	       ("M-t" . vertico-quick-embark)))

(use-package vertico-quick-extras
  :bind (:map vertico-map
	      ("M-t" . +vertico-quick-embark)))

(use-package vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-x '" . vertico-repeat))
  :init (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-package vertico-suspend
  :after vertico
  :bind ("M-z" . vertico-suspend))

;;;;;;;;;;;;;;;;
;;;; embark ;;;;

(use-package embark
  :ensure t
  ;; TODO: figure out a better way have repeat-help mode use embark
  :demand t
  :hide-whitespace embark-collect-mode
  :bind (("C-." . embark-act)
	 ("C-:" . embark-act-all)
	 :map help-map
	 ("b" . embark-bindings)
	 ("H-m" . embark-bindings-in-keymap)
	 ("C-h" . embark-prefix-help-command)
	 :map embark-collect-mode-map
	 ("a")
	 ("." . embark-act)
	 ("F" . consult-focus-lines)
	 :map embark-become-help-map
	 ("x" . describe-command)
	 :map vertico-map
	 ("M-s o" . embark-export))
  :custom
  (embark-cycle-key ".")
  (embark-quit-after-action nil)
  (embark-confirm-act-all nil)
  (prefix-help-command #'embark-prefix-help-command "Use embark instead of `describe-prefix-bindings'")
  (embark-indicators '(embark-minimal-indicator
		       embark-highlight-indicator
		       embark-isearch-highlight-indicator)))

(use-package embark-extras
  :bind (:map help-map
	 ("C-e" . +embark-on-last-message)
	 :map embark-general-map
	 ("C-<tab>" . +embark-select)
	 :map embark-collect-mode-map
	 ("C-<tab>" . +embark-select)
	 :map vertico-map
	 ("C-<tab>" . +embark-select)))

;;;;;;;;;;;;;;;;;
;;;; consult ;;;;

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (("M-y" . consult-yank-pop)
	 :map ctl-x-map
	 ("b" . consult-buffer)
	 ("C-q" . consult-kmacro)
	 :map search-map
	 ("f" . consult-find)
	 ("M-f" . consult-focus-lines) ;; idk
	 ("g" . consult-grep)
	 ("M-g" . consult-git-grep)
	 ("i" . consult-info)
	 ("M-k" . consult-keep-lines)
	 ("r" . consult-ripgrep)
	 ("M-r" . consult-recent-file)
	 :map goto-map
	 ("a" . consult-org-agenda)
	 ("e" . consult-flymake)
	 ("M-e" . consult-compile-error)
	 ("M-g" . consult-goto-line)
	 ("l" . consult-line)
	 ("M-l" . consult-line-multi)
	 ("o" . consult-outline)
	 ("M-k" . consult-bookmark)
	 :map isearch-mode-map
	 ("C-M-r" . consult-isearch-history)
	 :map minibuffer-local-map
	 ("C-M-r" . consult-history)
	 :map help-map
	 ("TAB" . consult-info))
  :custom
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "?")
  (consult-async-input-throttle 0.1)
  (consult-async-refresh-delay 0.1)
  (consult-buffer-sources
   '(consult--source-hidden-buffer
     consult--source-modified-buffer
     consult--source-buffer
     consult--source-recent-file))
  :init
  ;; TODO: is a use-package block inside a :config section better than with-eval-after-load?
  (with-eval-after-load 'pulsar
    (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
    (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry))
  :config
  ;; Hide recent files list (still available with "f" prefix)
  (consult-customize consult--source-recent-file :hidden t)
  ;; Replace functions (consult-multi-occur-test is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur))

(use-package consult-extras
  :bind ( :map goto-map
	       ("i" . my/consult-imenu-all)
	       ("m" . my/consult-mark-all)))

(use-package consult-imenu
  :config
  (setf (alist-get ?k (plist-get (alist-get 'emacs-lisp-mode consult-imenu-config) :types))
	'("Keymaps" font-lock-variable-name face)))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; TODO: consult-dir

;;;;;;;;;;;;;;;
;;;; corfu ;;;;

(use-package corfu
  :ensure t
  :hook ((prog-mode text-mode tex-mode ielm-mode) . corfu-mode)
  :bind ( :map corfu-map
	       ("M-SPC" . corfu-insert-separator)
	       ("TAB" . corfu-insert)
	       ("RET" . nil)
	       ("C-h" . corfu-info-documentation)
	       ("M-." . corfu-info-location))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-count 10)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-scroll-margin 5))

(use-package corfu-quick
  :after corfu
  :bind ( :map corfu-map
	       ;; ("C-t" . corfu-quick-complete)
	       ("C-t" . corfu-quick-jump)))

(use-package corfu-popupinfo
  :after corfu
  :custom (corfu-popupinfo-delay '(1.0 . 0.5))
  :config (corfu-popupinfo-mode 1)
  :bind ( :map corfu-map
	       ("C-v" . scroll-other-window)
	       ("M-v" . scroll-other-window-down)
	       ([remap corfu-info-documentation] . corfu-popupinfo-toggle)))

(use-package corfu-history
  :after corfu
  :init (corfu-history-mode 1)
  :config (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-extras
  :hook ((minibuffer-setup . +corfu-enable-always-in-minibuffer)
	 ((eshell-mode shell-mode comint-mode) . +corfu-shell-settings)))

;;;;;;;;;;;;;;;;;;;;;
;;;; completions ;;;;

;; adds :completions keyword for use-package forms
(use-package completions)

;;;;;;;;;;;;;;
;;;; cape ;;;;

(use-package cape
  :ensure t
  :completions
  (prog-mode (cape-file :depth 5))
  (org-mode (cape-elisp-block :depth -5)
	    (cape-file :depth 5))
  :init (setq-default completion-at-point-functions '()))

;; (use-package cape-extras
;;   :completions
;;   (prog-mode
;;    ;; TODO: learn how to use cape-keyword without it clobbering the eglot capf
;;    ;; Support keyword completions up to 3 chars. After that fallback to mode
;;    ;; specific completion and barring that back to keywords. Prevents keyword
;;    ;; matching obstructing language server mappings.
;;    (+cape-keyword-with-length-limit :depth 10)
;;    (cape-keyword :depth 50)))

;;;;;;;;;;;;;;;;;
;;;; project ;;;;

(use-package project
  :bind (:map goto-map
	 ("f" . project-find-file)
	 ("p" . project-switch-project)
	 :map project-prefix-map
	 ("C-g" . keyboard-quit))
  :custom
  (project-list-file (file-name-concat user-cache-directory "var/projects"))
  (project-switch-commands '((?f "Find file" project-find-file)
			     (?g "Find regexp" project-find-regexp)
			     (?d "Dired" project-dired)
			     (?b "Buffer" project-switch-to-buffer)
			     (?r "Query replace" project-query-replace-regexp)
			     (?v "Magit" magit-project-status)
			     (?k "Kill buffers" project-kill-buffers)
			     (?! "Shell command" project-shell-command)
			     (?e "Eshell" project-eshell))))

(use-package project-extras
  :bind ( :map project-prefix-map
	       ("DEL" . my/project-remove-project)))

;; TODO: snatch projel's rescan-directory and rescan-all-projects, put in project extras
;; projel-add-project-directory, projel--add-project, projecl-rescan-directory, projel-find-projects-in-dir, etc.
;; (use-package projel)

(use-package projection
  :ensure t
  :init (global-projection-hook-mode))

;;;;;;;;;;;;;;;;;
;;;; buffers ;;;;

(use-package buffers-extras
  :bind (:map ctl-x-map
	      ("k" . kill-current-buffer)))

;; ("m" . +move-buffer-file) ;; move buffer and file to DIR
;; ("r" . +rename-file-and-buffer) ;; rename current buffer and file to NEW-NAME
;; TODO: rename-file-and-buffer https://github.com/karthink/.emacs.d/blob/b0ae76c813b133619b2f29b179a7f1ab5193f534/lisp/better-buffers.el#L225-L240
;; TODO: move-buffer-file https://github.com/karthink/.emacs.d/blob/b0ae76c813b133619b2f29b179a7f1ab5193f534/lisp/better-buffers.el#L243-L257

;; ibuffer https://www.reddit.com/r/emacs/s/Ft0yZxEMVD
;; ibuffer-git https://github.com/jrockway/ibuffer-git
;; ibuffer-project? or ibuffer-git?
;; projection-ibuffer?
;; bufler?
;; adviced:kill-buffer--possibly-save https://christiantietze.de/posts/2023/09/kill-unsaved-buffer-ux-action-labels and xenodium
;; uniquify
;; buffer-move https://github.com/lukhas/buffer-move
;; epithet https://github.com/oantolin/epithet
;; uniquify
;; folding / yafolding
;; narrow

;;;;;;;;;;;;;;;;;
;;;; windows ;;;;

(use-package windows-extras
  :bind-keymap ("C-x w" . windows-map)
  :bind (("M-o" . other-window)
	 :map ctl-x-map
	 ("c" . delete-window)
	 (:map windows-map
	       ("c" . delete-window)
	       ("s" . split-window-below)
	       ("v" . split-window-right)
	       ;; ("t" . +toggle-window-split) ;; toggle windows between horizontal and vertical
	       ;; ("w" . +transpose-windows) ;; swaps buffers in current and next window
	       ))
  :init
  (define-prefix-command 'windows-map)
  (define-repeat-map windows-manage
    (:enter
     split-window-below
     split-window-right)
    (:continue
     "c" delete-window
     "n" next-buffer
     "o" other-window
     "p" previous-buffer
     "s" split-window-below
     "v" split-window-right
     "=" enlarge-window-horizontally
     "-" shrink-window-horizontally
     "}" enlarge-window
     "{" shrink-window)))

(use-package popper-extras)

;; TODO: +popper-switch-to-popup from karthink?
(use-package popper
  :ensure t
  :bind
  ("C-'" . popper-toggle)
  ("M-'" . popper-cycle)
  ("C-M-'" . popper-toggle-type)
  :custom
  (popper-window-height 0.40)
  (popper-echo-lines 1)
  :init
  (setq popper-reference-buffers (append +popper-reference-buffers '()))
  (popper-mode)
  (popper-echo-mode))

;;;;;;;;;;;;;;;
;;;; dired ;;;;

;;;; TODO: get better-jumper to mark dired buffers
(use-package dired
  :bind (:map ctl-x-map
	 ("d" . dired-jump)
	 ("C-j" . dired)
	 ("C-o" . nil)
	 ("C-t" . nil)
	 :map dired-mode-map
	 ("b" . dired-up-directory)
	 ("f" . dired-find-file))
  :custom
  (dired-switches-in-mode-line nil)
  (dired-do-revert-buffer t)
  (dired-vc-rename-file t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-create-destination-dirs 'ask)
  (delete-by-moving-to-trash t)
  ;; Adding human readable units and sorted by date
  (dired-listing-switches "-AGhlv --group-directories-first")
  ;; Try to guess the target directory for operations.
  (dired-dwim-target t)
  ;; Automatically refresh dired buffers when contents changes.
  (dired-auto-revert-buffer t))

;; dired-filter? what filters would i use? filter-by-videos from xenodium

;; TODO: should i use dired-filter for this instead?
;; hide dotfiles and hide gitignored files
(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
	      ("." . dired-omit-mode))
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files (concat ; dired-omit-files
		     "^\\." ;; hide all dotfiles
		     "\\|^.DS_STORE\\'"
		     "\\|^.project\\(?:ile\\)?\\'"
		     "\\|^.\\(svn\\|git\\)\\'"
		     "\\|^.ccls-cache\\'"
		     "\\|^.\\(?:\\.js\\)?\\.meta\\'"
		     "\\|^.\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")))

(use-package dired-extras
  :after dired
  :bind ( :map dired-mode-map
	       ("SPC" . +dired-scroll-other-window)
	       ("DEL" . +dired-scroll-other-window-down)
	       ("S-SPC" . +dired-scroll-other-window-down)
	       ("=" . +dired-ediff-files)
	       ("~" . +dired-home-directory)))

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(use-package dired-single
  :ensure t
  :bind (([remap dired-find-file] . dired-single-buffer)
	 ([remap dired-up-directory] . dired-single-up-directory)))

;; dired-imenu
;; dired-sort-by

(use-package wdired
  :after dired
  :bind (:map dired-mode-map
	      ("e" . wdired-change-to-wdired-mode))
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package dired-hist
  :ensure t
  :after dired
  :bind (:map dired-mode-map
	      ("l" . dired-hist-go-back)
	      ("r" . dired-hist-go-forward))
  :config (dired-hist-mode 1))

(use-package trashed
  :ensure t
  :custom
  (trashed-action-confirmer 'y-or-n-p)
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t))
  (trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; dired-ranger
;; `dired-ranger-copy' and `dired-ranger-paste' and `dired-ranger-move'

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
	      ("/" . dired-narrow-regexp)))

;; dired-subtree

(use-package dired-preview
  :ensure t
  :after dired
  :bind (:map dired-mode-map
	 ("P" . dired-preview-mode)
	 :map dired-preview-mode-map
	 ("SPC" . +dired-scroll-other-window)
	 ("DEL" . +dired-scroll-other-window-down)
	 ("S-SPC" . +dired-scroll-other-window-down)))

(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
	      (")" . dired-git-info-mode)))

;; dwim-shell-command for dired
;; dired-open-with https://github.com/FrostyX/dired-open-with
;; dired-open (dired-hacks) vs dired-launch https://codeberg.org/thomp/dired-launch
;; dired-rsync?
;; sudo-edit?
;; dired-du
;; casual-dired https://github.com/kickingvegas/casual-dired (for dired-sort tmenu)

;;;;;;;;;;;;;;;;;
;;;; editing ;;;;

(use-package editing-extras
  :custom
  (cycle-spacing-actions '(just-one-space delete-all-space restore))
  :config
  (my/with-region-or-line comment-or-uncomment-region)
  ;; (my/with-region-or-line kill-ring-save)
  (my/with-region-or-line kill-region)
  :bind ( ;; ("C-a" . my/back-to-indentation-or-beginning)
	 ;; ("M-a" . puni-beginning-of-sexp)
	 ;; ("C-S-a" . puni-syntactic-forward-punct)
	 ("C-S-b" . backward-to-word)
	 ("C-M-b" . backward-sexp) ;; TODO: puni-backward-sexp
	 ("M-c" . capitalize-dwim)
	 ;; ("C-S-c" . puni-convolute)
	 ("C-d" . delete-forward-char) ;; TODO: puni-forward-delete-char
	 ;; ("M-d" . puni-kill-word)
	 ;; ("M-e" . puni-end-of-sexp)
	 ;; ("C-S-e" . puni-syntactic-backward-punct)
	 ("C-S-f" . forward-to-word)
	 ("C-M-f" . forward-sexp) ; TODO: puni-forward-sexp
	 ;; ("M-h" . smart-mark) ; TODO: smart-mark (combobulate menu + easy-kill ux)
	 ;; ("M-i" . evilmi-jump-items-native)
	 ;; ("C-j" . smart-transpose)
	 ;; ("M-j" . smart-join-line)
	 ;; ("C-M-j" . smart-split-line)
	 ("C-k" . my/smart-kill-line) ; TODO: smart-soft-kill-line (first soft-kill to eol, then kill-whole-line)
	 ;; ("M-k" . smart-soft-kill-ring-save-line) ; TODO: smart-soft-kill-ring-save-line (first soft-kill-ring-save to eol, then kill-ring-save-whole-line)
	 ;; ("C-S-k" . smart-soft-kill-line-backward
	 ;; ("C-M-k" . smart-soft-kill-ring-save-backward)
	 ("M-l" . downcase-dwim)
	 ("M-o" . other-window)
	 ;; ("C-S-r" . puni-raise)
	 ;; ("C-S-s" . puni-split)
	 ("M-u" . upcase-dwim)
	 ("C-v" . my/scroll-up-half)
	 ("M-v" . my/scroll-down-half)
	 ;; ("C-w" . smart-kill-region) ; TODO: smart-kill-region (act on active region, else combobulate avy menu + easy-kill ux)
	 ;; ("M-w" . smart-kill-ring-save) ; TODO smart-kill-ring-save (act on active region, else combobulate avy menu + easy-kill ux)
	 ("C-M-y" . duplicate-dwim) ; TODO: smart-duplicate (act on active region, else combobulate menu + easy-kill ux)
	 ("C-z" . zap-up-to-char)
	 ("M-z" . my/zap-up-to-char-save)
	 ("C-S-z" . zap-to-char)
	 ("C-M-z" . my/zap-to-char-save)
	 ("C-DEL" . backward-delete-char-untabify)
	 ;; ("M-DEL" . puni-backward-kill-word)
	 ("C-TAB" . tab-to-tab-stop)
	 ("M-SPC" . my/cycle-spacing-impatient)
	 ("M-;" . comment-or-uncomment-region)
	 ("C-/" . undo-only)
	 ("C-?" . undo-redo)
	 ;; ("C-'" . nil) ; TODO: popper-toggle
	 ;; ("M-'" . abbrev-prefix-mark) ; TODO: popper-cycle
	 ;; ("C-\"" . nil) ; TODO: popper-toggle-type
	 ([M-up] . my/move-text-up)
	 ([M-down] . my/move-text-down)
	 ;; ("C-\\" . toggle-input-method) ; TODO: indent-region
	 ;; ("M-\\" . delete-horizontal-space) ; TODO: format-buffer
	 ;; ("C-|" . nil) ; TODO: my/pipe-region
	 ;; ("C-M-\\" . indent-region) ; TODO: nil
	 :map mode-specific-map
	 ("c" . +capitalize-word-toggle)
	 :map ctl-x-map
	 ;; Unbind
	 ("f" . nil))
  ;;   :bind (;; TODO: structural editing keymap
  ;; 	 ;; ("a" . combobulate-navigate-logical-previous)
  ;; 	 ;; ("b" . backward-sexp)
  ;; 	 ;; ("c" . embrace-commander)
  ;; 	 ;; ("C" . puni-convolute) ; exchages order of application of two closest outer forms
  ;; 	 ;; ("d" . combobulate-navigate-down) ; down into list
  ;; 	 ;; ("e" . combobulate-navigate-logical-next)
  ;; 	 ;; ("f" . forward-sexp)
  ;; 	 ;; ("j" . sp-transpose-sexp)
  ;; 	 ;; ("k" . combobulate-kill-node-dwim) ; kill the current node
  ;; 	 ;; ("C-k" . sp-kill-hybrid-sexp) ; kill everything till the end of parent node
  ;; 	 ;; ("n" . combobulate-navigate-next) ; forward sibling
  ;; 	 ;; ("p" . combobulate-navigate-previous) ; backward sibling
  ;; 	 ;; ("r" . puni-raise) ; uses sexp at point to replace its parent sexp
  ;; 	 ;; ("s" . puni-split) ; split the sexp around point in two
  ;; 	 ;; ("u" . combobulate-navigate-up) ; up into list
  ;; 	 ;; ("]" . puni-slurp-forward)
  ;; 	 ;; ("[" . puni-slurp-backward)
  ;; 	 ;; ("}" . puni-barf-forward)
  ;; 	 ;; ("{" . puni-barf-backward)
  ;;       ;; TODO: lasgun-map (t -> lasgun-mark-char-timer, SPC -> lasgun-make-multiple-cursors, x? -> lasgun-clear-lasgun-mark-ring, u? -> lasgun-pop-lasgun-mark)
  ;;       ;; TODO: tempel (tempel-insert, tempel-expand)
  ;;       ;; TODO: fold
  )

(use-package expand-region
  :ensure t
  :bind
  ("C-," . er/expand-region)
  :custom
  (expand-region-fast-keys-enabled nil)
  :init
  (define-repeat-map expand-region
    (:continue
     "," er/expand-region
     "." er/contract-region)
    (:enter er/expand-region
	    er/contract-region)))

(use-package multiple-cursors
  :ensure t
  :bind-keymap ("M-r" . mc/mark-map)
  :bind (("M-n" . mc/mark-next-like-this-symbol)
	 ("M-p" . mc/mark-previous-like-this-symbol)
	 :map mc/mark-map
	 ("." . mc/mark-all-dwim)
	 ("l" . mc/edit-lines)
	 ("r" . set-rectangular-region-anchor))
  :init
  (define-prefix-command 'mc/mark-map))

(use-package macrursors
  :disabled t
  :bind-keymap ("M-r" . macrursors-mark-map)
  :bind (("M-n" . macrursors-mark-next-instance-of)
	 ("M-p" . macrursors-mark-previous-instance-of)
	 :map macrursors-mode-map
	 ("M-r" . nil)
	 ("M-r M-r" . macrursors-end)
	 :map macrursors-mark-map
	 ("." . macrursors-mark-all-instances-of)
	 ("l" . macrursors-mark-all-lines)
	 ("C-n" . macrursors-mark-next-line)
	 ("C-p" . macrursors-mark-previous-line)
	 :map isearch-mode-map
	 ("M-r" . macrursors-mark-from-isearch)
	 ("M-s n" . macrursors-mark-next-from-isearch)
	 ("M-s p" . macrursors-mark-previous-from-isearch))
  :init
  (define-prefix-command 'macrursors-mark-map)
  (define-repeat-map macrursors-mode
    ("n" macrursors-mark-next-instance-of)
    ("p" macrursors-mark-previous-instance-of)
    ("C-n" macrursors-mark-next-line)
    ("C-p" macrursors-mark-previous-line)
    (:enter macrursors-mark-next-line
	    macrursors-mark-previous-line
	    macrursors-mark-next-from-isearch
	    macrursors-mark-previous-from-isearch)))

(use-package macrursors-select
  :bind ( :map macrursors-mark-map
	       ("SPC" . macrursors-select)
	       ("C-g" . macrursors-select-clear)))

(use-package rect
  :bind
  (:map rectangle-mark-mode-map
	("t" . string-rectangle)
	("o" . open-rectangle)
	("c" . clear-rectangle)
	("n" . rectangle-number-lines)
	("x" . rectangle-exchange-point-and-mark)
	("*" . calc-grab-rectangle)
	(":" . calc-grab-sum-down)
	("_" . calc-grab-sum-across)
	(" " . delete-whitespace-retangle)))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . #'easy-kill)
	 ([remap mark-paragraph] . #'easy-mark)
	 :map easy-kill-base-map
	 ("M-w" . easy-kill-cycle)
	 ("M-h" . easy-kill-cycle))
  :custom
  (easy-kill-try-things '(line symbol forward-line-edge sexp))
  (easy-mark-try-things '(line symbol forward-line-edge sexp))
  (easy-kill-cycle-ignored '(string-to-char-forward string-up-to-char-forward)))

(use-package easy-kill-extras
  :bind (:map easy-kill-base-map
	      ("," . +easy-kill-expand-region)
	      ("." . +easy-kill-contract-region))
  :init
  (setq easy-kill-alist '((?l line "\n")
			  (?w word " ")
			  ;; TODO: the targets from expand-region don't work very well
			  ;; should i just use knu's package?
			  (?s symbol " ") ;; from expand-region
			  (?< inside-pairs "") ;; from expand-region
			  (?> outside-pairs "") ;; from expand-region
			  (?\' inside-quotes "") ;; from expand-region
			  (?\" outside-quotes "") ;; from expand-region
			  (?$ forward-line-edge "")
			  (?^ backward-line-edge "")
			  (?d defun "\n\n")
			  (?b buffer "")
			  (?x sexp "\n")
			  (?L list "\n")
			  (?f string-to-char-forward "") ;; TODO: do i have string-to-char-forward?
			  (?F string-up-to-char-forward "") ;; TODO: do i have string-up-to-char-forward?
			  (?D defun-name " ")
			  (?B buffer-file-name))))

;; TODO: eventually extend `visual-replace' with query-replace-parallel and visual-regexp-steroids functionality
;; TODO: better keys to change the scope?
(use-package visual-replace
  :bind-keymap ("C-;" . visual-replace-map)
  :bind ( :map visual-replace-map
	       ("." . visual-replace-selected)
	       ("C-;" . visual-replace)
	       :map isearch-mode-map
	       ("C-;" . visual-replace-from-isearch))
  :custom (visual-replace-default-to-full-scope t)
  :init (defvar-keymap visual-replace-map))

(use-package replace
  :bind (("M-s M-o" . multi-occur)))

;; TODO: replace-extras
;; my/search-occur-urls

;; TODO: eventually improve surround to be more like nvim-surround and embrace.el
;; treesitter-backed semantic units
;; highlight semantic units
;; which-key or embark help popups
;; change-inner/outer
(use-package surround
  :ensure t
  :bind-keymap ("M-s a" . surround-keymap)
  :bind ( :map surround-keymap
	       ("a" . surround-insert)
	       ("c" . surround-change)
	       ("d" . surround-delete)))

(use-package embrace
  :ensure t
  :disabled t
  :hook (org-mode . embrace-org-mode-hook)
  :bind-keymap ("M-c" . embrace-inner-map)
  :bind ( :map embrace-inner-map
	       ("a" . embrace-add)
	       ("c" . embrace-change)
	       ("d" . embrace-delete))
  :custom (embrace-show-help-p nil)
  :init (defvar-keymap embrace-inner-map))

(use-package change-inner
  :ensure t
  :bind ( :map surround-keymap
	       ("i" . change-inner)
	       ("o" . change-outer)))

(use-package elec-pair
  :init (electric-pair-mode))

(use-package delsel
  :config (delete-selection-mode +1))

(use-package subword
  :hook ((prog-mode . subword-mode)))

(use-package subword-extras
  :bind (:map prog-mode-map
	      ("C-<backspace>" . +backward-delete-subword)))

(use-package smart-hungry-delete
  :ensure t
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
	 ([remap delete-backward-char] . smart-hungry-delete-backward-char)
	 ([remap delete-char] . smart-hungry-delete-forward-char)
	 ([remap delete-forward-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

(use-package simple
  :custom
  (set-mark-command-repeat-pop t "C-u is only needed once in C-u C-SPC to pop multiple locations.")
  (kill-do-not-save-duplicates t "Don't bother saving things to the kill-ring twice, remove duplicates.")
  (idle-update-delay 2 "Wait a bit longer than the default (0.5) seconds before assuming Emacs is idle.")
  (save-interprogram-paste-before-kill t "Increase mark ring size.")
  (global-mark-ring-max 500 "Increase mark ring size.")
  (kill-ring-max 1000))

;; TODO: diverted https://github.com/xenodium/dotsies/blob/main/emacs/ar/diverted.el

(use-package undo-fu-session
  :ensure t
  :custom (undo-fu-session-directory (var "undo-fu-session/"))
  :init (undo-fu-session-global-mode))

;; vundo (or undo-tree) https://github.com/casouri/vundo
;; puni https://github.com/AmaiKinono/puni
;; iedit? https://www.masteringemacs.org/article/iedit-interactive-multi-occurrence-editing-in-your-buffer
;; ts-docstr https://github.com/emacs-vs/ts-docstr

;;;;;;;;;;;;;;;;;;;;
;;;; navigation ;;;;

(use-package isearch
  :custom
  (isearch-regexp-lax-whitespace nil)
  (isearch-allow-scroll 'unlimited)
  (search-highlight t)
  (isearch-lazy-highlight t)
  (isearch-wrap-pause 'no-ding)
  (isearch-repeat-on-direction-change t)
  ;; Make regular isearch interpret the empty space as a regular expression that
  ;; matches any character between the words you give it
  (isearch-lax-whitespace t)
  (search-whitespace-regexp ".*?")
  ;; Display a counter showing the number of the current and the other matches.
  ;; Place it before the prompt, though it can be after it.
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  :bind
  ( :map isearch-mode-map
	 ("C-g" . isearch-cancel) ; instead of `isearch-abort'
	 ("M-<" . isearch-beginning-of-buffer)
	 ("M->" . isearch-end-of-buffer)
	 :map minibuffer-local-isearch-map
	 ("M-/" . isearch-complete-edit)))

(use-package isearch-extras
  ;; TODO: figure out a better way to apply the my/isearch-region advice
  :demand t
  :bind ( :map isearch-mode-map
	       ("M-g l" . my/consult-line-from-isearch)
	       ("C-r" . my/isearch-repeat-backward)
	       ("C-s" . my/isearch-repeat-forward)
	       ("C-M-w" . my/isearch-yank-region)
	       ("<backspace>" . my/isearch-abort-dwim)
	       ("<C-return>" . my/isearch-other-end)
	       ("C-SPC" . my/isearch-mark-and-exit))
  :config
  (advice-add 'isearch-forward :after 'my/isearch-region)
  (advice-add 'isearch-forward-regexp :after 'my/isearch-region)
  (advice-add 'isearch-backward-regexp :after 'my/isearch-region)
  (advice-add 'isearch-backward :after 'my/isearch-region))

;; TODO: phi-search

;; imenu https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L1593
(use-package imenu
  :hook
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry))

;; TODO: grep
;; TODO: wgrep https://github.com/mhayashi1120/Emacs-wgrep
;; https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L4039
;; cc-isearch-menu

(use-package better-jumper
  :ensure t
  :bind (("H-i" . better-jumper-jump-forward)
	 ("C-o" . better-jumper-jump-backward))
  :custom (better-jumper-add-jump-behavior 'replace)
  :init (better-jumper-mode))

(use-package better-jumper-extras
  :init
  (advice-add #'xref-find-definitions :around #'my/set-jump-maybe-a)
  (advice-add #'xref-go-back :around #'my/set-jump-maybe-a)
  (advice-add #'consult-imenu :around #'my/set-jump-maybe-a)
  (advice-add #'consult-line :around #'my/set-jump-maybe-a)
  (advice-add #'find-file :around #'my/set-jump-maybe-a))

(use-package xref
  :custom
  (xref-after-jump-hook '(recenter pulsar-pulse-line))
  (xref-after-return-hook '(recenter pulsar-pulse-line))
  (xref-search-program 'ripgrep))

(use-package buffer-local-xref
  :init (add-hook 'xref-backend-functions #'buffer-local-xref-activate 95))

(use-package dumb-jump
  :ensure t
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; TODO: smart-jump

;; TODO: avy
;; https://github.com/karthink/.emacs.d/blob/master/plugins/demo.el
;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-avy.el
;; https://karthinks.com/software/avy-can-do-anything
(use-package avy
  :ensure t
  :bind (("C-t" . avy-goto-char-timer)
	 :map goto-map
	 ("g" . avy-goto-line)
	 :map isearch-mode-map
	 ("C-t" . avy-isearch))
  :custom
  (avy-timeout-seconds 0.35)
  (avy-single-candidate-jump nil))

;; TODO: lasgun https://github.com/aatmunbaxi/lasgun.el
;; ("M-t" . lasgun-map)

;; TODO: mosey with smartparens?
;; TODO: shift-selection support for mosey commands?
(use-package mosey
  :ensure t
  :bind (("C-a" . mosey-backward-bounce)
	 ("C-e" . mosey-forward-bounce)))

;; TODO: forward and backward paragraph from xenodium
;; TODO: beginend https://github.com/DamienCassou/beginend

;; TODO: can avy + embark replace link-hint?
(use-package link-hint
  :ensure t
  :bind
  (:map mode-specific-map
	("l" . link-hint-open-link)))

(use-package tab-jump-out
  :ensure t
  :config
  ;; TODO: indent-for-tab-command-dwim from xenodium (uses folds)
  (tab-jump-out-global-mode 1)
  ;; (add-hook 'yas-before-expand-snippet-hook
  ;; 	(lambda () (tab-jump-out-mode -1)))
  ;; (add-hook 'yas-after-exit-snippet-hook
  ;; 	(lambda () (tab-jump-out-mode 1)))
  )

(use-package setup-dangerous-ctl-keys
  :init (add-hook 'after-make-frame-functions #'my/setup-dangerous-ctl-keys))

;;;;;;;;;;;;;;;;;;;
;;;; bookmarks ;;;;

;; harpoon https://github.com/kofm/harpoon.el
;; or frog-jumper
;; bookmark https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L3320
;; blist? https://github.com/emacsmirror/blist

;;;;;;;;;;;;;;
;;;; help ;;;;

;; TODO: help-apropos-map from karthink
(use-package help-extras
  :bind (:map help-map
	      ("C-b" . describe-bindings)
	      ("C-f" . describe-face)
	      ("C-k" . describe-keymap)
	      ("M-." . find-function-on-key)))

(use-package helpful
  :ensure t
  :bind (([remap describe-function] . helpful-callable)
	 ([remap describe-symbol] . helpful-symbol)
	 ([remap describe-variable] . helpful-variable)
	 ([remap describe-command] . helpful-command)
	 ([remap describe-key] . helpful-key)
	 :map help-map
	 ("C-." . helpful-at-point))
  :custom (helpful-max-buffers 1))

;; transient
;; embark help for isearch and avy

;; TODO: remove all the default repeat-mode maps
(use-package repeat
  :init (repeat-mode)
  :custom
  (repeat-keep-prefix t)
  (repeat-echo-function #'repeat-echo-mode-line)
  (repeat-echo-mode-line-string (propertize "[R]" 'face 'mode-line-emphasis)))

(use-package repeat-help
  :ensure t
  :after repeat
  :custom (repeat-help-popup-type 'embark)
  :init (repeat-help-mode))

;; TODO: keycast-mode from my dotfiles
(use-package keycast :ensure t)

;;;;;;;;;;;;;;
;;;; info ;;;;

;; info https://github.com/oantolin/emacs-config/blob/696641a592691737ba5a019c67f2af7a6cc09183/init.el#L235-L239
;; info-colors
;; info-variable-pitch

;;;;;;;;;;;;;;;;;
;;;; linting ;;;;

;; TODO: (:lint keyword) flymake-hook from flymake-collection  https://github.com/mohkale/flymake-collection

(use-package flymake
  :ensure t
  :config
  ;; Turn on flymake for all files which have a flymake mode
  (add-hook 'find-file-hook 'flymake-find-file-hook)
  :custom
  (flymake-no-changes-timeout 0.2)
  (flymake-timer 0.2)
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-note-bitmap '(+dot-fringe-bitmap compilation-info))
  (flymake-error-bitmap '(+dot-fringe-bitmap compilation-error))
  (flymake-warning-bitmap '(+dot-fringe-bitmap compilation-warning)))

;; flymake-collection https://github.com/mohkale/flymake-collection
;; how do i replace the flymake-quickdefs with flymake-collection?

(use-package flymake-quickdef
  :ensure t
  :after flymake
  :config
  (flymake-quickdef-backend
    flymake-golangci
    :pre-let ((golangci-exec (executable-find "golangci-lint")))
    :pre-check (unless golangci-exec (error "Cannot find golangci-lint executable"))
    :write-type 'file ; don't really use this
    :proc-form (list golangci-exec "run"
		     "--print-issued-lines=false" "--out-format=line-number"
		     "--enable-all" "--fast" fmqd-temp-file) ; --fast ones can run on single file
    :search-regexp "[^:]*:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 1)))
			    (col (string-to-number (match-string 2)))
			    (text (match-string 3))
			    (pos (flymake-diag-region fmqd-source lnum col))
			    (beg (car pos))
			    (end (cdr pos))
			    (msg (format "golangci> %s" text)))
		       (list fmqd-source beg end :warning msg)))
  (add-hook 'go-ts-mode-hook
	    (lambda ()
	      (add-hook 'flymake-diagnostic-functions 'flymake-golangci)))

  (flymake-quickdef-backend
    flymake-statix
    :pre-let ((statix-exec (executable-find "statix")))
    :pre-check (unless statix-exec (error "Cannot find statix executable"))
    :write-type 'file
    :proc-form (list statix-exec "check" "--format" "errfmt" fmqd-temp-file)
    :search-regexp "^\\([^>]+\\)>\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
			    (col (string-to-number (match-string 3)))
			    (text (match-string 4))
			    (pos (flymake-diag-region fmqd-source lnum col))
			    (beg (car pos))
			    (end (cdr pos))
			    (msg (format "statix> %s" text)))
		       (list fmqd-source beg end :warning msg)))
  (add-hook 'nix-ts-mode-hook
	    (lambda ()
	      (add-hook 'flymake-diagnostic-functions 'flymake-statix))))

;;;;;;;;;;;;;;;;;;;;
;;;; formatting ;;;;

(use-package apheleia
  :ensure t
  :init (apheleia-global-mode))

;; Adds :formatter keyword for use-package forms
(use-package apheleia-use-package)

;;;;;;;;;;;;;
;;;; lsp ;;;;

;; TODO: (:lsp keyword) eglot-server-programs and eglot-server-configuration?

(use-package eglot
  :ensure t ;; i want a more recent version of eglot for eglot--apply-text-edits with the silent arg
  :custom
  (eglot-extend-to-xref t)
  (eglot-autoreconnect nil)
  (eglot-confirm-server-initiated-edits nil)
  :init
  ;; Ask Eglot to stay away from completely taking over Flymake.
  (setq eglot-stay-out-of '(flymake eldoc-documentation-functions eldoc-documentation-strategy company))
  :config
  ;; Just add Eglot as another item in Flymake since we asked Eglot to stay
  ;; away from taking over Flymake.
  (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend 90)
  ;; disable eglot log
  (fset #'jsonrpc--log-event #'ignore))

(use-package eglot-extras
  :hook (after-init . +eglot-auto-ensure-all))

(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode))

(use-package apheleia-eglot)

(use-package consult-eglot
  :ensure t
  :bind (:map eglot-mode-map
	      ([remap xref-find-apropos] . consult-eglot-symbols)
	      ("M-s e" . consult-eglot-symbols)))

;; ctags with citre https://github.com/universal-ctags/citre

;;;;;;;;;;;;;
;;;; dap ;;;;

;; TODO: (:dap keyword)
;; dape https://github.com/svaante/dape
;; edebug

;;;;;;;;;;;;;;;;;;
;;;; snippets ;;;;

;; tempel https://github.com/minad/tempel
;; eglot-tempel https://github.com/fejfighter/eglot-tempel
;; tempel-collection https://github.com/Crandel/tempel-collection

;;;;;;;;;;;;;;;;;;;;
;;;; treesitter ;;;;

(use-package treesit
  :custom (treesit-font-lock-level 4))

;; TODO: treesit before eglot so i can use the build major mode alist
;; TODO: (:treesit keyword) add recipe to treesit-auto list
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

;;;;;;;;;;;;;;;;;
;;;; compile ;;;;

;; :compile keyword (compile-multi)

(use-package compile
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  ;; do i still need inheritenv for compile commands?
  (compilation-mode . hack-dir-local-variables-non-file-buffer)
  :hide-whitespace compilation-mode
  :bind (:map ctl-x-map
	 ("," . project-compile)
	 ("." . recompile)
	 :map compilation-mode-map
	 ("n" . next-error-no-select)
	 ("p" . previous-error-no-select)
	 ("{" . compilation-previous-file)
	 ("}" . compilation-next-file))
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output 'firt-error)
  (compilation-max-output-line-length nil)
  (compilation-message-face nil)
  (compilation-skip-threshold 2)
  :config
  (add-hook 'next-error-hook #'pulsar-pulse-line-red)
  (add-hook 'next-error-hook #'pulsar-recenter-top)
  (add-hook 'next-error-hook #'pulsar-reveal-entry))

(use-package compile-multi
  :ensure t
  :bind (:map ctl-x-map
	      ("/" . compile-multi)))

(use-package consult-compile-multi
  :ensure t
  :after compile-multi
  :config (consult-compile-multi-mode))

(use-package compile-multi-embark
  :after (embark compile-multi)
  :config (compile-multi-embark-mode +1))

(use-package projection-multi
  :ensure t
  :bind (([remap compile-multi] . projection-multi-compile)))

(use-package projection-multi-embark
  :ensure t
  :after (embark projection-multi)
  :config (projection-multi-embark-setup-command-map))

;;;;;;;;;;;;;;
;;;; prog ;;;;

(use-package eldoc
  :bind ("C-h ." . eldoc-print-current-symbol-info)
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  ;; Eldoc resizes the echo area display which is intrusive. Let's not do that.
  (eldoc-echo-area-use-multiline-p nil)
  ;; Skip showing documentation in the echo area and use an `eldoc-doc-buffer'
  ;; window if it is already displayed.
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package eldoc-extras
  :hook
  (emacs-lisp-mode . +eldoc-setup-elisp)
  (eglot-managed-mode . +eldoc-setup-eglot))

(use-package colorful-mode
  :ensure t
  :hook (prog-mode text-mode)
  :custom
  (colorful-use-prefix t)
  (colorful-extra-color-keyword-functions '(colorful-add-rgb-colors
					    colorful-add-hsl-colors
					    colorful-add-hex-colors))
  :config
  ;; external packages' minor-mode should NOT automatically set keys in my global map (C-x c in this case). blow it all up so it stops conflicting with my choices
  (setcdr colorful-mode-map nil))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :custom (hl-todo-wrap-movement t))

(use-package newcomment
  :bind ("M-;" . comment-line)
  :hook ((prog-mode . (lambda ()
			(set (make-local-variable
			      'comment-auto-fill-only-comments)
			     t)))))

;; evil-matchit https://github.com/redguardtoo/evil-matchit
;; symbols-outline https://github.com/liushihao456/symbols-outline.el
;; imenu-list https://github.com/bmag/imenu-list (alternative to symbols-outline)
;; treesitter-context https://github.com/zbelial/treesitter-context.el
;; outli https://github.com/jdtsmith/outli
;; nbarrientos outline config
;; ct.el https://github.com/neeasade/ct.el

;;;;;;;;;;;;
;;;; vc ;;;;

(use-package magit
  :bind (:map ctl-x-map
	      ("g" . magit-status)))

;; consult-git-log-grep https://github.com/ghosty141/consult-git-log-grep
;; git-commit-ts-mode https://github.com/danilshvalov/git-commit-ts-mode

;;;;;;;;;;;;;;
;;;; diff ;;;;

;; ediff

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :custom
  (fringes-outside-margins t)
  (git-gutter:update-interval 0.05))

(use-package git-gutter-fringe
  :ensure t
  :custom (git-gutter-fr:side 'left-fringe)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [#b00000111] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [#b00000111] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [#b00000001
						#b00000011
						#b00001111] nil nil 'bottom))

;;;;;;;;;;;;;;;;;;
;;;; terminal ;;;;

(use-package term
  :bind (:map term-mode-map
	      ("C-c C-j" . +term-toggle-mode)
	      ("C-c C-k" . +term-toggle-mode))
  :config
  ;; https://joelmccracken.github.io/entries/switching-between-term-mode-and-line-mode-in-emacs-term
  (defun +term-toggle-mode ()
    "Toggle term between line mode and char mode."
    (interactive)
    (if (term-in-line-mode)
	(term-char-mode)
      (term-line-mode))))

;; vterm or eat
;; terminal-here?
;; isend-mode https://github.com/ffevotte/isend-mode.el
;; eshell-visual-vterm https://github.com/accelbread/eshell-visual-vterm

;;;;;;;;;;;;;;;;
;;;; comint ;;;;

;; comint
(use-package comint
  :bind (:map comint-mode-map
	      ("C-l" . comint-clear-buffer)
	      ("M-<up>" . comint-previous-prompt)
	      ("M-<down>" . comint-next-prompt))
  :custom
  (comint-scroll-to-bottom-on-input t)
  (comint-scroll-to-bottom-on-output nil)
  (comint-input-autoexpand 'input)
  (comint-buffer-maximum-size 9999)
  (comint-completion-autolist t)
  (comint-input-ignoredups t)
  (comint-prompt-read-only t))

;; comint-fold https://github.com/jdtsmith/comint-fold

;; TODO: make pcmpl-args parse new commands on the fly
(use-package pcmpl-args :ensure t)

;;;;;;;;;;;;;;;
;;;; eshell ;;;;

;; TODO: karthink eshell buffer redirection
;; TODO: karthink eshell atuin

(use-package eshell-extras)

(use-package ffap-eshell
  :after (eshell ffap))

(use-package eshell
  :hook
  ((eshell-mode . +eshell-line-numbers-setup)
   (eshell-mode . +eshell-imenu-setup)
   (eshell-mode . +eshell-outline-setup)
   (eshell-mode . +eshell-expand-region-setup))
  :bind-keymap ("C-x t" . terminal-map)
  :bind (:map ctl-x-map
	 ("<return>" . project-eshell)
	 :map terminal-map
	 ("<return>" . project-eshell)
	 ("e" . +eshell-here)
	 ("E" . eshell)
	 :map eshell-mode-map
	 ("M-k" . eshell-kill-input)
	 ("C-l" . (lambda () (interactive)
		    (eshell/clear-scrollback)
		    (eshell-emit-prompt)))
	 ("C-c f" . +eshell-ffap-find-file)
	 ("C-c C-f" . +eshell-ffap-find-file)
	 ("C-c i" . +eshell-ffap-insert)
	 ("C-c o" . +eshell-export-last-output)
	 ("C-c M-w" . +eshell-kill-ring-save-output)
	 ("C-c >" . +eshell-redirect-to-buffer)
	 ("C-<return>" . +eshell-send-detached-input)
	 ("M-." . +eshell-insert-args) ;; only works if arg isn't a number
	 ("M-<up>" . eshell-previous-prompt)
	 ("M-<down>" . eshell-next-prompt))
  :custom
  (eshell-directory-name (etc "eshell/"))
  :init
  (define-prefix-command 'terminal-map)
  :config
  (setenv "PAGER" "cat")
  (setenv "EDITOR" "emacsclient")

  (advice-add 'eshell-life-is-too-much :after #'+delete-window-if-not-single)
  (advice-add 'eshell-mark-output :after #'activate-mark)

  (defalias 'eshell/b '+eshell/b)
  (defalias 'eshell/e '+eshell/emacs)
  (defalias 'eshell/ec '+eshell/emacs)
  (defalias 'eshell/emacs '+eshell/emacs)
  (defalias 'eshell/hat '+eshell/hat)
  (defalias 'eshell/v 'eshell-exec-visual)
  (defalias 'eshell/view '+eshell/view)
  (defalias 'eshell/x #'eshell/exit)
  (defalias 'eshell/z '+eshell/z)
  ;; It's nicer to type (range 0 3) in eshell.
  (defalias 'eshell/range #'number-sequence)
  (defalias 'range #'number-sequence))

(use-package esh-mode
  :custom
  (eshell-aliases-file (etc "eshell/aliases"))
  (eshell-login-script (etc "eshell/login"))
  (eshell-rc-script (etc "eshell/rc"))
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all))

;; TODO: systemctl --user restart emacs.service doesn't save eshell history
(use-package em-hist
  :custom
  (eshell-history-size 20000)
  (eshell-hist-ignoredups t)
  :config
  (advice-add #'eshell-add-input-to-history :around #'+eshell-add-input-to-history-a)
  (advice-add #'eshell-exec-visual :around #'+eshell-exec-visual-a))

(use-package em-glob
  :custom
  (eshell-glob-case-insensitive t)
  (eshell-error-if-no-glob t))

(use-package em-banner
  :custom (eshell-banner-message ""))

;; xenodium xterm-color?
(use-package em-term
  :custom (eshell-term-name "xterm-256color"))

;; pcmpl-git
;; pcmpl-args
;; pcomplete-extension
;; shrink-path

(use-package eshell-up
  :ensure t
  :config (defalias 'eshell/up #'eshell-up))

;; TODO: the esh-help setup function doesn't work, set it up myself
;; or write my own package
(use-package esh-help
  :disabled t
  :ensure t
  :config (setup-esh-help-eldoc))

(use-package eshell-syntax-highlighting
  :ensure t
  :hook (eshell-mode . eshell-syntax-highlighting-global-mode))

;; fish-completion?
;; bash-completion?
;; capf-autosuggest?

;;;;;;;;;;;;;;;
;;;; elisp ;;;;

;; package-lint-flymake

(use-package elisp-fontification)

(use-package elisp-indentation)

(use-package eros
  :ensure t
  :hook (emacs-lisp-mode . eros-mode)
  :init (add-hook 'eros-inspect-hooks (lambda () (flymake-mode -1))))

(use-package elisp-demos
  :ensure t
  :init
  (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package highlight-quoted
  :ensure t
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package highlight-numbers
  :ensure t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :custom (highlight-numbers-generic-regexp "\\_<[[:digit]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;;;;;;;;;;;
;;;; go ;;;;

;; golangci-lint-langserver?

;; formatting via `goimports', `gofumpt'
;; linting via `revive', `golangci'
;; testing (compile?)
;; import packages (GoGet, GoImport)
;; modify struct tags with `gomodifytags' https://github.com/brantou/emacs-go-tag
;; generate method stubs for implementing an interface via `impl' https://github.com/emacsorphanage/go-impl
;; generate json models via `quicktype' https://github.com/Artawower/quicktype.el
;; generate if err based on function return values via `iferr'
;; generate test boilerplate via `gotests'

;; go-fill-struct https://github.com/s-kostyaev/go-fill-struct

;;;;;;;;;;;;;;
;;;; rust ;;;;

;;;;;;;;;;;;;
;;;; nix ;;;;

(use-package nix-mode
  :ensure t
  :formatter
  (alejandra . ("alejandra")) nix-mode)

(use-package nix-ts-mode
  :ensure t
  :formatter (alejandra) nix-ts-mode)

;; nix3.el https://github.com/emacs-twist/nix3.el

;;;;;;;;;;;;;;;;
;;;; python ;;;;

;;;;;;;;;;;;;;;;
;;;; docker ;;;;

;; docker
;; dockerfile-mode
;; flymake-hadolint
;; docker-compose-mode

;;;;;;;;;;;;;;;;
;;;; remote ;;;;

;; tramp
;; docker-tramp
;; kele https://github.com/jinnovation/kele.el
;; devcontainer https://github.com/bradschartz/devcontainer.el
;; emacs-dev-containers https://github.com/alexispurlane/emacs-dev-containers
;; daemons https://github.com/cbowdon/daemons.el

;;;;;;;;;;;;;;;;;;
;;;; markdown ;;;;

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command "pandoc -t html5")
  (markdown-fontify-code-block-natively t))

;; grip-mode https://github.com/seagle0128/grip-mode

;;;;;;;;;;;;;
;;;; org ;;;;

;; org
(use-package org
  :custom
  (org-ellipsis "…")
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  ;; Org styling, hide markup etc.
  (org-hide-emphasis-markers t)
  (org-pretty-entities t))

(use-package org-agenda
  :custom
  ;; Agenda styling
  (org-agenda-tags-column 0)
  ;; (org-agenda-block-separator ?─)
  (org-agenda-time-grid)
  '((daily today require-timed)
    (800 1000 1200 1400 1600 1800 2000)
    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────"))

(use-package org-modern
  :ensure t
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda)))

(use-package org-block-capf
  :after org
  :completions (org-mode (org-block-capf)))

(use-package org-src-context
  :config
  (add-hook 'org-src-mode-hook #'eglot-ensure))

;; org-pdftools https://github.com/fuxialexander/org-pdftools
;; book-mode https://github.com/rougier/book-mode
;; org-inline-tags
;; org-timeblock https://github.com/ichernyshovv/org-timeblock

;;;;;;;;;;;;;;;;
;;;; biblio ;;;;

(use-package citar
  :ensure t
  :commands citar--bibliography-files
  ;; TODO: make notes-map prefix
  :bind (:map mode-specific-map
	      ("n o" . citar-open)
	      ("n p" . citar-open-files))
  :custom
  (citar-select-multiple nil)
  (citar-bibliography '("~/OneDrive/docs/lib.bib"))
  (citar-library-paths '("~/OneDrive/docs/books"))
  (citar-templates
   '((main . "${title:55} ${author editor:55} ${date year issued:4}")
     (suffix . "  ${tags keywords keywords:40}")
     (preview . "${author editor} ${title}, ${journal publisher container-title collection-title booktitle} ${volume} (${year issued date}).\n")
     (note . "#+title: Notes on ${author editor}, ${title}")))
  :completions (org-mode (citar-capf)))

(use-package citar-extras)

(use-package citar-embark
  :ensure t
  :init (citar-embark-mode))

(use-package citar-denote
  :ensure t
  :bind (:map mode-specific-map
	      ("n a" . citar-denote-add-citekey)
	      ("n C-a" . citar-denote-remove-citekey))
  :init (citar-denote-mode))

;;;;;;;;;;;;;;;
;;;; notes ;;;;

(use-package denote
  :ensure t
  ;; BUG: incompatible with diredfl-mode
  ;; TODO: turn off diredfl-mode only in denote directories?
  ;; :hook (dired-mode . denote-dired-mode-in-directories)
  :bind (:map mode-specific-map
	      ("n b" . denote-find-backlink)
	      ("n i" . denote-link-or-create)
	      ("n k" . denote-keywords-add)
	      ("n K" . denote-keywords-remove)
	      ("n l" . denote-find-link)
	      ("n r" . denote-rename-file)
	      ("n s" . denote-rename-file-using-front-matter))
  :custom
  (denote-directory (expand-file-name "~/OneDrive/notes/"))
  (denote-known-keywords '("emacs" "denote" "testing")))

(use-package consult-denote
  :ensure t
  :bind (:map mode-specific-map
	      ("n f" . consult-denote-find)
	      ("n g" . consult-denote-grep))
  :init (consult-denote-mode))

;; TODO: org-noter and denote integration. look at org-roam integration
;; creating a denote file and then linking to a document with org-noter doesn't
;; work because the document has a different time tag.
;; creating an org-noter file from inside a document doesn't give the file the
;; right name for denote, and doesn't add the correct metadata.
(use-package org-noter
  :ensure t
  :init
  (setq org-noter--show-arrow-hook '()) ;; default always gives an error, can't be bothered to fix
  (setq org-noter-always-create-frame nil)
  (setq org-noter-kill-frame-at-session-end nil)
  (setq org-noter-notes-search-path (list denote-directory)))

;; denote-explore https://github.com/pprevos/denote-explore
;; annotate https://github.com/bastibe/annotate.el
;; anki-editor https://github.com/louietan/anki-editor

;;;;;;;;;;;;;;;;;
;;;; reading ;;;;

(use-package pdf-tools
  :ensure t
  :mode ("\\.[Pp][Dd][Ff]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook
  (pdf-view-mode . pdf-tools-enable-minor-modes)
  (pdf-view-mode . (lambda () (progn
				(blink-cursor-mode -1)
				(display-line-numbers-mode -1)
				(hl-line-mode -1))))
  :custom (large-file-warning-threshold nil)
  :init (setq-default pdf-view-dispay-size 'fit-page)
  :config (pdf-tools-install :no-query))

;; Add support for pdf-view and DocView buffers to `save-place'.
(use-package saveplace-pdf-view :ensure t)

(use-package nov
  :ensure t
  :mode ("\\.[Ee][Pp][Uu][Bb]\\'" . nov-mode)
  :custom (nov-save-place-file (var "nov-save-place.el")))

(use-package djvu :ensure t)

;; wombag
;; elfeed
;; phundrak config elfeed

;;;;;;;;;;;;;;;
;;;; icons ;;;;

(use-package nerd-icons
  :ensure t
  :config
  (add-to-list 'nerd-icons-extension-icon-alist
	       '("lock" nerd-icons-codicon "nf-cod-lock"
		 :face nerd-icons-yellow))
  (add-to-list 'nerd-icons-mode-icon-alist
	       '(exwm-mode
		 nerd-icons-codicon "nf-cod-browser"
		 :face nerd-icons-purple)))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; TODO: hl-line mode doesn't highlight nerd-icons in dired
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; TODO: hl-line mode doesn't highlight nerd-icons in ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package magit-file-icons
  :ensure t
  :after magit
  :init (magit-file-icons-mode 1))

;; TODO create compile-multi-nerd-icons

;;;;;;;;;;;;;;;;;
;;;; secrets ;;;;

;; pass.el or passage.el https://github.com/anticomputer/passage.el
;; age.el https://github.com/anticomputer/age.el
;; sops https://github.com/djgoku/sops
;; pinentry

;;;;;;;;;;;;;;;
;;;; email ;;;;

;; notmuch

;;;;;;;;;;;;;;;;;;;;
;;;; web-search ;;;;

;; consult-omni https://github.com/armindarvish/consult-omni
;; engine-mode https://github.com/hrs/engine-mode
;; bookmark-web https://github.com/AuPath/bookmark-web

;;;;;;;;;;;;;
;;;; mpv ;;;;

;; org-mpv-notes
;; ready-player https://github.com/xenodium/ready-player

;;;;;;;;;;;;;;;
;;;; music ;;;;

;;;;;;;;;;;;;;;;;
;;;; storage ;;;;
;; dropbox https://github.com/lorniu/emacs-dropbox

;;;;;;;;;;;;;;;;;;;;
;;;; workspaces ;;;;

;; activities

;;;;;;;;;;;;
;;;; ai ;;;;

;; gptel

;;;;;;;;;;;;;;;;
;;;; extras ;;;;

;; fcitx.el https://github.com/cute-jumper/fcitx.el
;; add-all-to-list
