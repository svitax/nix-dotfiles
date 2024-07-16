;;; init.el -*- lexical-binding: t; -*-

;;;; package

;; "plugins/" contains downloaded packages or plugins I've written
(add-to-list 'load-path (concat user-emacs-directory "plugins"))

(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-enable-imenu-support t
	use-package-minimum-reported-time 0.01)
  (require 'use-package))

;; load this early to use the define-repeat-map macro before I load the rest of
;; the repeat-mode config
(use-package define-repeat-map)

;;;; files

(use-package no-litter)

(use-package files
  :bind (:map ctl-x-map
	      ("C-a" . find-file))
  :custom
  (make-backup-files nil)
  (backup-directory-alist (file-name-concat user-cache-directory "backup"))
  ;; (backup-directory-alist
  ;;  `((,(concat "\\`" (file-name-as-directory temporary-file-directory)))
  ;;    ("\\`/tmp/" . nil)
  ;;    ("\\`/dev/shm/" . nil)
  ;;    ("." . ,(var "backup/"))))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 5)
  (auto-save-default nil)
  (auto-save-interval 2400)
  (auto-save-timeout 300)
  (auto-save-list-file-prefix (var "auto-save/sessions/"))
  (auto-save-file-name-transforms
   `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
      ,(concat (file-name-as-directory temporary-file-directory) "\\2") t)
     ("\\`/tmp\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
     ("\\`/dev/shm\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
     (".*" ,(var "auto-save/") t)))
  (find-file-suppress-same-file-warnings t)
  ;; TODO: find a place to put these variables
  (echo-keystrokes 0.01)
  (server-client-instructions nil)
  (set-mark-command-repeat-pop t)
  (cycle-spacing-actions '(delete-all-space just-one-space restore))
  (words-include-escapes t)
  (view-read-only t) ;; TODO: view-mode
  (vc-follow-symlinks t)
  (word-wrap t)
  (kill-do-not-save-duplicates t)
  (y-or-n-p-use-read-key t)
  (use-short-answers t))

;; TODO: recentf-extras
;; TODO: vertico truncate so recentf file names in /nix/store/ don't get too big
;; TODO: maybe don't include /guix/store/ items from recentf in recentf-extras
(use-package recentf
  :init (recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 300)
  (recentf-save-file (var "recentf-save.el"))
  :config
  ;; Keep track of recently opened files. Also feeds into the list of recent
  ;; directories used by consult-dir.
  (defun doom--recentf-file-truename-fn (file)
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
	(abbreviate-file-name (file-truename (tramp-file-name-localname file)))
      file))

  ;; Exclude anything in runtime folders
  (add-to-list 'recentf-exclude
               (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
                                             "/run"))))
  ;; Exclude anything in nix store
  (add-to-list 'recentf-exclude (concat "^" (regexp-quote "/nix/store")))

  ;; Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
  ;; abbreviate $HOME -> ~ in filepaths (more portable, more reaadable, & saves
  ;; space)
  (add-to-list 'recentf-filename-handlers #'doom--recentf-file-truename-fn)
  ;; Text properties inflate the size of recentf's files, and there is no
  ;; purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  (defun doom--recentf-touch-buffer-h ()
    "Bump file in recent file list when it is switched or written to."
    (when buffer-file-name
      (recentf-add-file buffer-file-name))
    ;; Return nil for `write-file-functions'
    nil)

  (add-hook 'on-switch-window-hook 'doom--recentf-touch-buffer-h)
  (add-hook 'write-file-functions 'doom--recentf-touch-buffer-h)

  (defun doom--recentf-add-dired-directory-h ()
    "Add Dired directories to recentf file list."
    (recentf-add-file default-directory))

  (add-hook 'dired-mode-hook 'doom--recentf-add-dired-directory-h)

  ;; The most sensible time to clean up your recent files list is when you quit
  ;; Emacs (unless this is a long-running daemon session).
  (setq recent-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

(use-package saveplace
  :custom (save-place-file (var "save-place.el"))
  :init (save-place-mode))

(use-package cus-edit
  :custom (custom-file (file-name-concat user-cache-directory "etc/custom.el")))

(use-package autorevert
  :init (global-auto-revert-mode)
  :custom (global-auto-revert-non-file-buffers t))

;; TODO: rename-file-and-buffer https://github.com/karthink/.emacs.d/blob/b0ae76c813b133619b2f29b179a7f1ab5193f534/lisp/better-buffers.el#L225-L240
;; TODO: move-buffer-file https://github.com/karthink/.emacs.d/blob/b0ae76c813b133619b2f29b179a7f1ab5193f534/lisp/better-buffers.el#L243-L257

;;;; theme

(use-package ef-themes
  :ensure t)

;; TODO: modus-themes-extras
(use-package modus-themes
  :ensure t
  :preface
  (defun my/modus-themes-custom-faces ())
  ;; Using the hook lets our changes persist when we use the command
  ;; `modus-themes-toggle', `modus-themes-select', and `modus-themes-load-random'.
  (add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces)
  :init (modus-themes-select 'modus-operandi-tinted))

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

(use-package spacious-padding
  :ensure t
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

(use-package face-remap
  :hook ((text-mode notmuch-show-mode elfeed-show-mode) . my/enable-variable-pitch)
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

(use-package colorful-mode
  :ensure t
  :hook (prog-mode text-mode)
  :custom
  (colorful-use-prefix t)
  (colorful-extra-color-keyword-functions '(colorful-add-rgb-colors
					    colorful-add-hsl-colors
					    colorful-add-hex-colors)))

;; ct.el https://github.com/neeasade/ct.el

(use-package fontaine
  :ensure t
  ; :bind (:map ctl-x-map
  ; ("xf" . fontaine-set-preset))
  :custom
  (x-underline-at-descent-line nil)
  (fontaine-latest-state-file (var "fontaine-latest-state.eld"))
  (text-scale-remap-header-line t)
  (fontaine-presets '((regular)
		      (t
		       :default-family "Iosevka Comfy"
		       :default-height 160)))
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :custom (hl-todo-wrap-movement t))

;;;; editing

(use-package editing-extras
  :custom
  (cycle-spacing-actions '(just-one-space delete-all-space restore))
  :config
  (my/with-region-or-line comment-or-uncomment-region)
  (my/with-region-or-point-to-eol kill-ring-save)
  (my/with-region-or-sexp-or-line kill-region)
  :bind (("C-a" . my/back-to-indentation-or-beginning)
	 ;; ("M-a" . puni-beginning-of-sexp)
	 ;; ("C-S-a" . puni-syntactic-forward-punct)

	 ("C-S-b" . backward-to-word)
	 ("C-M-b" . backward-sexp) ;; TODO: puni-backward-sexp
	 
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
	 
	 ("C-l" . recenter)
	 ("M-l" . move-to-window-line-top-bottom)
	 ("C-S-l" . recenter-top-bottom)

	 ("M-o" . other-window)	 
	 
	 ;; ("C-S-r" . puni-raise)
	 
	 ;; ("C-S-s" . puni-split)
	 	 
	 ("C-v" . my/scroll-up-half)
	 ("M-v" . my/scroll-down-half)

	 ;; ("C-w" . smart-kill-region) ; TODO: smart-kill-region (act on active region, else combobulate avy menu + easy-kill ux)
	 ;; ("M-w" . smart-kill-ring-save) ; TODO smart-kill-ring-save (act on active region, else combobulate avy menu + easy-kill ux)
	 
	 ("C-M-y" . duplicate-dwim) ; TODO: smart-duplicate (act on active region, else combobulate menu + easy-kill ux)

	 ("C-z" . zap-up-to-char)
	 ("M-z" . my/zap-up-to-char-save)
	 ("C-S-z" . zap-to-char)
	 ("C-M-z" . my/zap-to-char-save)

	 ;; ("DEL" . backward-delete-char-greedy)
	 ("C-DEL" . backward-delete-char-untabify)
	 ;; ("M-DEL" . puni-backward-kill-word)
	 
	 ("C-TAB" . tab-to-tab-stop)

	 ("M-SPC" . my/cycle-spacing-impatient)

	 ("M-;" . comment-or-uncomment-region)
	 
	 ("C-/" . undo-only)
	 ("M-/" . undo-redo)
	 
	 ;; ("C-," . better-jumper-set-jump)
	 
	 ;; ("C-." . nil) ; TODO: embark-act
	 ;; ("M-." . xref-find-definitions) ; TODO: embark-dwim
	 ;; ("C-M-." . xref-find-apropos) ; TODO: embark-act-all

	 ;; ("C-'" . nil) ; TODO: popper-toggle
	 ;; ("M-'" . abbrev-prefix-mark) ; TODO: popper-cycle
	 ;; ("C-\"" . nil) ; TODO: popper-toggle-type

	 ([M-up] . my/move-text-up)
	 ([M-down] . my/move-text-down)

	 ;; ("C-\\" . toggle-input-method) ; TODO: indent-region
	 ;; ("M-\\" . delete-horizontal-space) ; TODO: format-buffer
	 ;; ("C-|" . nil) ; TODO: my/pipe-region
	 ;; ("C-M-\\" . indent-region) ; TODO: nil
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
;; 	 ;; ("g" . )
;; 	 ;; ("h" . )
;; 	 ;; ("i" . )
;; 	 ;; ("j" . sp-transpose-sexp)
;; 	 ;; ("k" . combobulate-kill-node-dwim) ; kill the current node
;; 	 ;; ("K?" . sp-kill-hybrid-sexp) ; kill everything till the end of parent node
;; 	 ;; ("l" . )
;; 	 ;; ("m" . )
;; 	 ;; ("n" . combobulate-navigate-next) ; forward sibling
;; 	 ;; ("o" . )
;; 	 ;; ("p" . combobulate-navigate-previous) ; backward sibling
;; 	 ;; ("q" . )
;; 	 ;; ("r" . puni-raise) ; uses sexp at point to replace its parent sexp
;; 	 ;; ("s" . puni-split) ; split the sexp around point in two
;; 	 ;; ("t" . )
;; 	 ;; ("u" . combobulate-navigate-up) ; up into list
;; 	 ;; ("v" . )
;; 	 ;; ("w" . )
;; 	 ;; ("x" . )
;; 	 ;; ("y" . )
;; 	 ;; ("z" . )
;; 	 ;; ("]" . puni-slurp-forward)
;; 	 ;; ("[" . puni-slurp-backward)
;; 	 ;; ("}" . puni-barf-forward)
;; 	 ;; ("{" . puni-barf-backward)

;;       ;; TODO: replace-map
;;       ;; TODO: embrace-map (a -> embrace-add, c -> embrace-change, d -> embrace-delete, i -> change-inner, o -> change-outer)
;;       ;; TODO: lasgun-map (t -> lasgun-mark-char-timer, SPC -> lasgun-make-multiple-cursors, x? -> lasgun-clear-lasgun-mark-ring, u? -> lasgun-pop-lasgun-mark)
;;       ;; TODO: tempel (tempel-insert, tempel-expand)
;;       ;; TODO: fold

  ;; 	 :map ctl-x-map)
  )

;; TODO: expand-region or make everything work with expreg
(use-package expreg
  :ensure t
  :bind (("C-'" . expreg-expand)
	 ("C-S-'" . expreg-contract)))

(use-package multiple-cursors
  :ensure t
  :disabled t
  :bind-keymap ("C-;" . mc/mark-map)
  :bind (("M-n" . mc/mark-next-like-this-symbol)
	 ("M-p" . mc/mark-previous-like-this-symbol)
	 :map mc/mark-map
	 ("." . mc/mark-all-dwim)
	 ("l" . mc/edit-lines))
  :init
  (define-prefix-command 'mc/mark-map))

(use-package macrursors
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

;; TODO: eventually improve surround to be more like nvim-surround and embrace.el
;; treesitter-backed semantic units
;; highlight semantic units
;; which-key help popups
;; change-inner/outer
(use-package surround
  :ensure t
  :bind-keymap ("M-c" . surround-keymap)
  :bind ( :map surround-keymap
	  ("a" . surround-insert)
	  ("c" . surround-change)
	  ("d" . surround-delete)))

;; uses expand-region
(use-package change-inner
  :ensure t
  :bind ( :map surround-keymap
	  ("i" . change-inner)
	  ("o" . change-outer)))

;; TODO: remove embrace
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

(use-package elec-pair
  :init (electric-pair-mode))

(use-package undo-fu-session
  :ensure t
  :custom (undo-fu-session-directory (var "undo-fu-session/"))
  :init (undo-fu-session-global-mode))

(use-package newcomment
  :bind ("M-;" . comment-line)
  :hook ((prog-mode . (lambda ()
			(set (make-local-variable
			      'comment-auto-fill-only-comments)
			     t)))))

;; vundo (or undo-tree) https://github.com/casouri/vundo
;; puni https://github.com/AmaiKinono/puni
;; iedit? https://www.masteringemacs.org/article/iedit-interactive-multi-occurrence-editing-in-your-buffer
;; ts-docstr https://github.com/emacs-vs/ts-docstr

;;;; navigation

;; TODO: xref
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

(use-package setup-dangerous-ctl-keys
  :init (add-hook 'after-make-frame-functions #'my/setup-dangerous-ctl-keys))

(use-package better-jumper
  :ensure t
  :bind (("H-i" . better-jumper-jump-forward)
	 ("C-o" . better-jumper-jump-backward)
	 ([remap xref-go-back] . better-jumper-jump-backward)
	 ([remap xref-go-forward] . better-jumper-jump-forward))
  :custom (better-jumper-add-jump-behavior 'replace)
  :init (better-jumper-mode))

(use-package better-jumper-extras
  :init
  (advice-add #'xref-find-definitions :around #'my/set-jump-maybe-a)
  (advice-add #'xref-go-back :around #'my/set-jump-maybe-a)
  (advice-add #'consult-imenu :around #'my/set-jump-maybe-a)
  (advice-add #'consult-line :around #'my/set-jump-maybe-a)
  (advice-add #'find-file :around #'my/set-jump-maybe-a))

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

;; TODO: evil-matchit https://github.com/redguardtoo/evil-matchit

;; TODO: beginend https://github.com/DamienCassou/beginend

;; TODO: can avy + embark replace link-hint?
(use-package link-hint
  :ensure t
  :bind
  (:map mode-specific-map
	("l" . link-hint-open-link)))

;; harpoon https://github.com/kofm/harpoon.el

(use-package smart-tab-over
  :config (smart-tab-over-global-mode))

;;;; modeline

;; TODO: replace doom-modeline with mood-line? or a custom builtin one?
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

;; keycast?

;;;; completion

(use-package minibuffer
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
  (minibuffer-electric-default-mode)
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

(use-package savehist
  :init (savehist-mode)
  :custom
  (savehist-file (var "savehist.el"))
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 300)
  (savehist-ignored-variables '(file-name-history))
  (savehist-additional-variables '(kill-ring register-alist
				   mark-ring global-mark-ring
				   search-ring regexp-search-ring))
  (history-length 1000)
  (history-delete-duplicates t)
  :config
  (defun ev/savehist-unpropertize-variables-h ()
    "Remove text properties from `kill-ring' to reduce savehist cache size."
    (setq kill-ring
	  (mapcar #'substring-no-properties
		  (cl-remove-if-not #'stringp kill-ring))
	  register-alist
	  (cl-loop for (reg . item) in register-alist
		   if (stringp item)
		   collect (cons reg (substring-no-properties item))
		   else collect (cons reg item))))

  (defun ev/savehist-remove-unprintable-registers-h ()
    "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwriteable tidbits."
    ;; Save new value in the temp buffer savehist is running
    ;; `savehist-save-hook' in. We don't want to actually remove the
    ;; unserializable registers in the current session!
    (setq-local register-alist
		(cl-remove-if-not #'savehist-printable register-alist)))

  (add-hook 'savehist-save-hook 'ev/savehist-unpropertize-variables-h)
  (add-hook 'savehist-save-hook 'ev/savehist-remove-unprintable-registers-h))

(use-package orderless
  :ensure t
  :config
  (defun prefixes-for-separators (pattern _index _total)
    (when (string-match-p "^[^][^\\+*]*[./-][^][\\+*$]*$" pattern)
      (cons 'orderless-prefixes pattern)))
  (cl-pushnew '(?` . orderless-regexp) orderless-affix-dispatch-alist)
  :custom
  (orderless-style-dispatchers
   '(orderless-affix-dispatch prefixes-for-separators)))

(use-package vertico
  :ensure t
  ;; TODO: vertico binds from karthink and oantolin
  :custom
  (vertico-count 10 "Items displayed, defaults to 10")
  (vertico-cycle t)
  (vertico-resize 'grow-only)
  :init (vertico-mode)
  :config (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word)
	      ("C-w" . vertico-directory-delete-word)
	      ("RET" . vertico-directory-enter))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; TODO: vertico-multiform from karthink and oantolin
(use-package vertico-multiform
  :after vertico
  :hook (vertico-mode . vertico-multiform-mode)
  :custom (vertico-multiform-commands
	   '((Info-menu (vertico-sort-function . nil)))))

;; from karthink or oantolin
;; TODO: vertico-unobtrusive
;; TODO: vertico-grid
;; TODO: vertico-quick
;; TODO: vertico-repeat
;; TODO: vertico-suspend
;; TODO: vertico-reverse
;; TODO: vertico-flat
;; TODO: vertico-buffer

;; TODO: corfu
;; TODO: cape extras

(use-package consult
  :ensure t
  :hook
  ((embark-collect-mode completion-list-mode) . consult-preview-at-point-mode)
  :bind (("M-y" . consult-yank-pop)
	 :map ctl-x-map
	 ("b" . consult-buffer)
	 ;; ("C-q" . consult-kmacro)
	 :map search-map
	 ("f" . consult-find)
	 ;; ("M-d" . consult-find)
	 ;; ("d" . consult-locate)
	 ;; ("M-e" . consult-isearch-history)
	 ("g" . consult-grep)
	 ("M-g" . consult-git-grep)
	 ("i" . consult-info)
	 ;; ("M-k" . consult-keep-lines)
	 ;; ("M-m" . consult-minor-mode-menu)
	 ("r" . consult-ripgrep)
	 ("M-r" . consult-recent-file)
	 ;; ("M-u" . consult-focus-lines)
	 ;; ("M-x" . consult-mode-command)
	 ;; ("M-/" . consult-ripgrep)
	 ;; ("M-;" . consult-complex-command)
	 :map goto-map
	 ("a" . consult-org-agenda)
	 ("e" . consult-flymake)
	 ("M-e" . consult-compile-error)
	 ("M-g" . consult-goto-line)
	 ("l" . consult-line)
	 ("M-l" . consult-line-multi)
	 ("o" . consult-outline)
	 ;; ("M-k" . consult-bookmark)
	 ;; :map project-prefix-map
	 ;; ("b" . consult-project-buffer)
	 ;; :map isearch-mode-map
	 ;; ("M-r" . consult-isearch-history)
	 ;; ("M-g l" . consult-line)
	 ;; :map minibuffer-local-map
	 ;; ("M-r" . consult-history)
	 :map help-map
	 ("TAB" . consult-info)
	 )
  :custom
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
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

  (defun my/consult-shell-command ()
    (interactive)
    (let* ((candidates (split-string
			(shell-command-to-string "compgen -c")
			"\n"
			t))
	   (command (consult--read
		     candidates
		     :prompt "Shell command: ")))
      (start-process-shell-command command nil command)))

  (defun my/consult-buffer-by-prefix (prefix caller show-preview)
    "Use consult to select a buffer prefixed by PREFIX#.

Show buffer previews if SHOW-PREVIEW is not nil."
    (let* ((consult--customize-alist
            (if show-preview
                (remove (list caller :preview-key nil) consult--customize-alist)
              consult--customize-alist))
           (my/consult--source-buffer-prefixed
            `(:name ,(format "Buffers (%s)" prefix)
              :category buffer
              :face consult-buffer
              :history buffer-name-history
              :state ,#'consult--buffer-state
              :default t
              :items
              ,(lambda ()
                 (consult--buffer-query
                  :sort 'visibility
                  :include (concat "^" prefix "#")
                  :as #'buffer-name))))
           (consult-buffer-sources (list my/consult--source-buffer-prefixed)))
      (consult-buffer)))
  (defun my/consult-buffer-icecat (arg)
    "Use consult to select an Icecat buffer."
    (interactive "P")
    (my/consult-buffer-by-prefix "I" this-command arg))
  (defun my/consult-buffer-ansi-term (arg)
    "Use consult to select an ansi-term buffer."
    (interactive "P")
    (my/consult-buffer-by-prefix "U" this-command arg))
  (defun my/consult-buffer-detached-command (arg)
    "Use consult to select a compilation buffer."
    (interactive "P")
    (my/consult-buffer-by-prefix "D" this-command arg))
  :config
  ;; Don't preview Icecat buffers
  (consult-customize my/consult-buffer-icecat :preview-key nil)
  ;; Hide recent files list (still available with "f" prefix)
  (consult-customize consult--source-recent-file :hidden t)
  ;; Replace functions (consult-multi-occur-test is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur))

(use-package consult-extras
  :bind ( :map goto-map
	  ("i" . my/consult-imenu-all)
	  ("m" . my/consult-mark-all)
	  ))

;; TODO: consult-dir
;; TODO: embark
;; TODO: embark-consult

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
	      ("M-a" . marginalia-cycle))
  :custom (marginalia-max-relative-age 0)
  :init (marginalia-mode)
  :config
  (advice-add 'marginalia--buffer-file :filter-return
	       (lambda (buffer-file)
		 (string-trim-left buffer-file "(compilation\\(<.+>\\)? run) "))))

;;;; search

;; isearch

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

;; grep
;; wgrep https://github.com/mhayashi1120/Emacs-wgrep
;; https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L4039
;; cc-isearch-menu

;;;; replace

(use-package replace
  :bind (("M-s M-o" . multi-occur)))

;; TODO: replace-extras
;; my/search-occur-urls

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

;;;; dired
;; diredfl
;; dired-single
;; dired-imenu
;; dired-sort-by
;; wdired
;; trashed
;; dired-ranger
;; `dired-ranger-copy' and `dired-ranger-paste' and `dired-ranger-move'
;; dired-narrow? to replace my/dired-limit-regexp?
;; `dired-narrow-regexp'
;; dired-subtree
;; dired-preview
;; dired-open vs dired-launch https://codeberg.org/thomp/dired-launch
;; dired-atool?
;; dired-rsync?
;; dired-gitignore?
;; sudo-edit?
;; dired-hide-dotfiles
;; dired-filter? what filters would i use?
;; dired-du
;; casual-dired https://github.com/kickingvegas/casual-dired

;;;; ibuffer
;; ibuffer https://www.reddit.com/r/emacs/s/Ft0yZxEMVD
;; ibuffer-git https://github.com/jrockway/ibuffer-git
;; projection-ibuffer?
;; bufler?

;;;; imenu
;; imenu https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L1593
(use-package imenu
  :hook
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry))

;; symbols-outline https://github.com/liushihao456/symbols-outline.el
;; imenu-list https://github.com/bmag/imenu-list

;;;; bookmark
;; bookmark https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L3320
;; blist? https://github.com/emacsmirror/blist
;; bookmark-web https://github.com/AuPath/bookmark-web

;;;; window

(use-package window
  :bind (("M-o" . other-window)
	 :map ctl-x-map
	 ;; ("C-w" . my/window-map)
	 ("C-c" . my/delete-window-or-delete-frame)
	 ("C-k" . my/kill-this-buffer))
  :hook
  ((epa-info-mode help-mode custom-mode) . visual-line-mode)
  :custom
  (window-combination-resize t)
  (even-window-sizes 'height-only)
  (window-sides-vertical nil)
  (switch-to-buffer-in-dedicated-window 'pop)
  (split-height-threshold 80)
  (split-width-threshold 125)
  (window-min-height 3)
  (window-min-width 30)
  (indicate-buffer-boundaries t)
  (indicate-empty-lines nil)
  (auto-window-vscroll nil)
  :preface
  (defun my/toggle-continuation-fringe-indicator ()
    "Toggle display of curly arrows when wrapping lines."
    (interactive)
    (setq-default
     fringe-indicator-alist
     (if (assq 'continuation fringe-indicator-alist)
	 (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist)
       (cons '(continuation right-curly-arrow left-curly-arrow) fringe-indicator-alist))))
  
  (defun my/delete-window-or-delete-frame (&optional window)
    "Delete WINDOW using `delete-window'.
If this is the sole window run `delete-frame' instead. WINDOW
must be a valid window and defaults to the selected one. Return
nil."
    (interactive)
    (condition-case nil
	(delete-window window)
      (error (if (and tab-bar-mode
                      (> (length (funcall tab-bar-tabs-function)) 1))
		 (tab-bar-cose-tab)
               (delete-frame)))))

  (defun my/kill-this-buffer (&optional arg)
    (interactive "P")
    (pcase arg
      ('4 (call-interactively #'kill-buffer))
      (_ (kill-buffer (current-buffer)))))
  :config
  (my/toggle-continuation-fringe-indicator))

;; frame
;; hl-line
;; whitespace
;; display-line-numbers
;; centered-cursor?
;; edwina https://github.com/ajgrf/edwina
;; popper

;;;; buffer
;; uniquify
;; buffer-move https://github.com/lukhas/buffer-move
;; epithet https://github.com/oantolin/epithet

;;;; vc
;; magit
(use-package magit
  :bind (:map ctl-x-map
	      ("C-v" . magit-status)))

;; diff-hl or gitgutter
;; consult-git-log-grep https://github.com/ghosty141/consult-git-log-grep
;; git-commit-ts-mode https://github.com/danilshvalov/git-commit-ts-mode

;;;; project
;; TODO: project
(use-package project
  :bind ( :map goto-map
	  ("f" . project-find-file)
	  ("p" . project-switch-project))
  :custom
  (project-list-file (file-name-concat user-cache-directory "var/projects")))

(use-package project-extras
  :bind ( :map project-prefix-map
	  ("DEL" . my/project-remove-project)))

;; projel
;; projection
;; projection-multi
;; projection-multi-embark
;; envrc
;; inheritenv
;; exec-path-from-shell
;; buffer-env https://github.com/astoff/buffer-env

;;;; compile

;; compile
(use-package compile
  :config
  (add-hook 'next-error-hook #'pulsar-pulse-line-red)
  (add-hook 'next-error-hook #'pulsar-recenter-top)
  (add-hook 'next-error-hook #'pulsar-reveal-entry))

;; compile-multi
;; consult-compile-multi
;; compile-multi-embark

;;;; help
;; help
;; helpful
;; helpful embark
;; eldoc
;; info https://github.com/oantolin/emacs-config/blob/696641a592691737ba5a019c67f2af7a6cc09183/init.el#L235-L239
;; info-colors
;; info-variable-pitch
;; transient
;; casual-avy https://github.com/kickingvegas/casual-avy
;; casual-isearch https://github.com/kickingvegas/casual-isearch
;; casual-suite? or make my own transients?

;; repeat
(use-package repeat
  ;; :disabled t
  :init (repeat-mode)
  :custom (repeat-keep-prefix t))

(use-package repeat-help
  :disabled t
  :ensure t
  :hook (repeat-mode . repeat-help-mode)
  :custom
  (repeat-help-popup-type 'embark))

;;;; org
;; book-mode https://github.com/rougier/book-mode
;; org-inline-tags
;; org-timeblock https://github.com/ichernyshovv/org-timeblock

;;;; eglot
;; eglot
(use-package eglot
  :custom (eglot-extend-to-xref t))
;; consult-eglot
;; emacs-lsp-booster-nix
;; eglot-booster https://github.com/jdtsmith/egloot-booster

;;;; ctags
;; citre https://github.com/universal-ctags/citre

;;;; flymake
;; flymake
;; flymake-collection

;;;; apheleia
;; apheleia
;; apheleia-use-package https://github.com/VojtechStep/apheleia-use-package.el
;; apheleia-eglot https://github.com/mohkale/apheleia-lsp

;;;; tree-sitter
;; treesit
;; treesit-auto
;; treesitter-context https://github.com/zbelial/treesitter-context.el

;;;; langs
;; elisp
;; elisp-fontification
;; elisp-indentation
;; eros
;; elisp-demos
;; highlight-quoted
;; highlight-numbers

;; nix
(use-package nix-mode
  :ensure t)
;; nix3.el https://github.com/emacs-twist/nix3.el

;; rust

;; go

;; python

;; markdown-mode
;; grip-mode https://github.com/seagle0128/grip-mode

;; docker
;; dockerfile-mode
;; flymake-hadolint
;; docker-compose-mode

;;;; snippets
;; tempel
;; eglot-tempel
;; tempel-collection
;; auto-activating-snippets

;;;; debug
;; dape
;; edebug

;;;; term
;; vterm or eat
;; comint
;; terminal-here?
;; isend-mode https://github.com/ffevotte/isend-mode.el
;; eshell-visual-vterm https://github.com/accelbread/eshell-visual-vterm
;; comint-fold https://github.com/jdtsmith/comint-fold

;;;; eshell
;; eshell
;; eshell-syntax-highlighting
;; eshell-bookmark
;; fish-completion?
;; bash-completion?
;; capf-autosuggest?

;;;; tramp
;; tramp
;; docker-tramp
;; kele https://github.com/jinnovation/kele.el
;; devcontainer https://github.com/bradschartz/devcontainer.el
;; emacs-dev-containers https://github.com/alexispurlane/emacs-dev-containers

;;;; biblio
;; citar
;; citar-embark
;; citar-denote

;;;; notes
;; denote
;; consult-denote
;; denote-explore
;; org-noter https://github.com/weirdNox/org-noter
;; annotate https://github.com/bastibe/annotate.el

;;;; reading
;; pdf-tools
;; saveplace-pdf-view
;; nov
;; wombag
;; elfeed
;; phundrak config elfeed

;;;; icons

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

;;;; secrets
;; pass.el or passage.el https://github.com/anticomputer/passage.el
;; age.el https://github.com/anticomputer/age.el
;; sops https://github.com/djgoku/sops
;; pinentry

;;;; email
;; notmuch

;;;; web-search
;; consult-omni https://github.com/armindarvish/consult-omni
;; engine-mode https://github.com/hrs/engine-mode

;;;; mpv
;; org-mpv-notes
;; ready-player https://github.com/xenodium/ready-player

;;;; music

;;;; storage
;; dropbox https://github.com/lorniu/emacs-dropbox

;;;; workspaces
;; activities

;;;; ai

;;;; extras
;; daemons https://github.com/cbowdon/daemons.el
;; fcitx.el https://github.com/cute-jumper/fcitx.el
;; casual-calc https://github.com/kickingvegas/casual-calc
;; github-linguist.el https://github.com/akirak/github-linguist.el
;; add-all-to-list
