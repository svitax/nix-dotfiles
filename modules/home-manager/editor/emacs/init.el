;;; init.el --- This is my init.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is where my Emacs config starts.

;;; Code:

;;;;;;;;;;;;;;;;;
;;;; startup ;;;;

(use-package startup
  :no-require
  :init (setopt inhibit-startup-screen t
		initial-major-mode 'fundamental-mode
		initial-scratch-message nil))

;;;;;;;;;;;;;;
;;;; lisp ;;;;

;; Additional load paths.
(eval-when-compile
  ;; "plugins/" contains downloaded packages or plugins I've written.
  (add-to-list 'load-path (concat user-emacs-directory "plugins"))
  ;; "lisp/" is configuration and glue code.
  (add-to-list 'load-path (concat user-emacs-directory "lisp")))

;;;;;;;;;;;;;;;;;;;;;
;;;; use-package ;;;;

;; use-package-enable-imenu-support must be set before requiring use-package
(use-package use-package
  :init
  (setopt use-package-enable-imenu-support t
	  ;; If you know your config works, this will make the byte-compiled file as minimal as possible in exchange for harder use-package debugging sessions.
	  use-package-expand-minimally t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; :xdg-cache and :xdg-state ;;;;

;; Many packages leave crumbs in `user-emacs-directory' or even
;; $HOME. `no-littering';; is a solution to this, but I want to adhere
;; to the XDG Base Directory Specification which `no-littering' has
;; eschewed.

;; We implement here two custom use-package keywords, `:xdg-state' and
;; `xdg-cache'. They give each package a directory of their own, per
;; the XDG specification, in which to store state or cache data.
(use-package use-package-xdg)

;; For example:
;; (use-package bookmark
;;   :xdg-state
;;   (bookmark-default-file "bookmarks.eld"))
;; 1. On load of 'bookmark', create an $XDG_STATE_HOME/emacs/bookmark directory if it doesn't already exist.
;; 2. Set ;; bookmark-default-file to ;; $XDG_STATE_HOME/emacs/bookmark/bookmark.eld.

;;;;;;;;;;;;;;
;;; :bind ;;;;

;; I want to bind many commands to keys in many keymaps, multiple
;; times. With `bind.el', I can throw a bunch of variable-value pairs
;; into it and not have to worry about wrapping each pair in
;; parentheses like `keymap-set' and friends. It also has support for
;; prefix, autoload, repeat-mode, and save&restore.
(use-package bind :ensure t)

;; The builtin `:bind' use-package keyword helps me have a package's
;; configuration tucked away neatly into a single `use-package'
;; block. It also makes it easy to navigate my init file with imenu.
;; Unfortunately, it also creates autoloads for all the commands.
;; I (almost) never want this because I don't care about my Emacs'
;; startup time (bless you, emacsclient) but I couldn't figure out how
;; to prevent the builtin :bind keyword from creating said
;; autoloads. So I replaced it with my custom implementation
;; leveraging `bind.el'
(use-package use-package-bind)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; :validate-custom ;;;;
;; https://github.com/xenodium/dotsies/blob/main/emacs/features/fe-package-extensions.el#L56

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE: :modify-syntax ;;;

;; use-package keyword to easily add characters like -, _, !, :, & as word
;; constituents, $ as paired delimiter, etc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE: :pretty-symbols ;;;

;;;;;;;;;;;;;;
;;;; meow ;;;;

;; NOTE: rectangle state https://github.com/Ziqi-Yang/.emacs.d/blob/
(use-package meow
  :ensure t
  :init
  ;; Forward C-d to H-d so we can rebind C- to page-down
  (global-set-key (kbd "H-d") 'delete-char)
  ;; (setq meow--kbd-delete-char "H-d")
  (setq meow--kbd-delete-char "<deletechar>")
  :config
  (setopt meow-use-clipboard t
	  meow-expand-hint-remove-delay 0
	  meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh
	  meow-cursor-type-region-cursor 'box
	  meow-use-cursor-position-hack t
	  ;; Disable keypad describe until we can use Embark prefix help
	  meow-keypad-describe-keymap-function nil
	  ;; delete-active-region t
	  )
  (use-package meow-extras)
  (bind (mode-specific-map
	 "," +compile-prefix-map
	 "b" +buffer-prefix-map
	 "e" +eval-prefix-map
	 "f" +file-prefix-map
	 "n" +notes-prefix-map
	 "p" +project-prefix-map
	 "q" +quit-prefix-map
	 "s" +search-prefix-map
	 ;; s; . repeat search?
	 ;; sr . vr/query-replace
	 "t" +toggle-prefix-map
	 ;; tg . git-gutter-mode
	 ;; tG . global-git-gutter-mode
	 ;; th . hl-line-mode
	 ;; tH . global-hl-line-mode
	 ;; tf . flymake-mode
	 ;; tn . display-line-numbers-mode
	 ;; tN . global-display-line-numbers-mode
	 ;; ti . imenu-list-smart-toggle
	 ;; tc . colorful-mode
	 ;; tC . global-colorful-mode
	 ;; tj . jinx-mode
	 ;; tJ . global-jinx-mode
	 ;; tt . consult-theme
	 ;; NOTE: "" +spelling-prefix-map
	 ;;  c . jinx-correct
	 ;;  l . jinx-languages
	 "u" #'meow-universal-argument
	 "v" +vc-prefix-map
	 "w" +window-prefix-map)
	('+match-prefix-map
	 "a" #'meow-bounds-of-thing
	 "i" #'meow-inner-of-thing))
  (meow-motion-overwrite-define-key
   ;; '("<escape>" . ignore)
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("a" . meow-prev)
   '("h" . meow-next)
   '("<escape>" . meow-simple-motion-mode))
  (meow-normal-define-key
   '("a" . meow-prev)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("C-d" . meow-page-down)
   '("e" . meow-right)
   '("f" . meow-find)
   '("g" . +goto-prefix-map)
   '("h" . meow-next)
   '("H" . +meow-join-line)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   ;; '("j" . meow-join)
   '("k" . meow-search)
   ;; '("k" . +meow-keep-selection)
   ;; '("K" . +meow-remove-selection)
   '("l" . meow-save)
   '("m" . +match-prefix-map)
   '("n" . meow-append)
   '("N" . meow-open-below)
   '("o" . meow-reverse)
   '("O" . meow-pop-selection)
   '("p" . yank)
   '("P" . yank-pop)
   '("q" . meow-quit)
   '("r" . +meow-replace)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . undo-redo)
   '("C-u" . meow-page-up)
   '("v" . meow-expand-mode)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-line)
   ;; '("X" . +meow-extend-to-line-end) ;; NOTE: extend selection to line end
   '("y" . meow-left)
   ;; '("z" . +view-prefix-map)
   ;; '("Z" . +sticky-view-prefix-map)
   '("<escape>" . meow-cancel-selection)
   '("<" . indent-rigidly-left) ;; TODO: if no selection, indent line
   '(">" . indent-rigidly-right)
   ;; '("'" . +register-prefix-map)
   '("-" . negative-argument)
   '(";" . repeat)
   ;; '("`" . +toggle-case-dwiam) ;; NOTE: nt-toggle-case-dwiam from nyaatouch
   ;; '("+" . +add-number) ;; NOTE: nt-add-number from nyaatouch
   ;; '("_" . +subtract-number) ;; NOTE: nt-subtract-one from nyaatouch
   ;; NOTE: find how to integrate the following commands
   ;; '("." . repeat-last-change) NOTE: (helix)
   ;; NOTE: '(">" . my/meow-next-thing)
   ;; >x . goto next diagnostic
   ;; >X . goto last diagnostic
   ;; NOTE: '("<" . my/meow-previous-thing)
   ;; <x . goto prev diagnostic
   ;; <X . goto first diagnostic
   )
  (meow-thing-register 'angle '(regexp "<" ">") '(regexp "<" ">"))
  (add-to-list 'meow-char-thing-table '(?A . angle))
  ;; start in insert
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (add-to-list 'meow-mode-state-list '(eshell-mode . insert))
  (add-to-list 'meow-mode-state-list '(git-commit-mode . insert))
  ;; simple motion
  (add-to-list 'meow-mode-state-list '(magit-status-mode . simple-motion))
  (add-to-list 'meow-mode-state-list '(dired-mode . simple-motion))
  (add-to-list 'meow-mode-state-list '(ediff-mode . simple-motion))
  (meow-global-mode 1))

(use-package meow-tree-sitter
  :ensure t
  :config (meow-tree-sitter-register-defaults))

;; NOTE: meow-paren-mode (integrate into special state) https://github.com/lilactown/kitten/blob/main/modules/kitten-lisp.el

;; NOTE: meow-vterm https://github.com/45mg/meow-vterm

;;;;;;;;;;;;;;;
;;;; faces ;;;;

(use-package fontaine
  :ensure t
  :xdg-state (fontaine-latest-state-file "fontaine-latest-state.eld")
  :init
  (setopt x-underline-at-descent-line nil
	  text-scale-remap-header-line t
	  fontaine-presets '((regular)
			     (t
			      :default-family "Iosevka Comfy"
			      ;; font height is 1/10pt.
			      :default-height 160)))
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1))

(use-package modus-themes
  :ensure t
  :init
  (setopt modus-themes-common-palette-overrides `((fringe unspecified)
						  (fg-line-number-active fg-main)
						  (bg-line-number-inactive unspecified)
						  (bg-line-number-active unspecified)
						  (bg-region bg-sage)
						  (fg-region unspecified)
						  (date-common cyan)
						  (date-deadline red-warmer)
						  (date-event magenta-warmer)
						  (date-holiday blue)
						  (date-scheduled magenta-cooler)
						  (date-weekday cyan-cooler)
						  (date-weekend blue-faint)
						  (mail-recipient fg-main)))
  (setopt modus-operandi-palette-overrides `((bg-mode-line-active bg-blue-intense)
					     (fg-mode-line-active fg-main)
					     (fg-heading-1 "#a01f64")
					     (fg-heading-2 "#2f5f9f")
					     (fg-heading-3 "#1a8388")))
  (setopt modus-vivendi-palette-overrides `((fg-main "#d6d6d4")
					    (bg-region bg-lavender)
					    (bg-main "#090909")
					    (fg-heading-1 magenta-faint)
					    (bg-mode-line-active bg-lavender)
					    (fg-mode-line-active "#ffffff")))
  (setopt modus-themes-bold-constructs t
	  modus-themes-variable-pitch-ui nil)
  :config
  (use-package themes-extras)
  (add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces)
  (add-hook 'ef-themes-post-load-hook #'my/ef-themes-custom-faces)
  (modus-themes-select 'modus-operandi))

(use-package pulsar
  :ensure t
  :config
  (pulsar-global-mode 1)
  (dolist (func '(xref-find-definitions
		  xref-go-back
		  xref-go-forward
		  meow-page-up
		  meow-page-down
		  beginning-of-buffer
		  end-of-buffer
		  recenter))
    (add-to-list 'pulsar-pulse-functions func)))

;; NOTE: pulsic https://github.com/ichernyshovvv/pulsic.el

(use-package whitespace
  :init
  ;; NOTE: whitespace-mode: show tabs and maybe newlines
  ;; `whitespace-mode' highlights each space/tab/newline with a face. In a small file, that may not matter but in larger files it is crippling.
  ;; Change it to use the display table where it substitutes other characters for spaces instead of using faces.
  (setopt whitespace-style '(space-mark tab-mark)))

;; NOTE: :hide-whitespace
;; NOTE: paren-face https://github.com/tarsius/paren-face

;;;;;;;;;;;;;;;;;;
;;;; modeline ;;;;

;; TODO: mode line
;; moody? minions? prots modeline? karthink? diminish?
;; blackout https://github.com/radian-software/blackout

(use-package simple
  :config
  (column-number-mode))

;;;;;;;;;;;;;;;;
;;;; frames ;;;;

(use-package frame
  :init
  (setopt
   ;; Consistent window title
   frame-title-format '("Emacs")
   ;; Hide the cursor in inactive windows.
   cursor-in-non-selected-windows nil
   ;; Avoid native dialogs
   use-dialog-box nil))

(use-package quit
  :no-require
  :ensure nil
  :bind (+quit-prefix-map
	 "q" #'save-buffers-kill-terminal
	 "r" #'restart-emacs
	 ;; NOTE: "k" +kill-all-emacsen (lambda emacs)
	 ))

(use-package display-line-numbers
  :init (global-display-line-numbers-mode))

(use-package virtual-auto-fill
  :ensure t
  :hook ((org-mode markdown-mode) . virtual-auto-fill-mode)
  :init (setopt fill-column 80))

;; NOTE: aggressive-fill-paragraph-mode https://github.com/davidshepherd7/aggressive-fill-paragraph-mode

;;;;;;;;;;;;;;;;
;;;; server ;;;;

(use-package server
  :init
  (setopt server-client-instructions nil))

;;;;;;;;;;;;;;;;
;;;; direnv ;;;;

(use-package envrc
  :ensure t
  :init (setopt envrc-show-summary-in-minibuffer nil)
  :config (envrc-global-mode))

;;;;;;;;;;;;;;;;;;
;;;; external ;;;;

;; NOTE: daemons https://github.com/cbowdon/daemons.el

;;;;;;;;;;;;;;;
;;;; files ;;;;

;; NOTE: find-file creates non-existent directories
(use-package files
  :init
  (setopt y-or-n-p-use-read-key t
	  use-short-answers t
	  confirm-kill-processes nil
	  confirm-kill-emacs 'yes-or-no-p)
  :config (bind +file-prefix-map "a" #'find-file))

(use-package persist-state
  :ensure t
  :config (persist-state-mode))

(use-package backup
  :no-require
  :ensure nil
  ;; Disable backup
  :init (setopt backup-inhibited t
		make-backup-files nil))

(use-package auto-save
  :no-require
  :ensure nil
  :xdg-state (auto-save-list-file-prefix "saves/")
  ;; Disable autosave
  :init (setopt auto-save-default nil))

(use-package lockfiles
  :no-require
  :ensure nil
  :config
  ;; Avoid creating lock files (ie. .#some-file.el)
  (setopt create-lockfiles nil))

;; NOTE: autorevert
;; (use-package autorevert
;;   :init
;;   (use-package auto-revert-extras)
;;   ;; Be quiet about reverts
;;   (setopt global-auto-revert-non-file-buffers t
;; 	  auto-revert-verbose nil)
;;   ;; global-auto-revert-mode can slow things down. try to enable it per active window.
;;   (add-to-list 'window-state-change-functions #'+window-state-state-change)
;;   :config (global-auto-revert-mode))

;; NOTE: recentf
;; (use-package recentf
;;   :xdg-state (recentf-save-file "recentf-save.el"))

(use-package saveplace
  :xdg-state (save-place-file "save-place.el")
  :config (save-place-mode))

;; NOTE: (setq savehist-additional-variables '(register-alist kill-ring))
;; NOTE: (make-variable-buffer-local 'register-alist) makes registers buffer-local, kind of like vim marks
(use-package savehist
  :xdg-state (savehist-file "savehist.el")
  :init (setopt history-length 1000
		history-delete-duplicates t)
  :config (savehist-mode))

;; NOTE: ffap
;; NOTE: sudo-edit https://github.com/nflath/sudo-edit

;;;;;;;;;;;;;;;;;;;;
;;;; minibuffer ;;;;

(use-package minibuffer
  :init
  (setopt enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode)
  :config
  (use-package minibuffer-extras)
  (advice-add #'completing-read-multiple :filter-args #'+crm-indicator)
  ;; Don't ignore cursor shape changes in minibuffer
  (delete (cons 'minibufferp 'meow--update-cursor-default)
	  meow-update-cursor-functions-alist)
  ;; Remove default minibuffer setup
  (remove-hook 'minibuffer-setup-hook 'meow--minibuffer-setup)
  ;; Use INSERT state in minibuffer by default
  (add-hook 'minibuffer-setup-hook 'meow-insert-mode))

;;;;;;;;;;;;;;;;;;;;
;;;; marginalia ;;;;

(use-package marginalia
  :ensure t
  :init (setopt marginalia-max-relative-age 0)
  :config (marginalia-mode))

;;;;;;;;;;;;;;;;;;;
;;;; orderless ;;;;

(use-package orderless
  :ensure t
  :config (setopt completion-styles '(orderless basic)))

;;;;;;;;;;;;;;;;;
;;;; vertico ;;;;

(use-package vertico
  :ensure t
  :init (setopt vertico-cycle t)
  :config (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (vertico-map
	 "DEL" #'vertico-directory-delete-char
	 "M-DEL" #'vertico-directory-delete-word
	 "RET" #'vertico-directory-enter)
  :config (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (mode-specific-map
	 ";" #'vertico-repeat)
  :config (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

;; NOTE: vertico-quick using C-s

;;;;;;;;;;;;;;;;
;;;; embark ;;;;

;; NOTE: embark

;;;;;;;;;;;;;;;;;
;;;; consult ;;;;

;; NOTE: consult-ripgrep-command "rg --null --ignore-case --type org -- line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"
(use-package consult
  :ensure t
  :config
  (bind (+buffer-prefix-map
	 "b" #'consult-buffer)
	(+search-prefix-map
	 "g" #'consult-ripgrep
	 "l" #'consult-outline
	 "s" #'consult-line)
	(+file-prefix-map
	 "i" #'consult-fd
	 "r" #'consult-recent-file))
  (with-eval-after-load 'pulsar
    (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
    (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)))

(use-package consult-imenu
  :bind (+search-prefix-map "i" #'consult-imenu))

(use-package consult-dir
  :ensure t
  :bind ((+file-prefix-map
	  "d" #'consult-dir)
	 (vertico-map
	  "C-x C-d" #'consult-dir
	  "C-x C-j" #'consult-dir-jump-file)))

;;;;;;;;;;;;;;;
;;;; corfu ;;;;

;; TODO: get rid of initial freeze when opening corfu for the first time
(use-package corfu
  :ensure t
  :bind (corfu-map
	 "M-SPC" #'corfu-insert-separator
	 "TAB" #'corfu-insert
	 "RET" nil
	 "C-h" #'corfu-info-documentation
	 "M-." #'corfu-info-location)
  :init
  (setopt corfu-auto nil
	  corfu-auto-prefix 2
	  corfu-auto-delay 0.0
	  corfu-count 10
	  corfu-cycle t
	  corfu-preview-current nil
	  corfu-preselect 'first
	  corfu-scroll-margin 5
	  ;; Enable indentation+completion using the TAB key.
	  tab-always-indent 'complete)
  :config
  (global-corfu-mode)
  (add-hook 'meow-insert-exit-hook 'corfu-quit))

(use-package corfu-popupinfo
  :after corfu
  :bind (corfu-map
	 "C-v" #'corfu-popupinfo-scroll-up
	 "M-v" #'corfu-popupinfo-scroll-down
	 "C-d" #'corfu-popupinfo-scroll-up
	 "C-u" #'corfu-popupinfo-scroll-down
	 [remap corfu-info-documentation] #'corfu-popupinfo-toggle)
  :init (setopt corfu-popupinfo-delay '(0.5 . 0.5)
		corfu-global-mode '((not eshell-mode)))
  :config (corfu-popupinfo-mode 1))

;; NOTE: corfu-quick
;; NOTE: corfu-history
;; NOTE: corfu for eshell
;; NOTE: corfu for vterm

;;;;;;;;;;;;;;;;;;;;;
;;;; :completions ;;;;

;; NOTE: :completions keyword for use-package forms

;;;;;;;;;;;;;;
;;;; cape ;;;;

;; NOTE: cape

;;;;;;;;;;;;;;;;;
;;;; project ;;;;

;; NOTE: what can i do with `project-extra-root-markers'
(use-package project
  :xdg-state (project-list-file "project-list.el")
  ;; :init
  ;; (setopt project-switch-commands '((?b "Buffer" project-switch-to-buffer)
  ;; 				    (?d "Dired" project-dired)
  ;; 				    (?e "Eshell" project-eshell)
  ;; 				    (?f "Find file" project-find-file)
  ;; 				    (?g "Find regexp" project-find-regexp)
  ;; 				    (?k "Kill buffers" project-kill-buffers)
  ;; 				    (?r "Query replace" project-query-replace-regexp)
  ;; 				    (?v "Magit" magit-project-status)
  ;; 				    (?! "Shell command" project-shell-command)))
  :bind ((+file-prefix-map
	  "f" #'project-find-file)
	 (+project-prefix-map
	  "b" #'project-switch-to-buffer
	  "d" #'project-dired
	  "e" #'project-eshell
	  "f" #'project-find-file
	  "g" #'project-find-regexp
	  "k" #'project-kill-buffers
	  "p" #'project-switch-project
	  "r" #'project-query-replace-regexp
	  "!" #'project-shell-command)))

;; NOTE: projection

;; NOTE: keep projects list always in sync with certain directories
;; snatch projel's rescan-directory and rescan-all-projects, put in project extras
;; projel-add-project-directory, projel--add-project, projecl-rescan-directory, projel-find-projects-in-dir, etc.

;;;;;;;;;;;;;;;;;
;;;; buffers ;;;;

(use-package buffer
  :no-require
  :ensure nil
  :config (bind ('+goto-prefix-map
		 "h" #'end-of-buffer
		 ;; "f" goto file
		 "g" #'beginning-of-buffer)
		(+buffer-prefix-map
		 "k" #'kill-current-buffer
		 ;; "K" #'crux-kill-other-buffers ;; NOTE: crux-kill-other-buffers
		 ;; "m" #'+move-buffer-file ;; NOTE: move buffer and file to DIR
		 ;; https://github.com/karthink/.emacs.d/blob/b0ae76c813b133619b2f29b179a7f1ab5193f534/lisp/better-buffers.el#L243-L257
		 ;; "r" #'+rename-file-and-buffer ;; NOTE: rename current buffer and file to NEW NAME
		 ;; https://github.com/karthink/.emacs.d/blob/b0ae76c813b133619b2f29b179a7f1ab5193f534/lisp/better-buffers.el#L225-L240
		 "s" #'save-buffer
		 )))

;; NOTE: epithet https://github.com/oantolin/epithet
;; NOTE: uniquify
;; NOTE: adviced:kill-buffer--possibly-save https://christiantietze.de/posts/2023/09/kill-unsaved-buffer-ux-action-labels and xenodium

;;;;;;;;;;;;;;;;;
;;;; ibuffer ;;;;

;; NOTE: ibuffer https://www.reddit.com/r/emacs/s/Ft0yZxEMVD
;; NOTE: ibuffer-git https://github.com/jrockway/ibuffer-git
;; NOTE: ibuffer-project? or ibuffer-git?
;; NOTE: projection-ibuffer?
;; NOTE: bufler?

;;;;;;;;;;;;;;;;;
;;;; windows ;;;;

;; NOTE: repeat map for enlarge and shrink window commands
(use-package window
  :no-require
  :ensure nil
  :config
  (use-package windows-extras)
  (bind ((:global-map)
	 "M-o" #'other-window
	 "C-v" #'+scroll-down-half-page
	 "M-v" #'+scroll-up-half-page)
	('+goto-prefix-map
	 "b" #'+meow-window-bottom
	 "c" #'+meow-window-middle
	 "t" #'+meow-window-top)
	(+window-prefix-map
	 ;; NOTE: "a" #'ace-window
	 ;; NOTE: "f" #'+toggle-window-split ;; toggle windows between horizontal and vertical (lambda emacs)
	 "k" #'delete-window
	 "o" #'other-window
	 ;; NOTE: "r" #'+rotate-windows ;; (lambda emacs)
	 ;; NOTE: "R" #'+rotate-windows-backward ;; (lambda emacs)
	 "s" #'split-window-below
	 ;; NOTE: "s" #'+split-window-below-and-focus ;; (lambda emacs)
	 "T" #'tear-off-window
	 "v" #'split-window-right
	 ;; NOTE: "v" #'+split-window-right-and-focus ;; (lambda emacs)
	 ;; NOTE: "w" #'ace-window
	 ;; NOTE: "x" #'+rotate-windows-backward ;; (lambda emacs)
	 "=" #'enlarge-window-horizontally
	 "-" #'shrink-window-horizontally
	 "}" #'enlarge-window
	 "{" #'shrink-window)))

(use-package popper
  :ensure t
  :bind ((:global-map)
	 "C-'" #'popper-toggle
	 "M-'" #'popper-cycle
	 "C-M-'" #'popper-toggle-type)
  :init
  (use-package popper-extras)
  (setopt popper-window-height (lambda (win)
				 (fit-window-to-buffer
				  win
				  (floor (frame-height) 3)
				  (floor (frame-height) 3)))
	  popper-reference-buffers (append +popper-reference-buffers '()))
  :config (popper-mode))

;; NOTE: switchy-window https://github.com/emacsmirror/switchy-window
;; NOTE: ace-window https://github.com/abo-abo/ace-window

;;;;;;;;;;;;;;
;;;; fold ;;;;

;;;;;;;;;;;;;;;;
;;;; narrow ;;;;

;; NOTE: "bn" #'narrow-to-region
;; NOTE: "bw" #'widen
;; TODO: logos https://github.com/protesilaos/logos

;;;;;;;;;;;;;;;;;
;;;; outline ;;;;

;; NOTE: outli https://github.com/jdtsmith/outli
;; NOTE: nbarrientos outline config
;; NOTE: outline-indent https://github.com/jamescherti/outline-indent.el

;;;;;;;;;;;;;;;
;;;; dired ;;;;

;; NOTE: dired-extras in dired use-package block
(use-package dired
  :config
  (bind (mode-specific-map
	 "d" #'dired-jump)
	(dired-mode-map
	 "b" #'dired-up-directory
	 "f" #'dired-find-file)))

;; TODO: dired-narrow
;; TODO: diredfl
;; TODO: dired-single
;; TODO: hide dotfiles and hide gitignored files (should i use dired-filter for this or copy from old-init.el)
;; NOTE: wdired
;; NOTE: dired-ranger (dired-ranger-copy and dired-ranger-paste and dired-ranger-move)
;; NOTE: dired-filter (what filters would i use? filter-by-videos from xenodium)
;; NOTE: dired-subtree
;; NOTE: dired-preview
;; NOTE: dired-git-info https://github.com/clemera/dired-git-info
;; NOTE: dired-open-with https://github.com/FrostyX/dired-open-with
;; NOTE: dired-rsync?
;; NOTE: dired-dy https://github.com/emacsmirror/dired-du

;;;;;;;;;;;;;;;;;;;;
;;;; navigation ;;;;

;; NOTE: avy
;; https://github.com/karthink/.emacs.d/blob/master/plugins/demo.el
;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-avy.el
;; https://karthinks.com/software/avy-can-do-anything
(use-package avy
  :ensure t
  :init
  (setopt avy-keys '(?n ?r ?t ?s ?h ?a ?e ?i) ;; Colemak-DH keyboard
	  avy-timeout-seconds 0.27
	  avy-single-candidate-jump nil)
  :config
  (meow-normal-define-key '("s" . avy-goto-char-timer)))

;; NOTE: lasgun https://github.com/aatmunbaxi/lasgun.el
;; "S" . lasgun
;; NOTE: forward and backward paragraph from xenodium
;; NOTE: beginend https://github.com/DamienCassou/beginend
;; NOTE: link-hint (can avy + embark replace link-hint?)
;; NOTE: tab-jump-out

;;;;;;;;;;;;;;;;
;;;; search ;;;;

;; NOTE: occur
;; NOTE: grep
;; NOTE: wgrep https://github.com/mhayashi1120/Emacs-wgrep
;; https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L4039
;; NOTE: consult-omni https://github.com/armindarvish/consult-omni
;; NOTE: docsim https://github.com/hrs/docsim.el

;;;;;;;;;;;;;;;;;
;;;; isearch ;;;;

(use-package isearch
  :bind (isearch-mode-map
	 "M-s s" #'consult-line
	 "C-SPC" #'+isearch-mark-and-exit
	 "DEL" #'+isearch-delete
	 "M->" #'isearch-end-of-buffer
	 "M-<" #'isearch-beginning-of-buffer)
  :init (setopt isearch-wrap-pause 'no-ding
		isearch-allow-scroll 'unlimited
		isearch-lazy-count t
		lazy-count-prefix-format "(%s/%s) "
		;; interpret space char as a wildcard
		search-whitespace-regexp ".*?"
		isearch-lax-whitespace t)
  :config
  (defun +isearch-delete ()
    "Delete the failed portion or last char if succesful search.

See also: https://emacs.stackexchange.com/a/10360/9198"
    (interactive)
    (if (= 0 (length isearch-string))
	(ding)
      (setq isearch-string (substring
			    isearch-string 0 (or (isearch-fail-pos) (1- (length isearch-string))))
	    isearch-message (mapconcat 'isearch-text-char-description isearch-string ""))
      (funcall (or isearch-message-function #'isearch-message) nil t)
      (if isearch-other-end (goto-char isearch-other-end))
      (isearch-search)
      (isearch-push-state)
      (isearch-update)))
  (defun +isearch-mark-and-exit ()
    "Mark the current search string and exit the search."
    (interactive)
    (push-mark isearch-other-end t 'activate)
    (setq deactivate-mark nil)
    (activate-mark)
    (isearch-done))
  (meow-normal-define-key '("*" . isearch-forward-thing-at-point)))

;;;;;;;;;;;;;;;
;;;; imenu ;;;;

;; NOTE: imenu https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L1593
;; NOTE: imenu-list https://github.com/bmag/imenu-list (alternative to symbols-outline) or eglot-hierarchy https://github.com/dolmens/eglot-hierarchy

;;;;;;;;;;;;;;;;;;;
;;;; bookmarks ;;;;

;; NOTE: bookmark https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L3320
;; NOTE: prot/bookmark-save-no-prompt
(use-package bookmark
  :xdg-state
  (bookmark-default-file "bookmarks.eld"))

;; TODO: gumshoe
;; TODO: harpoon https://github.com/otavioschwanck/harpoon.el or https://github.com/kofm/harpoon.el
;; TODO: fix C-m and C-i so they're separate from RET and TAB
;; NOTE: diverted https://github.com/xenodium/dotsies/blob/main/emacs/ar/diverted.el
;; NOTE: org-bookmark-heading https://github.com/alphapapa/org-bookmark-heading
;; NOTE: bookmark-frecency https://github.com/akirak/bookmark-frecency.el

;;;;;;;;;;;;;;;;;
;;;; editing ;;;;

(use-package text
  :no-require
  :ensure nil
  :config
  (bind '+goto-prefix-map
	"e" #'move-end-of-line
	"y" #'move-beginning-of-line))

(use-package mowie
  :ensure t
  :bind ((:global-map)
	 [remap move-beginning-of-line] #'+beginning-of-line
	 [remap move-end-of-line] #'+end-of-line)
  :config
  (defun +beginning-of-line ()
    (interactive "^")
    (mowie
     #'beginning-of-line
     #'beginning-of-visual-line
     #'mowie-beginning-of-code
     #'mowie-beginning-of-comment
     #'mowie-beginning-of-comment-text))
  (defun +end-of-line ()
    (interactive "^")
    (mowie
     #'end-of-line
     #'end-of-visual-line
     #'mowie-end-of-code)))

(use-package move-text
  :ensure t
  :bind ((:global-map)
	 "M-<up>" #'move-text-up
	 "M-<down>" #'move-text-down)
  :config
  ;; NOTE: add +indent-region-advice to move-text-extras?
  (defun +indent-region-advice (&rest ignored)
    "Re-indent the text in-and-around a text move."
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
	  (indent-region (region-beginning) (region-end))
	(indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  (advice-add 'move-text-up :after '+indent-region-advice)
  (advice-add 'move-text-down :after '+indent-region-advice))

;; NOTE: don't auto pair < in org-mode
(use-package elec-pair
  :init (electric-pair-mode))

(use-package delsel
  :config (delete-selection-mode +1))

;; NOTE: https://github.com/Kungsgeten/selected.el

(use-package smart-hungry-delete
  :ensure t
  :bind ((:global-map)
	 [remap backward-delete-char-untabify] #'smart-hungry-delete-backward-char
	 [remap delete-backward-char] #'smart-hungry-delete-backward-char
	 [remap delete-char] #'smart-hungry-delete-forward-char
	 [remap delete-forward-char] #'smart-hungry-delete-forward-char)
  :config (smart-hungry-delete-add-default-hooks))

;; NOTE: ws-butler
;; NOTE: subword
;; NOTE: interactive-align https://github.com/mkcms/interactive-align

(use-package expand-region
  :ensure t
  :init (setopt expand-region-fast-keys-enabled nil)
  :bind (meow-normal-state-keymap
	 "Y" #'er/expand-region
	 "E" #'er/contract-region))

;; NOTE: highlight surround.el semantic units
;; NOTE: surround.el treesitter-backed semantic units
;; NOTE: surround.el which-key or embark help popups
(use-package surround
  :ensure t
  :config
  (bind +match-prefix-map
	"s" #'surround-insert
	"r" #'surround-change
	"d" #'surround-delete))

;; TODO: macrursors
;; TODO: macrursors + avy
(use-package macrursors
  :bind (((:global-map)
	  "C-;" #'macrursors-mark-map
	  "M-n" #'macrursors-mark-next-instance-of
	  "M-e" #'macrursors-mark-previous-instance-of)
	 (meow-normal-state-keymap
	  "G" #'+macrursors-select)
	 (macrursors-mode-map
	  "C-'" #'macrursors-hideshow)
	 (macrursors-mark-map
	  "C-;" #'macrursors-end
	  "C-g" #'macrursors-early-quit
	  "n" #'macrursors-mark-next-line
	  "e" #'macrursors-mark-previous-line
	  "C-SPC" nil
	  "SPC" nil
	  "." #'macrursors-mark-all-instances-of
	  "w" #'macrursors-mark-all-words
	  "W" #'macrursors-mark-all-symbols
	  "x" #'macrursors-mark-all-lines)
	 (isearch-mode-map
	  "C-;" #'macrursors-mark-from-isearch
	  "M-n" #'macrursors-mark-next-from-isearch
	  "M-e" #'macrursors-mark-previous-from-isearch))
  :init (define-prefix-command 'macrursors-mark-map)
  :config
  (use-package macrursors-extras)
  (use-package macrursors-select)
  ;; (use-package macrursors-select-expand) ;; NOTE: currently in one of karthink's branches
  (dolist (mode '(corfu-mode +toggle-meow-during-macro))
    (add-hook 'macrursors-pre-finish-hook mode)
    (add-hook 'macrursors-post-finish-hook mode))
  (when (featurep 'meow)
    ;; (add-hook 'macrursors-mode-hook #'meow-insert)
    ;; Disable meow's beacon state because it conflict with macrursors
    (defun meow--maybe-toggle-beacon-state ())
    ;; (add-hook 'macrursors-mode-hook #'+toggle-meow-during-macro)
    ))

;;;;;;;;;;;;;;;;;;;
;;;; kill-ring ;;;;

;; NOTE: kill-ring
;; NOTE: clean-kill-ring https://github.com/NicholasBHubbard/clean-kill-ring.el

;;;;;;;;;;;;;;
;;;; undo ;;;;

;; NOTE: undo-fu-session

;;;;;;;;;;;;;;;;;
;;;; replace ;;;;

;; TODO: visual-replace
;; eventually replace visual-replace with query-replace-parallel and visual-regexp-steroids functionality
;; query-replace-parallel https://github.com/hokomo/query-replace-parallel

;;;;;;;;;;;;;;
;;;; help ;;;;

;; TODO: helpful

(use-package transient
  :xdg-state
  (transient-history-file "history.el")
  (transient-levels-file "levels.el")
  (transient-values-file "values.el"))

;;;;;;;;;;;;;;
;;;; info ;;;;

;; NOTE: info https://github.com/oantolin/emacs-config/blob/696641a592691737ba5a019c67f2af7a6cc09183/init.el#L235-L239
;; NOTE: info-colors
;; NOTE: info-variable-pitch https://github.com/kisaragi-hiu/info-variable-pitch

;;;;;;;;;;;;;;;;;
;;;; linting ;;;;

;; TODO: (:lint keyword) flymake-hook from flymake-collection  https://github.com/mohkale/flymake-collection
;; TODO: flymake
;; TODO: flymake-collection
;; TODO: flymake-margin https://github.com/LionyxML/flymake-margin

;;;;;;;;;;;;;;;;;;;;
;;;; formatting ;;;;

(use-package apheleia
  :ensure t
  :config
  (meow-normal-define-key
   '("=" . apheleia-format-buffer))
  (apheleia-global-mode))

;; Adds :format keyword for use-package forms
(use-package apheleia-use-package)

;;;;;;;;;;;;;
;;;; lsp ;;;;

;; NOTE: (:lsp keyword) eglot-server-programs and eglot-server-configuration?
;; TODO: eglot
;; TODO: eglot-booster
;; TODO: apheleia-eglot
;; NOTE: consult-eglot
;; NOTE: citre https://github.com/universal-ctags/citre

;;;;;;;;;;;;;
;;;; dap ;;;;

;; NOTE: (:dap keyword)
;; NOTE: dape

;;;;;;;;;;;;;;;;;;
;;;; snippets ;;;;

;; TODO: tempel https://github.com/minad/tempel
;; NOTE: eglot-tempel https://github.com/fejfighter/eglot-tempel
;; NOTE: tempel-collection https://github.com/Crandel/tempel-collection

;; NOTE: yasnippet-capf (if i decide to use yasnippet) https://github.com/elken/yasnippet-capf

;;;;;;;;;;;;;;;;;;;;
;;;; treesitter ;;;;

;; TODO: treesit
;; TODO: treesit-auto
;; NOTE: ts-docstr https://github.com/emacs-vs/ts-docstr

;;;;;;;;;;;;;;;;;
;;;; compile ;;;;

;; NOTE: :compile keyword (compile-multi)
;; TODO: compile
;; NOTE: compile-multi

;;;;;;;;;;;;;;
;;;; prog ;;;;

;; TODO: xref
(use-package xref
  :config
  (bind ((:global-map)
	 "C-t" #'xref-go-back
	 "M-t" #'xref-go-forward)
	('+goto-prefix-map
	 "d" #'xref-find-definitions
	 "r" #'xref-find-references))

  ;; TODO: this basically makes xref into a jumplist. do i want to keep xref jumps from regular jumps with gumshoe?
  (customize-set-variable 'xref-history-storage 'xref-window-local-history)
  (defun +xref-push-point-to-marker-stack (&rest r)
    (xref-push-marker-stack (point-marker)))
  (defun +xref-stack-current-position ()
    (interactive)
    (+xref-push-point-to-marker-stack))
  (dolist (func '(isearch-forward isearch-forward-regexp
		  isearch-backward isearch-backward-regexp))
    (advice-add func :before '+xref-push-point-to-marker-stack)))

(use-package consult-xref
  :init
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

;; TODO: eldoc
;; TODO: colorful-mode
;; NOTE: ct.el https://github.com/neeasade/ct.el

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config (setopt hl-todo-wrap-movement t))

(use-package consult-todo
  :ensure t
  :bind (+search-prefix-map
	 "T" #'consult-todo))

(use-package newcomment
  :bind ((:global-map)
	 "M-;" #'comment-line))

;; TODO: evil-matchit https://github.com/redguardtoo/evil-matchit
;; NOTE: treesitter-context https://github.com/zbelial/treesitter-context.el

;;;;;;;;;;;;;;;;;;;;;
;;;; scaffolding ;;;;

;; NOTE: prefab (for cookiecutter) https://github.com/LaurenceWarne/prefab.el

;;;;;;;;;;;;
;;;; vc ;;;;

;; TODO: magit
(use-package magit
  :ensure t
  :bind ((+vc-prefix-map
	  "s" #'magit-status)
	 (+project-prefix-map
	  "v" #'magit-project-status)))

(use-package git-modes :ensure t)

;; NOTE: consult-git-log-grep https://github.com/ghosty141/consult-git-log-grep
;; NOTE: consult-ls-git https://github.com/rcj/consult-ls-git
;; NOTE: git-commit-ts-mode https://github.com/danilshvalov/git-commit-ts-mode
;; NOTE: emsg-blame https://github.com/ISouthRain/emsg-blame
;; NOTE: consult-gh https://github.com/armindarvish/consult-gh
;; NOTE: conventional-commit https://github.com/akirak/conventional-commit.el

;;;;;;;;;;;;;;
;;;; diff ;;;;

;; TODO: git-gutter
;; NOTE: ediff

;;;;;;;;;;;;;;;;;;
;;;; terminal ;;;;

;; TODO: mistty https://github.com/szermatt/mistty or vterm
;; NOTE: eshell-visual-vterm https://github.com/accelbread/eshell-visual-vterm
;; NOTE: isend-mode https://github.com/ffevotte/isend-mode.el
;; NOTE: awscli-capf https://github.com/sebasmonia/awscli-capf
;; NOTE: vterm-capf https://github.com/twlz0ne/vterm-capf

;;;;;;;;;;;;;;;;
;;;; comint ;;;;

;; NOTE: comint
;; NOTE: comint-fold https://github.com/jdtsmith/comint-fold

;;;;;;;;;;;;;;;;
;;;; eshell ;;;;

;; NOTE: if mistty or vterm doesn't suit my needs, look into setting up eshell with all the bells and whistles (pcmpl, aliases, syntax highlighting, prompt, etc.)
;; NOTE: eshell
;; NOTE: karthink eshell buffer redirection
;; NOTE: karthink eshell atuin
(use-package eshell
  :xdg-state
  (eshell-aliases-file "aliases")
  (eshell-directory-name "")
  (eshell-login-script "login")
  (eshell-rc-script "rc"))

;;;;;;;;;;;;;;;
;;;; shell ;;;;

;; NOTE: dwim-shell-commad https://github.com/xenodium/dwim-shell-command

;;;;;;;;;;;;;;;
;;;; elisp ;;;;

(use-package elisp-fontification)

(use-package elisp-indentation)

(use-package eros
  :ensure t
  :hook (emacs-lisp-mode . eros-mode)
  :bind (+eval-prefix-map
	 "b" #'eval-buffer
	 "e" #'eval-last-sexp
	 ;; ec . lem-eval-current-form (lambda emacs)
	 "f" #'eval-defun
	 "r" #'eval-last-region)
  :init (add-hook 'eros-inspect-hooks (lambda () (flymake-mode -1))))

;; NOTE: emacs-inspector https://github.com/mmontone/emacs-inspector
;; NOTE: eros-inspector https://github.com/port19x/eros-inspector

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

;; NOTE: package-lint-flymake
;; NOTE: relint https://github.com/mattiase/relint

;;;;;;;;;;;;
;;;; go ;;;;

;;;;;;;;;;;;;;
;;;; rust ;;;;

;; NOTE: cargo-jump-xref https://github.com/eval-exec/cargo-jump-xref.el

;;;;;;;;;;;;;
;;;; nix ;;;;

(use-package nix-mode
  :ensure t
  :formatter
  (alejandra . ("alejandra")) nix-mode)

(use-package nix-ts-mode
  :ensure t
  :formatter (alejandra) nix-ts-mode)

;; NOTE: nix3.el https://github.com/emacs-twist/nix3.el

;;;;;;;;;;;;;;;;
;;;; python ;;;;

;;;;;;;;;;;;;;;;
;;;; scheme ;;;;

;; NOTE: https://github.com/port19x/geiser-overlay

;;;;;;;;;;;;;;;;;;;;;
;;;; common lisp ;;;;

;; NOTE: https://github.com/fosskers/sly-overlay

;;;;;;;;;;;;;;
;;;; json ;;;;

;; NOTE: counsel-jq (but with consult) https://github.com/200ok-ch/counsel-jq

;;;;;;;;;;;;;;
;;;; http ;;;;

;; NOTE: verb https://github.com/federicotdn/verb
;; NOTE: swagg https://github.com/isamert/swagg.el

;;;;;;;;;;;;;;;
;;;; tramp ;;;;

;; NOTE: tramp
;; NOTE: docker-tramp
(use-package tramp
  :xdg-state
  (tramp-auto-save-directory "auto-save/")
  (tramp-persistency-file-name "persistency.el"))

;;;;;;;;;;;;;;;;
;;;; docker ;;;;

;; NOTE: docker
;; NOTE: dockerfile-mode
;; NOTE: flymake-hadolint
;; NOTE: docker-compose-mode

;;;;;;;;;;;;;;;;;;;;
;;;; kubernetes ;;;;

;; NOTE: kele https://github.com/jinnovation/kele.el
;; NOTE: kubed https://github.com/eshelyaron/kubed

;;;;;;;;;;;;;;;;;;;;;;
;;;; devcontainer ;;;;

;; NOTE: devcontainer https://github.com/bradschartz/devcontainer.el
;; NOTE: emacs-dev-containers https://github.com/alexispurlane/emacs-dev-containers

;;;;;;;;;;;;;;;;;;
;;;; assembly ;;;;

;; NOTE: beardbolt https://github.com/joaotavora/beardbolt
;; NOTE: rmsbolt https://github.com/emacsmirror/rmsbolt

;;;;;;;;;;;;;;;
;;;; sxhkd ;;;;

;; NOTE: sxhkd-mode https://github.com/xFA25E/sxhkd-mode
;; NOTE: make sxhkd-mode work in swhkd

;;;;;;;;;;;;;;;;;;
;;;; markdown ;;;;

;; TODO: markdown mode
;; NOTE: grip-mode https://github.com/seagle0128/grip-mode

;;;;;;;;;;;;;
;;;; org ;;;;

(use-package org
  :bind (org-mode-map
	 "C-'" #'popper-toggle)
  :init
  (setopt org-startup-folded 'content))

;; TODO: book-mode https://github.com/rougier/book-mode or org-modern
;; TODO: org-appear https://github.com/awth13/org-appear (needed after org-modern??)
;; NOTE: org-modern-indent https://github.com/jdtsmith/org-modern-indent
;; TODO: corg https://github.com/isamert/corg.el
;; TODO: org-gtd (tasks) https://github.com/Trevoke/org-gtd.el
;; NOTE: org-transclusion-http https://github.com/alphapapa/org-transclusion-http
;; NOTE: org-inline-tags https://github.com/incandescentman/org-inline-tags
;; NOTE: org-super-links https://github.com/toshism/org-super-links
;; NOTE: org-recur https://github.com/mrcnski/org-recur
;; NOTE: org-tidy https://github.com/jxq0/org-tidy
;; NOTE: org-mind-map https://github.com/the-ted/org-mind-map

;;;;;;;;;;;;;;;;;;;;
;;;; org-agenda ;;;;

;; TODO: org-super-agenda https://github.com/alphapapa/org-super-agenda
;; NOTE: org-caldav https://github.com/dengste/org-caldav
;; NOTE: org-gcal https://github.com/myuhe/org-gcal.el
;; NOTE: org-timeblock https://github.com/ichernyshovvv/org-timeblock

;;;;;;;;;;;;;;;;;;
;;;; spelling ;;;;

;; TODO: jinx
;; NOTE: flymake-vale https://github.com/tpeacock19/flymake-vale

;;;;;;;;;;;;;;;;
;;;; biblio ;;;;

;; NOTE: citar

(use-package citar
  :ensure t
  ;; :commands citar--bibliography-files
  :bind ((+notes-prefix-map
	  "b" +bibliography-prefix-map)
	 (+bibliography-prefix-map
	  "b" #'citar-open))
  :init
  (setopt
   citar-select-multiple nil
   citar-bibliography '("~/OneDrive/docs/lib.bib")
   citar-library-paths '("~/OneDrive/docs/books")
   citar-templates
   '((main . "${title:55} ${author editor:55} ${date year issued:4}")
     (suffix . "  ${tags keywords keywords:40}")
     (preview . "${author editor} ${title}, ${journal publisher container-title collection-title booktitle} ${volume} (${year issued date}).\n")
     (note . "#+title: Notes on ${author editor}, ${title}"))))

(use-package persid)

;; NOTE: biblio
;; NOTE: create biblio-persid if sufficient
;; NOTE: biblio-openlibrary https://github.com/fabcontigiani/biblio-openlibrary
;; NOTE: biblio-zotero https://github.com/gkowzan/biblio-zotero
;; NOTE: biblio-bibsonomy https://github.com/andreasjansson/biblio-bibsonomy.el
;; NOTE: biblio-gscholar https://github.com/seanfarley/biblio-gscholar.el

;;;;;;;;;;;;;;;
;;;; notes ;;;;

(use-package denote
  :ensure t
  :bind (+notes-prefix-map
	 "d" #'denote
	 "i" #'denote-link-or-create
	 "k" #'denote-keywords-add
	 "K" #'denote-keywords-remove
	 "l" #'denote-find-link
	 "L" #'denote-find-backlink
	 "r" #'denote-rename-file
	 "s" #'denote-rename-file-using-front-matter)
  :init (setopt denote-directory (expand-file-name "~/OneDrive/notes/")))

;; NOTE: remove denote buffers from consult Buffers group, leave only in Denote buffers group
(use-package consult-denote
  :ensure t
  :bind (+notes-prefix-map
	 "n" #'consult-denote-find
	 "g" #'consult-denote-grep)
  :config (consult-denote-mode))

;; TODO: org-zettel-ref-mode https://github.com/yibie/org-zettel-ref-mode
;; TODO: write convert-to-org.py in .go
;; TODO: dwim-shell-command to call convert-to-org.go
;; NOTE: use markdownload to download webpages for org-zettel-ref-mode
;; NOTE: blk https://github.com/mahmoodsh36/blk
;; NOTE: denote-explore https://github.com/pprevos/denote-explore
;; NOTE: org-remark (is this better than org-zettel-ref-mode?) https://github.com/nobiot/org-remark
;; NOTE: annotate https://github.com/bastibe/annotate.el

;;;;;;;;;;;;;
;;;; pdf ;;;;

;; NOTE: pdf-tools https://github.com/fuxialexander/org-pdftools
;; NOTE: saveplace-pdf-view

;;;;;;;;;;;;;;
;;;; epub ;;;;

;; NOTE: nov
;; NOTE: djvu

;;;;;;;;;;;;;;;;;
;;;; secrets ;;;;

;; NOTE: age.el https://github.com/anticomputer/age.el
;; NOTE: pass.el https://github.com/NicolasPetton/pass or passage.el https://github.com/anticomputer/passage.el
;; NOTE: sops https://github.com/djgoku/sops
;; NOTE: pinentry
;; NOTE: password-store-menu https://github.com/rjekker/password-store-menu

;;;;;;;;;;;;;
;;;; rss ;;;;

;; NOTE: elfeed 
;; NOTE: phundrak config elfeed
;; NOTE: elfeed-webkit https://github.com/fritzgrabo/elfeed-webkit

;;;;;;;;;;;;;;;;;;
;;;; wallabag ;;;;

;; NOTE: wombag

;;;;;;;;;;;;;;;
;;;; email ;;;;

;; NOTE: notmuch
;; NOTE: ol-notmuch https://github.com/tarsius/ol-notmuch
;; NOTE: notmuch-addr https://github.com/tarsius/notmuch-addr
;; NOTE: notmuch-bookmarks https://github.com/publicimageltd/notmuch-bookmarks

;;;;;;;;;;;;;;;;;
;;;; browser ;;;;

;; NOTE: nyxt https://github.com/migalmoreno/nyxt.el

;;;;;;;;;;;;;;;;;;;;
;;;; web-search ;;;;

;; NOTE: consult-omni https://github.com/armindarvish/consult-omni
;; NOTE: engine-mode https://github.com/hrs/engine-mode
;; NOTE: bookmark-web https://github.com/AuPath/bookmark-web

;;;;;;;;;;;;;
;;;; mpv ;;;;

;; NOTE: org-mpv-notes https://github.com/bpanthi977/org-mpv-notes
;; NOTE: ready-player https://github.com/xenodium/ready-player

;;;;;;;;;;;;;;;
;;;; music ;;;;

;;;;;;;;;;;;;;;;;
;;;; youtube ;;;;

;; NOTE: yeetube https://github.com/Boruch-Baum/emacs-yeetube.el

;;;;;;;;;;;;;;;;;
;;;; storage ;;;;

;; NOTE: dropbox https://github.com/lorniu/emacs-dropbox

;;;;;;;;;;;;;;;;;;;;
;;;; workspaces ;;;;

;; NOTE: project-tab-groups https://github.com/fritzgrabo/project-tab-groups
;; NOTE: tab-bar-echo-area https://github.com/fritzgrabo/tab-bar-echo-area
;; NOTE: activities.el if project-tab-groups isn't to my liking

;;;;;;;;;;;;
;;;; ai ;;;;

;; NOTE: gptel
;; NOTE: gptel-quick https://github.com/karthink/gptel-quick
;; NOTE: magit-gptcommit https://github.com/douo/magit-gptcommit
;; NOTE: elysium https://github.com/lanceberge/elysium
;; NOTE: evedel https://github.com/daedsidog/evedel

;;;;;;;;;;;;;;;
;;;; input ;;;;

;; NOTE: fcitx.el https://github.com/cute-jumper/fcitx.el

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

;; NOTE: hl-line mode doesn't highlight nerd-icons in dired
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; NOTE: hl-line mode doesn't highlight nerd-icons in ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package magit-file-icons
  :ensure t
  :after magit
  :config (magit-file-icons-mode 1))

;; NOTE: nerd-icons-multimodal https://github.com/abougouffa/nerd-icons-multimodal
;; NOTE: create compile-multi-nerd-icons

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; window-manager ;;;;

;; idk if i want this
;; NOTE: shackle https://depp.brause.cc/shackle/
;; NOTE: sway https://github.com/thblt/sway.el

;;;;;;;;;;;;;;;;
;;;; extras ;;;;

(use-package dash :ensure t)

;; NOTE: paw (language learning) https://github.com/chenyanming/paw
;; NOTE: anki-editor (learning) https://github.com/anki-editor/anki-editor
;; NOTE: emacs-everywhere https://github.com/tecosaur/emacs-everywhere
;; NOTE: ros (screenshots) https://github.com/LionyxML/ros
;; NOTE: work-timer https://github.com/krisbalintona/work-timer
;; NOTE: exercism-modern https://github.com/elken/exercism-modern
