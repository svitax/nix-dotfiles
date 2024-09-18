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
;;; TODO: :modify-syntax ;;;

;; use-package keyword to easily add characters like -, _, !, :, & as word
;; constituents, $ as paired delimiter, etc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO: :pretty-symbols ;;;

;;;;;;;;;;;;;;
;;;; meow ;;;;

;; TODO: rectangle state https://github.com/Ziqi-Yang/.emacs.d/blob/
;; TODO: smartparen/puni state https://github.com/eshrh/nyaatouch/blob/master/nyaatouch.el
;; TODO: multicursor.el support for meow's beacon state
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
	  ;; Disable keypad describe until we can use Embark prefix help
	  meow-keypad-describe-keymap-function nil)
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
	 ;; TODO: "" +spelling-prefix-map
	 ;;  c . jinx-correct
	 ;;  l . jinx-languages
	 "u" #'meow-universal-argument
	 "v" +vc-prefix-map
	 "w" +window-prefix-map)
	;; TODO: find a better block to put meow-window commands
	('+goto-prefix-map
	 "b" #'+meow-window-bottom
	 "c" #'+meow-window-middle
	 "t" #'+meow-window-top)
	('+match-prefix-map
	 "a" #'meow-bounds-of-thing
	 "i" #'meow-inner-of-thing))
  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev)
   '("<escape>" . ignore))
  (meow-normal-define-key
   '("a" . meow-append)
   '("A" . +meow-append-line-end)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   ;; TODO: '("C" . my/meow-cursor-below)
   '("d" . meow-delete)
   '("D" . meow-kill)
   '("C-d" . meow-page-down)
   '("e" . meow-prev)
   '("E" . +meow-lookup)
   '("f" . meow-till)
   '("F" . +meow-till-backwards)
   '("g" . +goto-prefix-map)
   '("G" . meow-grab)
   '("h" . +match-prefix-map)
   ;; hm => matching bracket / matchit
   '("i" . meow-right)
   ;; TODO: '("j" . my/meow-keep) ;; keep selections matching regex (multicursor)
   ;; TODO: '("J" . my/meow-remove) ;; remove selections matching regex (multicursor)
   '("k" . meow-search)
   ;; TODO: '("k" . my/meow-next)
   '("l" . meow-insert)
   '("L" . +meow-insert-line-start)
   '("m" . meow-left)
   '("n" . meow-next)
   '("N" . +meow-join-line)
   ;; TODO: N => join lines inside selection
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-yank)
   ;; TODO: '("P" . my/meow-yank-before)
   '("q" . meow-quit)
   '("r" . meow-replace)
   ;; TODO: '("s" . my/meow-select)
   '("t" . meow-find)
   '("T" . +meow-find-backwards)
   '("u" . undo-only)
   '("U" . undo-redo)
   '("C-u" . meow-page-up)
   ;; TODO: '("v" . my/meow-extend-state)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-line)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("<escape>" . meow-cancel-selection)
   '("-" . negative-argument) ;; TODO: do i need negative argument? deos find/till backwards + line-backwards cover my use-cases?
   ;; TODO: '(">" . my/meow-next-thing)
   ;; >x . goto next diagnostic
   ;; >X . goto last diagnostic
   ;; TODO: '("<" . my/meow-previous-thing)
   ;; <x . goto prev diagnostic
   ;; <X . goto first diagnostic
   '(";" . repeat)
   '("'" . meow-reverse))
  (meow-thing-register 'angle '(regexp "<" ">") '(regexp "<" ">"))
  (add-to-list 'meow-char-thing-table '(?a . angle))
  ;; start in insert
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (add-to-list 'meow-mode-state-list '(eshell-mode . insert))
  (meow-global-mode 1))

;; meow-tree-sitter https://github.com/skissue/meow-tree-sitter
;; meow-vterm https://github.com/45mg/meow-vterm
;; nyaatouch https://github.com/eshrh/nyaatouch

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
  (dolist (func '(meow-page-up
		  meow-page-down
		  beginning-of-buffer
		  end-of-buffer
		  recenter))
    (add-to-list 'pulsar-pulse-functions func)))

(use-package whitespace
  :init
  ;; TODO: whitespace-mode: show tabs and maybe newlines
  ;; `whitespace-mode' highlights each space/tab/newline with a face. In a small file, that may not matter but in larger files it is crippling.
  ;; Change it to use the display table where it substitutes other characters for spaces instead of using faces.
  (setopt whitespace-style '(space-mark tab-mark)))

;; TODO: :hide-whitespace
;; TODO: paren-face https://github.com/tarsius/paren-face

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
	 ;; TODO: "k" +kill-all-emacsen (lambda emacs)
	 ))

(use-package display-line-numbers
  :init (global-display-line-numbers-mode))

;; visual-fill-column https://github.com/joostkremers/visual-fill-column or virtual-auto-fill https://github.com/luisgerhorst/virtual-auto-fill
;; aggressive-fill-paragraph-mode https://github.com/davidshepherd7/aggressive-fill-paragraph-mode

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

;; TODO: daemons https://github.com/cbowdon/daemons.el

;;;;;;;;;;;;;;;
;;;; files ;;;;

;; TODO: find-file creates non-existent directories
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

;; TODO: autorevert
;; (use-package autorevert
;;   :init
;;   (use-package auto-revert-extras)
;;   ;; Be quiet about reverts
;;   (setopt global-auto-revert-non-file-buffers t
;; 	  auto-revert-verbose nil)
;;   ;; global-auto-revert-mode can slow things down. try to enable it per active window.
;;   (add-to-list 'window-state-change-functions #'+window-state-state-change)
;;   :config (global-auto-revert-mode))

;; TODO: recentf
;; (use-package recentf
;;   :xdg-state (recentf-save-file "recentf-save.el"))

;; TODO: saveplace
;; (use-package saveplace
;;   :xdg-state (save-place-file "save-place.el")
;;   :config (save-place-mode))

(use-package savehist
  :xdg-state (savehist-file "savehist.el")
  :init (setopt history-length 1000
		history-delete-duplicates t)
  :config (savehist-mode))

;; TODO: ffap
;; sudo-edit https://github.com/nflath/sudo-edit

;;;;;;;;;;;;;;;;;;;;
;;;; minibuffer ;;;;

(use-package minibuffer
  :init
  (setopt enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode)
  :config
  (use-package minibuffer-extras)
  (advice-add #'completing-read-multiple :filter-args #'+crm-indicator))

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
  :bind (vertico-map
	 "C-n" #'vertico-next
	 "C-e" #'vertico-previous)
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

;; TODO: vertico-quick using C-s

;;;;;;;;;;;;;;;;
;;;; embark ;;;;

;; TODO: embark

;;;;;;;;;;;;;;;;;
;;;; consult ;;;;

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
  (setopt corfu-auto t
	  corfu-auto-prefix 2
	  corfu-auto-delay 0.0
	  corfu-count 10
	  corfu-cycle t
	  corfu-preview-current nil
	  corfu-preselect 'first
	  corfu-scroll-margin 5)
  :config
  (global-corfu-mode)
  (add-hook 'meow-insert-exit-hook 'corfu-quit))

(use-package corfu-popupinfo
  :after corfu
  :bind (corfu-map
	 "C-d" #'corfu-popupinfo-scroll-up
	 "C-u" #'corfu-popupinfo-scroll-down
	 [remap corfu-info-documentation] #'corfu-popupinfo-toggle)
  :init (setopt corfu-popupinfo-delay '(0.5 . 0.5)
		corfu-global-mode '((not eshell-mode)))
  :config (corfu-popupinfo-mode 1))

;; TODO: corfu-quick
;; TODO: corfu-history
;; TODO: corfu for eshell
;; TODO: corfu for vterm

;;;;;;;;;;;;;;;;;;;;;
;;;; :completions ;;;;

;; TODO: :completions keyword for use-package forms

;;;;;;;;;;;;;;
;;;; cape ;;;;

;; TODO: cape

;;;;;;;;;;;;;;;;;
;;;; project ;;;;

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

;; TODO: projection

;; TODO: keep projects list always in sync with certain directories
;; snatch projel's rescan-directory and rescan-all-projects, put in project extras
;; projel-add-project-directory, projel--add-project, projecl-rescan-directory, projel-find-projects-in-dir, etc.

;;;;;;;;;;;;;;;;;
;;;; buffers ;;;;

(use-package buffer
  :no-require
  :ensure nil
  :config (bind ('+goto-prefix-map
		 "e" #'end-of-buffer
		 ;; "f" goto file
		 "g" #'beginning-of-buffer)
		(+buffer-prefix-map
		 "k" #'kill-current-buffer
		 ;; "K" #'crux-kill-other-buffers ;; TODO: crux-kill-other-buffers
		 ;; "m" #'+move-buffer-file ;; TODO: move buffer and file to DIR
		 ;; https://github.com/karthink/.emacs.d/blob/b0ae76c813b133619b2f29b179a7f1ab5193f534/lisp/better-buffers.el#L243-L257
		 "r" #'+rename-file-and-buffer ;; TODO: rename current buffer and file to NEW NAME
		 ;; https://github.com/karthink/.emacs.d/blob/b0ae76c813b133619b2f29b179a7f1ab5193f534/lisp/better-buffers.el#L225-L240
		 "s" #'save-buffer)))

;; TODO: epithet https://github.com/oantolin/epithet
;; uniquify
;; adviced:kill-buffer--possibly-save https://christiantietze.de/posts/2023/09/kill-unsaved-buffer-ux-action-labels and xenodium

;;;;;;;;;;;;;;;;;
;;;; ibuffer ;;;;

;; ibuffer https://www.reddit.com/r/emacs/s/Ft0yZxEMVD
;; ibuffer-git https://github.com/jrockway/ibuffer-git
;; ibuffer-project? or ibuffer-git?
;; projection-ibuffer?
;; bufler?

;;;;;;;;;;;;;;;;;
;;;; windows ;;;;

;; TODO: repeat map for enlarge and shrink window commands
(use-package window
  :no-require
  :ensure nil
  :config
  (use-package windows-extras)
  (bind ((:global-map)
	 "M-o" #'other-window
	 "C-v" #'+scroll-down-half-page
	 "M-v" #'+scroll-up-half-page)
	(+window-prefix-map
	 ;; TODO: "a" #'ace-window
	 ;; TODO: "f" #'+toggle-window-split ;; toggle windows between horizontal and vertical (lambda emacs)
	 "k" #'delete-window
	 "o" #'other-window
	 ;; TODO: "r" #'+rotate-windows ;; (lambda emacs)
	 ;; TODO: "R" #'+rotate-windows-backward ;; (lambda emacs)
	 "s" #'split-window-below
	 ;; TODO: "s" #'+split-window-below-and-focus ;; (lambda emacs)
	 "T" #'tear-off-window
	 "v" #'split-window-right
	 ;; TODO: "v" #'+split-window-right-and-focus ;; (lambda emacs)
	 ;; TODO: "w" #'ace-window
	 ;; TODO: "x" #'+rotate-windows-backward ;; (lambda emacs)
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

;; switchy-window https://github.com/emacsmirror/switchy-window
;; ace-window https://github.com/abo-abo/ace-window

;;;;;;;;;;;;;;
;;;; fold ;;;;

;;;;;;;;;;;;;;;;
;;;; narrow ;;;;

;; logos https://github.com/protesilaos/logos

;;;;;;;;;;;;;;;;;
;;;; outline ;;;;

;; outli https://github.com/jdtsmith/outli
;; nbarrientos outline config
;; outline-indent https://github.com/jamescherti/outline-indent.el

;;;;;;;;;;;;;;;
;;;; dired ;;;;

;; TODO: dired-extras in dired use-package block
(use-package dired
  :config
  (bind (mode-specific-map
	 "d" #'dired-jump)
	(dired-mode-map
	 "b" #'dired-up-directory
	 "f" #'dired-find-file)))

;; dired-narrow
;; diredfl
;; dired-single
;; hide dotfiles and hide gitignored files (should i use dired-filter for this or copy from old-init.el)
;; wdired
;; TODO: dired-ranger (dired-ranger-copy and dired-ranger-paste and dired-ranger-move)
;; dired-filter (what filters would i use? filter-by-videos from xenodium)
;; dired-subtree
;; TODO: dired-preview
;; TODO: dired-git-info https://github.com/clemera/dired-git-info
;; dired-open-with https://github.com/FrostyX/dired-open-with
;; dired-rsync?
;; dired-dy https://github.com/emacsmirror/dired-du

;;;;;;;;;;;;;;;;;
;;;; editing ;;;;

(use-package text
  :no-require
  :ensure nil
  :config
  (bind '+goto-prefix-map
	"i" #'end-of-line
	"m" #'beginning-of-line))

;; TODO: mowie

;; TODO: move text up and down

;; TODO: don't auto pair < in org-mode
(use-package elec-pair
  :init (electric-pair-mode))

(use-package delsel
  :config (delete-selection-mode +1))

;; TODO: https://github.com/Kungsgeten/selected.el

(use-package smart-hungry-delete
  :ensure t
  :bind ((:global-map)
	 [remap backward-delete-char-untabify] #'smart-hungry-delete-backward-char
	 [remap delete-backward-char] #'smart-hungry-delete-backward-char
	 [remap delete-char] #'smart-hungry-delete-forward-char
	 [remap delete-forward-char] #'smart-hungry-delete-forward-char)
  :config (smart-hungry-delete-add-default-hooks))

;; TODO: ws-butler
;; TODO: subword
;; interactive-align https://github.com/mkcms/interactive-align

(use-package expand-region
  :ensure t
  :init (setopt expand-region-fast-keys-enabled nil)
  :bind (meow-normal-state-keymap
	 "," #'er/expand-region
	 "." #'er/contract-region))

;; TODO: highlight surround.el semantic units
;; TODO: surround.el treesitter-backed semantic units
;; TODO: surround.el which-key or embark help popups
(use-package surround
  :ensure t
  :config
  (bind +match-prefix-map
	"s" #'surround-insert
	"r" #'surround-change
	"d" #'surround-delete))

;; TODO: macrursors

;;;;;;;;;;;;;;;;;
;;;; replace ;;;;

;; TODO: visual-replace
;; eventually replace visual-replace with query-replace-parallel and visual-regexp-steroids functionality
;; query-replace-parallel https://github.com/hokomo/query-replace-parallel

;;;;;;;;;;;;;;;;;;;
;;;; kill-ring ;;;;

;; TODO: kill-ring
;; clean-kill-ring https://github.com/NicholasBHubbard/clean-kill-ring.el

;;;;;;;;;;;;;;
;;;; undo ;;;;

;; TODOO undo-fu-session

;;;;;;;;;;;;;;;;;;;;
;;;; navigation ;;;;

;; TODO: avy
;; https://github.com/karthink/.emacs.d/blob/master/plugins/demo.el
;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-avy.el
;; https://karthinks.com/software/avy-can-do-anything
(use-package avy
  :ensure t
  :init
  (setopt avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o) ;; Colemak-DH keyboard
	  avy-timeout-seconds 0.35
	  avy-single-candidate-jump nil)
  :config
  (meow-normal-define-key '("s" . avy-goto-char-timer)))

;; TODO: lasgun https://github.com/aatmunbaxi/lasgun.el
;; "S" . lasgun

;; TODO: forward and backward paragraph from xenodium
;; TODO: beginend https://github.com/DamienCassou/beginend
;; TODO: link-hint (can avy + embark replace link-hint?)
;; TODO: tab-jump-out

;;;;;;;;;;;;;;;;
;;;; search ;;;;

;; TODO: occur
;; TODO: grep
;; TODO: wgrep https://github.com/mhayashi1120/Emacs-wgrep
;; https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L4039
;; consult-omni https://github.com/armindarvish/consult-omni

;;;;;;;;;;;;;;;;;
;;;; isearch ;;;;

;;;;;;;;;;;;;;;
;;;; imenu ;;;;

;; imenu https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L1593
;; https://github.com/dolmens/eglot-hierarchy

;;;;;;;;;;;;;;;;;;;
;;;; bookmarks ;;;;

;; TODO: bookmark https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L3320
;; TODO: harpoon https://github.com/otavioschwanck/harpoon.el or https://github.com/kofm/harpoon.el
;; TODO: gumshoe
;; TODO: diverted https://github.com/xenodium/dotsies/blob/main/emacs/ar/diverted.el

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

;; info https://github.com/oantolin/emacs-config/blob/696641a592691737ba5a019c67f2af7a6cc09183/init.el#L235-L239
;; info-colors
;; info-variable-pitch https://github.com/kisaragi-hiu/info-variable-pitch

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

;; TODO: (:lsp keyword) eglot-server-programs and eglot-server-configuration?
;; TODO: eglot
;; TODO: eglot-booster
;; TODO: apheleia-eglot
;; TODO: consult-eglot
;; citre https://github.com/universal-ctags/citre

;;;;;;;;;;;;;
;;;; dap ;;;;

;; TODO: (:dap keyword)
;; TODO: dape

;;;;;;;;;;;;;;;;;;
;;;; snippets ;;;;

;; TODO: tempel https://github.com/minad/tempel
;; TODO: eglot-tempel https://github.com/fejfighter/eglot-tempel
;; TODO: tempel-collection https://github.com/Crandel/tempel-collection

;; yasnippet-capf (if i decide to use yasnippet) https://github.com/elken/yasnippet-capf

;;;;;;;;;;;;;;;;;;;;
;;;; treesitter ;;;;

;; TODO: treesit
;; TODO: treesit-auto
;; TODO: ts-docstr https://github.com/emacs-vs/ts-docstr

;;;;;;;;;;;;;;;;;
;;;; compile ;;;;

;; TODO: :compile keyword (compile-multi)
;; TODO: compile
;; TODO: compile-multi

;;;;;;;;;;;;;;
;;;; prog ;;;;

;; TODO: xref
;; TODO: eldoc
;; TODO: colorful-mode
;; TODO: ct.el https://github.com/neeasade/ct.el

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config (setopt hl-todo-wrap-movement t))

(use-package newcomment
  :hook ((prog-mode . (lambda ()
			(set (make-local-variable
			      'comment-auto-fill-only-comments)
			     t)))))

;; TODO: evil-matchit https://github.com/redguardtoo/evil-matchit
;; TODO: imenu-list https://github.com/bmag/imenu-list (alternative to symbols-outline) or eglot-hierarchy https://github.com/dolmens/eglot-hierarchy
;; TODO: treesitter-context https://github.com/zbelial/treesitter-context.el

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

;; consult-git-log-grep https://github.com/ghosty141/consult-git-log-grep
;; consult-ls-git https://github.com/rcj/consult-ls-git
;; TODO: git-commit-ts-mode https://github.com/danilshvalov/git-commit-ts-mode
;; TODO: emsg-blame https://github.com/ISouthRain/emsg-blame
;; consult-gh https://github.com/armindarvish/consult-gh

;;;;;;;;;;;;;;
;;;; diff ;;;;

;; TODO: git-gutter
;; TODO: ediff

;;;;;;;;;;;;;;;;;;
;;;; terminal ;;;;

;; mistty https://github.com/szermatt/mistty or vterm
;; eshell-visual-vterm https://github.com/accelbread/eshell-visual-vterm
;; isend-mode https://github.com/ffevotte/isend-mode.el
;; awscli-capf https://github.com/sebasmonia/awscli-capf
;; vterm-capf https://github.com/twlz0ne/vterm-capf

;;;;;;;;;;;;;;;;
;;;; comint ;;;;

;; TODO: comint
;; TODO: comint-fold https://github.com/jdtsmith/comint-fold

;;;;;;;;;;;;;;;;
;;;; eshell ;;;;

;; TODO: if mistty or vterm doesn't suit my needs, look into setting up eshell with all the bells and whistles (pcmpl, aliases, syntax highlighting, prompt, etc.)
;; TODO: eshell
;; TODO: karthink eshell buffer redirection
;; TODO: karthink eshell atuin
(use-package eshell
  :xdg-state
  (eshell-aliases-file "aliases")
  (eshell-directory-name "")
  (eshell-login-script "login")
  (eshell-rc-script "rc"))

;;;;;;;;;;;;;;;
;;;; shell ;;;;

;; dwim-shell-commad https://github.com/xenodium/dwim-shell-command

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

;; TODO: package-lint-flymake
;; relint https://github.com/mattiase/relint

;;;;;;;;;;;;
;;;; go ;;;;

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
;;;; scheme ;;;;

;; https://github.com/port19x/geiser-overlay

;;;;;;;;;;;;;;;;;;;;;
;;;; common lisp ;;;;

;; https://github.com/fosskers/sly-overlay

;;;;;;;;;;;;;;
;;;; json ;;;;

;; counsel-jq (but with consult) https://github.com/200ok-ch/counsel-jq

;;;;;;;;;;;;;;
;;;; http ;;;;

;; TODO: verb https://github.com/federicotdn/verb
;; swagg https://github.com/isamert/swagg.el

;;;;;;;;;;;;;;;
;;;; tramp ;;;;

;; TODO: tramp
;; TODO: docker-tramp
(use-package tramp
  :xdg-state
  (tramp-auto-save-directory "auto-save/")
  (tramp-persistency-file-name "persistency.el"))

;;;;;;;;;;;;;;;;
;;;; docker ;;;;

;; TODO: docker
;; TODO: dockerfile-mode
;; TODO: flymake-hadolint
;; TODO: docker-compose-mode

;;;;;;;;;;;;;;;;;;;;
;;;; kubernetes ;;;;

;; TODO: kele https://github.com/jinnovation/kele.el
;; TODO: kubed https://github.com/eshelyaron/kubed

;;;;;;;;;;;;;;;;;;;;;;
;;;; devcontainer ;;;;

;; TODO: devcontainer https://github.com/bradschartz/devcontainer.el
;; TODO: emacs-dev-containers https://github.com/alexispurlane/emacs-dev-containers

;;;;;;;;;;;;;;;;;;
;;;; assembly ;;;;

;; beardbolt https://github.com/joaotavora/beardbolt
;; rmsbolt https://github.com/emacsmirror/rmsbolt

;;;;;;;;;;;;;;;;;;
;;;; markdown ;;;;

;; TODO: markdown mode
;; TODO: grip-mode https://github.com/seagle0128/grip-mode

;;;;;;;;;;;;;
;;;; org ;;;;

(use-package org
  :bind (org-mode-map
	 "C-'" #'popper-toggle))

;; TODO: book-mode https://github.com/rougier/book-mode or org-modern
;; org-modern-indent https://github.com/jdtsmith/org-modern-indent
;; TODO: corg https://github.com/isamert/corg.el
;; org-transclusion-http https://github.com/alphapapa/org-transclusion-http
;; org-inline-tags https://github.com/incandescentman/org-inline-tags
;; org-super-links https://github.com/toshism/org-super-links
;; org-recur https://github.com/mrcnski/org-recur
;; org-tidy https://github.com/jxq0/org-tidy
;; org-mind-map https://github.com/the-ted/org-mind-map

;;;;;;;;;;;;;;;;;;;;
;;;; org-agenda ;;;;

;; org-caldav https://github.com/dengste/org-caldav
;; org-gcal https://github.com/myuhe/org-gcal.el
;; org-timeblock https://github.com/ichernyshovvv/org-timeblock

;;;;;;;;;;;;;;;;;;
;;;; spelling ;;;;

;; TODO: jinx

;;;;;;;;;;;;;;;;
;;;; biblio ;;;;

;; TODO: citar

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

;; TODO: remove denote buffers from consult Buffers group, leave only in Denote buffers group
(use-package consult-denote
  :ensure t
  :bind (+notes-prefix-map
	 "n" #'consult-denote-find
	 "g" #'consult-denote-grep)
  :config (consult-denote-mode))

;; TODO: blk https://github.com/mahmoodsh36/blk
;; TODO: denote-explore https://github.com/pprevos/denote-explore
;; TODO: org-zettel-ref-mode https://github.com/yibie/org-zettel-ref-mode
;; org-remark (is this better than org-zettel-ref-mode?) https://github.com/nobiot/org-remark
;; annotate https://github.com/bastibe/annotate.el

;;;;;;;;;;;;;
;;;; pdf ;;;;

;; TODO: pdf-tools https://github.com/fuxialexander/org-pdftools
;; TODO: saveplace-pdf-view

;;;;;;;;;;;;;;
;;;; epub ;;;;

;; TODO: nov
;; TODO: djvu

;;;;;;;;;;;;;;;;;
;;;; secrets ;;;;

;; TODO: age.el https://github.com/anticomputer/age.el
;; TODO: pass.el https://github.com/NicolasPetton/pass or passage.el https://github.com/anticomputer/passage.el
;; TODO: sops https://github.com/djgoku/sops
;; TODO: pinentry
;; password-store-menu https://github.com/rjekker/password-store-menu

;;;;;;;;;;;;;
;;;; rss ;;;;

;; TODO: elfeed 
;; TODO: phundrak config elfeed
;; elfeed-webkit https://github.com/fritzgrabo/elfeed-webkit

;;;;;;;;;;;;;;;;;;
;;;; wallabag ;;;;

;; TODO: wombag

;;;;;;;;;;;;;;;
;;;; email ;;;;

;; TODO: notmuch

;;;;;;;;;;;;;;;;;
;;;; browser ;;;;

;; nyxt https://github.com/migalmoreno/nyxt.el

;;;;;;;;;;;;;;;;;;;;
;;;; web-search ;;;;

;; TODO: consult-omni https://github.com/armindarvish/consult-omni
;; TODO: engine-mode https://github.com/hrs/engine-mode
;; TODO: bookmark-web https://github.com/AuPath/bookmark-web

;;;;;;;;;;;;;
;;;; mpv ;;;;

;; TODO: org-mpv-notes https://github.com/bpanthi977/org-mpv-notes
;; TODO: ready-player https://github.com/xenodium/ready-player

;;;;;;;;;;;;;;;
;;;; music ;;;;

;;;;;;;;;;;;;;;;;
;;;; youtube ;;;;

;; yeetube https://github.com/Boruch-Baum/emacs-yeetube.el

;;;;;;;;;;;;;;;;;
;;;; storage ;;;;

;; dropbox https://github.com/lorniu/emacs-dropbox

;;;;;;;;;;;;;;;;;;;;
;;;; workspaces ;;;;

;; TODO: project-tab-groups https://github.com/fritzgrabo/project-tab-groups
;; TODO: tab-bar-echo-area https://github.com/fritzgrabo/tab-bar-echo-area
;; activities.el if project-tab-groups isn't to my liking

;;;;;;;;;;;;
;;;; ai ;;;;

;; gptel
;; gptel-quick https://github.com/karthink/gptel-quick
;; magit-gptcommit https://github.com/douo/magit-gptcommit
;; elysium https://github.com/lanceberge/elysium
;; evedel https://github.com/daedsidog/evedel

;;;;;;;;;;;;;;;
;;;; input ;;;;

;; fcitx.el https://github.com/cute-jumper/fcitx.el

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

;; nerd-icons-multimodal https://github.com/abougouffa/nerd-icons-multimodal
;; TODO: create compile-multi-nerd-icons

;;;;;;;;;;;;;;;;
;;;; extras ;;;;

(use-package dash :ensure t)

;; paw (language learning) https://github.com/chenyanming/paw
;; anki-editor (learning) https://github.com/anki-editor/anki-editor
;; emacs-everywhere https://github.com/tecosaur/emacs-everywhere
;; ros (screenshots) https://github.com/LionyxML/ros
