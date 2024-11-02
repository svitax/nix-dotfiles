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

;; TODO: don't use meow-keypad, just make a leader map and then i can finally just use emark-prefix-help
;; NOTE: rectangle state https://github.com/Ziqi-Yang/.emacs.d/blob/
(use-package meow
  :ensure t
  :init
  ;; Forward C-d to H-d so we can  C- to page-down
  (global-set-key (kbd "H-d") 'delete-char)
  (setq meow--kbd-delete-char "H-d")
  ;; (setq meow--kbd-delete-char "<deletechar>")
  :config
  (use-package meow-extras)
  ;; (add-hook 'meow-mode-hook (lambda () (setq delete-active-region t)))
  (setopt meow-use-clipboard t
	  meow-expand-hint-remove-delay 0
	  meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh
	  meow-cursor-type-region-cursor 'box
	  meow-use-cursor-position-hack t
	  ;; delete-active-region t
	  ;; Disable keypad describe until we can use Embark prefix help
	  meow-keypad-describe-keymap-function nil)
  (bind (mode-specific-map
	 "," +compile-prefix-map
	 "a" +agenda-prefix-map
	 "b" +buffer-prefix-map
	 ;; c (C-c)
	 ;; d dired
	 "e" +eval-prefix-map
	 "f" +file-prefix-map
	 ;; g (C-M-)
	 ;; h help
	 ;; i
	 ;; j
	 ;; k
	 "l" +bibliography-prefix-map
	 ;; m (M-)
	 "n" +notes-prefix-map
	 "o" +org-prefix-map
	 "p" +project-prefix-map
	 "q" +quit-prefix-map
	 ;; "r" +register-prefix-map  ; register/bookmark
	 "s" +search-prefix-map
	 ;; sr . vr/query-replace
	 "t" +toggle-prefix-map
	 ;; th . hl-line-mode
	 ;; tH . global-hl-line-mode
	 ;; ti . imenu-list-smart-toggle
	 ;; tj . jinx-mode
	 ;; tJ . global-jinx-mode
	 ;; tt . consult-theme
	 ;; NOTE: "" +spelling-prefix-map
	 ;;  c . jinx-correct
	 ;;  l . jinx-languages
	 "u" #'meow-universal-argument
	 "v" +vc-prefix-map
	 "w" +window-prefix-map
	 ;; x
	 ;; y
	 ;; z
	 )
	('+match-prefix-map
	 "a" #'meow-bounds-of-thing
	 "i" #'meow-inner-of-thing))
  (meow-motion-overwrite-define-key
   ;; '("<escape>" . meow-simple-motion-mode)
   '("<escape>" . ignore)
   ;; Use a to move up, h to move down.
   ;; '("a" . meow-prev)
   ;; '("h" . meow-next)
   ;; '("a" . "p")
   '("A" . +meow-eldoc)
   ;; '("h" . "n")
   )
  (meow-normal-define-key
   '("a" . meow-prev)
   '("A" . +meow-eldoc)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . +meow-kill)
   '("C-d" . meow-page-down)
   '("e" . meow-right)
   '("f" . meow-find)
   '("g" . +goto-prefix-map)
   '("h" . meow-next)
   '("H" . +meow-join-line)
   '("i" . meow-insert)
   '("I" . +meow-insert-line-start)
   ;; '("j" . meow-join)
   '("j" . meow-pop-selection)
   '("J" . meow-reverse)
   ;; '("k" . meow-search)
   ;; '("k" . +meow-keep-selection)
   ;; '("K" . +meow-remove-selection)
   ;; '("" . +register-prefix-map) 
   ;; '("l" . structural-mode) 
   '("m" . +match-prefix-map)
   '("n" . meow-append)
   '("N" . +meow-append-line-end)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . yank)
   '("P" . yank-pop)
   '("q" . meow-quit)
   '("r" . +meow-replace)
   '("R" . +meow-duplicate)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . undo-redo)
   '("C-u" . meow-page-up)
   '("v" . meow-expand-mode)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-line) ;; NOTE: make it include eol
   '("X" . +meow-expand-line-end)
   '("y" . meow-left)
   ;; '("z" . +view-prefix-map)
   ;; '("Z" . +sticky-view-prefix-map)
   '("'" . meow-save)
   '("<escape>" . meow-cancel-selection)
   '("<" . indent-rigidly-left) ;; NOTE: if no selection, indent line
   '(">" . indent-rigidly-right)
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
  (add-to-list 'meow-mode-state-list '(vterm-mode . vterm-insert))
  (add-to-list 'meow-mode-state-list '(meow-vterm-mode . vterm-insert))
  (add-to-list 'meow-mode-state-list '(eshell-mode . insert))
  (add-to-list 'meow-mode-state-list '(git-commit-mode . insert))
  ;; special
  (add-to-list 'meow-mode-state-list '(Info-mode . motion))
  ;; simple motion
  (add-to-list 'meow-mode-state-list '(pdf-view-mode . simple-motion))
  ;; (add-to-list 'meow-mode-state-list '(nov-mode . simple-motion))
  ;; (add-to-list 'meow-mode-state-list '(magit-status-mode . simple-motion))
  ;; (add-to-list 'meow-mode-state-list '(dired-mode . simple-motion))
  ;; (add-to-list 'meow-mode-state-list '(ediff-mode . simple-motion))
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
  :config
  (use-package themes-extras)
  (setopt modus-themes-common-palette-overrides `((fg-region unspecified)
						  (date-common cyan)
						  (date-deadline red-warmer)
						  (date-event magenta-warmer)
						  (date-holiday blue)
						  (date-scheduled magenta-cooler)
						  (date-weekday cyan-cooler)
						  (date-weekend blue-faint)
						  (bg-prose-block-contents unspecified)
						  (bg-prose-block-delimiter unspecified)
						  (fg-prose-block-delimiter fg-dim)
						  
						  (bg-completion "#c0deff")
						  (bg-region bg-sage)
						  (border-mode-line-active unspecified)
						  (border-mode-line-inactive unspecified)
						  (fringe unspecified)
						  (fg-line-number-active fg-main)
						  (fg-line-number-inactive "gray50")
						  (bg-line-number-active unspecified)
						  (bg-line-number-inactive unspecified)
						  ;; (mail-recipient fg-main)
						  (mail-recipient indigo)
						  ,@modus-themes-preset-overrides-faint))
  (setopt modus-operandi-palette-overrides `(;; (bg-mode-line-active bg-inactive)
					     (bg-mode-line-active bg-dim)
					     (bg-mode-line-inactive bg-dim)
					     ;; (bg-mode-line-active bg-blue-intense)
					     ;; (fg-mode-line-active fg-main)
					     (fg-heading-1 "#a01f64")
					     (fg-heading-2 "#2f5f9f")
					     (fg-heading-3 "#1a8388")))
  (setopt modus-vivendi-palette-overrides `(;; Lower contrast
					    (fg-main "#d6d6d4")
					    (bg-main "#090909")
					    (bg-region bg-lavender)
					    (fg-heading-1 magenta-faint)
					    (bg-mode-line-active bg-lavender)
					    (fg-mode-line-active "#ffffff")))
  (setopt modus-themes-bold-constructs t
	  ;; modus-themes-completions '((t . (bold)))
	  ;; modus-themes-prompts '(bold)
	  modus-themes-variable-pitch-ui nil)
  ;; :config
  (add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces)
  (add-hook 'ef-themes-post-load-hook #'my/ef-themes-custom-faces)
  (modus-themes-select 'modus-operandi))

(use-package pulsar
  :ensure t
  :config
  (dolist (func '(xref-find-definitions
		  xref-go-back
		  xref-go-forward
		  meow-page-up
		  meow-page-down
		  beginning-of-buffer
		  end-of-buffer
		  recenter))
    (add-to-list 'pulsar-pulse-functions func))
  
  (pulsar-global-mode 1))

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

(use-package druid-modeline
  :config
  (druid-modeline-text-mode t)
  (add-hook 'prog-mode-hook #'druid-modeline-prog-mode)
  (add-hook 'messages-buffer-mode-hook #'druid-modeline-text-mode)
  (add-hook 'org-mode-hook #'druid-modeline-org-mode)
  (add-hook 'vterm-mode-hook #'druid-modeline-vterm-mode)
  (add-hook 'eshell-mode-hook #'druid-modeline-eshell-mode)
  (add-hook 'pdf-view-mode-hook #'druid-modeline-pdf-mode))

;; NOTE: karthink's store-action-key+cmd and keycast-capture-avy-dispatch
(use-package keycast
  :ensure t
  :config
  (use-package keycast-extras)
  
  (setopt keycast-mode-line-format "%2s%k%c%R"
	  keycast-mode-line-window-predicate 'mode-line-window-selected-p
	  keycast-mode-line-remove-tail-elements nil)
  
  (dolist (input '(self-insert-command org-self-insert-command eshell-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))
  
  (dolist (event '( mouse-event-p mouse-movement-p mwheel-scroll handle-select-window
                    mouse-set-point mouse-drag-region))
    (add-to-list 'keycast-substitute-alist `(,event nil)))
  
  (druid-modeline-keycast-mode))

;; moody? minions? prots modeline? karthink? diminish?
;; blackout https://github.com/radian-software/blackout

;; (use-package simple :config (column-number-mode))

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
	 ;; NOTE: "k" +kill-all-emacsen (lambda emacs)
	 "q" #'save-buffers-kill-terminal
	 "r" #'restart-emacs))

;; NOTE: don't turn on line-numbers by default, but put it in the toggle prefix map
(use-package display-line-numbers
  :bind (+toggle-prefix-map
	 "C-l" #'global-display-line-numbers-mode
	 "l" #'display-line-numbers-mode))

(use-package virtual-auto-fill
  :ensure t
  :hook ((org-mode markdown-mode) . virtual-auto-fill-mode)
  :init
  (setopt fill-column 80))

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
  :init
  (setopt envrc-show-summary-in-minibuffer nil)
  :config
  (envrc-global-mode))

;;;;;;;;;;;;;;;;;;
;;;; external ;;;;

;; NOTE: daemons https://github.com/cbowdon/daemons.el

;;;;;;;;;;;;;;;
;;;; files ;;;;

;; NOTE: find-file creates non-existent directories
(use-package files
  :bind ((+file-prefix-map
	  "f" #'find-file))
  :init
  (setopt y-or-n-p-use-read-key t
	  use-short-answers t
	  confirm-kill-processes nil
	  confirm-kill-emacs 'yes-or-no-p))

(use-package persist-state
  :ensure t
  :config
  (persist-state-mode))

(use-package backup
  :no-require
  :ensure nil
  ;; Disable backup
  :init
  (setopt backup-inhibited t
	  make-backup-files nil))

(use-package auto-save
  :no-require
  :ensure nil
  :xdg-state (auto-save-list-file-prefix "saves/")
  ;; Disable autosave
  :init
  (setopt auto-save-default nil))

(use-package lockfiles
  :no-require
  :ensure nil
  :config
  ;; Avoid creating lock files (ie. .#some-file.el)
  (setopt create-lockfiles nil))

;; NOTE: autorevert
;; (use-package autorevert
;;     :init
;;     (use-package auto-revert-extras)
;;     ;; Be quiet about reverts
;;     (setopt global-auto-revert-non-file-buffers t
;; 	  auto-revert-verbose nil)
;;     ;; global-auto-revert-mode can slow things down. try to enable it per active window.
;;     (add-to-list 'window-state-change-functions #'+window-state-state-change)
;;     :config (global-auto-revert-mode))

;; NOTE: recentf
;; (use-package recentf
;;   :xdg-state (recentf-save-file "recentf-save.el"))

;; NOTE: +save-place-suppress-message doesn't work
(use-package saveplace
  :xdg-state (save-place-file "save-place.el")
  :config
  (defun +save-place-suppress-message (fun &rest args)
    "Suppress minibuffer messages from `save-place-mode'."
    (let ((inhibit-message t))
      (apply fun args)))
  (advice-add 'save-place-mode :around #'+save-place-suppress-message)
  
  (save-place-mode))

;; NOTE: (setq savehist-additional-variables '(register-alist kill-ring))
;; NOTE: (make-variable-buffer-local 'register-alist) makes registers buffer-local, kind of like vim marks
(use-package savehist
  :xdg-state (savehist-file "savehist.el")
  :init
  (setopt history-length 1000
	  history-delete-duplicates t)
  :config
  (savehist-mode))

;; NOTE: ffap
;; NOTE: sudo-edit https://github.com/nflath/sudo-edit

;;;;;;;;;;;;;;;;;;;;
;;;; minibuffer ;;;;

(use-package minibuffer
  :init
  (setopt enable-recursive-minibuffers t ;; NOTE: prot explanation
	  echo-keystrokes 0.25) ;; NOTE: prot explanation
  
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
  :init
  (setopt marginalia-max-relative-age 0)
  :config
  (marginalia-mode))

;;;;;;;;;;;;;;;;;;;
;;;; orderless ;;;;

(use-package orderless
  :ensure t
  :config
  (setopt completion-styles '(orderless basic)))

;;;;;;;;;;;;;;;;;
;;;; vertico ;;;;

(use-package vertico
  :ensure t
  :bind ((vertico-map
	  "M-a" #'vertico-previous-group
	  "C-d" #'vertico-scroll-up
	  "M-h" #'vertico-next-group
	  "C-u" #'vertico-scroll-down))
  :init
  (setopt vertico-scroll-margin 0
	  vertico-count 5
	  vertico-cycle t
	  vertico-resize 'grow-only)
  :config
  (use-package vertico-extras)
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (vertico-map
	 "DEL" #'vertico-directory-delete-char
	 "M-DEL" #'vertico-directory-delete-word
	 "RET" #'vertico-directory-enter)
  :config
  ;; This works with `file-name-shadow-mode' enabled. When you are in a
  ;; sub-directory and use, say, `find-file' to go to your home '~/' or
  ;; root '/' directory, Vertico will clear the old path to keep only your
  ;; current input.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico
  :bind ((vertico-map
	  "TAB" #'+vertico-minimal-complete)
	 (vertico-multiform-map
	  "<down>" #'+vertico-minimal-next
	  "<up>" #'+vertico-minimal-previous
	  "C-l" #'vertico-multiform-vertical))
  :config
  (setq vertico-multiform-commands
	`(;; Maximal
	  (eshell-atuin-history ,@+vertico-multiform-maximal)))
  
  (setq vertico-multiform-categories
	`(;; Maximal
	  (imenu ,@+vertico-multiform-maximal)
	  ;; Minimal
	  (file ,@+vertico-multiform-minimal
		(vertico-preselect . prompt)
		(vertico-sort-function . +vertico-sort-directories-first))
	  (t ,@+vertico-multiform-minimal)))
  
  (vertico-multiform-mode))

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
;; NOTE: consult-find-args (concat "find . -not ( " "-path */.git* -prune " "-or -path */.cache* -prune )")
(use-package consult
  :ensure t
  :bind ((+buffer-prefix-map
	  "b" #'consult-buffer)
	 (+search-prefix-map
	  "g" #'consult-ripgrep
	  "l" #'consult-outline
	  "s" #'consult-line)
	 (+file-prefix-map
	  "i" #'consult-find
	  "r" #'consult-recent-file))
  :init (setopt consult-project-function nil) ;; always work from the current directory (use `project-*' commands or `cd' to switch directory)
  :config
  (with-eval-after-load 'pulsar
    (setq consult-after-jump-hook nil) ; reset it to avoid conflicts with my function
    (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
      (add-hook 'consult-after-jump-hook fn)))
  
  (consult-customize
   consult-bookmark consult--source-buffer consult-recent-file
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark consult--source-project-buffer
   ;; consult-denote-buffer-source consult-denote-subdirectory-source consult-denote-silo-source
   ;; consult-info
   :preview-key "M-."
   consult-theme
   :preview-key (list :debounce 0.3 "M-.")))

(use-package consult-imenu
  :bind (+search-prefix-map "C-i" #'consult-imenu
			    "i" #'consult-imenu-multi))

(use-package consult-dir
  :ensure t
  :bind ((+file-prefix-map
	  "d" #'consult-dir)
	 (vertico-map
	  "C-x C-d" #'consult-dir
	  "C-x C-j" #'consult-dir-jump-file)))

;;;;;;;;;;;;;;;
;;;; corfu ;;;;

(use-package corfu
  :ensure t
  :bind (corfu-map "SPC" #'corfu-insert-separator
		   "TAB" #'corfu-complete
		   "RET" #'+corfu-quit-and-newline
		   "C-h" #'corfu-info-documentation
		   "M-." #'corfu-info-location)
  :init
  (setopt corfu-cycle t
	  corfu-preview-current nil
	  corfu-min-width 20
	  ;; corfu-preselect 'first
	  ;; corfu-scroll-margin 5
	  ;; Enable indentation+completion using the TAB key.
	  tab-always-indent 'complete)
  :config
  (global-corfu-mode)
  (add-hook 'meow-insert-exit-hook 'corfu-quit)
  (defun +corfu-quit-and-newline ()
    "Quit Corfu completion and insert newline."
    (interactive)
    (progn
      (corfu-quit)
      (newline))))

(use-package corfu-popupinfo
  :after corfu
  :bind (corfu-map
	 "C-v" #'corfu-popupinfo-scroll-up
	 "M-v" #'corfu-popupinfo-scroll-down
	 "C-d" #'corfu-popupinfo-scroll-up
	 "C-u" #'corfu-popupinfo-scroll-down
	 [remap corfu-info-documentation] #'corfu-popupinfo-toggle)
  :init (setopt corfu-popupinfo-delay '(0.25 . 0.25)
		corfu-global-mode '((not eshell-mode)))
  :config (corfu-popupinfo-mode 1))

;; NOTE: corfu-quick

(use-package corfu-history
  :after corfu
  :config
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

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
  :init
  ;; (setopt project-switch-commands '((?b "Buffer" project-switch-to-buffer)
  ;; 				    (?d "Dired" project-dired)
  ;; 				    (?e "Eshell" project-eshell)
  ;; 				    (?f "Find file" project-find-file)
  ;; 				    (?g "Find regexp" project-find-regexp)
  ;; 				    (?k "Kill buffers" project-kill-buffers)
  ;; 				    (?r "Query replace" project-query-replace-regexp)
  ;; 				    (?v "Magit" magit-project-status)
  ;; 				    (?! "Shell command" project-shell-command)))
  :bind ((+project-prefix-map
	  "b" #'project-switch-to-buffer
	  "d" #'project-dired
	  "e" #'project-eshell
	  "f" #'project-find-file
	  ;; "g" #'project-find-regexp
	  "g" #'+project-consult-ripgrep
	  "k" #'project-kill-buffers
	  "p" #'project-switch-project
	  "r" #'project-query-replace-regexp
	  "!" #'project-shell-command))
  :config
  (defun +project-consult-ripgrep (&optional dir initial)
    "Search with `rg' for files in DIR with INITIAL input with `consult-project-function'
set to the default project function."

    (interactive)
    (let ((consult-project-function 'consult--default-project-function))
      (consult-ripgrep dir initial))))

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
	 "K" #'delete-other-windows
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
	  popper-reference-buffers (append +popper-reference-buffers '())
	  popper-mode-line "")
  :config
  ;; TODO add popper-druid-modeline patch to popper-extras
  ;; Popper attempts to set `mode-line-format' for all popup buffers, but it
  ;; uses the default value which overrides the buffer-local value set by
  ;; `druid-modeline'. This is a small patch to make it stop using the default
  ;; value. Eventually I'd like to see a customization option to leave the
  ;; mode-line alone, but there are still some bugs to fix.
  ;; See https://github.com/karthink/popper/issues/38
  (defun popper--modified-mode-line ()
    "Return modified mode-line string."
    (when popper-mode-line
      (if (consp mode-line-format)
          (if (member popper-mode-line mode-line-format)
              mode-line-format
            (append (cl-subseq (default-value 'mode-line-format) 0 popper-mode-line-position)
		    (append (cl-subseq mode-line-format 0 popper-mode-line-position)
			    (list popper-mode-line
				  (nthcdr popper-mode-line-position
					  (default-value 'mode-line-format)))))
            (nthcdr popper-mode-line-position mode-line-format))))
    mode-line-format)
  (defun popper--restore-mode-lines (win-buf-alist)
    "Restore the default value of `mode-line-format'.
This applies to popup-buffers in the list WIN-BUF-ALIST."
    (dolist (buf (mapcar 'cdr win-buf-alist))
      (when (buffer-live-p buf)
	(with-current-buffer buf
          (setq mode-line-format (default-value 'mode-line-format))
          (setq mode-line-format (delete popper-mode-line mode-line-format))
          (force-mode-line-update)))))

  (popper-mode))

;; NOTE: switchy-window https://github.com/emacsmirror/switchy-window
;; NOTE: ace-window https://github.com/abo-abo/ace-window

;;;;;;;;;;;;;;
;;;; fold ;;;;

;;;;;;;;;;;;;;;;
;;;; narrow ;;;;

(use-package narrow
  :no-require
  :bind ((meow-normal-state-keymap
	  "[" #'backward-page
	  "]" #'forward-page)
	 (+buffer-prefix-map
	  "n" #'narrow-to-region
	  "w" #'widen)))

(use-package olivetti
  :ensure t
  :init (setq olivetti-body-width 0.7
	      olivetti-minimum-body-width 80))

(use-package logos
  :ensure t
  :bind (((:global-map)
	  [remap narrow-to-region] #'logos-narrow-dwim
	  [remap forward-page] #'logos-forward-page-dwim
	  [remap backward-page] #'logos-backward-page-dwim)
	 (+toggle-prefix-map
	  "f" #'logos-focus-mode))
  :init (setopt logos-outlines-are-pages t)
  :config
  (use-package logos-extras)
  (add-hook 'logos-focus-mode-hook #'+logos-olivetti)
  (add-hook 'logos-page-motion-hook #'+logos-recenter-top)
  (add-hook 'logos-page-motion-hook #'+logos-reveal-entry))

;;;;;;;;;;;;;;;;;
;;;; outline ;;;;

;; NOTE: outli https://github.com/jdtsmith/outli
;; NOTE: nbarrientos outline config
;; NOTE: outline-indent https://github.com/jamescherti/outline-indent.el

;;;;;;;;;;;;;;;
;;;; dired ;;;;

(use-package dired
  :bind ((mode-specific-map
	  "d" #'dired-jump)
	 (dired-mode-map
	  ;; Lower keys for commands not operating on all the marked files
	  "a" #'dired-previous-line
	  ;; "b"
	  "d" #'dired-flag-file-deletion
	  "e" #'dired-find-file
	  ;; "f"
	  "g" #'revert-buffer
	  "h" #'dired-next-line
	  ;; "i" #'wdired
	  "I" #'dired-maybe-insert-subdir
	  "j" #'dired-goto-file
	  "k" #'dired-do-kill-lines
	  ;; "l"
	  "m" #'dired-mark
	  ;; "n" #'wdired
	  "r" #'dired-do-redisplay
	  "t" #'dired-toggle-marks
	  "u" #'dired-unmark
	  "x" #'dired-do-flagged-delete
	  "y" #'dired-up-directory
	  "'" #'dired-copy-filename-as-kill
	  "C-+" #'dired-create-empty-file
	  "+" #'dired-create-directory
	  "RET" #'dired-find-file
	  ;; sort
	  "o" #'dired-sort-toggle-or-edit ;; wdired?
	  ;; Upper case keys (except !) for operating on the marked files
	  "A" #'dired-do-find-regexp
	  "B" #'dired-do-byte-compile
	  "C" #'dired-do-copy
	  "D" #'dired-do-delete
	  ;; "E"
	  ;; "F"
	  ;; "G" #'dired-do-chgrp ;; FIXME needs a better binding
	  "H" #'dired-do-hardlink
	  ;; "J"
	  ;; "K"
	  "L" #'dired-do-load
	  "M" #'dired-do-chmod
	  "N" #'dired-do-man
	  "O" #'dired-do-chown
	  "P" #'dired-do-print
	  "Q" #'dired-do-find-regexp-and-replace
	  "R" #'dired-do-rename
	  "S" #'dired-do-symlink
	  "T" #'dired-do-touch
	  "U" #'dired-unmark-all-marks
	  ;; "V"
	  ;; "W"
	  "X" #'dired-do-shell-command
	  ;; "Y"
	  "Z" #'dired-do-compress
	  "c" #'dired-do-compress-to
	  "!" #'dired-do-shell-command ;; TODO find better bind
	  "&" #'dired-do-async-shell-command ;; TODO find better bind
	  ;; "TAB" #'dired-subtree-toggle
	  ;; <backtab> #'dired-subtree-remove
	  
	  "~" #'+dired-home-directory))
  :init (setopt dired-switches-in-mode-line nil
		dired-do-revert-buffer t
		dired-vc-rename-file t
		dired-clean-confirm-killing-deleted-buffers nil
		dired-kill-when-opening-new-dired-buffer t
		dired-create-destination-dirs 'ask
		dired-create-destination-dirs-on-trailing-dirsep t
		dired-recursive-copies 'always
		dired-recursive-deletes 'always
		delete-by-moving-to-trash t
		;; Adding human readable units and sorted by date
		dired-listing-switches "-AGhlv --group-directories-first"
		;; Try to guess the target directory for operations.
		dired-dwim-target t
		;; Automatically refresh dired buffers when contents changes.
		dired-auto-revert-buffer #'dired-directory-changed-p)
  :config
  (use-package dired-extras)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package dired-single
  :ensure t
  :bind ((:global-map)
	 [remap dired-find-file] #'dired-single-buffer
	 [remap dired-up-directory] #'dired-single-up-directory))

(use-package dired-narrow
  :ensure t
  :bind (dired-mode-map
	 "/" #'dired-narrow-regexp))

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

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

;; NOTE: go through stuff in `isearch-extras.el' to see which funcs i can delete
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
  (use-package isearch-extras)
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
  :config (use-package mowie-extras))

(use-package move-text
  :ensure t
  :bind ((:global-map)
	 "M-<up>" #'move-text-up
	 "M-<down>" #'move-text-down)
  :config
  (use-package move-text-extras)
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
  :bind (meow-normal-state-keymap
	 "C-," #'er/expand-region
	 "C-." #'er/contract-region)
  :init
  (setopt expand-region-fast-keys-enabled nil))

;; NOTE: highlight surround.el semantic units
;; NOTE: surround.el treesitter-backed semantic units
;; NOTE: surround.el which-key or embark help popups
(use-package surround
  :ensure t
  :bind ((+match-prefix-map
	  "s" #'surround-insert
	  "r" #'surround-change
	  "d" #'surround-delete)))

;; TODO: macrursors or multiple-cursors
;; TODO: macrursors + avy
(use-package macrursors
  :bind (((:global-map)
	  "C-;" #'macrursors-mark-map
	  "M-h" #'macrursors-mark-next-instance-of
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

(use-package help
  :bind (help-map "M-." #'find-function-on-key
		  "C-f" #'describe-function
		  "f" #'describe-face
		  "C-k" #'describe-key
		  "k" #'describe-keymap
		  "C-o" #'describe-symbol
		  "o" #'describe-distribution))

(use-package helpful
  :ensure t
  :bind (((:global-map)
	  [remap describe-function] #'helpful-callable
	  [remap describe-symbol] #'helpful-symbol
	  [remap describe-variable] #'helpful-variable
	  [remap describe-command] #'helpful-command
	  [remap describe-key] #'helpful-key)
	 (help-map "." #'helpful-at-point))
  :custom (helpful-max-buffers 1))

(use-package transient
  :xdg-state
  (transient-history-file "history.el")
  (transient-levels-file "levels.el")
  (transient-values-file "values.el"))

;;;;;;;;;;;;;;
;;;; info ;;;;

;; NOTE: packages' info files never get picked up for me. is it because of emacsWithPackagesFromUsePackage? is it services.emacs?
;; NOTE: info https://github.com/oantolin/emacs-config/blob/696641a592691737ba5a019c67f2af7a6cc09183/init.el#L235-L239
(use-package info
  :bind ((Info-mode-map
	  ;; TODO: extract to meow-collection
	  "y" #'meow-left
	  "h" #'meow-next
	  "a" #'meow-prev
	  "e" #'meow-right
	  "b" #'meow-back-word
	  "B" #'meow-back-symbol
	  "f" #'meow-find
	  "j" #'meow-pop-selection
	  "J" #'meow-reverse
	  "m" #'+match-prefix-map
	  "q" #'meow-quit
	  "t" #'meow-till
	  "v" #'meow-expand-mode
	  "w" #'meow-next-word
	  "W" #'meow-next-symbol
	  "x" #'meow-line
	  "X" #'+meow-expand-line-end
	  "-" #'negative-argument
	  ";" #'repeat
	  "<escape>" #'meow-cancel-selection
	 ;;;;;;;;;;;;;;;
	  "'" #'Info-copy-current-node-name
	  "d" #'Info-directory
	  "o" #'Info-menu ;; (m) menu items of current node
	  "u" #'Info-up
	  "?" #'Info-summary
	  "{" #'Info-next ;; (n) stay on same hierarchical level
	  "}" #'Info-prev ;; (p) stay on same hierarchical level
	  "]" #'Info-forward-node ;; goes through all levels
	  "[" #'Info-backward-node ;; goes through all levels
	  "RET" #'Info-follow-nearest-node
	  "SPC" nil ;; #'Info-scroll-up pages down
	  "DEL" nil ;; #'Info-scroll-down pages up
	  "C-u" #'Info-scroll-down ;; can i somehow scroll only half the screen?
	  "C-d" #'Info-scroll-up
	  "r" nil ;; #'Info-history-forward (r can be used by something else since we don't modify Info buffers)
	  "l" nil ;; reserve for structural-mode #'Info-history-back
	  "C-t" #'Info-history-back
	  "C-o" #'Info-history-back
	  "C-i" #'Info-history-forward
	  "L" #'Info-history
	  "<tab>" #'Info-next-reference
	  "S-<tab>" #'Info-prev-reference
	  "t" nil ;; #'Info-top-node
	  "<" #'Info-top-node
	  ">" #'Info-final-node
	  "O" #'Info-goto-node ;; (g) all nodes in info file
	  "F" #'Info-follow-reference ;; referece items in node (not menu items)
	  "i" #'Info-index ;; search the info manual's index
	  "," nil ;; reserve for localleader #'Info-index-next: go to next match of index search
	  "I" #'Info-virtual-index ;; get a virtual index; a node with menu items of matching index entries
	  "s" nil ;; reserve for avy #'Info-search (probably just use isearch or consult-line)
	  "A" #'info-apropos ;; searches the index across all info manuals (good as long as i don't ever need eldoc in Info buffers)
	  )))

;; NOTE: info-colors
;; NOTE: info-variable-pitch https://github.com/kisaragi-hiu/info-variable-pitch

;;;;;;;;;;;;;;;;;
;;;; linting ;;;;

;; TODO: (:lint keyword) flymake-hook from flymake-collection  https://github.com/mohkale/flymake-collection
;; TODO: flymake
(use-package flymake
  :bind ((+toggle-prefix-map
	  "d" #'flymake-mode)))

;; TODO: flymake-collection
;; TODO: flymake-margin https://github.com/LionyxML/flymake-margin

;;;;;;;;;;;;;;;;;;;;
;;;; formatting ;;;;

;; TODO don't auto-format on save
(use-package apheleia
  :ensure t
  :config
  (meow-normal-define-key
   '("=" . apheleia-format-buffer))
  (apheleia-global-mode))

;; Adds :format keyword for use-package forms
(use-package apheleia-use-package)

;;;;;;;;;;;;;;;;;;;;
;;;; treesitter ;;;;

;; TODO: treesit

;; NOTE: treesit before eglot so i can use the build major mode alist
;; NOTE: (:treesit keyword) add recipe to treesit-auto list
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

;; NOTE: ts-docstr https://github.com/emacs-vs/ts-docstr

;;;;;;;;;;;;;
;;;; lsp ;;;;

;; NOTE: (:lsp keyword) eglot-server-programs and eglot-server-configuration?
;; TODO: prevent eglot capf from being in front of templ
(use-package eglot
  ;; :ensure t ;; i want a more recent version of eglot for eglot--apply-text-edits with the silent arg
  :init
  (setopt eglot-extend-to-xref t
	  eglot-autoshutdown t)
  (setq eglot-stay-out-of '(flymake eldoc-documentation-functions eldoc-documentation-strategy "company"))
  :config
  (use-package eglot-extras)
  ;; Auto ensure every possibly relevant major mode
  ;; (+eglot-auto-ensure-all)
  ;; Just add Eglot as another item in Flymake since we asked Eglot to stay
  ;; away from taking over Flymake.
  (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend 90)
  ;; Eglot automatically adds `eglot--mode-line-format' to `mode-line-misc-info'
  ;; I don't like that. Let's remove it.
  (add-hook 'eglot-managed-mode-hook #'+eglot-remove-mode-line-misc-info)
  ;; disable eglot log
  (fset #'jsonrpc--log-event #'ignore))

(use-package eglot-booster
  :config (eglot-booster-mode))

;; TODO: apheleia-eglot
;; NOTE: consult-eglot
;; NOTE: citre https://github.com/universal-ctags/citre

;;;;;;;;;;;;;
;;;; dap ;;;;

;; NOTE: (:dap keyword)
;; NOTE: dape

;;;;;;;;;;;;;;;;;;
;;;; snippets ;;;;

(use-package tempel
  :ensure t
  :init
  ;; NOTE: set up this capf using :completions
  (defun +templ-setup-capf ()
    (setq-local completion-at-point-functions
		(cons #'tempel-complete
		      completion-at-point-functions)))
  (add-hook 'conf-mode-hook '+templ-setup-capf)
  (add-hook 'text-mode-hook '+templ-setup-capf)
  (add-hook 'prog-mode-hook '+templ-setup-capf))

;; NOTE: eglot-tempel https://github.com/fejfighter/eglot-tempel

(use-package tempel-collection :ensure t :after tempel)

;; NOTE: yasnippet-capf (if i decide to use yasnippet) https://github.com/elken/yasnippet-capf

;;;;;;;;;;;;;;;;;
;;;; compile ;;;;

;; NOTE: :compile keyword (compile-multi)
;; TODO: compile
;; NOTE: compile-multi

;;;;;;;;;;;;;;
;;;; prog ;;;;

;; TODO: xref and xref-extras
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
  :init (setq xref-show-xrefs-function #'consult-xref
	      xref-show-definitions-function #'consult-xref))

(use-package eldoc
  :bind (help-map "C-." #'eldoc-print-current-symbol-info)
  :init
  (setopt eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
	  ;; Eldoc resizes the echo area display which is intrusive. Let's not do that.
	  eldoc-echo-area-use-multiline-p nil
	  ;; Skip showing documentation in the echo area and use an `eldoc-doc-buffer'
	  ;; window if it is already displayed.
	  eldoc-echo-area-prefer-doc-buffer t)
  :config
  (use-package eldoc-extras)
  (add-hook 'emacs-lisp-mode-hook #'+eldoc-setup-elisp)
  (add-hook 'eglot-managed-mode-hook #'+eldoc-setup-eglot))

;; NOTE: colorful-extra-color-keyword-functions warning
(use-package colorful-mode
  :ensure t
  :hook (prog-mode text-mode)
  :bind (+toggle-prefix-map
	 "c" #'colorful-mode
	 "C-c" #'global-colorful-mode)
  :init
  (setopt colorful-use-prefix t
	  colorful-extra-color-keyword-functions '(colorful-add-rgb-colors
						   colorful-add-hsl-colors
						   colorful-add-hex-colors))
  :config
  ;; external packages' minor-modes should NOT automatically set keys
  ;; in my global map (C-x c in this case).
  ;; blow it all up so it stops conflicting with my choices
  (setcdr colorful-mode-map nil))

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
  :bind ((magit-mode-map
	  "h" #'magit-section-forward
	  ;; #'magit-dispatch
	  "a" #'magit-section-backward
	  ;; #'magit-cherry-apply
	  )
	 (+vc-prefix-map
	  "s" #'magit-status)
	 (+project-prefix-map
	  "v" #'magit-project-status))
  :init (setopt magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-modes :ensure t)

;; NOTE: consult-git-log-grep https://github.com/ghosty141/consult-git-log-grep
;; NOTE: consult-ls-git https://github.com/rcj/consult-ls-git
;; NOTE: git-commit-ts-mode https://github.com/danilshvalov/git-commit-ts-mode
;; NOTE: emsg-blame https://github.com/ISouthRain/emsg-blame
;; NOTE: consult-gh https://github.com/armindarvish/consult-gh
;; NOTE: conventional-commit https://github.com/akirak/conventional-commit.el

;;;;;;;;;;;;;;
;;;; diff ;;;;

;; NOTE: git-gutter:update-interval warning
(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :init (setopt fringes-outside-margins t
		git-gutter:update-interval 0.05))

(use-package git-gutter-fringe
  :ensure t
  :bind
  ((+vc-prefix-map "h" #'git-gutter:next-hunk
		   "a" #'git-gutter:previous-hunk)
   (+toggle-prefix-map "C-g" #'git-gutter-mode
		       "g" #'global-git-gutter-mode))
  :init (setopt git-gutter-fr:side 'left-fringe)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [#b11111000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [#b11111000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [#b11111000] nil nil '(center repeated)))

;; NOTE: ediff

;;;;;;;;;;;;;;;;;;
;;;; terminal ;;;;

(use-package vterm
  :ensure t
  :bind ((+project-prefix-map
	  "t" #'+project-vterm))
  :init
  (setopt
   ;; vterm-buffer-name-string nil
   ;; vterm-buffer-name-string "*vterm %s*"
   vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x"
			     "M-o" "C-y" "M-y" "C-'" "M-'" "M-:"))
  :config
  (defun +project-suffixed-buffer-name (mode)
    (concat "*"
	    (downcase mode)
	    " "
	    (if-let ((proj (project-current t)))
		(file-name-as-directory (project-root proj))
              (file-name-as-directory
	       (file-name-nondirectory
		(directory-file-name default-directory))))
            "*"))
  ;; TODO fix +project-vterm
  (defun +project-vterm ()
    "Start `vterm' in the current project's root directory.
If a buffer already exists for running vterm in the project's root,
switch to it. Otherwise, create a new vterm buffer.

With \\[universal-argument] prefix-arg, create a new vterm buffer even
if one already exists."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
	   (vterm-buffer-name (+project-suffixed-buffer-name "vterm"))
	   (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
	  (pop-to-buffer-same-window vterm-buffer)
	(vterm vterm-buffer-name)))))

(use-package meow-vterm
  :config
  ;; vterm sometimes does not respect the cursor type.
  ;; Prevent it from ever messing with the cursor by using dynamic binding
  ;; to prevent "set_cursor_type" from changing the cursor.
  ;; https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-1183650463
  (advice-add #'vterm--redraw :around
	      (lambda (fun &rest args)
		(let ((cursor-type cursor-type))
		  (apply fun args))))
  
  (add-hook 'vterm-mode-hook #'meow-vterm-mode)
  (meow-define-keys 'vterm-normal
    '("p" . meow-vterm-yank)
    '("P" . meow-vterm-yank-pop)
    '("u" . meow-vterm-undo)
    '("d" . meow-vterm-kill)
    '("G" . ignore)))

;; NOTE: vterm-capf https://github.com/twlz0ne/vterm-capf
;; NOTE: awscli-capf https://github.com/sebasmonia/awscli-capf
;; NOTE: eshell-visual-vterm https://github.com/accelbread/eshell-visual-vterm
;; NOTE: isend-mode? https://github.com/ffevotte/isend-mode.el
;; NOTE: mistty? https://github.com/szermatt/mistty

(use-package eat
  :ensure t
  :disabled t
  :config (add-hook 'eshell-mode-hook #'eat-eshell-mode))

;;;;;;;;;;;;;;;;
;;;; comint ;;;;

;; NOTE: comint
;; NOTE: comint-fold https://github.com/jdtsmith/comint-fold

;;;;;;;;;;;;;;;;
;;;; eshell ;;;;

;; NOTE: if mistty or vterm doesn't suit my needs, look into setting up eshell with all the bells and whistles (pcmpl, aliases, syntax highlighting, prompt, etc.)
;; NOTE: eshell
;; NOTE: karthink eshell buffer redirection
(use-package eshell
  :xdg-state
  (eshell-aliases-file "aliases")
  (eshell-directory-name "")
  (eshell-login-script "login")
  (eshell-rc-script "rc"))

(use-package em-hist)

;; NOTE: if eshell-atuin isn't to my liking, karthink has some config around
;; eshell history that might be worth looking at
(use-package eshell-atuin
  :ensure t
  :bind (eshell-hist-mode-map
	 "<up>" #'eshell-atuin-history
	 "<down>" #'eshell-atuin-history
	 "M-n" #'eshell-atuin-history
	 "M-p" #'eshell-atuin-history
	 "M-r" #'eshell-atuin-history)
  ;; :init
  ;; (setopt eshell-atuin-search-fields '(time duration command directory host)
  ;; 	  eshell-atuin-history-format "%-80c %-40i %>10t %d %h")
  :config (eshell-atuin-mode))

(use-package eshell-syntax-highlighting
  :ensure t
  :config (add-hook 'eshell-mode-hook #'eshell-syntax-highlighting-global-mode))

;;;;;;;;;;;;;;;
;;;; shell ;;;;

(use-package bash-completion
  :ensure t
  :disabled t
  :config
  ;; NOTE add to bash-completion-extras
  (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
  (defun +eshell-bash-completion-capf-nonexclusive ()
    "Bash completion function for `completion-at-point-functions'.

Returns the same list as the one returned by
`bash-completion-dynamic-complete-nocomint' appended with \(:exclusive no)
 so that other completion functions are tried when bash-completion fails to
match the text at point."
    
    (let* ((bol-pos (save-mark-and-excursion
		      (eshell-bol)
		      (point)))
	   (compl (bash-completion-dynamic-complete-nocomint
		   bol-pos
		   (point) t)))
      (when compl
	(append compl '(:exclusive no)))))
  (defun +eshell-setup-bash-completion-h ()
    (add-hook 'completion-at-point-functions
	      #'+eshell-bash-completion-capf-nonexclusive nil t))
  
  (add-hook 'eshell-mode-hook #'+eshell-setup-bash-completion-h))

(use-package fish-completion
  :ensure t
  :config (global-fish-completion-mode))

;; NOTE: dwim-shell-commad https://github.com/xenodium/dwim-shell-command

;;;;;;;;;;
;; repl ;;

;; NOTE repl-toggle
;; SPC t r

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

;;;;;;;;;;;
;;;; c ;;;;

;; (setopt eglot-server-programs
;; 	(nconc '(((c-mode c-ts-mode c++-mode c++-ts-mode)
;; 		  . ("clangd"
;; 		     ;; Performance
;; 		     "-j=8"
;; 		     "--background-index"
;; 		     "--background-index-priority=normal"
;; 		     "--pch-storage=memory"
;; 		     ;; Formatting
;; 		     "--fallback-style=Google"
;; 		     ;; Completion
;; 		     "--clang-tidy"
;; 		     "--all-scopes-completion"
;; 		     "--completion-style=detailed"
;; 		     "--function-arg-placeholders"
;; 		     "--header-insertion=never"
;; 		     "--header-insertion-decorators")))
;; 	       eglot-server-programs))

;;;;;;;;;;;;
;;;; go ;;;;

;;;;;;;;;;;;;;
;;;; rust ;;;;

;; NOTE: cargo-jump-xref https://github.com/eval-exec/cargo-jump-xref.el

;;;;;;;;;;;;;
;;;; nix ;;;;

(use-package nix-mode
  ;; :formatter (alejandra . ("alejandra")) nix-mode
  ;; :formatter (alejandra) nix-ts-mode
  :ensure t)

(use-package nix-ts-mode
  :ensure t
  :formatter (nixfmt) nix-ts-mode
  :init
  (add-hook 'nix-ts-mode-hook #'eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (when-let ((server (assoc 'nix-mode eglot-server-programs)))
      (setcar server '(nix-mode nix-ts-mode))))
  ;; (with-eval-after-load 'apheleia
  ;; (cl-pushnew '((nix-mode nix-ts-mode) . nixfmt) apheleia-mode-alist :test #'equal))
  (setopt
   ;; eglot-server-programs
   ;; (nconc '((((nix-mode :language-id "nix")
   ;; 	      (nix-ts-mode :language-id "nix"))
   ;; 	     . ("nixd")))
   ;; 	  eglot-server-programs)
   eglot-workspace-configuration
   (nconc '((:nixd . (:nixpkgs (:expr "import <nixpkgs> { }") ;; "import (builtins.getFlake \"/PATH/TO/FLAKE\").inputs.nixpkgs { }"
		      :formatting (:command ["nixfmt"])
		      :options (:nixos (:expr "(builtins.getFlake \"/home/evermind/nix-dotfiles\").nixosConfigurations.nixos.options")
				;; "(builtins.getFlake \"github.com:vimjoyer/nixconf\").nixosConfigurations.CONFIGNAME.options"
				;; "(builtins.getFlake (\"git+file://\" + toString ./.)).nixosConfigurations.CONFIGNAME.options"
				:home_manager (:expr "(builtins.getFlake \"/home/evermind/nix-dotfiles\").homeConfigurations.\"evermind@nixos\".options")))))
	  eglot-workspace-configuration)))

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
  :xdg-state (org-id-locations-file ".org-id-locations")
  :bind
  ((+org-prefix-map "c" #'org-capture)
   (org-mode-map "C-'" 'nil))
  :init
  (setopt org-return-follows-link t
	  org-startup-folded 'content
	  org-ellipsis " ▾"
	  org-imenu-depth 7
	  org-cycle-separator-lines 0
	  org-structure-template-alist '(("s" . "src")
					 ("e" . "src emacs-lisp")
					 ("x" . "example")
					 ("X" . "export")
					 ("q" . "quote"))
	  org-fontify-quote-and-verse-blocks t
	  org-fontify-whole-heading-line t
	  org-bookmark-names-plist nil)
  :config
  ;; Open Org links in current window. Default is find-file-other-window.
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  
  (with-eval-after-load 'pulsar
    (dolist (hook '(org-follow-link-hook))
      (add-hook hook #'pulsar-recenter-middle)
      (add-hook hook #'pulsar-reveal-entry))))

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda))
  :init (setopt org-modern-star 'replace))

;; NOTE: org-modern-indent https://github.com/jdtsmith/org-modern-indent

;; TODO: org-gtd (tasks) https://github.com/Trevoke/org-gtd.el
;; TODO: org-edna (next is wonky)
(use-package org-gtd
  :ensure t
  :bind ((+org-prefix-map
	  "C-c" #'org-gtd-capture
	  "e" #'org-gtd-engage
	  "p" #'org-gtd-process-inbox)
	 (org-gtd-clarify-map
	  "C-c c" #'org-gtd-organize))
  :init
  (setopt org-gtd-directory "~/OneDrive/zettelkasten/"
	  org-edna-use-inheritance t
	  org-gtd-organize-hooks nil
	  org-gtd-update-ack "3.0.0")
  :config
  (org-edna-mode)
  ;; these are defconst so i have to set them here instead
  (setq org-gtd-inbox "inbox"
	org-gtd-default-file-name "tasks"))

;; NOTE: doesn't work very well or well without corfu-auto
(use-package corg
  :init (add-hook 'org-mode-hook 'corg-setup))

;; NOTE: org-transclusion-http https://github.com/alphapapa/org-transclusion-http
;; NOTE: org-inline-tags https://github.com/incandescentman/org-inline-tags
;; NOTE: org-super-links https://github.com/toshism/org-super-links
;; NOTE: org-recur https://github.com/mrcnski/org-recur
;; NOTE: org-tidy https://github.com/jxq0/org-tidy
;; NOTE: org-mind-map https://github.com/the-ted/org-mind-map

;;;;;;;;;;;;;;;;;;;;
;;;; org-agenda ;;;;

(use-package org-agenda
  :bind (+agenda-prefix-map
	 "a" #'org-agenda))

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

(use-package parsebib)

;; NOTE: citar
(use-package citar
  ;; :ensure t
  ;; :commands citar--bibliography-files
  :bind ((mode-specific-map
	  "l" +bibliography-prefix-map)
	 (+bibliography-prefix-map
	  "l" #'citar-open))
  :init
  (setopt citar-select-multiple nil
	  citar-bibliography '("~/OneDrive/zettelkasten/reference/bibliography.bib")
	  citar-library-paths '("~/OneDrive/zettelkasten/reference/")
	  citar-notes-paths '("~/OneDrive/zettelkasten/")
	  citar-templates
	  '((main . "${title:55} ${author editor:55} ${date year issued:4}")
	    (suffix . "  ${tags keywords keywords:40}")
	    (preview . "${author editor} ${title}, ${journal publisher container-title collection-title booktitle} ${volume} (${year issued date}).\n")
	    (note . "#+title: Notes on ${author editor}, ${title}")))
  :config
  (use-package citar-extras)
  (setq citar-indicators
	(list
	 ;; +citar-indicator-cited-icons ;; +citar-indicator-links-icons
	 +citar-indicator-files-icons +citar-indicator-notes-icons)))

(use-package persid)

(use-package biblio
  :ensure t
  :bind (+bibliography-prefix-map
	 "a" #'+biblio-lookup)
  :config (use-package biblio-extras))

;; NOTE: create biblio-persid if sufficient

;; NOTE: biblio-openlibrary https://github.com/fabcontigiani/biblio-openlibrary
(use-package biblio-openlibrary)

;; NOTE: biblio-zotero https://github.com/gkowzan/biblio-zotero
;; NOTE: biblio-bibsonomy https://github.com/andreasjansson/biblio-bibsonomy.el
;; NOTE: biblio-gscholar https://github.com/seanfarley/biblio-gscholar.el

(use-package biblio-gbooks :ensure t)

;;;;;;;;;;;;;;;
;;;; notes ;;;;

;; NOTE: denote-dired-mode in specific directories
;; NOTE: appendfilename (utility to append string to title/file name of marked files)
;; NOTE: date2name? (utility to rename file with current timestamp)
;; NOTE: filetags? (utility to append keywords to marked files, or remove keywords with -KEYWORD)
;; NOTE: controlled vocabulary (known keywords) should be limited, the fewer the better (never more than 100)
;; NOTE: keywords should always be in plural and general
(use-package denote
  :ensure t
  :bind (+notes-prefix-map
	 "d" #'+denote-dired-jump
	 "i" #'denote-link-or-create
	 "l" #'denote-find-backlink
	 "C-l" #'denote-find-link
	 "n" #'denote
	 "r" #'denote-rename-file-using-front-matter
	 "C-r" #'denote-rename-file
	 "t" #'denote-keywords-remove
	 "C-t" #'denote-keywords-add)
  :init
  (setopt denote-directory (expand-file-name "~/OneDrive/zettelkasten/"))
  (defun +denote-dired-jump ()
    (interactive)
    (dired denote-directory)
    (diredfl-mode -1)
    (denote-dired-mode 1)))

;; NOTE: remove denote buffers from consult Buffers group, leave only in Denote buffers group (use consult-customize?)
(use-package consult-denote
  :ensure t
  :bind ((+file-prefix-map
	  "n" #'consult-denote-find)
	 (+search-prefix-map
	  "n" #'consult-denote-grep))
  :init (setopt consult-denote-find-command #'consult-find)
  :config (consult-denote-mode))

;; NOTE: blk https://github.com/mahmoodsh36/blk
;; NOTE: denote-explore https://github.com/pprevos/denote-explore
;; NOTE: org-remark (is this better than org-zettel-ref-mode?) https://github.com/nobiot/org-remark
(use-package org-remark
  :ensure t
  :bind ((+notes-prefix-map
	  "]" #'org-remark-view-next
	  "[" #'org-remark-view-prev
	  "C-k" #'org-remark-remove
	  "k" #'org-remark-delete
	  "m" #'org-remark-mark-line
	  "C-m" #'org-remark-mark
	  "o" #'org-remark-open
	  "C-o" #'org-remark-open))
  :config
  (use-package org-remark-info :after info :config (org-remark-info-mode +1))
  (use-package org-remark-eww :after eww :config (org-remark-eww-mode +1))
  (use-package org-remark-nov :after nov :config (org-remark-nov-mode +1))
  (org-remark-global-tracking-mode)
  (org-remark-create "red-line"
		     '(:underline (:color "dark red" :style wave))
		     '(CATEGORY "review" help-echo "Review this"))
  (org-remark-create "yellow"
		     '(:underline "gold" :background "lemon chiffon")
		     '(CATEGORY "important"))
  (org-remark-create "line-alt"
		     'diff-hunk-header
		     '(org-remark-type-line)))

;; NOTE: annotate https://github.com/bastibe/annotate.el

(use-package org-noter
  :disabled t
  :ensure t
  :bind (org-noter-doc-mode-map
	 "M-h" #'org-noter-sync-next-note
	 "M-a" #'org-noter-sync-prev-note
	 "C-M-h" #'org-noter-sync-next-page-or-chapter
	 "C-M-a" #'org-noter-sync-prev-page-or-chapter)
  :init (setopt org-noter-always-create-frame nil
		org-noter-use-indirect-buffer nil
		org-noter-kill-frame-at-session-end nil
		org-noter-notes-search-path (list denote-directory))
  :config
  (use-package org-noter-extras)
  (setq org-noter--show-arrow-hook '()) ;; default always gives an error, can't be bothered to fix
  (setq org-noter-create-session-from-document-hook '(+org-noter-denote--create-session-from-document-file)))

;;;;;;;;;;;;;
;;;; pdf ;;;;

(use-package pdf-tools
  :ensure t
  :hook
  (pdf-view-mode . (lambda () (progn
				(blink-cursor-mode -1)
				(display-line-numbers-mode -1)
				(hl-line-mode -1))))
  :bind (pdf-view-mode-map
	 "h" #'pdf-view-next-line-or-next-page ;; NOTE: gets overriden by meow-next
	 "a" #'pdf-view-previous-line-or-previous-page ;; NOTE: gets overriden by meow-prev
	 "y" #'image-backward-hscroll
	 "e" #'image-forward-hscroll
	 "H" #'pdf-view-next-page-command
	 "A" #'pdf-view-previous-page-command)
  :init
  (setopt large-file-warning-threshold nil)
  (setq-default pdf-view-display-size 'fit-page)
  :config
  (require 'pdf-occur)
  (require 'pdf-history)
  (require 'pdf-links)
  (require 'pdf-outline)
  (require 'pdf-annot)
  (require 'pdf-sync)
  (pdf-tools-install :no-query))

;; Add support for pdf-view and DocView buffers to `save-place'.
(use-package saveplace-pdf-view :ensure t)

;;;;;;;;;;;;;;
;;;; epub ;;;;

(use-package nov
  :ensure t
  :xdg-state (nov-save-place-file "nov-save-place.el")
  :bind ((nov-mode-map
	  ;; TODO extract to meow-collection
	  "y" #'meow-left
	  "h" #'meow-next
	  "a" #'meow-prev
	  "e" #'meow-right
	  "b" #'meow-back-word
	  "B" #'meow-back-symbol
	  "f" #'meow-find
	  "j" #'meow-pop-selection
	  "J" #'meow-reverse
	  "m" #'+match-prefix-map
	  "q" #'meow-quit
	  "t" #'meow-till
	  "v" #'meow-expand-mode
	  "w" #'meow-next-word
	  "W" #'meow-next-symbol
	  "x" #'meow-line
	  "X" #'+meow-expand-line-end
	  "-" #'negative-argument
	  ";" #'repeat
	  "<escape>" #'meow-cancel-selection
	  ;;;;;;;;;;;;;
	  "SPC" nil ;; #'nov-scroll-up pages down
	  "DEL" nil ;; #'nov-scroll-down pages up
	  "C-u" #'nov-scroll-down ;; can i somehow scroll only half the screen?
	  "C-d" #'nov-scroll-up
	  "C-i" #'nov-history-forward
	  "C-o" #'nov-history-back
	  "C-t" #'nov-history-backward
	  "l" nil ;; #'nov-history-backward
	  "r" nil ;; #'nov-history-forward
	  "[" #'nov-previous-document ;; prev chapter
	  "]" #'nov-next-document ;; next chapter
	  "p" nil ;; #'nov-previous-document
	  "n" nil ;; #'nov-next-document
	  "d" #'nov-goto-toc ;; (t)
	  "i" #'nov-goto-toc ;; (t)
	  "C" #'nov-view-content-source ;; (V)
	  "c" #'nov-view-source ;; (v)
	  "A" #'nov-reopen-as-archive ;; (a)
	  "?" #'nov-display-metadata ;; (m) 
	  "g" #'nov-render-document))
  :init
  ;; Activate nov-mode for epub files
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  ;; Implement more flexible text filling?
  (setq nov-text-width 80)
  (add-hook 'nov-mode-hook #'virtual-auto-fill-mode))

(use-package djvu :ensure t)

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
