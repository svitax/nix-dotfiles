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
(setq max-specpdl-size 10000)

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

;; Additional load paths.
;; "plugins/" contains downloaded packages or plugins I've written.
(add-to-list 'load-path (concat user-emacs-directory "plugins"))

;; Load no-litter early.
(use-package no-litter)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package extensions ;;

;; TODO: gcmh
;; TODO: validate https://github.com/xenodium/dotsies/blob/main/emacs/features/fe-packages-extensions.el#L56

;; TODO: (:completion keyword) completion+ from mohkale

;; TODO: (:lint keyword) flymake-hook from flymake-collection  https://github.com/mohkale/flymake-collection

;; TODO: (:format keyword) apheleia-use-package https://github.com/VojtechStep/apheleia-use-package.el

;; TODO: (:treesit keyword) add recipe to treesit-auto list

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
	:custom (modus-themes-common-palette-overrides `((fringe unspecified))))

(use-package themes-extras
	:init
	(add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces)
	(add-hook 'ef-themes-post-load-hook #'my/ef-themes-custom-faces)
	:config
	(modus-themes-select 'modus-operandi-tinted))

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

(use-package indent
	:hook (after-change-major-mode . +set-indent-offset))

(use-package hide-whitespace
	:hook (change-major-mode-after-body . +set-trailing-whitespace))

;; display-line-numbers

(use-package dot-fringe)

;; TODO: better variable pitch mode for org
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

(use-package with-editor
	:ensure t
	:hook ((eshell-mode . with-editor-export-editor)
					(term-exec . with-editor-export-editor)
					(shell-mode . with-editor-export-editor)
					(vterm-mode . with-editor-export-editor)))

;; envrc
;; inheritenv
;; exec-path-from-shell
;; buffer-env https://github.com/astoff/buffer-env

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
	(large-file-warning-threshold nil)
	:config
	(defun +files-create-non-existent-directory ()
		"Create a non-existent directory."
		(when-let* ((file-name buffer-file-name)
								 (parent-directory (file-name-directory file-name)))
			(when (and (not (file-exists-p parent-directory))
							(y-or-n-p (format "Create `%s' dis? " parent-directory)))
				(make-directory parent-directory t))))

	(add-to-list 'find-file-not-found-functions
		#'+files-create-non-existent-directory))

(use-package autorevert
	:init (global-auto-revert-mode)
	:custom
	;; Be quiet about reverts
	(global-auto-revert-non-file-buffers t)
	(auto-revert-verbose nil)
	:config
	;; global-auto-revert-mode can slow things down. try to enable it per active window.
	(defun +window-state-state-change (state)
		(let* ((old-selected-window (old-selected-window))
						(old-buffer (when old-selected-window
													(window-buffer old-selected-window)))
						(selected-window (selected-window))
						(new-buffer (when selected-window
													(window-buffer selected-window))))
			(when old-buffer
				(with-current-buffer old-buffer
					(when buffer-file-name
						(auto-revert-mode -1))))
			(when new-buffer
				(with-current-buffer new-buffer
					(when buffer-file-name
						(auto-revert-mode +1)))))))

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

;; TODO: where do i put server?
(use-package server
	:custom
	(server-client-instructions nil))

;;  TODO: find a place to put these variables
;; 	(echo-keystrokes 0.01)
;; 	(cycle-spacing-actions '(delete-all-space just-one-space restore))
;; 	(words-include-escapes t)
;; 	(view-read-only t) ;; TODO: view-mode
;; 	(vc-follow-symlinks t)
;; 	(word-wrap t)

;; TODO: rename-file-and-buffer https://github.com/karthink/.emacs.d/blob/b0ae76c813b133619b2f29b179a7f1ab5193f534/lisp/better-buffers.el#L225-L240
;; TODO: move-buffer-file https://github.com/karthink/.emacs.d/blob/b0ae76c813b133619b2f29b179a7f1ab5193f534/lisp/better-buffers.el#L243-L257

;; TODO: savehist-extras
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

;;;;;;;;;;;;;;;;;;;;
;;;; minibuffer ;;;;
(use-package minibuffer
	:bind ( :map minibuffer-local-completion-map
					("<backtab>" . minibuffer-force-complete))
	:hook
	(completion-list-mode . force-truncate-lines)
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

;;;;;;;;;;;;;;;;;;;;
;;;; marginalia ;;;;

(use-package marginalia
	:ensure t
	:bind (:map minibuffer-local-map
					("M-a" . marginalia-cycle))
	:custom (marginalia-max-relative-age 0)
	:init (marginalia-mode)
	:config
	(defun marginalia--file-owner (attrs) ; Only display UID
		"Return file owner given ATTRS."
		(let ((uid (file-attribute-user-id attrs)))
			(when (/= (user-uid) uid)
				(or (user-login-name uid) uid))))
	(pcase-dolist (`(,regexp . ,category)
									'(("\\burl\\b" . url)
										 ("\\bHistory\\b" . history)
										 ("\\bdefinitions\\b" . xref-location)
										 ("\\bxref\\b" . xref-location)))
		(setf (alist-get regexp marginalia-prompt-categories
						nil nil #'equal)
			category))
	(advice-add 'marginalia--buffer-file :filter-return
	  (lambda (buffer-file)
			(string-trim-left buffer-file "(compilation\\(<.+>\\)? run) "))))

;;;;;;;;;;;;;;;;;;;
;;;; orderless ;;;;

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

;;;;;;;;;;;;;;;;;
;;;; vertico ;;;;

(use-package vertico
	:ensure t
	:bind ( :map vertico-map
					("H-m" . +vertico-multiform-monocle)
					("C-l" . +vertico-multiform-unobtrusive)
					("C-M-n" . vertico-next-group)
					("C-M-p" . vertico-previous-group)
					("C-j" . +vertico-really-exit))
	:custom
	(vertico-count 10 "Items displayed, defaults to 10")
	(vertico-cycle t)
	(vertico-resize 'grow-only)
	:preface
	(defun +vertico-multiform-monocle ()
		(interactive)
		(setq-local vertico-buffer-display-action '(display-buffer-full-frame))
		(vertico-multiform-buffer))
	(defun +vertico-multiform-unobtrusive ()
		"Toggle between vertico-unobtrusive/vertico-flat and default vertico."
		(interactive)
		(vertico-multiform-vertical))
	(defun +vertico-really-exit ()
		(interactive)
		(if minibuffer--require-match
			(minibuffer-complete-and-exit)
			(exit-minibuffer)))
	:init (vertico-mode))

(use-package vertico-directory
	:after vertico
	:hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
	:bind ( :map vertico-map
					("DEL" . vertico-directory-delete-char)
					("M-DEL" . vertico-directory-delete-word)
					("RET" . vertico-directory-enter)))

;; TODO: vertico-multiform from karthink and oantolin
(use-package vertico-multiform
	:after vertico
	:init (vertico-multiform-mode)
	:custom
	(vertico-multiform-commands
		'(("\\`execute-extended-command" flat
				(vertico-flat-annotate . t)
				(marginalia-annotator-registry (command marginalia-annotate-binding)))
			 ;; (magit:--author flat)
			 )))

(use-package vertico-grid
	:after vertico
	:custom
	(vertico-grid-separator "    ")
	(vertico-grid-lookahead 50))

(use-package vertico-quick
	:after vertico
	:bind ( :map vertico-map
					("M-<tab>" . vertico-quick-insert)
					("'" . vertico-quick-jump)
					("C-t" . vertico-quick-exit)
					("M-t" . vertico-quick-embark))
	:config
	(defun vertico-quick-embark (&optional arg)
		"Embark on candidate using quick keys."
		(interactive)
		(when (vertico-quick-jump)
			(embark-act arg))))

(use-package vertico-repeat
	:after vertico
	:hook (minibuffer-setup . vertico-repeat-save)
	;; TODO: i might want this bond for `recompile'
	:bind (("C-x ." . vertico-repeat))
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
					;; ("M-." . embark-dwim)
					("C-M-." . embark-act-all)
					:map help-map
					("b" . embark-bindings)
					("C-b" . describe-bindings)
					("H-m" . embark-bindings-in-keymap)
					("C-e" . +embark-on-last-message)
					("C-h" . embark-prefix-help-command)
					:map embark-general-map
					("C-<tab>" . +embark-select)
					:map embark-collect-mode-map
					("a")
					("." . embark-act)
					("C-<tab>" . +embark-select)
					("F" . consult-focus-lines)
					:map vertico-map
					("C-<tab>" . +embark-select)
					("M-s o" . embark-export))
	:custom
	(embark-cycle-key ".")
	(embark-quit-after-action nil)
	(embark-confirm-act-all nil)
	(prefix-help-command #'embark-prefix-help-command "Use embark instead of `describe-prefix-bindings'")
	(embark-indicators '(embark-minimal-indicator
												embark-highlight-indicator
												embark-isearch-highlight-indicator))
	:config
	(defun +embark-on-last-message (arg)
		"Act on the last message displayed in the echo area."
		(interactive "P")
		(with-current-buffer "*Messages*"
			(goto-char (1- (point-max)))
			(embark-act arg)))

	(defun +embark-select ()
		(interactive)
		(prog1 (embark-select)
			(if (minibufferp)
				(when (bound-and-true-p vertico-mode)
					(vertico-next))
				(call-interactively #'next-line)))))

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
					("M-f" . consult-focus-lines)
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

	;; TODO: put these in consult-extras
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
	(corfu-auto-delay 0.5)
	(corfu-count 10)
	(corfu-cycle t)
	(corfu-preview-current nil)
	(corfu-preselect 'first)
	(corfu-scroll-margin 5))

(use-package corfu-quick
	:after corfu
	:bind ( :map corfu-map
					("C-t" . corfu-quick-complete)
					("'" . corfu-quick-jump)))

(use-package corfu-popupinfo
	:after corfu
	:custom (corfu-popupinfo-delay '(1.0 . 0.5))
	:config (corfu-popupinfo-mode 1)
	:bind ( :map corfu-map
					([remap corfu-info-documentation] . corfu-popupinfo-toggle)))

(use-package corfu-history
	:after corfu
	:init (corfu-history-mode 1)
	:config (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-extras
	:hook ((minibuffer-setup . +corfu-enable-always-in-minibuffer)))

;;;;;;;;;;;;;;;;;;;;;
;;;; completions ;;;;

(use-package completions)

;;;;;;;;;;;;;;
;;;; cape ;;;;

(use-package cape
	:ensure t
	:completions
	(prog-mode (cape-file :depth 5)
		;; Support keyword completions up to 3 chars. After that fallback to mode
		;; specific completion and barring that back to keywords. Prevents keyword
		;; matching obstructing language server mappings.
		(+cape-keyword-with-length-limit :depth 10)
		(cape-keyword :depth 50))
	(org-mode (cape-elisp-block :depth -5)
		(cape-file :depth 5))
	:preface
	(defun +cape-wrap-max-prefix-length (capf length)
		"Call CAPF and ensure that prefix length is greater or equal than LENGTH.
If the prefix is long enough, enforce auto completion."
		(pcase (funcall capf)
			(`(,beg ,end ,table . ,plist)
				(when (<= (- end beg) length)
					`(,beg ,end ,table
						 :company-prefix-length t
						 ,@plist)))))

	(defun +cape-capf-max-prefix-length (capf length)
		(lambda () (funcall #'+cape-wrap-max-prefix-length capf length)))
	:init (setq-default completion-at-point-functions '())
	:config (defalias '+cape-keyword-with-length-limit (+cape-capf-max-prefix-length #'cape-keyword 2)))

;; TODO: cape extras?

;;;;;;;;;;;;;;;;;
;;;; project ;;;;

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

;;;;;;;;;;;;;;;;;
;;;; buffers ;;;;

(use-package buffers-extras
	:bind (:map ctl-x-map
					("k" . kill-current-buffer)))

;; ("m" . +move-buffer-file) ;; move buffer and file to DIR
;; ("r" . +rename-file-and-buffer) ;; rename current buffer and file to NEW-NAME

;; ibuffer https://www.reddit.com/r/emacs/s/Ft0yZxEMVD
;; ibuffer-git https://github.com/jrockway/ibuffer-git
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

(use-package popper
	:ensure t
	:bind
	("C-'" . popper-toggle)
	("M-'" . popper-cycle)
	("C-M-'" . popper-toggle-type)
	:custom
	(popper-window-height 0.40)
	(popper-echo-lines 1)
	(popper-reference-buffers
		'("\\*Messages\\*"
			 "[Oo]utput\\*$"
			 "\\*Completions\\*"
			 "\\*compilation\\*"
			 "\\*Async Shell Command\\*"
			 "\\*Detached Shell Command\\*"
			 "\\*Bufler\\*"
			 "\\*IBuffer\\*"
			 "^\\*Apropos"
			 "^\\*eldoc\\*"
			 "^\\*ielm\\*"
			 "^\\*gptel-ask\\*"
			 ("^\\*Warnings\\*$" . hide)
			 pdf-outline-mode
			 help-mode
			 helpful-mode
			 Man-mode
			 woman-mode
			 eshell-mode
			 shell-mode
			 eat-mode
			 vterm-mode
			 inferior-python-mode
			 fennel-repl-mode
			 jupyter-repl-mode
			 Custom-mode
			 compilation-mode
			 edebug-eval-mode
			 messages-buffer-mode))
	:init
	(popper-mode)
	(popper-echo-mode))

;;;;;;;;;;;;;;;
;;;; dired ;;;;

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
;; dwim-shell-command for dired

;;;;;;;;;;;;;;;;;
;;;; editing ;;;;

(use-package editing-extras
	:custom
	(cycle-spacing-actions '(just-one-space delete-all-space restore))
	:config
	(my/with-region-or-line comment-or-uncomment-region)
	(my/with-region-or-line kill-ring-save)
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
					("M-/" . undo-redo)
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

;; TODO: easy-kill

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
	:hook ((prog-mode . subword-mode))
	:bind ("M-DEL" . +backward-delete-subword)
	:config
	;; From	http://stackoverflow.com/questions/6133799/delete-a-word-without-adding-it-to-the-kill-ring-in-emacs
	(defun +backward-delete-subword (arg)
		"Delete characters backward until encountering the beginning of a word.
With argument ARG, do it that many times."
		(interactive "p")
		(delete-region (point)
			(progn
				(subword-backward arg)
				(point)))))

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

;; help
;; helpful
;; helpful embark
;; transient
;; casual-avy https://github.com/kickingvegas/casual-avy
;; casual-isearch https://github.com/kickingvegas/casual-isearch
;; casual-suite? or embark help?

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

;; keycast

;;;;;;;;;;;;;;
;;;; info ;;;;

;; info https://github.com/oantolin/emacs-config/blob/696641a592691737ba5a019c67f2af7a6cc09183/init.el#L235-L239
;; info-colors
;; info-variable-pitch

;;;;;;;;;;;;;;;;;
;;;; linting ;;;;

;; :lint keyword
;; TODO: flymake
(use-package flymake
	:custom
	(flymake-note-bitmap '(+dot-fringe-bitmap compilation-info))
	(flymake-error-bitmap '(+dot-fringe-bitmap compilation-error))
	(flymake-warning-bitmap '(+dot-fringe-bitmap compilation-warning)))

;; flymake-collection

;;;;;;;;;;;;;;;;;;;;
;;;; formatting ;;;;

;; :format keyword
;; apheleia
;; apheleia-use-package https://github.com/VojtechStep/apheleia-use-package.el
;; apheleia-eglot https://github.com/mohkale/apheleia-lsp

;;;;;;;;;;;;;
;;;; lsp ;;;;

;; :lsp keyword

;; eglot
(use-package eglot
	:custom (eglot-extend-to-xref t))
;; consult-eglot
;; emacs-lsp-booster-nix
;; eglot-booster https://github.com/jdtsmith/egloot-booster

;; ctags
;; citre https://github.com/universal-ctags/citre

;;;;;;;;;;;;;
;;;; dap ;;;;

;; :dap keyword
;; dape
;; edebug

;;;;;;;;;;;;;;;;;;
;;;; snippets ;;;;

;; tempel
;; eglot-tempel
;; tempel-collection
;; auto-activating-snippets

;;;;;;;;;;;;;;;;;;;;
;;;; treesitter ;;;;

;; :treesit keyword?
;; treesit
;; treesit-auto

;;;;;;;;;;;;;;;;;
;;;; compile ;;;;

;; :compile keyword (compile-multi)
(use-package compile
	:config
	(add-hook 'next-error-hook #'pulsar-pulse-line-red)
	(add-hook 'next-error-hook #'pulsar-recenter-top)
	(add-hook 'next-error-hook #'pulsar-reveal-entry))

;; compile-multi
;; consult-compile-multi
;; compile-multi-embark
;; projection-multi
;; projection-multi-embark

;;;;;;;;;;;;;;
;;;; prog ;;;;

;; eldoc

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

;; ct.el https://github.com/neeasade/ct.el

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

;; TODO: evil-matchit https://github.com/redguardtoo/evil-matchit

;; symbols-outline https://github.com/liushihao456/symbols-outline.el
;; imenu-list https://github.com/bmag/imenu-list (alternative to symbols-outline)
;; treesitter-context https://github.com/zbelial/treesitter-context.el

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

;; BUG: diff-hl-margin-mode doesn't play nice with selections and show-paren
;; diff-hl-flydiff-mode is also too inconsistent
(use-package diff-hl
	:ensure t
	:hook ((diff-hl-mode . diff-hl-margin-mode)
					(diff-hl-mode . diff-hl-flydiff-mode))
	:custom
	(diff-hl-side 'left)
	(diff-hl-flydiff-delay 0.8)
	(diff-hl-margin-symbols-alist
		'((insert . "┃")
			 (delete . "┃")
			 (change . "┃")
			 (unknown . "┃")
			 (ignored . "┃")))
	:init (global-diff-hl-mode))


;;;;;;;;;;;;;;;;;;
;;;; terminal ;;;;

;; vterm or eat
;; comint
;; terminal-here?
;; isend-mode https://github.com/ffevotte/isend-mode.el
;; eshell-visual-vterm https://github.com/accelbread/eshell-visual-vterm
;; comint-fold https://github.com/jdtsmith/comint-fold

;;;;;;;;;;;;;;;
;;;; shell ;;;;

;; eshell
;; eshell-syntax-highlighting
;; eshell-bookmark
;; fish-completion?
;; bash-completion?
;; capf-autosuggest?

;;;;;;;;;;;;;;;
;;;; elisp ;;;;

;; elisp
;; elisp-fontification
;; elisp-indentation
;; eros
;; elisp-demos
;; highlight-quoted
;; highlight-numbers

;;;;;;;;;;;;
;;;; go ;;;;

;;;;;;;;;;;;;;
;;;; rust ;;;;

;;;;;;;;;;;;;
;;;; nix ;;;;

(use-package nix-mode
	:ensure t
	:indent (nix-mode 2))
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

;; markdown-mode
;; grip-mode https://github.com/seagle0128/grip-mode

;;;;;;;;;;;;;
;;;; org ;;;;

;; org
;; book-mode https://github.com/rougier/book-mode
;; org-inline-tags
;; org-timeblock https://github.com/ichernyshovv/org-timeblock

;;;;;;;;;;;;;;;;
;;;; biblio ;;;;

;; citar
;; citar-embark
;; citar-denote

;;;;;;;;;;;;;;;
;;;; notes ;;;;

;; denote
;; consult-denote
;; denote-explore
;; org-noter https://github.com/weirdNox/org-noter
;; annotate https://github.com/bastibe/annotate.el

;;;;;;;;;;;;;;;;;
;;;; reading ;;;;

;; pdf-tools
;; saveplace-pdf-view
;; nov
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
