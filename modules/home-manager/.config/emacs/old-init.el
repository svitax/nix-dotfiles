;;; init.el -- My Emacs config
;;; Commentary:
;;; Code:
(use-package emacs
  :defer 0
  :custom
  (enable-recursive-minibuffers t)
  (ring-bell-function #'ignore) ;; Do not make unecessary sounds
  (inhibit-startup-message t) ;; disable the default emacs startup screen
  (inhibit-startup-screen t)
  (inhibit-splash-screen t)
  (gc-cons-threshold (* 50 1000 1000)) ;; The default is 800 kilobytes. Measured in bytes
  (column-number-mode t) ;; line numbers
  (display-line-numbers-type 'relative)
  (visible-bell nil)
  (hl-line-mode t)
  (comp-async-report-warnings-errors nil) ;; silence compiler warnings as they can be pretty disruptive
  ;; (completion-cycle-threshold 3) ;; TAB cycle if there are only few candidates
  (tab-always-indent t) ;; enable indentation+completion using the TAB key
  ;; use-package is automatically added by the nix emacs overlay
  ;; here we just make sure we don't have to keep typing `:ensure t`
  (use-package-always-ensure t)
  ;; reduce the clutter in the fringes; we'd like to reserve that space for more
  ;; useful information, like git-gutter and flymake
  (indiacte-buffer-boundaries t)
  (indicate-empty-lines nil)
  (scroll-conservatively 101)
  (auto-scroll-vscroll nil)
  ;; don't resize emacs in steps
  (window-resize-pixelwise t)
  (frame-resize-pixelwise t)
  (use-dialog-box nil) ;; no popup dialogs
  (setq-default tab-width 4)
  (setq-default evil-shift-width tab-width)
  (setq-default indent-tabs-mode nil)
  (setq-default show-trailing-whitespace t)
  (setq-default indicate-empty-lines t)
  (setq-default indicate-indicate-buffer-boundaries 'left))

;; don't show trailing whitespace in term buffers
(dolist (mode '(vterm-mode-hook
                term-mode-hook
                shell-mode-hook
                eat-mode-hook
                eat-eshell-mode-hook
                eat-eshell-visual-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (setq show-trailing-whitespace nil))))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1) ; disable the toolbar
(tooltip-mode -1) ; disable tooltips
(menu-bar-mode -1) ; disable the menu bar
(set-fringe-mode 10) ; give some breathing room

(defun dir-concat (dir file) (concat (file-name-as-directory dir)
				     file))

(setq user-cache-directory (expand-file-name "~/.cache/emacs/"))
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(use-package emacs
  :custom
  (user-emacs-directory (expand-file-name "~/.cache/emacs/"))
  (url-history-file (expand-file-name "url/history" user-emacs-directory)))

;; Use no-literring to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  ;; If you want to move everything out of the ~/.emacs.d folder
  ;; reliably, set `user-emacs-directory` before loading no-littering!
  :custom
  ;; no-littering doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Avoid constant errors on Windows about the coding system by setting the default to UTF-8
(set-default-coding-systems 'utf-8)

;; Start the Emacs server from this instance so that all emacslient calls are routed here
(server-start)

;; enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
;; override some modes which derive from the above
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; fonts
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :weight 'medium :height 170)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :weight 'medium :height 170)
(set-face-attribute 'variable-pitch nil :font "Noto Sans" :weight 'medium :height 170)

;; Enable proper Unicode glyph support
;; https://config.daviwill.com/emacs#enable-proper-unicode-glyph-support

;; Emojis in buffers
;; https://config.daviwill.com/emacs#emojis-in-buffers

;; diminish hides pesky minor modes from the modelines
;; (use-package diminish)

;; (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
;; (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)
(use-package all-the-icons-nerd-fonts)

(use-package nerd-icons-completion
  :functions nerd-icons-completion-mode nerd-icons-completion-marginalia-setup
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t)
  :functions doom-themes-org-config
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-gruvbox-dark-variant "hard")
  :config
  (doom-themes-org-config)) ; corrects (and improves) org-mode's native fontification.

(use-package doom-nano-modeline
  :functions doom-nano-modeline-mode
  :custom (doom-nano-modeline-position 'bottom)
  :config
  (doom-nano-modeline-mode 1))

(defun rune/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))
(use-package evil
  :functions evil-mode evil-set-undo-system evil-global-set-key evil-set-initial-state evil-define-key evil-normalize-keymaps
  :defines evil-insert-state-map evil-normal-state-map
  :hook (evil-mode-hook . rune/evil-hook)
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-define-key 'normal 'global (kbd "C-s") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "C-,") 'xref-go-back)
  (evil-define-key 'normal 'global (kbd "C-;") 'xref-go-forward)

  (evil-define-key 'normal 'global (kbd "g l") 'evil-end-of-line)
  (evil-define-key 'normal 'global (kbd "g h") 'evil-first-non-blank)

  ;; use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-define-key 'normal 'global (kbd "M-h") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "M-j") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "M-k") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "M-l") 'evil-window-right)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :functions evil-collection-init
  :config
  (evil-collection-init))

(use-package evil-escape
  :after evil
  :functions evil-escape-mode
  :custom
  (evil-escape-excluded-states '(normal visual))
  (evil-escape-excluded-major-modes '(vterm-mode))
  :config
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode))

(use-package evil-commentary
  :after evil
  :functions evil-commentary-mode
  :defines evil-commentary-mode-map
  :config
  (evil-commentary-mode)
  (define-key evil-commentary-mode-map (kbd "C-/") 'evil-commentary-line))

(use-package evil-surround
  :functions global-evil-surround-mode
  :config
  (evil-define-key 'operator evil-surround-mode-map "r" 'evil-surround-edit)
  (evil-define-key 'operator evil-surround-mode-map "s" nil)
  (global-evil-surround-mode 1))

(use-package evil-multiedit
  :functions evil-multiedit-default-keybinds
  :config
  (evil-multiedit-default-keybinds))

;; (use-package evil-mc
;;              :config
;;              (evil-define-key ('(normal visual) 'global
;;                               "gzm" #'evil-mc-make-all-cursors
;;                               "gzu" #'evil-mc-undo-all-cursors
;;                               "gzz" #'+evil/mc-toggle-cursor
;;                               "gzc" #'+evil/mc-make-cursor-here
;;                               "gzn" #'evil-mc-make-and-goto-next-cursor
;;                               "gzp" #'evil-mc-make-and-goto-prev-cursor
;;                               "gzN" #'evil-mc-make-and-goto-last-cursor
;;                               "gzP" #'evil-mc-make-and-goto-first-cursor
;;                               (with-eval-after-load 'evil-mc
;;                                                     (evil-define-key '(normal visual) evil-mc-key-map
;;                                                                      (kbd "C-n") #'evil-mc-make-and-goto-next-cursor
;;                                                                      (kbd "C-N") #'evil-mc-make-and-goto-prev-cursor
;;                                                                      (kbd "C-p") #'evil-mc-make-and-goto-prev-cursor
;;                                                                      (kbd "C-P") #'evil-mc-make-and-goto-first-cursor)))
;;              (global-evil-mc-mode 1)))

;; ESC cancells all
(global-set-key [escape] 'keyboard-escape-quit)
;; Since I let `evil-mode' take over `C-u' for buffer scrolling, I need to rebind the
;; `universal-argument' command to another key sequence.
(global-set-key (kbd "C-M-u") 'universal-argument)

(use-package general
  :functions general-evil-setup general-create-definer leader-def local-leader-def
  :defines leader-def local-leader-def
  :after evil
  :config (general-evil-setup))

;; set up 'SPC' as the leader key
(general-create-definer leader-def
  :states '(normal visual insert emacs)
  :keymaps 'override
  :prefix "SPC" ; set leader
  :global-prefix "M-SPC") ; access leader in insert mode

;; set up ',' as the local leader key
(general-create-definer local-leader-def
  :states '(normal visual insert emacs)
  :keymaps 'override
  :prefix "," ; set leader
  :global-prefix "M-,") ; access leader in insert mode

(leader-def
  "a" '(eglot-code-actions :which-key "Code actions")
  "b" '(consult-buffer :which-key "Open buffer picker")
  "B" '(consult-line-literal :which-key "Search in current buffer")
  "d" '(consult-flymake :which-key "Open diagnostics")
  "e" '(dired-jump :which-key "Open file explorer")
  "E" '(find-file :which-key "Open file browser")
  "f" '(consult-projectile :which-key "Open file picker")
  "F" '(consult-dir :which-key "Open directory picker")
  "j" '(consult-global-mark :which-key "Open mark-ring picker")
  "k" '(eglot-format :which-key "Format document") ;; TODO: merge eglot-format with format-all-the-code
  "m" '(magit-status :which-key "Git status")
  "q" '(evil-quit :which-key "Quit")
  "r" '(eglot-rename :which-key "Rename symbol")
  "s" '(consult-eglot-symbols :which-key "Open symbol picker")
  "y" '(consult-yank-pop :which-key "Open kill-ring picker")
  "SPC" '(execute-extended-command :which-key "M-x")
  "/" '(consult-grep :which-key "Global search at cwd"))

(use-package which-key
  :diminish
  :functions which-key-mode
  :init (which-key-mode 1)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  (which-key-side-window-max-height 0.25)
  (which-key-idle-delay 0.8)
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit t)
  (which-key-separator " -> "))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :functions consult--read consult--file-preview
  :custom
  (consult-project-function #'projectile-project-root)
  (consult-narrow-key "<")
  (consult-async-min-input 2)
  (completion-in-region-function #'consult-completion-in-region)
  ;; (read-file-name-function #'consult-find-file-with-preview)
  :config
  ;; NOTE: this doesn't work exactly like the default find-file (can't navigate directories)
  ;; i don't know enough elisp to fix this
  (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
    (interactive)
    (let ((default-directory (or dir default-directory))
          (minibuffer-completing-file-name t))
      (consult--read #'read-file-name-internal :state (consult--file-preview)
                     :prompt prompt
                     :initial initial
                     :require-match mustmatch
                     :predicate pred)))

  ;; Use the `orderless' completion style, restricted to `orderless-literal'
  (defun consult-line-literal ()
    (interactive)
    (let ((completion-styles '(orderless))
          (orderless-matching-styles '(orderless-literal))
          (completion-category-defaults nil)
          (completion-category-overrides nil))
      (consult-line)))
  :init
  (keymap-set minibuffer-local-map "C-r" 'consult-history))

(use-package marginalia
  :functions marginalia-mode
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (customize-set-variable 'marginalia-annotators
                          '(marginalia-annotators-heavy
                            marginalia-annotators-light
                            nil))
  (marginalia-mode 1))

(use-package embark
  ;; :bind
  ;; (("C-." . embark-act)
  ;;  ("C-;" . embark-dwim))
  :init
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult)
;; :hook
;; (embark-collect-mode . consult-preview-at-point-mode)

(use-package prescient
  :functions prescient-persist-mode
  :config (prescient-persist-mode)
  :custom
  (prescient-filter-method '(literal initialism prefix regexp))
  (prescient-use-char-folding t)
  (prescient-use-case-folding 'smart)
  (prescient-sort-full-matches-first t)
  (prescient-sort-length-enable t))

(use-package vertico
  :functions vertico-mode
  :custom
  (vertico-cycle t)
  :init (vertico-mode))

(use-package vertico-prescient
  :functions vertico-prescient-mode
  :init (vertico-prescient-mode 1))

;; save history across various prompts
(use-package savehist
  :defer 2
  :hook (after-init . savehist-mode)
  :custom
  (savehist-file (dir-concat user-cache-directory "savehist"))
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  :init
  (savehist-mode))

(use-package orderless
  :demand t
  :functions
  orderless-define-completion-style +orderless--consult-suffix orderless-matching-styles
  orderless-escapable-split-on-space +orderless-consult-dispatch orderless-affix-dispatch
  orderless-style-dispatchers
  :defines
  +orderless-with-initialism orderless-component-separator orderless-style-dispatchers
  orderless-fast
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (command (styles +orderless-with-initialism))
                                   (variable (styles +orderless-with-initialism))
                                   (symbol (styles +orderless-with-initialism))))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                     #'orderless-affix-dispatch))
  :config
  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))
  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp))))
;; :custom
;; (completion-styles '(orderless basic))
;; (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :functions projectile-mode
  :defines projectile-mode-map
  :custom
  (projectile-project-search-path '("~/projects/" "~/projects/github.com/" "~/projects/github.com/svitax/"))
  (projectile-switch-project-action #'projectile-dired)
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))
(use-package consult-projectile)
(use-package consult-dir
  :custom
  (consult-dir-project-list-function #'consult-dir-projectile-dirs)
  (consult-dir-default-command #'consult-projectile-find-file)
  :init
  (defun consult-dir--zoxide-dirs ()
    "Return list of zoxide dirs."
    (split-string (shell-command-to-string "zoxide query -l") "\n" t))
  ;; a consult source that calls the zoxide-dirs function
  (defvar consult-dir--source-zoxide
    `(:name "zoxide"
            :narrow ?z
            :category file
            :face consult-file
            :history file-name-history
            :enabled ,(lambda () (executable-find "zoxide"))
            :items ,#'consult-dir--zoxide-dirs)
    "zoxide directory source for `consult-dir'.")
  :config
  ;; adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t))

;; Keep track of recently opened files.
;; Also feeds into the list of recent directories used by consult-dir
(use-package recentf
  :custom
  (recentf-save-file (dir-concat user-cache-directory "recent"))
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 300)
  :config
  (recentf-mode 1))

(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package eglot
  :ensure nil
  :functions rune/eglot-manage-mode-initialize
  :hook
  ;; (python-ts-mode . eglot-ensure)
  ;; (rust-ts-mode . eglot-ensure)
  ;; (nix-mode . eglot-ensure)
  ;; (toml-ts-mode . eglot-ensure)
  (toml-ts-mode . (lambda ()
                    (setq tab-width 2)
                    (setq require-final-newline t)))
  :custom
  (eglot-events-buffer-size 0) ;; disable eglot logging (improves performance)
  (eglot-autoshutdown t) ;; shutdown server when last managed buffer is killed
  :init
  (defun rune/eglot-manage-mode-initialize ()
    (setq-local
     eldoc-documentation-functions
     (list
      #'eglot-signature-eldoc-function
      ;; #'eglot-hover-eldoc-function
      #'flymake-eldoc-function)))
  (add-hook 'eglot-managed-mode-hook #'rune/eglot-manage-mode-initialize)

  (defun rune/add-eglot-hooks (mode-list)
    "Add `eglot-ensure' to modes in MODE-LIST.

The mode must be loaded, i.e. found with `fboundp'. A mode which
is not loaded will not have a hook added, in which case add it
manually with something like this:

`(add-hook 'some-mode-hook #'eglot-ensure)'"
    (dolist (mode-def mode-list)
      (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
	(cond
	 ((listp mode) (rune/add-eglot-hooks mode))
	 (t
	  (when (and (fboundp mode)
		     (not (eq 'clojure-mode mode)) ; prefer cider
		     (not (eq 'lisp-mode mode)) ; prefer sly/slime
		     (not (eq 'scheme-mode mode)) ; prefer geiser
		     )
	    (let ((hook-name (format "%s-hook" (symbol-name mode))))
	      (message "adding eglot to %s" hook-name)
	      (add-hook (intern hook-name) #'eglot-ensure))))))))

  (defun rune/lsp-bin-exists-p (mode-def)
    "Return non-nil if LSP binary of MODE-DEF is found via `executable-find'."
    (let ((lsp-program (cdr mode-def)))
      ;; `lsp-program' is either a list of strings or a function object
      ;; calling `eglot-alternatives'.
      (if (functionp lsp-program)
	  (condition-case nil
	      (car (funcall lsp-program))
	    ;; When an error occurs it's because Eglot checked for a
	    ;; binary and didn't find one among alternatives.
	    (error nil))
	(message "%s" lsp-program)
	(executable-find (car lsp-program)))))

  (defun eglot-auto-ensure-all ()
    "Add `eglot-ensure' to major modes that offer LSP support.

Major modes are only selected if the major mode's associated LSP
binary is detected on the system."
    (when (require 'eglot nil :noerror)
      (rune/add-eglot-hooks eglot-server-programs ;; (seq-filter
			    ;; #'rune/lsp-bin-exists-p
			    ;; eglot-server-programs)
			    )))
  :config
  (eglot-auto-ensure-all)
  ;; (add-to-list 'eglot-server-programs '(toml-ts-mode . ("taplo" "lsp" "stdio")))
  ;; (add-to-list 'eglot-server-programs '(conf-toml-mode . ("taplo" "lsp" "stdio")))
  ;; TODO: setup pyright and ruff-lsp on same buffer
  ;; (add-to-list 'eglot-server-programs '(foo-mode . ,(eglot-alternatives
  ;; 						     `(("pyright" "--stdio")
  ;; 						       ("ruff-lsp" "--fast")))))
  (setq-default eglot-workspace-configuration
                '((:rust-analyzer
                   :completion (:callable (:snippets "fill_arguments"))
                   :checkOnSave (:command "clippy"
                                          :allTargets :json-false)))))
;; BUG: consult-eglot pulls in different version of eglot with unfixed errors. disable until errors dissappear.
(use-package consult-eglot :disabled t)

(use-package apheleia
  :config
  (apheleia-global-mode +1)
  (require 'cl-lib)
  (cl-defun apheleia-indent-eglot-managed-buffer
      (&key buffer scratch callback &allow-other-keys)
    (with-current-buffer scratch
      (setq-local eglot--cached-server
		  (with-current-buffer buffer
		    (eglot-current-server)))
      (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
	(eglot-format-buffer))
      (funcall callback)))
  (add-to-list 'apheleia-formatters '(eglot-managed . apheleia-indent-eglot-managed-buffer))
  ;; TODO: do i need this apheleia-mode-alist stuff?
  ;; (add-to-list 'apheleia-mode-alist '(rustic-mode . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(python-mode . 'black)))

(use-package flymake-collection
  :hook ((after-init . flymake-collection-hook-setup)
         (emacs-lisp-mode . flymake-mode)
         (emacs-lisp-mode . rune/elisp-manage-mode-initialize))
  :init
  (defun rune/elisp-manage-mode-initialize ()
    (setq-local
     eldoc-documentation-functions
     (list
      ;; #'eglot-signature-eldoc-function
      ;; #'eglot-hover-eldoc-function
      #'flymake-eldoc-function))))

;; (use-package flycheck :functions global-flycheck-mode :init (global-flycheck-mode))
;; (use-package consult-flycheck)

;; automatically handle switching to tree-sitter versions of major modes,
;; can install grammars, etc.
(use-package treesit-auto
  :init
  (defun rune/configure-tree-sitter (opt-in-only)
    "Configure tree-sitter for Emacs 29 or later.

OPT-IN-ONLY is a list of symbols of language grammars to
auto-install instead of all grammars."
    ;; only attempt to use tree-sitter when Emacs was built with it.
    (when (member "TREE_SITTER" (split-string system-configuration-features))
      (when (require 'treesit-auto nil :noerror)
	;; add all items of opt-in-only to the `treesit-auto-langs'.
	(when opt-in-only
	  (if (listp opt-in-only)
	      (customize-set-variable 'treesit-auto-langs opt-in-only)
	    (customize-set-variable 'treesit-auto-langs (list opt-in-only))))
	;; prefer tree-sitter modes
	(global-treesit-auto-mode)
	;; install all the tree-sitter grammars
	;; (treesit-auto-install-all)
	;; configure `auto-mode-alist' for tree-sitter modes relying on
	;; `fundamental-mode'
	(treesit-auto-add-to-auto-mode-alist))
      (when (locate-library "combobulate")
	;; perhaps too gross of an aplication, but the *-ts-modes
	;; eventually derive from this mode.
	(add-hook 'prog-mode-hook #'combobulate-mode))))

  (defun rune/configure-tree-sitter-pre-29 ()
    "Configure tree-sitter for Emacs 28 or earlier."

    (defun rune/tree-sitter-load (load-symbol)
      "Setup tree-sitter for a language.

This must be called in the user's configuration to configure
tree-sitter for LANG-SYMBOL.

Example: `(rune/tree-sitter-load 'python)'"
      (tree-sitter-require lang-symbol)
      (let ((mode-hook-name
	     (intern (format "%s-mode-hook" (symbol-name lang-symbol)))))
	(add-hook mode-hook-name #'tree-sitter-mode))))

  (defun configure-tree-sitter (&optional opt-in-only)
    "Configure tree-sitter.

Requires a C compiler (gcc, cc, c99) installed on the system.
Note that OPT-IN-ONLY only affects setups with Emacs 29 or later.

For Emacs 29 or later:
Requires Emacs to be built using \"--with-tree-sitter\".
All language grammars are auto-installed unless they are a member
of OPT-IN-ONLY, in which case *only* those grammars are
installed."
    (if (version< emacs-version "29")
	(rune/configure-tree-sitter-pre-29)
      (rune/configure-tree-sitter opt-in-only)))

  :config
  (configure-tree-sitter))

;; (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(rustic-mode . rust-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))

;; editorconfig is a cross-editor/ide configuration tool to control
;; indentation, spaces vs tabs, etc.
;; (use-package editorconfig)

(use-package rustic
  :custom
  ;; (rustic-lsp-client 'eglot)
  (rustic-lsp-setup-p 'f))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package corfu
  :functions
  global-corfu-mode corfu-popupinfo-scroll-down corfu-popupinfo-scroll-up corfu-popupinfo-toggle
  corfu-popupinfo-mode corfu-insert corfu-mode corfu-enable-in-minibuffer
  :defines corfu-map corfu-insert-separator
  :custom
  ;; (corfu-cycle t) ;; allows cycling through candidates
  (corfu-auto t) ;; enable auto completion
  (corfu-auto-prefix 2) ; complete with less prefix keys
  (corfu-auto-delay 0)
  (corfu-popupinfo-delay '0.5)
					; (corfu-separator ?\s) ;; orderless field separator
					; (corfu-quit-at-boundary nil) ;; never quit at completion boundary
					; (corfu-quit-no-match nil)
					; (corfu-preview-current t) ;; disable current candidate preview
					; (corfu-preselect 'prompt) ;; preselect the prompt
					; (corfu-on-exact-match nil) ;; configure handling of exact matches
					; (corfu-scroll-margin 5) ;; use scroll margin
  :config
  ;; fast prefix filtering with Orderless
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         `(orderless-regexp . ,(concat "^" (regexp-quote word)))))
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))
  (setq-local completion-styles '(orderless-fast basic))

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-echo-delay nil
                  corfu-popup-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (eldoc-add-command #'corfu-insert)
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  :bind
  (:map corfu-map
        ("M-u" . #'corfu-popupinfo-scroll-down)
        ("M-d" . #'corfu-popupinfo-scroll-up)
        ("M-t" . #'corfu-popupinfo-toggle)
        ("M-SPC" . corfu-insert-separator)
        ("TAB" . corfu-insert)
        ("RET" . nil)))

(use-package corfu-prescient
  :functions corfu-prescient-mode
  :init (corfu-prescient-mode 1))

(use-package nerd-icons-corfu
  :after corfu
  :defines corfu-margin-formatters
  :functions nerd-icons-corfu-formatter
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :functions
  cape-file cape-dabbrev cape-wrap-silent cape-wrap-purify cape-elisp-symbol cape-wrap-buster
  cape-capf-super eglot-completion-at-point rune/eglot-capf rune/ignore-elisp-keywords rune/setup-elisp
  cape-capf-predicate
  ;; Setup Cape for better completion-at-point support and more
  :config
  (defun rune/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
		       #'eglot-completion-at-point
		       ;; #'tempel-expand
		       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'rune/eglot-capf)
  :init
  ;; Add useful defaults completion sources from cape
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for Corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package elec-pair
  :defer
  :config
  (electric-pair-mode +1))

(use-package envrc
  :functions envrc-global-mode
  :init (envrc-global-mode))

(use-package popper
  :functions popper-mode popper-echo-mode
  :bind (("M-n" . popper-cycle)
         ("C-<tab>" . popper-toggle)
         ("C-'" . popper-toggle-type))
  :config
  (evil-define-key 'normal 'global (kbd "M-n") 'popper-cycle)
  (evil-define-key 'normal 'global (kbd "C-<tab>") 'popper-toggle)
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*Backtrace\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "^\\*eshell.*\\*$" eshell-mode
     "^\\*shell.*\\*$" shell-mode
     "^\\*term.*\\*$" term-mode
     "^\\*vterm.*\\*$" vterm-mode
     "^\\*eat.*\\*$" eat-mode
     help-mode
     compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package eat
  :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  :config
  (dolist (key '([?\e ?o] [?\e ?`] (kbd "C-`") [?\e 67108960]))
    (push key eat-semi-char-non-bound-keys))
  (eat-update-semi-char-mode-map)
  ;; (run-at-time 0 nil #'eat-reload)
  (setq eat-kill-buffer-on-exit t))
;; (setq eshell-visual-commands '()))

;; ** Eshell spawning
;;
;; Calling an exiting eshell
(use-package eshell
  ;; :init
  ;; (evil-define-key 'normal 'global (kbd "C-a") 'eshell)
  :bind (("C-a" . eshell)
         ("C-<return>" . eshell-here))
  :config
  (advice-add 'eshell-life-is-too-much :after #'delete-window-if-not-single)
  (advice-add 'eshell-mark-output :after #'activate-mark)
  ;; From http://howardism.org/Technical/Emacs/eshell-fun.html
  (defun eshell-here ()
    "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
		       (file-name-directory (buffer-file-name))
                     default-directory))
           (name   (car (last (split-string parent "/" t)))))
      (if-let* ((eshell-name (concat "*eshell: " name "*"))
                (existing-eshell-buffer (get-buffer eshell-name)))
          (select-window (display-buffer existing-eshell-buffer))
        (select-window (display-buffer (eshell "new")))
        (rename-buffer eshell-name)
        ;; (insert (concat "ls"))
        (eshell-send-input)))))

;; ** Eshell appearance
;; Including the prompt.
;; TODO: better faces for eshell prompt
(use-package eshell
  :defer
  :config
  (setq eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'rune/eshell-default-prompt-fn)
  ;; From the Doom emacs config
  (setq eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face)))

  (defun rune/eshell-default-prompt-fn ()
    "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
    (concat (if (bobp) "" "\n")
            (when (bound-and-true-p conda-env-current-name)
	      (propertize (concat "(" conda-env-current-name ") ")
                          'face 'rune/eshell-prompt-git-branch))
            (let ((pwd (eshell/pwd)))
	      (propertize (if (equal pwd "~")
			      pwd
                            (abbreviate-file-name pwd))
                          'face 'rune/eshell-prompt-pwd))
            (propertize (rune/eshell--current-git-branch)
                        'face 'rune/eshell-prompt-git-branch)
            (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
            " "))

  (defsubst rune/eshell--current-git-branch ()
    ;; TODO Refactor me
    (cl-destructuring-bind (status . output)
        (with-temp-buffer (cons
                           (or (call-process "git" nil t nil "symbolic-ref" "-q" "--short" "HEAD")
			       (call-process "git" nil t nil "describe" "--all" "--always" "HEAD")
			       -1)
                           (string-trim (buffer-string))))
      (if (equal status 0)
          (format " [%s]" output)
        "")))

  (defface rune/eshell-prompt-pwd '((t (:inherit font-lock-keyword-face)))
    "TODO"
    :group 'eshell)

  (defface rune/eshell-prompt-git-branch '((t (:inherit font-lock-builtin-face)))
    "TODO"
    :group 'eshell))

;; ** Eshell built-ins
(use-package eshell
  :defines
  eshell-history-size eshell-buffer-name eshell-hist-ignoredups eshell-scroll-to-bottom-on-input
  eshell-output-filter-functions eshell-mode-map
  :hook ((eshell-mode . rune/eshell-keys-and-modes)
         (eshell-first-time-mode . rune/eshell-first-load-settings))
  :custom
  (eshell-ls-initial-args '("-alth"))
  :config
  (defun rune/eshell-first-load-settings ()
    (setq eshell-visual-commands (append eshell-visual-commands
                                         '("btm" "fzf" "pulsemixer" "mpv"
                                           "ncmpcpp" "progress" "julia"
                                           "ranger" "watch" "bluetoothctl"
                                           "nh"))
          ;; eshell-input-filter-functions '(eshell-expand-history-references)
          eshell-hist-ignoredups t
          eshell-destroy-buffer-when-process-dies t
          eshell-directory-name (dir-concat user-cache-directory "eshell/")
          eshell-history-file-name (concat (file-name-as-directory
                                            eshell-directory-name)
                                           "history")
          eshell-last-dir-ring-file-name (concat (file-name-as-directory
						  eshell-directory-name)
						 "lastdir")
          eshell-history-size 4096
          eshell-glob-case-insensitive t
          eshell-error-if-no-glob t)
    (setq eshell-aliases-file
          (concat (file-name-as-directory
                   eshell-directory-name)
                  "alias"))
    (eshell-read-aliases-list))

  (defun rune/eshell-keys-and-modes ()
    (setq outline-regexp eshell-prompt-regexp)
    (abbrev-mode 1)
    (setq-local imenu-generic-expression
                '(("λ: " " λ \\(.*\\)" 1)))
    (define-key eshell-mode-map (kbd "H-<return>") 'rune/delete-window-or-delete-frame)
    (define-key eshell-hist-mode-map (kbd "M-s") nil)
    (define-key eshell-mode-map (kbd "C-c C-SPC") 'eshell-mark-output)
    (define-key eshell-mode-map (kbd "C-<return>") 'rune/eshell-send-detached-input)
    ;; Bind some useful keys for evil mode
    (define-key eshell-mode-map (kbd "C-r") 'consult-history)
    (define-key eshell-mode-map (kbd "<home>") 'eshell-bol)
    (setq-local company-minimum-prefix-length 2)
    ;; (setq-local completion-in-region-function #'consult-completion-in-region)
    (setq eshell-cmpl-cycle-cutoff-length 2)))

;; Custom functions for use in eshell
(use-package eshell
  :defer
  :config
  (defalias 'eshell/v 'eshell-exec-visual)
  (defalias 'eshell/x #'eshell/exit)

  (defun rune/eshell-insert-args (&optional num)
    "Insert the NUMth argument of the previous command.

NUM counts from the end"
    (interactive "p")
    (let ((valid-pos)
          (N (length eshell-last-arguments)))
      (save-excursion
        (beginning-of-line)
        (if (looking-at eshell-prompt-regexp)
            (setq valid-pos t)))
      (if valid-pos
          (insert (substring-no-properties
                   (nth (- N num) eshell-last-arguments)))
        (call-interactively #'xref-find-definitions))))

  ;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
  (defun eshell/b (regexp)
    "Output buffer content of buffer matching REGEXP."
    (cl-loop for buf in (buffer-list)
             thereis
             (and (string-match-p regexp (buffer-name buf))
                  (with-current-buffer buf
                    (buffer-substring-no-properties (point-min) (point-max))))))

  ;; From https://github.com/LemonBreezes/.doom.d/blob/master/modules/private/eshell/autoload.el
  (defun eshell/hat (&rest files)
    "Output FILES with highlighting."
    (dolist (f files)
      (eshell-print (rune/eshell-file-contents f))))

  (defun eshell/view (&optional file)
    (if (or (not file)
            (not (file-exists-p file))
            (file-directory-p file))
        (dired-other-window default-directory)
      (find-file-other-window file)
      (read-only-mode 1)
      (view-mode 1)))

  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell."
    (let ((eshell-dirs (delete-dups (mapcar 'abbreviate-file-name
                                            (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                   :narrow ?e
                                                   :category file
                                                   :face consult-file
                                                   :items ,eshell-dirs))
	       (consult-dir-sources (cons consult-dir--source-eshell consult-dir-sources)))
          (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
		       (completing-read "cd: " eshell-dirs)))))))

  ;; From https://protesilaos.com/dotemacs
  (defun rune/eshell-export-output (&optional arg)
    "Export output of the last command to a buffer.
With prefix ARG, also copy the prompt and input."
    (interactive)
    (let ((orig (current-buffer))
          (beg (if arg (eshell-beginning-of-input)
                 (eshell-beginning-of-output)))
          (end (eshell-end-of-output))
          (buffer (get-buffer-create
                   (format "*eshell export: %s*"
                           (buffer-substring-no-properties
                            (eshell-beginning-of-input)
                            (1- (eshell-beginning-of-output)))))))
      (with-current-buffer buffer
        (font-lock-mode)
        (insert-buffer-substring orig beg end)
        (goto-char (point-min)))
      ;; Taken from `eshell-kill-output'
      (goto-char (eshell-beginning-of-output))
      (insert (format "Exported to %S\n" buffer))
      (delete-region (point) (eshell-end-of-output))
      (goto-char (point-max))
      (pop-to-buffer buffer)))

  (defun rune/eshell-copy-output (&optional arg)
    "Copy output of the last command to the kill ring. With prefix
argument arg, Also copy the prompt and input."
    (interactive "P")
    (copy-region-as-kill (if arg (eshell-beginning-of-input)
                           (eshell-beginning-of-output))
                         (eshell-end-of-output))
    (message (if arg "Copied last input and output to kill ring."
	       "Copied last output to kill ring.")))
  (add-hook
   'eshell-mode-hook
   (lambda ()
     (define-key eshell-mode-map (kbd "M-.") 'rune/eshell-insert-args)
     (define-key eshell-mode-map (kbd "C-c M-w") 'rune/eshell-copy-output)
     (define-key eshell-hist-mode-map (kbd "C-c C-l") 'rune/eshell-export-output)))

  ;;From https://github.com/nbarrientos/dotfiles/.emacs.d/init.el
  (defun rune/eshell-send-detached-input (&optional arg)
    "Send the current Eshell input to a compilation buffer.
With universal prefix argument bury the compilation buffer and
send a notification when the process has exited."
    (interactive "p")
    (let* ((cmd (buffer-substring
                 eshell-last-output-end (point-max)))
           (hostname (car (split-string
                           (or
                            (file-remote-p default-directory 'host)
                            (system-name))
                           "\\.")))
           (compile-command nil)
           (compilation-buffer-name-function
            (lambda (_major-mode)
	      (format "D# %s (%s)" cmd hostname)))
           (compilation-buffer (compile cmd)))
      (when (equal arg 4)
        (with-current-buffer compilation-buffer
          (switch-to-prev-buffer (get-buffer-window (current-buffer)))
          (setq-local compilation-finish-functions
		      `((lambda (buffer str)
                          (notifications-notify
                           :body ,cmd
                           :timeout 8000
                           :category "detached_process"
                           :actions '("default" "Switch to buffer")
                           :on-action (lambda (id key) (switch-to-buffer-other-window ,(buffer-name compilation-buffer)))
                           :title (format "Process running in '%s' finished!" ,hostname)
                           :urgency (if (string-prefix-p "finished" str) 'normal 'critical)))))))
      (eshell-add-input-to-history cmd)
      (eshell-reset)))

  ;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
  (defun rune/eshell-buffer-contents (buffer)
    "Return fontified buffer contents for BUFFER."
    (with-current-buffer buffer
      (font-lock-ensure (point-min) (point-max))
      (buffer-string)))

  ;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
  (defun rune/eshell-file-contents (file)
    "Return fontified file contents for FILE."
    (let ((buffer (get-file-buffer file)))
      (if buffer
          (rune/eshell-buffer-contents buffer)
        (unwind-protect
            (rune/eshell-buffer-contents
             (setq buffer
                   (let ((inhibit-message t)
                         (non-essential t)
                         (enable-dir-local-variables nil)
                         (enable-local-variables (and enable-local-variables :safe)))
                     (find-file-noselect file))))
          (when buffer
            (kill-buffer buffer)))))))

;; (use-package fish-completion
;;   :hook (eshell-mode . fish-completion-mode))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; (use-package diff-hl
;;   :init
;;   (global-diff-hl-mode)
;;   (diff-hl-flydiff-mode))

(use-package git-gutter
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 0.05))

(use-package git-gutter-fringe
  :config
  (setq-default fringes-outside-margins t)
  ;; (setq-default left-fringe-width 20)
  ;; (setq-default right-fringe-width 20)
  ;; (set-face-foreground 'git-gutter-fr:modified "LightGreen")
  (define-fringe-bitmap 'git-gutter-fr:added [#b11111000] nil nil '(center repeated))
  (set-face-foreground 'git-gutter-fr:modified "Goldenrod")
  (define-fringe-bitmap 'git-gutter-fr:modified [#b11111000] nil nil '(center repeated))
  ;; (set-face-foreground 'git-gutter-fr:modified "LightRed")
  (define-fringe-bitmap 'git-gutter-fr:deleted [#b11111000
                                                #b11111100
                                                #b11111110
                                                #b11111111
                                                #b11111111
                                                #b11111110
                                                #b11111100
                                                #b11111000]
    nil nil 'bottom))

(use-package avy
  :custom
  (avy-timeout-seconds 0.25)
  :init
  ;; evil-avy
  ;; TODO: make avy-forward-char-in-line jump to first match even when there are multiple matches.
  ;; TODO: have avy-next `;' and avy-prev `,' available after the first jump
  ;; fabd2f
  (defun avy-forward-char-in-line (char &optional back)
    "Jump forward to the currently visible CHAR in the current line.
If BACK is t, jump backward."
    (interactive (list (read-char "char: " t)))

    (let ((avy-all-windows nil))
      (avy-with avy-goto-char
        (avy-process
         (save-restriction
           (if (null back)
	       (narrow-to-region (point)
                                 (line-end-position))
             (narrow-to-region (line-beginning-position)
			       (point)))
           (avy--regex-candidates (regexp-quote (string char))))
         (avy--style-fn avy-style)))))
  (evil-define-motion evil-avy-find-char (count char)
    "Use avy to move forward to char in line."
    :jump t
    :type inclusive
    (interactive "<c><C>")
    (if (null count) (avy-forward-char-in-line char)
      (evil-find-char count char)))

  (evil-define-motion evil-avy-find-char-to (count char)
    "Use avy to move till char in line."
    :jump t
    :type inclusive
    (interactive "<c><C>")
    (if (null count)
        (progn
          (avy-forward-char-in-line char)
          (backward-char))
      (evil-find-char-to char)))

  (evil-define-motion evil-avy-find-char-backward (count char)
    "Use avy to move backward to char in line."
    :jump t
    :type exclusive
    (interactive "<c><C>")
    (if (null count)
        (avy-forward-char-in-line char)
      (evil-find-char-backward count char)))

  (evil-define-motion evil-avy-find-char-to-backward (count char)
    "Use avy to move backward till char in line."
    :jump t
    :type exclusive
    (interactive "<c><C>")
    (if (null count)
        (progn
          (avy-forward-char-in-line char t)
          (forward-char))
      (evil-find-char-to-backward count char)))

  ;; Replace motions
  (evil-define-key 'normal evil-avy-mode-map
    "f" 'evil-avy-find-char
    "F" 'evil-avy-find-char-backward
    "t" 'evil-avy-find-char-to
    "T" 'evil-avy-find-char-to-backward)
  (evil-define-key 'operator evil-avy-mode-map
    "f" 'evil-avy-find-char
    "F" 'evil-avy-find-char-backward
    "t" 'evil-avy-find-char-to
    "T" 'evil-avy-find-char-to-backward)
  (evil-define-key 'visual evil-avy-mode-map
    "f" 'evil-avy-find-char
    "F" 'evil-avy-find-char-backward
    "t" 'evil-avy-find-char-to
    "T" 'evil-avy-find-char-to-backward)
  (evil-define-key 'motion evil-avy-mode-map
    "f" 'evil-avy-find-char
    "F" 'evil-avy-find-char-backward
    "t" 'evil-avy-find-char-to
    "T" 'evil-avy-find-char-to-backward)
  (evil-define-key 'motion evil-avy-mode-map
    ";" 'avy-next
    "," 'avy-prev)

  (define-minor-mode evil-avy-mode
    "Toggle evil-avy-mode.
Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When evil-avy-mode is active, it replaces some of the normal, visual, operator,
and motion state keybindings to invoke avy commands."
    :init-value nil
    :lighter nil
    :keymap (make-sparse-keymap)
    :global t
    :group 'avy)

  (evil-define-key 'normal 'global (kbd "s") 'evil-avy-goto-char-timer)
  (evil-define-key 'operator global (kbd "s") 'evil-avy-goto-char-timer)
  :config (evil-avy-mode 1))

;; Automatically clean whitespace
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Use Parinfer for Lispy languages
;; TODO: i don't really know how to use parinfer yet
;; (use-package parinfer-rust-mode
;;   :hook ((clojure-mode . parinfer-rust-mode)
;;          (emacs-lisp-mode . parinfer-rust-mode)
;;          (common-lisp-mode . parinfer-rust-mode)
;;          (scheme-mode . parinfer-rust-mode)
;;          (lisp-mode . parinfer-rust-mode)))

;; TODO: Set up the `compile' package and ensure that compilation output automatically scrolls
;; https://config.daviwill.com/emacs#compilation
;;; init.el ends here
