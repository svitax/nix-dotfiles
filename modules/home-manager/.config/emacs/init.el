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
  ;; (completion-cycle-threshold 3) ;; TAB cycle if there are only few candidates
  (tab-always-indent t) ;; enable indentation+completion using the TAB key
  ;; use-package is automatically added by the nix emacs overlay
  ;; here we just make sure we don't have to keep typing `:ensure t`
  (use-package-always-ensure t)
  :init
  (setq-default tab-width 2)
  (setq-default evil-shift-width tab-width)
  (setq-default indent-tabs-mode nil)
  (setq-default show-trailing-whitespace t)
  (setq-default indicate-empty-lines t)
  (setq-default indicate-indicate-buffer-boundaries 'left))

;; don't show trailing whitespace in term buffers
(dolist (mode '(vterm-mode-hook
                term-mode-hook
                shell-mode-hook
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

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(use-package emacs
  :custom
  (user-emacs-directory (expand-file-name "~/.cache/emacs/"))
  (url-history-file (expand-file-name "url/history" user-emacs-directory)))

;; If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
(use-package no-littering
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
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 200)
(set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 200)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 200)

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
;; (use-package all-the-icons-completion
;;   :functions all-the-icons-completion-marginalia-setup
;;   :init (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))
;; (use-package all-the-icons-dired
;;   :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

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

(use-package evil
  :functions evil-mode evil-set-undo-system evil-global-set-key evil-set-initial-state evil-define-key evil-normalize-keymaps
  :defines evil-insert-state-map evil-normal-state-map
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

(global-set-key [escape] 'keyboard-escape-quit)

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
  "B" '(consult-line :which-key "Search in current buffer")
  "d" '(consult-flymake :which-key "Open diagnostics")
  "e" '(find-file :which-key "Open file browser")
  "f" '(consult-projectile :which-key "Open file picker")
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
    (consult-async-min-input 1)
    (completion-in-region-function #'consult-completion-in-region)
    ;; (read-file-name-function #'consult-find-file-with-preview)
    :config
    (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
      (interactive)
      (let ((default-directory (or dir default-directory))
            (minibuffer-completing-file-name t))
        (consult--read #'read-file-name-internal :state (consult--file-preview)
                       :prompt prompt
                       :initial initial
                       :require-match mustmatch
                       :predicate pred)))
    :init
    (keymap-set minibuffer-local-map "C-r" 'consult-history))

(use-package consult-dir)

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

(use-package embark-consult
  ;; :hook
  ;; (embark-collect-mode . consult-preview-at-point-mode)
  )

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

  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless-fast basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space
        orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                          #'orderless-affix-dispatch)))
;; :custom
;; (completion-styles '(orderless basic))
;; (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
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

(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package eglot
  :ensure nil
  :functions my/eglot-manage-mode-initialize
  :hook
  (python-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (nix-mode . eglot-ensure)
  (toml-ts-mode . eglot-ensure)
  (toml-ts-mode . (lambda ()
                    (setq tab-width 2)
                    (setq require-final-newline t)))
  :custom
  (eglot-events-buffer-size 0) ;; disable eglot logging (improves performance)
  :init
  (defun my/eglot-manage-mode-initialize ()
    (setq-local
          eldoc-documentation-functions
          (list
           #'eglot-signature-eldoc-function
           ;; #'eglot-hover-eldoc-function
           #'flymake-eldoc-function
           )))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-manage-mode-initialize)
  :config
  (add-to-list 'eglot-server-programs '(toml-ts-mode . ("taplo" "lsp" "stdio")))
  (add-to-list 'eglot-server-programs '(conf-toml-mode . ("taplo" "lsp" "stdio")))
  (setq-default eglot-workspace-configuration
                '((:rust-analyzer
                   :completion (:callable (:snippets "fill_arguments"))
                   :checkOnSave (:command "clippy"
                                          :allTargets :json-false)))))
;; BUG: consult-eglot pulls in different version of eglot with unfixed errors. disable until errors dissappear.
(use-package consult-eglot :disabled t)

(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))
;; (use-package flycheck :functions global-flycheck-mode :init (global-flycheck-mode))
;; (use-package consult-flycheck)

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(rustic-mode . rust-ts-mode))
(add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))

(use-package rustic
  :custom
  (rustic-lsp-client 'eglot))
(use-package nix-mode
  :mode "\\.nix\\'")

(use-package corfu
  :functions
  global-corfu-mode corfu-popupinfo-scroll-down corfu-popupinfo-scroll-up corfu-popupinfo-toggle
  corfu-popupinfo-mode corfu-insert corfu-mode corfu-enable-in-minibuffer
  :defines corfu-map corfu-insert-separator
  :custom
  (corfu-cycle t) ;; allows cycling through candidates
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
        ("M-d" . #'corfu-popupinfo-scroll-down)
        ("M-u" . #'corfu-popupinfo-scroll-up)
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
  cape-capf-super eglot-completion-at-point my/eglot-capf my/ignore-elisp-keywords my/setup-elisp
  cape-capf-predicate
  ;; Setup Cape for better completion-at-point support and more
  :config
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       ;; #'tempel-expand
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
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
  :custom
  (electric-pair-inhibit-predicate
   `(lambda (c)
      (if (char-equal c?\<) t (,electric-pair-inhibit-predicate c))))
  :config
  (electric-pair-mode +1))

(use-package envrc
  :functions envrc-global-mode
  :init (envrc-global-mode))

(use-package popper
  :functions popper-mode popper-echo-mode
  :bind (("M-n" . popper-cycle)
         ("C-a" . popper-toggle)
         ("C-'" . popper-toggle-type))
  :config
  (evil-define-key 'normal 'global (kbd "M-n") 'popper-cycle)
  (evil-define-key 'normal 'global (kbd "C-a") 'popper-toggle)
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "^\\*eshell.*\\*$" eshell-mode
     "^\\*shell.*\\*$" shell-mode
     "^\\*term.*\\*$" term-mode
     "^\\*vterm.*\\*$" vterm-mode
     help-mode
     compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package vterm
  :commands vterm
  :custom
  (term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (vterm-max-scrollback 10000))

(use-package eshell
  :defines
  eshell-history-size eshell-buffer-name eshell-hist-ignoredups eshell-scroll-to-bottom-on-input
  eshell-output-filter-functions eshell-mode-map)
(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  ;; Bind some useful keys for evil mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-nol)
  (evil-normalize-keymaps)

  (setq eshell-history-size 10000
        eshell-buffer-name 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-mode . rune/configure-eshell))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :custom (git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package avy
  :init
  ;; evil-avy
  ;; TODO: make avy-forward-char-in-line jump to first match even when there are multiple matches.
  ;; TODO: have avy-next `;' and avy-prev `,' available after the first jump
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
;;; init.el ends here
