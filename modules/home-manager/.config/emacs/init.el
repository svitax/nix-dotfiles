(use-package emacs
             :defer 0
             :init
             (setq enable-recursive-minibuffers t
                   ring-bell-function #'ignore ;; Do not make unecessary sounds
                   inhibit-startup-message t ;; disable the default emacs startup screen
                   inhibit-startup-screen t
                   inhibit-splash-screen t
                   gc-cons-threshold (* 50 1000 1000) ;; The default is 800 kilobytes. Measured in bytes
                   column-number-mode t ;; line numbers
                   display-line-numbers-type 'relative
                   visible-bell nil
                   hl-line-mode t
                   completion-cycle-threshold 3 ;; TAB cycle if there are only few candidates
                   tab-always-indent 'complete ;; enable indentation+completion using the TAB key
                   )
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
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
        (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; use-package is automatically added by the nix emacs overlay
;; here we just make sure we don't have to keep typing `:ensure t`
(setq use-package-always-ensure t)

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

(use-package all-the-icons-completion
             :init (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))
(use-package all-the-icons-dired
  :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package doom-themes
             :init (load-theme 'doom-gruvbox t)
             :config
             (setq doom-themes-enable-bold t
                   doom-themes-enable-italic t)
             ; (doom-themes-visual-bell-config) ; enable flashing mode-line on errors
             (doom-themes-org-config)) ; corrects (and improves) org-mode's native fontification.

(use-package doom-nano-modeline
  :init (setq doom-nano-modeline-position 'bottom)
             :config
             (doom-nano-modeline-mode 1))

(use-package evil
             :init
             (setq evil-want-C-u-scroll t)
             (setq evil-want-C-i-jump t)
             (setq evil-want-integration t)
             (setq evil-want-keybinding nil)
             (setq evil-vsplit-window-right t)
             (setq evil-split-window-below t)
             :config
             (evil-mode 1)
             (evil-set-undo-system 'undo-redo)

             (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
             (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
             (define-key evil-normal-state-map (kbd "C-t") 'popper-toggle)
             (define-key evil-normal-state-map (kbd "C-n") 'popper-cycle)

             ;; use visual line motions even outside of visual-line-mode buffers
             (evil-global-set-key 'motion "j" 'evil-next-visual-line)
             (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

             (evil-set-initial-state 'messages-buffer-mode 'normal)
             (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
             :after evil
             :config
             (evil-collection-init))

(use-package evil-escape
             :after evil
             :init
             (setq evil-escape-excluded-states '(normal visual)
                   evil-escape-excluded-major-modes '(vterm-mode))
             :config
             (setq-default evil-escape-delay 0.2)
             (setq-default evil-escape-key-sequence "jk")
             (evil-escape-mode))

(use-package evil-commentary
             :after evil
             :config
             (evil-commentary-mode)
             (define-key evil-commentary-mode-map (kbd "C-/") 'evil-commentary-line))

(use-package evil-surround
             :config
             (global-evil-surround-mode 1))

(use-package evil-multiedit
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
  "b" '(consult-buffer :which-key "Open buffer picker")
  "B" '(consult-line :which-key "Search in current buffer")
  "d" '(consult-flycheck :which-key "Open diagnostics")
  "e" '(eval-expression :which-key "evaluate elisp expression")
  ;; "f" '(affe-find :which-key "Open file picker")
  "f" '(consult-projectile :which-key "Open file picker")
  "F" '(find-file :which-key "Open file browser")
  "m" '(magit-status :which-key "Git status")
  "/" '(affe-grep :which-key "Global search at cwd"))

(use-package which-key
             :init (which-key-mode 1)
             :diminish
             :config
             (setq which-key-side-window-location 'bottom
                   which-key-sort-order #'which-key-key-order-alpha
                   which-key-sort-uppercase-first nil
                   which-key-add-column-padding 1
                   which-key-max-display-columns nil
                   which-key-min-display-lines 6
                   which-key-side-window-slot -10
                   which-key-side-window-max-height 0.25
                   which-key-idle-delay 0.8
                   which-key-max-description-length 25
                   which-key-allow-imprecise-window-fit t
                   which-key-separator " -> "))

(use-package consult
             :hook (completion-list-mode . consult-preview-at-point-mode)
             :custom
             (setq consult-project-function #'projectile-project-root)
             :config
             (setq consult-narrow-key "<")
             ;(setq read-file-name-function #'consult-find-file-with-preview)

             (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
               (interactive)
               (let ((default-directory (or dir default-directory))
                     (minibuffer-completing-file-name t))
                 (consult--read #'read-file-name-internal :state (consult--file-preview)
                                :prompt prompt
                                :initial initial
                                :require-match mustmatch
                                :predicate pred))))

(use-package consult-dir)

(use-package affe
  :config
  (consult-customize affe-grep :preview-key "M-.")
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
             :init (marginalia-mode))

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
                            (window-parameters (mode-line-format . none))))
             )

(use-package embark-consult
             ;; :hook
             ;; (embark-collect-mode . consult-preview-at-point-mode)
             )

(use-package vertico
             :init
             (vertico-mode))

(savehist-mode)

(use-package orderless
  :demand t
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
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless basic)
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
  :init
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/projects"))
  (setq projectile-switch-project-action #'projectile-dired)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  )

(use-package consult-projectile)

(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
             :config
             (global-git-gutter-mode +1))

(use-package eglot
  :hook
  (python-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (nix-mode . eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '((:rust-analyzer
                   :completion (:callable (:snippets "fill_arguments"))
                   :checkOnSave (:command "clippy"
                                          :allTargets :json-false)))))

(use-package flycheck :init (global-flycheck-mode))
(use-package consult-flycheck)

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(rustic-mode . rust-ts-mode))

(use-package rustic)
(use-package nix-mode
  :mode "\\.nix\\'")

(use-package corfu
  :custom
  ; (corfu-cycle t) ;; enable cycling for `corfu-next/previous'
  (corfu-auto t) ;; enable auto completion
  ; (corfu-separator ?\s) ;; orderless field separator
  ; (corfu-quit-at-boundary nil) ;; never quit at completion boundary
  ; (corfu-quit-no-match nil)
  ; (corfu-preview-current nil) ;; disable current candidate preview
  ; (corfu-preselect 'prompt) ;; preselect the prompt
  ; (corfu-on-exact-match nil) ;; configure handling of exact matches
  ; (corfu-scroll-margin 5) ;; use scroll margin
  :init
  (global-corfu-mode)
  (setq corfu-auto-delay 0
        corfu-auto-prefix 1)
  :bind
  (:map corfu-map
        ;; ("SPC" . corfu-insert-separator)
        ("RET" . nil)))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package envrc
             :init (envrc-global-mode))

(use-package popper
  :bind (("M-n" . popper-cycle)
         ("C-t" . popper-toggle)
         ("C-'" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
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
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000))

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
