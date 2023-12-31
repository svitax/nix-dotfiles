;; The default is 800 kilobytes. Measured in bytes
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
        (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; use-package is automatically added by the nix emacs overlay
;; here we just make sure we don't have to keep typing `:ensure t`
(setq use-package-always-ensure t)

;; Do not make unecessary sounds
(setq ring-bell-function #'ignore)

;; disable the default emacs startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1) ; disable the toolbar
(tooltip-mode -1) ; disable tooltips
(menu-bar-mode -1) ; disable the menu bar
(set-fringe-mode 10) ; give some breathing room

;; line numbers
(column-number-mode)
(setq display-line-numbers-type 'relative)

;; enable line numbers for some modes
(dolist (mode '(text-mode-hook
                 prog-mode-hook
                 conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
;; override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; fonts
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 200)
(set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 200)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 200)

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-themes
             :init (load-theme 'doom-gruvbox t)
             :config
             (setq doom-themes-enable-bold t
                   doom-themes-enable-italic t)
             (doom-themes-visual-bell-config) ; enable flashing mode-line on errors
             (doom-themes-org-config)) ; corrects (and improves) org-mode's native fontification.

(use-package doom-modeline
             :init (doom-modeline-mode 1))

(use-package evil
             :init
             (setq evil-want-C-u-scroll t)
             (setq evil-want-integration t)
             (setq evil-want-keybinding nil)
             (setq evil-vsplit-window-right t)
             (setq evil-split-window-below t)
             :config
             (evil-mode)
             (evil-set-undo-system 'undo-redo))

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
             (evil-commentary-mode))

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

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

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
  "e" '(eval-expression :which-key "evaluate elisp expression")
  "f" '(consult-find :which-key "Open file picker")
  "F" '(find-file :which-key "Open file browser"))


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
             :custom (setq consult-project-function #'projectile-project-root)
             :config (setq consult-narrow-key "<"))

(use-package marginalia
             :init (marginalia-mode))

(use-package embark
             ;; :bind
             ;; (("C-." . embark-act)
             ;;  ("C-;" . embark-dwim))
             :init
             (setq prefix-help-command #'embark-prefix-help-command)
             (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
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

(use-package emacs
             :init
             (setq enable-recursive-minibuffers t))

(use-package orderless
             :init
             (setq completion-style '(orderless basic)
                   completion-category-defaults nil
                   completion-category-overrides '((file (styles partial-completion)))))

(use-package projectile
  :init
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/projects/"))
  (setq projectile-switch-project-action #'projectile-dired)
  ;; :bind (:map projectile-mode-map
  ;;             ("s-p" . projectile-command-map)
  ;;             ("C-c p" . projectile-command-map))
  )

(use-package consult-projectile)

(use-package magit
             ;; :bind (("C-x g" . magit-status))
             )

(use-package git-gutter
             :config
             (global-git-gutter-mode +1))
