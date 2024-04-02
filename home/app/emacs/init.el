;;; init.el -*- lexical-binding: t; -*-

(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-enable-imenu-support t
        use-package-minimum-reported-time 0.01)
  (require 'use-package))

;; "plugins/" contains downloaded packages or plugins I've written
(add-to-list 'load-path (concat user-emacs-directory "plugins"))
;; "lisp/" is configuration and glue code.
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(defvar user-cache-directory "~/.cache/emacs/"
  "Location where files created by Emacs are placed.")

(defvar user-etc-directory
  (file-name-concat user-cache-directory "etc/")
  "The directory where packages place their configuration files.")

(defvar user-var-directory
  (file-name-concat user-cache-directory "var/")
  "The directory where packages place their persistent data files.")

(defun expand-etc-file-name (file)
  "Expand filename FILE relative to `user-etc-directory'."
  (file-name-concat user-etc-directory
                    (convert-standard-filename file)))

(defun expand-var-file-name (file)
  "Expand filename FILE relative to `user-var-directory'."
  (file-name-concat user-var-directory
                    (convert-standard-filename file)))

(defalias 'etc #'expand-etc-file-name)
(defalias 'var #'expand-var-file-name)

;;; evil

(use-package general
  :ensure t
  :init (general-evil-setup))

(use-package general-extras
  :after general
  :config
  (+general-global-menu! "goto" "g")
  (+general-global-menu! "notes" "n")
  (+general-global-menu! "search" "s")
  (+general-global-menu! "vc" "v")
  (+general-global-menu! "toggle" "x")
  (+general-global-menu! "package" "/"))

(use-package evil
  :ensure t
  :general-config
  (general-nmap
    ;; for some reason `xref-find-definitions' and by
    ;; consequence `evil-goto-definition' stops working randomly
    ;; for some elisp symbols. `embark-dwim' doesn't for
    ;; some reason?
    ;; "gd" 'embark-dwim
    "gd" 'xref-find-definitions
    "C-S-t" 'xref-go-forward
    "L" 'evil-end-of-line
    "H" 'my/back-to-indentation-or-beginning)
  (general-omap
    "L" 'evil-end-of-line
    "H" 'my/back-to-indentation-or-beginning)
  :custom
  (evil-want-keybinding nil)
  (evil-symbol-word-search t)
  (evil-echo-state nil)
  (evil-undo-system 'undo-redo)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-Y-yank-to-eol t)
  (evil-respect-visual-line-mode t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  :init (evil-mode))

(use-package evil-collection
  :ensure t
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-find-usages-bindings t)
  :init (evil-collection-init))

(use-package anzu
  :ensure t
  :init (global-anzu-mode))

(use-package evil-anzu :ensure t)

(use-package evil-surround
  :ensure t
  :general (general-vmap "s" 'evil-surround-region)
  :init (global-evil-surround-mode))

;;; theme

(use-package theme)

;; (use-package doom-themes
;;   :ensure t
;;   :custom
;;   (doom-themes-enable-bold t)
;;   (doom-themes-enable-italic t)
;;   (doom-gruvbox-dark-variant "hard")
;;   :init
;;   (add-hook 'enable-theme-functions #'doom-themes-custom-faces)
;;   (load-theme 'doom-gruvbox t))

(use-package modus-themes
  :ensure t
  :init
  ;; Using the hook lets our changes persist when we use the commands
  ;; `modus-themes-toggle', `modus-themes-select', and `modus-themes-load-random'.
  (add-hook 'modus-themes-post-load-hook #'modus-themes-custom-faces)
  (add-hook 'modus-themes-post-load-hook #'my/modus-themes-flymake-bitmaps)
  (modus-themes-select 'modus-vivendi)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  ;; Control the style of command prompts (e.g. minibuffer, shell, IRC clients).
  ;; `modus-themes-prompts' are either nil (the default), or a list of
  ;; properties that may include any of those symbols: `italic', `WEIGHT'
  (modus-themes-prompts '(bold))
  (modus-themes-common-palette-overrides `(;; To hide the border around the active and inactive mode lines, we
                                           ;; set their color to that of the underlying background
                                           (bg-mode-line-active bg-inactive)
                                           (fg-mode-line-active fg-main)
                                           ;; line-numbers
                                           (fg-line-number-active fg-main)
                                           (bg-line-number-inactive bg-main)
                                           (fg-line-number-inactive fg-dim)
                                           ;; links
                                           (underline-link unspecified)
                                           (underline-link-visited unspecified)
                                           (underline-link-symbolic unspecified)
                                           ;; Enable underlining matching parenthesis by applying a color to them
                                           (underline-paren-match fg-main)
                                           ;; Make the fringe invisible
                                           (fringe unspecified))))

(use-package ef-themes
  :ensure t
  :init
  ;; Using the hook lets our changes persist when we use the commands
  ;; `ef-themes-toggle', `ef-themes-select', and `ef-themes-load-random'.
  (add-hook 'ef-themes-post-load-hook #'ef-themes-custom-faces))

(use-package pulsar
  :ensure t
  :general-config (+general-global-toggle
                    "l" 'pulsar-pulse-line
                    "L" 'pulsar-highlight-dwim)
  :init (pulsar-global-mode 1)
  :hook
  (next-error . pulsar-pulse-line-red)
  (next-error . pulsar-recenter-top)
  (next-error . pulsar-reveal-entry)
  (minibuffer-setup . pulsar-pulse-line-cyan)
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry)
  (better-jumper-post-jump . pulsar-pulse-line-magenta)
  (dumb-jump-after-jump . pulsar-pulse-line-magenta)
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-cyan)
  :config
  ;; TODO: remove unused functions from pulsar-pulse-functions
  (dolist (func '(evil-goto-definition evil-scroll-down evil-scroll-up
                  ;; evil-yank evil-paste-after evil-paste-before evil-delete
                  ;; evil-delete-line evil-delete-whole-line
                  evil-goto-last-change evil-goto-last-change-reverse
                  evil-jump-backward evil-jump-forward
                  pop-tag-mark my/transpose-windows my/toggle-window-split
                  my/split-window-below my/split-window-right
                  my/delete-window-or-delete-frame my/kill-this-buffer
                  xref-find-definitions xref-find-references xref-go-forward
                  xref-go-back embark-dwim embark-find-definition))
    (add-to-list 'pulsar-pulse-functions func)))

(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :custom-face
  (goggles-added ((t :inherit pulsar-green)))
  (goggles-changed ((t :inherit pulsar-yellow)))
  (goggles-removed ((t :inherit pulsar-red))))

(use-package lin
  :ensure t
  :init (lin-global-mode)
  :custom (lin-face 'lin-magenta))

(use-package spacious-padding
  :ensure t
  :bind ("<f8>" . spacious-padding-mode)
  ;; :init (spacious-padding-mode)
  :custom
  (spacious-padding-subtle-mode-line '(:mode-line-active 'default
                                       :mode-line-inactive 'vertical-border))
  (spacious-padding-widths '( :internal-border-width 16
                              :header-line-width 4
                              :mode-line-width 2
                              :tab-width 2
                              :right-divider-width 24
                              :scroll-bar-width 8)))

(use-package rainbow-mode
  :ensure t
  :hook ((prog-mode text-mode) . rainbow-mode)
  :general-config (+general-global-toggle
                    "c" 'rainbow-mode)
  :custom
  (rainbow-ansi-colors nil)
  (rainbow-x-colors nil))

(use-package cursory
  :ensure t
  :general-config (+general-global-toggle
                    "p" 'cursory-set-preset)
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

(use-package fontaine
  :ensure t
  :general-config (+general-global-toggle
                    "f" 'fontaine-set-preset)
  :custom
  (x-underline-at-descent-line nil)
  (text-scale-remap-header-line t)
  (fontaine-latest-state-file (var "fontaine-latest-state.eld"))
  (fontaine-presets '((small
                       :default-family "JetBrains Mono Nerd Font"
                       :default-height 115
                       :variable-pitch-family "Iosevka Comfy Duo")
                      (regular) ; like this it uses all the fallback values and is named `regular'
                      (medium
                       :default-family "JetBrains Mono Nerd Font"
                       :default-weight semilight
                       :default-height 160
                       :bold-weight medium)
                      (large
                       :inherit bold
                       :default-height 180)
                      (live-stream
                       :default-family "JetBrains Mono Nerd Font"
                       :default-height 180
                       :default-weight medium
                       :fixed-pitch-family "Iosevka Comfy Wide Motion"
                       :variable-pitch-family "Iosevka Comfy Wide Dup"
                       :bold-weight extrabold)
                      (presentation
                       :default-height 200)
                      (t
                       :default-family "JetBrains Mono Nerd Font"
                       :default-weight regular
                       :default-slant normal
                       :default-height 130
                       :fixed-pitch-family "Iosevka Comfy"
                       :variable-pitch-family "Iosevka Comfy Motion Duo")))
  :init
  ;; Set last preset of fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'medium))
  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
  (add-hook 'fontaine-set-preset-hook #'fontaine-store-latest-preset)
  ;; Persist font configurations while switching themes. The
  ;; `enable-theme-functions' is from Emacs 29.
  (add-hook 'enable-theme-functions #'fontaine-apply-current-preset))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :custom (hl-todo-wrap-movement t))

(use-package consult-todo
  :ensure t
  :general-config (+general-global-goto
                    "t" 'consult-todo))

;; TODO: variable-pitch-mode https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-theme.el
;; TODO: visible-mark https://git.sr.ht/~iank/visible-mark/tree/c1852e13b6b61982738b56977a452ec9026faf1b
;; TODO: highlight-thing https://github.com/fgeller/highlight-thing.el/tree/ad788d7a7ee9eb287a8cca3adb21510b89270dca
;; TODO: idle-highlight-mode? https://codeberg.org/ideasman42/emacs-idle-highlight-mode
;; TODO: continuation-fringe-indicator? https://xenodium.com/toggling-emacs-continuation-fringe-indicator

;;; editing

(use-package editing-extras
  :bind
  ("C-a" . my/back-to-indentation-or-beginning)
  ("M-a" . beginning-of-defun) ; override `backward-sentence' ; TODO: puni-beginning-of-sexp
  ("C-M-a" . backward-sentence) ; override `beginning-of-defun' ; TODO: puni-syntactic-forward-punct
  ("M-B" . backward-to-word)
  ("M-e" . end-of-defun) ; override `forward-sentence' ; TODO: puni-end-of-sexp
  ("C-M-e" . forward-sentence) ; override `end-of-defun' ; TODO: puni-syntactic-forward-punct
  ("M-f" . forward-to-word) ; override `forward-word'
  ("M-F" . forward-word)
  ("M-k" . kill-sexp) ; override `kill-sentence'
  ("C-M-k" . kill-sentence) ; override `kill-sexp'
  ("M-L" . my/mark-line)
  ("C-o" . my/open-next-line) ; override `open-line'
  ("C-M-o" . my/open-previous-line) ; override `split-line'
  ("C-w" . my/backward-kill-word-or-region)
  ("M-w" . kill-ring-save) ; advised
  ("C-x ," . beginning-of-buffer)
  ("C-x ." . end-of-buffer)
  ("C-z" . zap-up-to-char)
  ("M-z" . my/zap-to-char-save)
  ("M-SPC" . my/cycle-spacing-impatient) ; override `cycle-spacing'
  ("M-;" . my/comment-dwim)
  ("C-/" . undo-only)
  ("C-M-/" . undo-redo)
  ("C-M-|" . indent-region)
  ("C-|" . my/pipe-region)
  ([M-up] . my/move-text-up)
  ([M-down] . my/move-text-down))

;; TODO: rect (oantolin)

(use-package crux
  :ensure t
  ;; TODO: evil-visual-line includes the next line as well
  :general (general-nvmap "R" 'crux-duplicate-current-line-or-region))

;; TODO: evil-multiedit
;; TODO: evil-mc
;; (use-package iedit
;;   :ensure t
;;   :bind
;;   ("C-;" . iedit-mode)
;;   (:map iedit-mode-keymap
;;      ("ESC" . iedit--quit)))

;; (use-package iedit-extras
;;   :general (+general-global-search
;;           "n" 'my/iedit-1-down
;;           "p" 'my/iedit-1-up))

;; (use-package easy-kill
;;   :ensure t
;;   :bind (([remap kill-ring-save] . easy-kill)))

;; (use-package easy-kill-extras
;;   :bind (:map easy-kill-base-map
;;               ("." . my/easy-kill-expand-region)
;;               ("," . my/easy-kill-contract-region)))

(use-package elec-pair
  :init (electric-pair-mode))

(use-package replace
  :bind (:map occur-mode-map ("C-x C-q" . occur-edit-mode)))

(use-package visual-regexp
  :ensure t
  :general-config (global-definer "r" 'vr/query-replace))

(use-package vundo
  :ensure t
  :general-config (global-definer "u" 'vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t)
  (vundo-window-max-height 8))

(use-package undo-fu-session
  :ensure t
  :custom (undo-fu-session-directory (var "undo-fu-session/"))
  :init (undo-fu-session-global-mode))

;; TODO: puni
;; TODO: visual-regexp
;; TODO: avy-zap https://github.com/cute-jumper/avy-zap
;; TODO: surround or embrace https://github.com/mkleehammer/surround or https://github.com/cute-jumper/embrace.el
;; TODO: easy-kill/expreg-easy-kill?
;; TODO: ts-docstr https://github.com/emacs-vs/ts-docstr
;; TODO: change-inner https://github.com/magnars/change-inner.el
;; TODO: annotate https://github.com/bastibe/annotate.el

;;; navigation

;; FIXME: avy
;; https://github.com/karthink/.emacs.d/blob/master/plugins/demo.el
;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-avy.el
;; https://karthinks.com/software/avy-can-do-anything
(use-package avy
  :ensure t
  :general
  (general-nvmap "C-s" 'avy-goto-char-timer)
  (+general-global-search "j" 'avy-goto-line)
  :custom
  (avy-timeout-seconds 0.35)
  (avy-single-candidate-jump nil)
  :bind
  ("C-s" . avy-goto-char-timer)
  ([remap goto-line] . avy-goto-line)
  (:map isearch-mode-map
        ("M-q" . avy-isearch)))

(use-package xref
  :custom
  (xref-auto-jump-to-first-definition 'move)
  (xref-auto-jump-to-first-xref 'move))

(use-package dumb-jump
  :ensure t
  :config
  (add-to-list 'dumb-jump-project-denoters ".project")
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate t))

(use-package expreg
  :ensure t
  :general (general-nvmap "RET" 'expreg-expand))

;; TODO: drag-stuff
;; (use-package drag-stuff
;;   :ensure t
;;   :general-config
;;   (general-nvmap
;;     "M-<up>" 'drag-stuff-up
;;     "M-<down>" 'drag-stuff-down
;;     "M-<left>" 'drag-stuff-left
;;     "M-<right>" 'drag-stuff-right))

;; (use-package better-jumper
;;   :ensure t
;;   :init (better-jumper-mode)
;;   :bind
;;   ([remap xref-pop-marker-stack] . better-jumper-jump-backward)
;;   ([remap xref-go-back] . better-jumper-jump-backward)
;;   ([remap xref-go-forward] . better-jumper-jump-forward)
;;   :custom (better-jumper-context 'window))

;; (use-package better-jumper-extras)

(use-package evil-matchit
  :ensure t
  :init (global-evil-matchit-mode))

;; TODO: harpoon https://github.com/kofm/harpoon.el
;; TODO: ace-link https://github.com/abo-abo/ace-link
;; TODO: buffer-local-xref https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L2454
;; TODO: smart-tab-over?
;; TODO: treesit-jump? https://github.com/dmille56/treesit-jump

;;; modeline
(use-package mood-line
  :ensure t
  :custom
  (mood-line-format (mood-line-defformat
                     :left
                     (((mood-line-segment-modal) . "  ")
                      ((mood-line-segment-anzu) . "  ")
                      ((mood-line-segment-buffer-status) . "  ")
                      ((mood-line-segment-project) . "/")
                      ((mood-line-segment-buffer-name)   . "  ")
                      ((mood-line-segment-cursor-position) . "  ")
                      (mood-line-segment-misc-info))
                     :right
                     (((mood-line-segment-vc)             . "  ")
                      (mood-line-segment-major-mode))))
  :config
  (defun mood-line-segment-misc-info ()
    "Return the current value of `mode-line-misc-info'."
    (let ((misc-info (format-mode-line mode-line-misc-info)))
      misc-info))
  (mood-line-mode)
  (column-number-mode))
;; https://github.com/sebasmonia/dotfiles/.emacs/init.el
;; https://gist.github.com/clemera
;; https://github.com/jerrypnz/.emacs.d/blob/master/lisp/jp-modeline.el

(use-package keycast :ensure t)

(use-package keycast-extras
  :after keycast
  :general-config (+general-global-toggle "k" 'keycast-mode)
  :commands keycast-mode
  :init (keycast-mode))

;;; completion

(use-package minibuffer
  :custom
  (completion-styles '(orderless basic) "The basic completion style is specified
as fallback in addition to orderless in order to ensure that completion commands
which rely on dynamic completion tables work correctly")
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

(use-package minibuffer-extras)

(use-package savehist
  :init
  (savehist-mode)
  :custom
  (savehist-file (var "savehist.el"))
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 300)
  (savehist-ignored-variables '(file-name-history))
  (savehist-additional-variables '(kill-ring register-alist
                                   mark-ring global-mark-ring
                                   search-ring regexp-search-ring))
  (history-length 1000)
  (history-delete-duplicates t))

(use-package savehist-extras)

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

(use-package corfu
  :ensure t
  :hook
  ((prog-mode text-mode tex-mode ielm-mode) . corfu-mode)
  ((eshell-mode comint-mode) . corfu-mode)
  :bind (:map corfu-map
              ("M-SPC" . 'corfu-insert-separator)
              ("TAB" . corfu-insert)
              ("RET" . nil)
              ("M-h" . nil)
              ("C-h" . corfu-info-documentation)
              ("M-m" . 'my/corfu-move-to-minibuffer)
              ("M-." . corfu-info-location))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.05)
  (corfu-count 10)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-quit-no-match 'separator)
  (corfu-scroll-margin 5))

(use-package corfu-extras
  :hook
  (minibuffer-setup . my/corfu-enable-in-minibuffer)
  ;; (minibuffer-setup . my/corfu-enable-always-in-minibuffer)
  :custom
  (corfu-sort-override-function 'my/corfu-combined-sort))

(use-package corfu-popupinfo
  :after corfu
  :config (corfu-popupinfo-mode 1)
  :bind (:map corfu-map
              ([remap corfu-info-documentation] . corfu-popupinfo-toggle))
  :custom (corfu-popupinfo-delay '(2.0 . 0.05)))

(use-package corfu-echo
  :after corfu
  :config (corfu-echo-mode)
  :custom (corfu-echo-delay '(0.05 . 0.05)))

(use-package corfu-history
  :after corfu
  :init (corfu-history-mode 1)
  :config (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-quick
  :after corfu
  :bind (:map corfu-map
              ("M-q" . corfu-quick-insert))
  :custom (corfu-quick1 "asdfghjkl;"))

(use-package cape
  :ensure t
  :bind ("M-/" . cape-dabbrev)
  :custom (cape-dabbrev-check-other-buffers nil))

(use-package cape-extras
  :hook
  (emacs-lisp-mode . my/setup-elisp-capf)
  (python-base-mode . my/setup-python-capf)
  ;; (jupyter-repl-interaction-mode . my/setup-jupyter-eglot-capf)
  ((text-mode prog-mode) . my/add-cape-capf))

;; TODO: org-block-capf https://github.com/xenodium/org-block-capf
;; TODO: org-src-context https://github.com/karthink/org-src-context

(use-package consult
  :ensure t
  :hook ((embark-collect-mode completion-list-mode) . consult-preview-at-point-mode)
  :general-config
  (global-definer
    "b" 'consult-buffer
    "q" 'consult-kmacro
    "y" 'consult-yank-pop)
  (+general-global-search
    "d" 'consult-find
    "D" 'consult-locate
    "e" 'consult-isearch-history
    "g" 'consult-grep
    "G" 'consult-git-grep
    "i" 'consult-info
    "k" 'consult-keep-lines
    "m" 'consult-man
    "r" 'consult-ripgrep
    "u" 'consult-focus-lines
    "x" 'consult-mode-command
    ";" 'consult-complex-command)
  (+general-global-goto
    "e" 'consult-compile-error
    "f" 'consult-flymake
    "g" 'consult-goto-line
    "i" 'consult-imenu
    "I" 'consult-imenu-multi
    "k" 'consult-bookmark
    "l" 'consult-line
    "L" 'consult-line-multi
    "m" 'consult-mark
    "M" 'consult-global-mark
    "o" 'consult-outline)
  (general-def :keymaps 'project-prefix-map "b" 'consult-project-buffer)
  (general-def :keymaps 'isearch-mode-map "C-r" 'consult-isearch-history)
  (general-def :keymaps 'minibuffer-local-map "C-r" 'consult-history)
  (general-def :keymaps 'consult-narrow-map "?" 'consult-narrow-help)
  :custom
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur))

(use-package consult-dir
  :ensure t
  :general-config
  (global-definer "z" 'consult-dir)
  (general-def :keymaps 'minibuffer-local-filename-completion-map
    "C-M-z" 'consult-dir
    "C-M-j" 'consult-dir-jump-file))

;; FIXME: embark alist?
(use-package embark
  :ensure t
  :general
  (general-nvmap
    "C-." 'embark-act
    "C-:" 'embark-act-all
    "M-." 'embark-dwim)
  (general-def :keymaps 'help-map
    "b" 'embark-bindings
    "B" 'embark-bindings-at-point
    "M" 'embark-bindings-in-keymap
    "C-h" 'embark-prefix-help-command)
  (general-def :keymaps 'vertico-map
    "C-c C-o" 'embark-collect
    "C-M-l" 'embark-export)
  :custom
  (embark-quit-after-action nil)
  ;; Replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command "Replace the key help with a completing-read interface")
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-confirm-act-all nil))

(use-package embark-extras
  :after embark
  :bind (:map minibuffer-local-map
              ("C-<tab>" . my/embark-select)))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom (marginalia-max-relative-age 0)
  :init (marginalia-mode))

(use-package vertico
  :ensure t
  :general-config
  (general-imap :keymaps 'vertico-map
    "C-j" 'vertico-next
    "C-k" 'vertico-previous
    "C-u" 'vertico-scroll-down
    "C-d" 'vertico-scroll-up)
  :bind
  (:map vertico-map
        ("C-<return>" . vertico-exit-input)
        ("M-n" . vertico-next-group)
        ("M-p" . vertico-previous-group)
        ("M-q" . vertico-quick-jump))
  (:map minibuffer-local-map
        ("<up>" . previous-history-element)
        ("<down>" . next-history-element))
  :custom
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-resize 'grow-only)
  :init (vertico-mode)
  :config (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-package vertico-directory
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("C-w" . vertico-directory-delete-word)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-multiform
  :after vertico
  :hook (vertico-mode . vertico-multiform-mode))

;;; search
(use-package isearch
  :general-config
  (general-def :keymaps 'isearch-mode-map
    "M-." 'isearch-forward-thing-at-point)
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil))

;; TODO: grep
;; TODO: wgrep https://github.com/mhayashi1120/Emacs-wgrep
;; https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L4039
;; TODO: occur
;; TODO:: isearch-extras
;; TODO: imenu https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L1593
;; TODO: cc-isearch-menu

;;; dired

(use-package dired
  :general
  (global-definer "e" 'dired-jump)
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-AGhlvX")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-auto-revert-buffer 'dired-buffer-stale-p)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (delete-by-moving-to-trash t)
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-extras
  :hook (evil-collection-setup . (lambda (&rest a)
                                   (general-nmap :keymaps '(dired-mode-map)
                                     "a" 'find-file
                                     "~" #'my/dired-home-directory)))
  :preface
  (defun my/dired-home-directory ()
    (interactive)
    (dired-single-buffer (expand-file-name "~/")))
  :general
  (general-nmap
    :keymaps '(dired-mode-map)
    "l" 'dired-find-file
    "h" 'dired-up-directory
    "/" 'my/dired-limit-regexp))

(use-package wdired
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package trashed
  :ensure t
  :custom
  (trashed-action-confirmer 'y-or-n-p)
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t))
  (trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(use-package dired-single
  :ensure t
  :bind
  ([remap dired-find-file] . dired-single-buffer)
  ([remap dired-up-directory] . dired-single-up-directory))

;; TODO: dirvish???
;; TODO: dired-subtree
;; TODO: dired-preview
;; TODO: dired-open vs dired-launch
;; https://codeberg.org/thomp/dired-launch
;; TODO: dired-atool?
;; TODO: dired-rsync?
;; TODO: dired-gitignore?
;; TODO: sudo-edit?
;; TODO: dired-hide-dotfiles

;;; ibuffer
(use-package ibuffer
  :general (global-definer "B" 'ibuffer-jump))

(use-package ibuffer-vc
  :ensure t
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic))))
  :init
  (setq ibuffer-formats '((mark modified read-only vc-status-mini " "
                           (name 18 18 :left :elide)
                           " "
                           (size 9 -1 :right)
                           " "
                           (mode 16 16 :left :elide)
                           " "
                           (vc-status 16 16 :left)
                           " "
                           vc-relative-file))))

;; FIXME: https://www.reddit.com/r/emacs/s/Ft0yZxEMVD
;; FIXME: ibuffer-git https://github.com/jrockway/ibuffer-git
;; TODO: projection-ibuffer?
;; TODO: bufler?

;;; bookmark
;; FIXME: dogears (vs better-jumper)
;; TODO: bookmark https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L3320
;; TODO: blist? https://github.com/emacsmirror/blist

;;; window

(use-package window
  :bind ("M-o" . other-window)
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
  (auto-window-vscroll nil))

(use-package window-extras
  :general
  (global-definer
    "c" 'my/delete-window-or-delete-frame
    "k" 'my/kill-this-buffer
    "w" 'evil-window-map)
  (general-def
    :keymaps '(evil-window-map)
    "C-h" nil))

(use-package hl-line
  :custom (hl-line-sticky-flag nil))

(use-package whitespace
  :init (global-whitespace-mode)
  :bind ("<f6>" . whitespace-mode)
  :custom
  (whitespace-display-mappings '((tab-mark ?\t [?\u21E5 ?\t])
				 (newline-mark ?\n [?\u21A9 ?\n])
				 (newline ?\n [?\u21A9 ?\n])
				 (space-mark ?\  [?\u00B7] [?.])))
  (whitespace-style '(lines-char
		      ;; newline
		      face tabs tab-mark trailing missing-newline-at-eof
		      space-after-tab::tab space-after-tab::space
		      space-before-tab::tab space-before-tab::space))
  ;; whitespace-style '(face trailing newline newline-mark tabs tab-mark)
  ;; whitespace-style '(face empty trailing tabs tab-mark)
  (whitespace-action '(cleanup auto-cleanup))
  (whitespace-global-modes '(not shell-mode magit-mode magit-diff-mode
			     ibuffer-mode dired-mode occur-mode erc-mode)))

(use-package display-line-numbers
  :init (global-display-line-numbers-mode)
  :bind ("<f7>" . display-line-numbers-mode)
  :custom
  (display-line-numbers-major-tick 0)
  (display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (display-line-numbers-widen t))

(use-package centered-cursor)

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
  (use-package popper-extras
    :custom
    (popper-echo-transform-function #'my/popper-message-shorten))
  (popper-mode)
  (popper-echo-mode))
;; :config (define-key org-mode-map (kbd "C-'") nil))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t))

;; TODO: display-buffer-alist
;; TODO: windmove
;; TODO: recursive-narrow https://github.com/nflath/recursive-narrow
;; TODO: narrow
;;   (put 'narrow-to-region 'disabled nil)
;; TODO: ts-fold
;; TODO: breadcrumb
;; TODO: swap-windows https://github.com/karthink/.emacs.d/blob/master/lisp/better-buffers.el#L153-L167
;; TODO: toggle-window-split https://github.com/karthink/.emacs.d/blob/master/lisp/better-buffers.el#L125-L151

;;; vc

;; TODO: magit https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/lisp/setup-vc.el#L259
(use-package magit
  :ensure t
  :general
  (+general-global-vc
    "b" 'magit-blame
    "C" 'magit-clone
    "d" 'magit-diff-dwim
    "g" 'magit-status
    "i" 'magit-init
    "l" 'magit-log
    "s" 'magit-state)
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)
  ;; (magit-diff-hide-trailing-cr-characters t)
  ;; (magit-diff-refine-ignore-whitespace t)
  )

(use-package diff-hl
  :ensure t
  ;; TODO: gs to stage hunk in diff-hl
  ;; :general
  ;; (general-def
  ;;   "gs" #'diff-hl-stage-current-hunk)
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

;; TODO: ediff https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/lisp/setup-diff.el#L11
;; prot's ediff video
;; TODO: diff-mode
;; TODO: diff-mode https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/lisp/setup-diff.el#L1
;; TODO: vc https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/lisp/setup-vc.el#L4
;; prot's vc video
;; TODO: transient
;; TODO: gist?
;; TODO: smerge?
;; TODO: git-modes?
;; TODO: forge
;; TODO: blamer? https://github.com/Artawower/blamer.el
;; does vc provide good enough support? or does sideline-blame provide better ui?
;; TODO: consult-gh?
;; TODO: magit-commit-mark https://codeberg.org/ideasman42/emacs-magit-commit-mark
;; TODO: difftastic https://github.com/kryger/difftastic.el
;; TODO: prr https://github.com/danobi/prr
;; TODO: emacs-pr-review https://github.com/blahgeek/emacs-pr-review
;; TODO: abridge-diff https://github.com/jdtsmith/abridge-diff
;; TODO: diffview-mode https://github.com/mgalgs/diffview-mode
;; TODO: magit-imerge https://github.com/magit/magit-imerge
;; (use-package magit-imerge
;;   :ensure t
;;   :after magit
;;   :init (transient-append-suffix 'magit-merge "m" '("M" "magit-imerge" magit-imerge)))
;; TODO: git-timemachine
;; (use-package git-timemachine
;;   :ensure t
;;   ;; TODO: gt to toggle git-timemachine
;;   :general
;;   (general-def
;;     "gt" #'git-timemachine-toggle)
;;   :custom (git-timemachine-show-minibuffer-details t))
;; TODO: git-commit
;; (use-package git-commit
;;   :ensure t
;;   :after magit
;;   :custom
;;   (git-commit-summary-max-length 72)
;;   (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
;;   :config
;;   (evil-set-initial-state 'git-commit-mode 'insert)
;;   (global-git-commit-mode 1))
;; TODO: git-modes
;; (use-package git-modes
;;   :ensure t
;;   :mode ("/.dockerignore\\'" . gitignore-mode))

;;; project

;; TODO: project https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/lisp/setup-project.el#L12
(use-package project
  :general
  (global-definer
    "f" 'project-find-file
    "j" 'project-switch-project)
  :custom
  (project-switch-commands #'project-find-file)
  (project-list-file (file-name-concat user-cache-directory "var/projects")))

;; FIXME: projection map SPC p https://github.com/mohkale/projection
(use-package projection
  :ensure t
  ;; :general-config
  ;; ([remap project-compile] 'projection-build-project)
  :init (global-projection-hook-mode))

(use-package projection-multi
  :ensure t
  :general-config
  ([remap compile-multi] 'projection-multi-compile))

(use-package projection-multi-embark
  :ensure t
  :after embark
  :after projection-multi
  :demand t
  :config (projection-multi-embark-setup-command-map))

;; TODO: editorconfig https://github.com/editorconfig/editorconfig-emacs
;; TODO: projel https://github.com/KarimAziev/projel

(use-package envrc
  :ensure t
  :init (envrc-global-mode))

;; TODO: inheritenv https://github.com/purcell/inheritenv
(use-package inheritenv
  :ensure t
  ;; Envrc sets environment variables in Emacs buffer-locally. This allows users
  ;; to have different buffer-local paths for executables in different projects.
  ;; However when Emacs libraries run background processes on behalf of a user,
  ;; they often run processes in temporary buffers that do not inherit the calling
  ;; buffer's environment. This results in executables not being found, or the
  ;; wrong versions of executables being picked up.
  ;; `inheritenv' provides the macro `inheritenv-add-advice' which wraps any
  ;; command with an advice function so it inherits buffer-local variables.
  ;; This is useful for when we discover problems we can't patch upstream.
  :config
  ;; (inheritenv-add-advice 'jupyter-run-repl)
  ;; (inheritenv-add-advice 'flymake-ruff--run-checker)
  (inheritenv-add-advice 'run-python)
  (inheritenv-add-advice 'run-python-internal))

(use-package exec-path-from-shell
  :ensure t
  :custom (exec-path-from-shell-arguments '("-l"))
  :config (when (or (daemonp) (memq window-system '(mac ns x)))
            (exec-path-from-shell-initialize)))

;;; compile
(use-package compile
  :hook (compilation-filter . ansi-color-compilation-filter)
  :general (global-definer
             "," 'project-compile
             "." 'recompile)
  :custom
  (compilation-max-output-line-length nil)
  (compilation-scroll-output t)
  (compilation-always-kill t))

(use-package compile-extras
  :hook (python-base-mode . my/run-with-python))

(use-package compile-multi
  :ensure t
  :general (global-definer "/" 'compile-multi))

(use-package consult-compile-multi
  :ensure t
  :after compile-multi
  :config (consult-compile-multi-mode))

(use-package compile-multi-embark
  :ensure t
  :after embark
  :after compile-multi
  :demand t
  :config (compile-multi-embark-mode +1))

;; TODO: compile-extras

;;; help

(use-package help :general (global-definer "h" 'help-command))

(use-package helpful
  :ensure t
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-symbol] . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-command] . helpful-command)
   ([remap describe-key] . helpful-key)
   :map help-map
   ("C-." . helpful-at-point)
   ("." . helpful-at-point)
   ("C" . describe-command))
  :custom (helpful-max-buffers 1))

;; TODO: helpful embark
(use-package helpful
  :after (helpful embark)
  :bind (:map embark-become-help-map
              ("f" . helpful-callable)
              ("v" . helpful-variable)
              ("C" . helpful-command)))

(use-package eldoc
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package info-colors
  :ensure t
  :commands info-colors-fontify-node
  :hook (Info-selection . info-colors-fontify-node))

;; TODO: transient
(use-package transient
  :custom
  (transient-history-file (var "transient/history.el"))
  (transient-levels-file (etc "transient/levels.el"))
  (transient-values-file (etc "transient/values.el")))

(use-package repeat
  :init (repeat-mode)
  :custom
  (repeat-keep-prefix t)
  (repeat-echo-function #'repeat-echo-mode-line)
  (repeat-echo-mode-line-string
   (propertize "[R]" 'face 'mode-line-emphasis))
  :config
  (put 'other-window 'repeat-map nil))

(use-package repeat-help
  :ensure t
  :hook (repeat-mode . repeat-help-mode)
  :custom (repeat-help-popup-type 'embark))

;; TODO: devdocs?
;; TODO: ghelp?
;; TODO: lookup
;; TODO: region-bindings? https://andreyor.st/posts/2023-01-30-region-bindings-and-common-lisp-modes

;;; org
;; TODO: org-modern
;; TODO: org-block-capf https://github.com/xenodium/org-block-capf
;; TODO: bookmode?
;; TODO: polymode?
;; TODO: ox-ipynb?
;; TODO: openwith-mode?
;; TODO: org-transclusion?
;; TODO: olivetti?
;; TODO: org-agenda
;; TODO: org-super-agenda?
;; TODO: org-timeblock?
;; TODO: org-hyperscheduler?
;; TODO: org-anki https://github.com/eyeinsky/org-anki
;; TODO: org-noter https://github.com/org-noter/org-noter
;; TODO: doct https://github.com/progfolio/doct
;; TODO: org-msg https://github.com/jeremy-compostella/org-msg
;; TODO: org-web-tools https://github.com/alphapapa/org-web-tools
;; TODO: toc-org https://github.com/snosov1/toc-org
;; TODO: org-view-mode https://github.com/amno1/org-view-mode

;; TODO: emacs-calfw??? https://github.com/kiwanami/emacs-calfw

;;; langs

;; elisp
(require 'elisp-fontification)
(require 'elisp-indentation)

(use-package eros
  :ensure t
  :general
  (major-mode-definer emacs-lisp-mode-map
    "b" 'eval-buffer
    "d" 'eval-defun
    "e" 'eval-last-sexp)
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
  :custom (highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;; TODO: package-lint https://github.com/purcell/package-lint
;; TODO: macrostep https://github.com/emacsorphanage/macrostep

;; common lisp
;; TODO: common lisp modes https://andreyor.st/posts/2023-01-30-region-bindings-and-common-lisp-modes

;; rust
(use-package rust-mode :ensure t)

;; python
(use-package python
  :general-config
  (major-mode-definer
    :keymaps 'python-base-mode-map
    "b" 'python-shell-send-buffer
    "d" 'python-shell-send-defun
    "e" 'python-shell-send-statement)
  :custom
  (python-shell-dedicated 'project)
  (python-shell-interpreter "python")
  (python-shell-interpreter-args "")
  (python-indent-guess-indent-offset-verbose nil))

;; (use-package jupyter
;;   ;; `jupyter-completion-at-point' has a hard dependency on `company-doc-buffer'
;;   ;; to provide completion documentation popups
;;   :ensure t
;;   :general-config
;;   (major-mode-definer python-base-mode-map
;;     ;; "a" taken
;;     "b" 'jupyter-eval-buffer
;;     "jc" 'jupyter-connect-repl
;;     "d" 'jupyter-eval-defun
;;     "e" 'jupyter-eval-line-or-region
;;     "jg" 'jupyter-repl-associate-buffer
;;     "jj" 'jupyter-run-repl
;;     "jk" 'jupyter-repl-restart-kernel
;;     "jl" 'jupyter-server-list-kernels
;;     ;; "r" taken
;;     "'" 'jupyter-repl-pop-to-buffer)
;;   :custom (jupyter-repl-echo-eval-p t))

;; TODO: poetry
;; (use-package poetry)
;; TODO: python-mls
;; (use-package python-mls)
;; TODO: code-cells
;; (use-package code-cells)
;; TODO: numpydoc
;; (use-package numpydoc)
;; TODO: docstring-mode
;; (use-package docstring-mode)

;; nix
(use-package nix-mode :ensure t)

(use-package nix-ts-mode
  :ensure t
  ;; :mode "\\.nix\\'"
  :config
  ;; Register Eglot servers on the `nix-ts-mode' in addition to the already
  ;; configured `nix-mode'
  (with-eval-after-load 'eglot
    (when-let ((server (assoc 'nix-mode eglot-server-programs)))
      (setcar server '(nix-mode nix-ts-mode)))))

;; TODO: org-nix-shell https://github.com/AntonHakansson/org-nix-shell

;; markdown
(use-package markdown-mode
  :ensure t
  :general
  (major-mode-definer
    :keymaps '(markdown-mode-map)
    :major-modes '(gfm-mode markdown-mode)
    "mc" 'markdown-insert-code
    "ml" 'markdown-insert-link)
  :custom (markdown-command "pandoc -t html5"))

;; TODO: json https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L2115
;; TODO: json-mode? https://github.com/json-emacs/json-mode
;; TODO: jsonian
;; TODO: yaml-pro https://github.com/zkry/yaml-pro
;; TODO: cargo-mode https://github.com/ayrat555/cargo-mode
;; TODO: templ-ts-mode https://github.com/danderson/templ-ts-mode
;; TODO: graphql-mode https://github.com/davazp/graphql-mode
;; TODO: logview https://github.com/doublep/logview
;; TODO: pkgbuild-mode https://github.com/juergenhoetzel/pkgbuild-mode
;; TODO: systemd-mode https://github.com/holomorph/systemd-mode
;; TODO: journalctl-mode https://github.com/SebastienMeisel/journalctl-mode
;; TODO: flymake-guile

;;; lsp
(use-package eglot
  :general
  (minor-mode-definer
    :keymaps 'eglot--managed-mode
    "a" 'eglot-code-actions
    "r" 'eglot-rename)
  ;; TODO: evil-lookup for eglot
  (general-def :keymaps 'eglot-mode-map
    "K" 'eldoc-doc-buffer)
  :custom
  (eglot-events-buffer-size 0)    ; disable eglot logging (improves performance)
  (eglot-autoshutdown t)            ; shutdown server when last managed buffer is killed
  (eglot-sync-connect 1)
  (eglot-connect-timeout 10)
  (eglot-send-changes-idle-time 0.5)
  (eglot-auto-display-help-buffer nil)
  (eglot-report-progress nil)
  (eglot-extend-to-xref t)
  (eglot-workspace-configuration
   '((:rust-analyzer . (:completion (:callable (:snippets "fill_arguments"))
                        :checkOnSave (:command "clippy" :allTargets :json-false))))))

(use-package eglot-extras
  :hook
  (after-init . my/eglot-auto-ensure-all)
  (eglot-managed-mode . my/eglot-eldoc-settings)
  (eglot-managed-mode . my/eglot-remove-mode-line-misc-info)
  (eglot-managed-mode . flymake-mode)
  :config
  (add-to-list 'eglot-server-programs
               '(toml-ts-mode . ("taplo" "lsp" "stdio"))))

;; TODO: emacs-lsp-booster in nix https://github.com/blahgeek/emacs-lsp-booster
;; TODO: eglot-booster https://github.com/jdtsmith/eglot-booster

(use-package consult-eglot
  :ensure t
  :general
  (minor-mode-definer
    :keymaps 'eglot--managed-mode
    "s" 'consult-eglot-symbols))

;; TODO: consult-eglot-embark

;;; checkers
(use-package flymake :custom (flymake-fringe-indicator-position 'right-fringe))

(use-package flymake-extras :after flymake)

(use-package flymake-collection
  :ensure t
  :config
  (defun my/python-mode-setup-flymake-collections ()
    (add-hook 'flymake-diagnostic-functions 'flymake-collection-ruff nil t)
    (add-hook 'flymake-diagnostic-functions 'flymake-collection-mypy nil t)
    (flymake-mode +1))
  (add-hook 'python-ts-mode-hook 'my/python-mode-setup-flymake-collections))

;; TODO: flymake-quickdef https://github.comabougouffa/minemacs/modules/me-checkers.el
;; TODO: flymake-pyre (uses quickdef) https://github.com/juergenhoetzel/flymake-pyre
;; TODO: flymake-relint https://github.com/liuyinz/flymake-relint

(use-package apheleia
  :ensure t
  :general
  (minor-mode-definer
    :keymaps 'apheleia-mode
    "F" 'apheleia-format-buffer)
  :init (apheleia-global-mode))

(use-package apheleia-extras :after apheleia)

(use-package sideline
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :custom (sideline-backends-right '(sideline-flymake)))

(use-package sideline-flymake
  :ensure t
  :custom
  (sideline-flymake-display-mode 'point)
  (sideline-flymake-show-backend-name t))

;; FIXME: sideline-eglot https://github.com/emacs-sideline/sideline-eglot
;; (use-package sideline-eglot :ensure t)
;; TODO: snap-indent https://github.com/jeffvalk/snapindent
;; TODO: jinx https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L2553d

;;; tree-sitter
(use-package treesit
  :custom (treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode)
  (setq treesit-auto-langs (append treesit-auto-langs '(nix elisp)))
  (setq treesit-auto-recipe-list
        (append treesit-auto-recipe-list
                (list (make-treesit-auto-recipe
                       :lang 'nix
                       :ts-mode 'nix-ts-mode
                       :remap 'nix-mode
                       :url "https://github.com/nix-community/tree-sitter-nix"
                       :revision "master"
                       :source-dir "src"
                       :ext "\\.nix\\'")
                      (make-treesit-auto-recipe
                       :lang 'elisp
                       :ts-mode 'emacs-lisp-ts-mode
                       :remap 'emacs-lisp-mode
                       :url "https://github.com/Wilfred/tree-sitter-elisp"
                       :ext "\\.el\\'"))))
  (treesit-auto-add-to-auto-mode-alist 'all))

;;; snippets
;; TODO: tempel
;; TODO: eglot-tempel
;; TODO: tempel-collection

;;; debug
;; TODO: dape
;; TODO: edebug https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L2580

;;; term
;; TODO: eshell
(use-package eshell
  :custom
  (eshell-aliases-file (etc "eshell/aliases"))
  (eshell-directory-name (var "eshell/"))
  (eshell-login-script (etc "eshell/login"))
  (eshell-rc-script (etc "eshell/rc")))
;; TODO: eat
;; TODO: comint
;; TODO: emacs-fish-completion https://github.com/LemonBreezes/emacs-fish-completion
;; TODO: emacs-bash-completion https://github.com/szermatt/emacs-bash-completion
;; TODO: dwim-shell-command? https://github.com/xenodium/dwim-shell-command
;; TODO: terminal-here
;; TODO: eshell-syntax-highlighting
;; TODO: load-bash-alias
;; TODO: with-editor https://github.com/magit/with-editor
;; TODO: pcmpl-args
;; (use-package pcmpl-args :ensure t)
;; (use-package pcmpl-args-extras
;;   :hook ((eshell-mode . my/pcmpl-args-eshell-settings)
;;       (eshell-mode shell-mode) . my/pcmpl-args-capf-ensure))
;; TODO: comint-mime https://github.com/astoff/comint-mime

;;; tramp
;; TODO: tramp https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L3274
(use-package tramp
  :custom
  (tramp-auto-save-directory (var "tramp/auto-save"))
  (tramp-persistency-file-name (var "tramp/persistency.el")))

;;; docker
;; TODO: docker https://github.com/Silex/docker.el
;; TODO: docker-tramp https://github.com/emacs-pe/docker-tramp.el
;; TODO: dockerfile-mode https://github.com/spotify/dockerfile-mode
;; there's a dockerfile-language-server and treesit might have dockerfile
;; TODO: flymake-hadolint https://github.com/buzztaiki/flymake-hadolint
;; TODO: docker-compose-mode https://github.com/meqif/docker-compose-mode

;;; biblio

(use-package citar
  :ensure t
  ;; so my `sx-biblio-bibtex-lookup' function loads in citar
  :commands citar--bibliography-files
  :general (+general-global-notes
             "p" 'citar-open-files
             "o" 'citar-open)
  :custom
  (citar-bibliography '("~/OneDrive/docs/lib.bib"))
  (citar-library-paths '("~/OneDrive/docs/books/"))
  (citar-templates
   '((main . "${title:55} ${author editor:55} ${date year issued:4}")
     (suffix . "  ${tags keywords keywords:40}")
     (preview . "${author editor} ${title}, ${journal publisher container-title collection-title booktitle} ${volume} (${year issued date}).\n")
     (note . "#+title: Notes on ${author editor}, ${title}"))))

(use-package citar-extras :after citar)

(use-package citar-embark
  :ensure t
  :init (citar-embark-mode))

(use-package citar-denote
  :ensure t
  :init (citar-denote-mode)
  :general
  (+general-global-notes
    "a" 'citar-denote-add-citekey
    "A" 'citar-denote-remove-citekey))

;; TODO: bibtex
;; TODO: citar-capf
;; TODO: bibtex-capf? https://github.com/mclear-tools/bibtex-capf
;; TODO: biblio-gscholar
;; TODO: biblio-bibsonomy https://github.com/andreasjansson/biblio-bibsonomy.el
;; TODO: persid https://github.com/rougier/persid
;; TODO: biblio-zotero https://github.com/gkowzan/biblio-zotero

;;; notes

(use-package denote
  :ensure t
  ;; BUG: incompatible with diredfl-mode
  :hook (dired-mode . denote-dired-mode-in-directories)
  :general
  (+general-global-notes
    "b" 'denote-find-backlink
    "i" 'denote-link-or-create
    "k" 'denote-keywords-add
    "K" 'denote-keywords-remove
    "l" 'denote-find-link
    "r" 'denote-rename-file
    "s" 'denote-rename-file-using-front-matter)
  :custom
  (denote-directory (expand-file-name "~/OneDrive/notes/"))
  (denote-known-keywords '("emacs" "denote" "testing")))

(use-package consult-notes
  :ensure t
  :general
  (+general-global-notes
    "f" 'consult-notes
    "/" 'consult-notes-search-in-all-notes)
  :custom
  ;; Search only for text files in Denote dir
  (consult-notes-denote-files-function (function denote-directory-text-only-files))
  :init (consult-notes-denote-mode))

(use-package denote-explore
  :ensure t
  :general
  (+general-global-notes
    "S" 'denote-explore-single-keywords
    "Z" 'denote-explore-zero-keywords
    "R" 'denote-explore-rename-keyword
    "s" 'denote-explore-sort-keywords))

;;; reading

(use-package pdf-tools
  :ensure t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook
  (pdf-view-mode . pdf-tools-enable-minor-modes)
  (pdf-view-mode . (lambda () (progn
                                (blink-cursor-mode -1)
                                (display-line-numbers-mode -1)
                                (hl-line-mode -1))))
  :init (setq-default pdf-view-display-size 'fit-page)
  :custom (large-file-warning-threshold nil)
  :config (pdf-tools-install :no-query))

;; Add support for pdf-view and DocView buffers to `save-place'
(use-package saveplace-pdf-view :ensure t)

;; FIXME: nov
(use-package nov
  :ensure t
  :mode ("\\.[eE][pP][uU][bB]\\'" . nov-mode)
  :general
  (general-nmap :keymaps 'nov-mode-map
    "RET" #'nov-scroll-up)
  :custom (nov-save-place-file (var "nov-save-place.el")))

;; TODO: pager-map https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L3718
;; TODO: pdfgrep https://github.com/jeremy-compostella/pdfgrep/

;;; icons
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

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; TODO: compile-multi-nerd-icons???

;;; pass
;; TODO: pass https://github.com/NicolasPetton/pass

;;; email
;; TODO: notmuch https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/lisp/setup-email.el#L39
;; TODO: consult-notmuch https://github.com/emacsmirror/consult-notmuch
;; TODO: notmuch-bookmarks https://github.com/publicimageltd/notmuch-bookmarks
;; TODO: ol-notmuch https://git.sr.hl/~tarsius/ol-notmuch
;; TODO: notmuch-transient https://git.sr.hl/~tarsius/notmuch-transient
;; TODO: notmuch-maildir https://git.sr.hl/~tarsius/notmuch-maildir
;; TODO: notmuch-addr https://git.sr.hl/~tarsius/notmuch-addr
;; TODO: sendmail https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/lisp/setup-email.el#L7

;;; rss
;; TODO: elfeed https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/lisp/setup-elfeed.el#L21
;; TODO: elfeed-tube
;; TODO: elfeed-summary
;; TODO: elfeed-score https://github.com/sp1ff/elfeed-score
;; TODO: elfeed-org https://github.com/remyhonig/elfeed-org
;; TODO: nano-elfeed https://github.com/rougier/nano-elfeed
;; TODO: declutter https://github.com/sanel/declutter
;; TODO: rdrview https://github.com/eafer/rdrview

;;; read-later
;; TODO: wombag https://github.com/karthink/wombag
;; TODO: pocket-reader? https://github.com/alphapapa/pocket-reader.el

;;; irc
;; TODO: erc https://github.com/karthink/.emacs.d/blob/4ab4829fde086cb665cba00ee5c6a42d167e14eb/init.el#L3557

;;; mpv
;; TODO: empv https://github.com/isamert/empv.el
;; TODO: elcast https://github.com/douglasdavis/elcast
;; TODO: yeetube https://thanosapollo.org/projects/yeetube
;; or the fork https://github.com/Boruch-Baum/emacs-yeetube.el
;; TODO: ytel?
;; TODO: mpv extensions
;; mpv-sponsorblock
;; auto-save-state
;; playlist-manager
;; undo-redo
;; streamsave
;; youtube-upnext

;;; music
;; TODO: listen https://github.com/alphapapa/listen.el

;;; core
;; TODO: optimizations
;; TODO: defaults?
;; TODO: explain-pause-mode
;; TODO: profiler

;;; files

(use-package files
  :general-config
  (global-definer "a" 'find-file)
  (major-mode-definer "s" 'save-buffer)
  (major-mode-definer "S" 'save-some-buffers)
  :custom
  (make-backup-files nil)
  (backup-directory-alist (file-name-concat user-cache-directory "backup"))
  (backup-directory-alist
   `((,(concat "\\`" (file-name-as-directory temporary-file-directory)))
     ("\\`/tmp/" . nil)
     ("\\`/dev/shm/" . nil)
     ("." . ,(var "backup/"))))
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

;; TODO: vertico truncate so recentf file names in /nix/store/ don't get too big
;; TODO: maybe don't exclude /nis/store/ items from recentf in recentf-extras
(use-package recentf
  :init (recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 300)
  (recentf-save-file (var "recentf-save.el")))

(use-package recentf-extras)

(use-package saveplace
  :custom (save-place-file (var "save-place.el"))
  :init (save-place-mode))

(use-package cus-edit
  :custom (custom-file (file-name-concat user-cache-directory "etc/custom.el")))

(use-package autorevert
  :init (global-auto-revert-mode))

;; TODO: rename-file-and-buffer https://github.com/karthink/.emacs.d/blob/master/lisp/better-buffers.el#L217-L231
;; TODO: move-buffer-file https://github.com/karthink/.emacs.d/blob/master/lisp/better-buffers.el#L234-L248

;;; workspaces
;; TODO: beframe
;; TODO: activities
;; TODO: frames-only-mode https://github.com/davidshepherd7/frames-only-mode

;;; ai
;; TODO: gptel

;;; extras
;; TODO: leetcode https://github.com/kaiwk/leetcode.el
;; TODO: code-compass https://github.com/ag91/code-compass
;; TODO: verb https://github.com/federicotdn/verb
;; TODO: restclient.el https://github.com/pashky/restclient.el
;; TODO: prodigy
;; TODO: reverso
;; TODO: pomm/tmr
;; TODO: turbolog
;; TODO: persistent-kmacro
;; TODO: macrursors?
;; TODO: daemons?
;; TODO: casual https://github.com/kickingvegas/Casual

;;; guix
;; guix.el https://github.com/alezost/guix.el
;; guix-packaging https://github.com/ryanprior/emacs-guix-packaging
;; sway.el https://github.com/thblt/sway.el
;; exwm https://github.com/emacs-exwm/exwm
