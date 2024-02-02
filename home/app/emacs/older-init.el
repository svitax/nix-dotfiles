;;; init.el --- Initialization -*- lexical-binding: t; no-byte-compile: t -*-
;; Author: Svitax Erevos
;; Keywords: Emacs configuration
;; Homepage: https://github.com/svitax/nix-dotfiles.git
;;; Commentary:
;; My init.el
;;; Code:

;;; Early-init

(use-package early-init
  :no-require
  :unless (featurep 'early-init)
  :config
  (load-file (locate-user-emacs-file "early-init.el")))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;;; Init

;; Always defer use-package packages. This means that if I really need a
;; a package, I will go to my config and edit the use-package recipe to lazy
;; load it. This reduces my startup time significantly
(setq use-package-always-defer t)
(require 'use-package)

;; Add modules directory
(add-to-list 'load-path (concat user-emacs-directory "modules"))

;;; Core -- the heart of the beast
(require 'core-lib)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5
;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set initial major mode to fundamental-mode, to avoid possible eager loading on text-mode.
(setq initial-major-mode 'fundamental-mode)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
(setq file-name-handler-alist nil)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; Garbage collection strategy used by doom-emacs. Enforce a sneaky Garbage Collection
;; strategy to minimize GC interference with user activity. During normal use a high GC
;; threshold is set. When idling GC is triggered and a low threshold is set.
;; A more detailed explanation can be found at http://akrl.sdf.org/
(use-package gcmh
  :demand t
  :init
  (setq
   gcmh-idle-delay 'auto  ; default is 15s
   gcmh-auto-idle-delay-factor 10
   gcmh-high-cons-threshold (* 16 1024 1024))
  :config
  (gcmh-mode 1))

;; Emacs daemon doesn't seem to look for environment variables in the usual places
;; like .profile and such. Installing the package exec-path-from-shell, we make sure
;; that those important config files are loaded.
(use-package exec-path-from-shell
  :defer 3
  :init (setq exec-path-from-shell-arguments '("-l"))
  :config
  (when (or (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

;; BUG: doesn't work on NixOS
;; Benchmark Emacs Startup time without ever leaving your Emacs.
;; (use-package esup
;;   :commands esup
;;   :init (setq esup-user-init-file (file-truename "~/.emacs.d/init.el")))

;;; Constants

(defvar sx-indent-tabs-mode nil
  "Default: indent with spaces instead of tabs. See `indents-tabs-mode'.")
(defvar sx-tab-width 4
  "Default: indent with 4 columns. See `tab-width'.")
(defvar sx-fill-column 80
  "Default: add 80 characters as fill-column. See `fill-column'.")
(defvar sx-dired-listing-switches "-lAXhv --group-directories-first"
  "Default: show hidden files with full information in human readable format.
See `dired-listing-switches'.")
(defvar sx-dired-all-the-icons t
  "Default: show icons on dired buffers.")
(defvar sx-dired-all-the-icons-show-colors t
  "Default: show colors on the icons on dired buffers. Only works if `sx-dired-all-the-icons' is `t'.")
(defvar sx-shr-max-image-proportion 0.6
  "Default: scale images to 0.6 when decoding. See `shr-max-image-proportion'.")
(defvar sx-eww-downloads-directory (expand-file-name "/tmp/eww-downloads")
  "Default: use /tmp/eww-downloads as downloads dir. See `eww-download-directory'.")
(defvar sx-eww-history-limit 150
  "Default: only keep 150 searches as history. See `eww-history-limit'.")
(defvar sx-popper-reference-buffers '("\\*Messages\\*"
                                      "\\*Warnings\\*"
                                      "Output\\*$"
                                      "\\*Async Shell Command\\*"
                                      "\\*Shell Command Output\\*"
                                      compilation-mode
                                      "^\\*eshell.*\\*$" eshell-mode
                                      "^\\*shell.*\\*$" shell-mode
                                      "^\\*term.*\\*$" term-mode
                                      "^\\*eat.*\\*$" eat-mode
                                      help-mode helpful-mode
                                      "^.*\\/harpoon\\/.*\\#.*$" harpoon-mode)
  "Default: all buffers that would make sense with the loaded packages.
See `popper-reference-buffers'")
(defvar sx-aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f ?g)
  "Default: Home-row keys for `ace-window' jumping. See `aw-keys'.")
(defvar sx-corfu-auto t
  "Default: `corfu' completion automatically pops-up on `sx-corfu-auto-prefix' characters.
See `corfu-auto'.")
(defvar sx-corfu-auto-prefix 1
  "Default: number of characters on which to pop-up `corfu' auto completion. Only valid
 if `sx-corfu-auto' is `t'. See `corfu-auto-prefix'.")
(defvar sx-corfu-auto-delay 0.25
  "Default: delay to pop-up `corfu' completion. Only valid if `sx-corfu-auto' is `t'.
See `corfu-auto-delay'.")
(defvar sx-corfu-popupinfo-delay 1
  "Default: delay to show help on `corfu' completion candidates. See `corfu-popupinfo-delay'.")
(defvar sx-corfu-in-minibuffer t
  "Default: `corfu' pop-up also happens on the minibuffer.")
(defvar sx-cape-dabbrev-min-length 3
  "Default: number of characters before starting cape dabbrev, for in-buffer keyword
completion. See `cape-dabbrev-min-length'.")
;; (defvar sx-rg-command "rga"
(defvar sx-rg-command "rg"
  "Default: use `rga' CLI to search between lines. See `consult-ripgrep-args'.")
(defvar sx-fd-command "fd"
  "Default: use `fd' command for file lookups. See `affe-find-command'.")
(defvar sx-consult-narrow-key "<"
  "Default: use this key to narrow candidates when running `consult' commands.
See `consult-narrow-key'.")
(defvar sx-consult-preview-key (list :debounce 0.5 'any)
  "Default: automatic preview on 0.5 seconds for most commands.
See `consult-preview-key'")
(defvar sx-notes-directory (expand-file-name "~/Documents/slipbox/")
  "Default: my slipbox's path.")
(defvar sx-denote-directory (concat sx-notes-directory "pages/")
  "Default: pages directory inside our `sx-notes-directory'.
See `denote-directory'.")
(defvar sx-notes-dailies-directory (expand-file-name (concat sx-denote-directory "../dailies/"))
  "Default: directory where to store dailies, which contain DONE items when moving headers from
TODO to DONE.")
(defvar sx-org-default-notes-file (concat sx-denote-directory "20230109T114537--refile__learning_tasks.org")
  "Default: `sx-notes-directory' 's refile.org. See `org-default-notes-file'.")
(defvar sx-org-meetings-file (concat sx-denote-directory "20230112T171448--meetings__project_learning.org")
  "Default: my personal meetings file, you can change it to the file that denote generates for you.")
(defvar sx-denote-prompts '(title keywords)
  "Default: only prompt for the title and keywords on file creation.
See `denote-prompts'.")
(defvar sx-git-commit-summary-max-length 50
  "Default: number of characters that the commit message should have.
See `git-commit-summary-max-length'.")
(defvar sx-git-commit-fill-column 72
  "Default: set fill column to this number if on `git-commit-mode'.")
(defvar sx-tab-bar-show nil
  "Default: hide tab-bar, though use it in the shadows. See `tab-bar-show'.")
(defvar sx-python-shell-interpreter "python3"
  "Default: command to use when running python. See `python-shell-interpreter'.")
(defvar sx-markdown-command "pandoc -t html5"
  "Default: command to use when compiling markdown to html.
See `markdown-command'.")

;;; General

(use-package general)
(require 'general)

(after! general
  (general-evil-setup)
  (defun sx-which-key-mode-name (arg)
    "Return string of the major-mode definer, removing the -mode suffix."
    `(,(cadr (split-string (car arg) " ")) .
      ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))

  ;; Create SPC leader key, to be used in the macro.
  (general-create-definer global-definer
    :keymaps 'override
    :states  '(normal hybrid motion visual operator)
    :prefix  "SPC"
    :non-normal-prefix "C-SPC")

  ;; Add a definer for each of the major-modes
  (general-create-definer major-mode-definer
    :keymaps 'override
    :states '(normal hybrid motion visual operator)
    :prefix "SPC m"
    "" '(:ignore t :which-key 'sx-which-key-mode-name))

  ;; Add an additional minor-mode-definer, for each of the modes.
  ;; It is key to remember that in this case, the :keymaps option refers to the minor-mode,
  ;; not the keymap.
  (general-create-definer minor-mode-definer
    :keymaps 'override
    :definer 'minor-mode
    :states '(normal hybrid motion visual operator)
    :prefix "SPC m")

  ;; Since I let `evil-mode' take over `C-u' for buffer scrolling, I need to rebind the
  ;; `universal-argument' command to another key sequence.
  (general-nmap "C-M-u" 'universal-argument)

  ;; Emulate different default Emacs keys, as I'm transitioning into evil.
  (global-definer
    ""     nil
    "SPC" '(execute-extended-command :which-key "M-x"))

  ;; Macro to define all key-pockets. It adapts to the name passed, and defines additonal macros to be
  ;; used to define keybindings. See `general-global-buffer' below.
  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
      Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "+general-global-" name))
         :wrapping global-definer
         :prefix-map (quote ,(intern (concat "+general-global-" name "-map")))
         :infix ,infix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern (concat "+general-global-" name))
        ,@body)))

  ;; Buffer commands.
  (global-definer "k" '(kill-current-buffer :which-key "Kill buffer"))
  (global-definer "q" '(kill-emacs :which-key "Quit Emacs"))

  ;; Window commands.
  (global-definer "c" '(delete-window :which "Close window"))
  (+general-global-menu! "window" "w"
    "=" '(balance-windows :which-key "Balance windows")
    "s" '(split-window-vertically :which-key "Split window vertically")
    "v" '(split-window-horizontally :which-key "Split window horizontally")
    "m" '(delete-other-windows :which-key "Delete other windows")
    "c" '(delete-window :which-key "Close window"))

  ;; Project commands, using the built-in `project'.
  (+general-global-menu! "project" "p")

  ;; File commands.
  (general-mmap "C-s" 'save-buffer)
  (global-definer
    "e" '(dired-jump :which-key "File explorer")
    "f" '(project-find-file :which-key "File picker")
    "i" '(find-file :which-key "File browser"))

  (+general-global-menu! "eval" "e")
  (+general-global-menu! "org" "o")
  (+general-global-menu! "help" "h")
  (+general-global-menu! "notes" "n")
  (+general-global-menu! "magit" "g")
  (+general-global-menu! "jump" "j"))

;;; Evil

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol t
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo
        evil-search-module 'evil-search
        evil-vsplit-window-right t
        evil-split-window-below t)
  :general
  (general-mmap
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    "M-h" 'evil-window-left
    "M-j" 'evil-window-down
    "M-k" 'evil-window-up
    "M-l" 'evil-window-right
    "gh" 'evil-first-non-blank
    "gl" 'evil-end-of-line
    "gD" 'xref-find-definitions-other-window)
  (general-nmap
    [escape] 'sx-nohl-and-quit
    "K" nil)
  (general-vmap
    [escape] 'sx-nohl-and-quit
    "C-u" 'scroll-down
    "C-d" 'scroll-up
    "J" (concat ":m '>+1" (kbd "RET") "gv=gv")
    "K" (concat ":m '<-2" (kbd "RET") "gv=gv"))
  (general-mmap
    "C-u" 'sx-center-scroll-half-page-up
    "C-d" 'sx-center-scroll-half-page-down)
  (:keymaps
   '(minibuffer-local-map
     minibuffer-local-ns-map
     minibuffer-local-completion-map
     minibuffer-local-must-match-map
     minibuffer-local-isearch-map)
   [escape] 'keyboard-escape-quit)
  :config
  (defun sx-nohl-and-quit ()
    "Run nohighlight on escape, in normal mode."
    (interactive)
    (evil-ex-nohighlight)
    (keyboard-quit))

  (defun sx-center-scroll-half-page-down ()
    "Center window after scrolling half page down."
    (interactive)
    (evil-scroll-down nil)
    (evil-scroll-line-to-center nil))

  (defun sx-center-scroll-half-page-up ()
    "Center window after scrolling half page up."
    (interactive)
    (evil-scroll-up nil)
    (evil-scroll-line-to-center nil))

  ;; Stop copying each visual state move to the clipboard.
  (advice-add #'evil-visual-update-x-selection :override #'ignore)
  (fset 'evil-redirect-digit-argument 'ignore)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'magit-status-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'org-agenda-mode 'motion)

  (evil-mode t))

(use-package evil-collection
  :commands evil-collection-init
  :init
  (setq
   evil-collection-outline-bind-tab-p nil
   evil-collection-setup-minibuffer t))

(defvar +evil-collection-disabled-list
  '(anaconda-mode buff-menu calc comint company custom eldoc elisp-mode
    ert free-keys helm help indent image kotlin-mode outline replace
    shortdoc simple slime lispy)
  "A list of `evil-collection' modules to ignore. See the definition of this
variable for an explanation of the defaults (in comments). See
`evil-collection-mode-list' for a list of available options.")

(defvar evil-collection-mode-list
  `(
    2048-game ag alchemist anaconda-mode apropos arc-mode atomic-chrome
    auto-package-update beginend bluetooth bm bookmark
    (buff-menu "buff-menu") calc calendar cider cmake-mode comint
    company compile consult corfu (custom cus-edit) cus-theme daemons
    dashboard deadgrep debbugs debug devdocs dictionary diff-hl
    diff-mode dired dired-sidebar disk-usage doc-view docker ebib ebuku
    edbi edebug ediff eglot explain-pause-mode elfeed eldoc elisp-mode
    elisp-refs elisp-slime-nav embark emms epa ert eshell eval-sexp-fu
    evil-mc eww ,@(if (> emacs-major-version 28) '(emoji)) fanyi finder
    flycheck flymake forge free-keys geiser ggtags git-timemachine gnus
    go-mode grep guix hackernews helm help helpful hg-histedit hungry-delete
    ibuffer image image-dired image+ imenu imenu-list
    (indent "indent") indium info ivy js2-mode leetcode lispy log-edit
    log-view lsp-ui-imenu lua-mode kotlin-mode macrostep man
    (magit magit-repos magit-submodule) magit-section magit-todos
    markdown-mode monky mpc mu4e mu4e-conversation neotree newsticker
    notmuch nov omnisharp org org-present org-roam osx-dictionary outline
    p4 (package-menu package) pass (pdf pdf-tools) popup proced prodigy
    profiler python quickrun racer racket-describe realgud reftex replace
    restclient rg ripgrep rjsx-mode robe rtags ruby-mode scheme scroll-lock
    selectrum sh-script ,@(if (> emacs-major-version 27) '(shortdoc))
    simple simple-mpc slime sly snake so-long speedbar tablist tar-mode
    telega (term term ansi-term multi-term) tetris thread tide timer-list
    transmission trashed tuareg typescript-mode vc-annotate vc-dir vc-git
    vdiff vertico view vlf vterm vundo w3m wdired wgrep which-key woman
    xref xwidget yaml-mode youtube-dl zmusic (ztree ztree-diff)))

(defun +evil-collection-init (module &optional disabled-list)
  "Initialize evil-collection-MODULE.

Unlike `evil-collection-init', this respects `+evil-collection-disabled-list',
and complains if a module is loaded too early (during startup)."
  (unless (memq (or (car-safe module) module) disabled-list)
    (doom-log "editor:evil: loading evil-collection-%s %s"
              (or (car-safe module) module)
              (if doom-init-time "" "(too early!)"))
    (with-demoted-errors "evil-collection error: %s"
      (evil-collection-init (list module)))))

(after! evil-collection
  (mapc #'+evil-collection-init '(comint custom)))

(after! evil
  (add-transient-hook! 'help-mode
    (+evil-collection-init 'help))
  (add-transient-hook! 'Buffer-menu-mode
    (+evil-collection-init '(buff-menu "buff-menu")))
  (add-transient-hook! 'calc-mode
    (+evil-collection-init 'calc))
  (add-transient-hook! 'image-mode
    (+evil-collection-init 'image))
  (add-transient-hook! 'emacs-lisp-mode
    (+evil-collection-init 'elisp-mode))
  (add-transient-hook! 'occur-mode
    (+evil-collection-init 'replace))
  (add-transient-hook! 'indent-rigidly
    (+evil-collection-init '(indent "indent")))
  ;; (add-transient-hook! 'minibuffer-setup-hook
  ;;   (when evil-collection-setup-minibuffer
  ;;     (+evil-collection-init 'minibuffer)
  ;;     (evil-collection-minibuffer-insert)))
  (add-transient-hook! 'process-menu-mode
    (+evil-collection-init '(process-menu simple)))
  (add-transient-hook! 'shortdoc-mode
    (+evil-collection-init 'shortdoc))
  (add-transient-hook! 'tabulated-list-mode
    (+evil-collection-init 'tabulated-list))
  (add-transient-hook! 'tab-bar-mode
    (+evil-collection-init 'tab-bar))

  (dolist (mode evil-collection-mode-list)
    (dolist (req (or (cdr-safe mode) (list mode)))
      (with-eval-after-load req
        (+evil-collection-init mode +evil-collection-disabled-list)))))

(use-package evil-commentary
  :hook (doom-first-input . evil-commentary-mode)
  :general
  ("C-/" 'evil-commentary-line))

(after! dired
  (general-nmap
    :keymaps '(dired-mode-map)
    "l" 'dired-find-file
    "h" 'dired-up-directory))

(use-package evil-matchit
  :hook (doom-first-input . global-evil-matchit-mode))

(use-package evil-surround
  :hook (doom-first-input . global-evil-surround-mode)
  :general
  (general-def :states 'operator :keymaps 'evil-surround-mode-map
    "r" 'evil-surround-edit
    "s" nil)
  (general-def :states 'visual :keymaps 'evil-surround-mode-map
    "R" 'evil-surround-edit))

(use-package avy
  :general
  (general-nmap
    "s" 'evil-avy-goto-char-timer)
  :init
  (setq avy-timeout-seconds 0.25))

;;; Theme

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-gruvbox-dark-variant "hard"))

;; (use-package modus-themes
;;   :demand
;;   :init
;;   (require 'modus-themes)

;;   (defun rune/modus-themes-tab-bar-colors ()
;;     "Override tab faces to have even less variety"
;;     (modus-themes-with-colors
;;       (custom-set-faces
;;        `(tab-bar ((,c
;;                 :height 0.8
;;                 :background ,bg-main
;;                 :box nil)))
;;        `(tab-bar-tab ((,c
;;                     :background ,bg-main
;;                     :underline (:color ,blue-intense :style line)
;;                     :box (:line-width 2 :style flat-button))))
;;        `(tab-bar-tab-inactive ((,c
;;                              :background ,bg-main
;;                              :box (:line-width 2 :style flat-button)))))))
;;   (add-hook 'modus-themes-after-load-theme-hook #'rune/modus-themes-tab-bar-colors)

;;   ;; Generally user options should never be touched by a theme. However, according
;;   ;; to the maintainer of modus-themes, certain cases like `hl-todo-keyword-faces'
;;   ;; and the `flymake-*-bitmap' variants merit an exception.
;;   ;; This is annoying because I don't like the face used for Flymake bitmaps.
;;   ;; I would like them to not have a background color. These variables need to be
;;   ;; set after loading the theme.
;;   (defun rune/modus-themes-flymake-bitmaps ()
;;     "Override Flymake bitmaps to blend into the fringe"
;;     (customize-set-variable
;;      'flymake-error-bitmap '(flymake-double-exclamation-mark compilation-error))
;;     (customize-set-variable
;;      'flymake-warning-bitmap '(exclamation-mark compilation-warning))
;;     (customize-set-variable
;;      'flymake-note-bitmap '(exclamation-mark compilation-info)))
;;   (add-hook 'modus-themes-after-load-theme-hook #'rune/modus-themes-flymake-bitmaps)

;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-to-toggle '(modus-vivendi))
;;   (setq modus-themes-bold-constructs t
;;      modus-themes-mixed-fonts t

;;      ;; Control the style of command prompts (e.g. minibuffer, shell, IRC clients).
;;      ;; `modus-themes-prompts' are either nil (the default), or a list of
;;      ;; properties that may include any of those symbols: `italic',;; `WEIGHT'
;;      modus-themes-prompts '(bold))

;;   ;; Define some palette overrides using the presets
;;   (customize-set-variable 'modus-themes-common-palette-overrides
;;                        `(;; To hide the border around the active and inactive mode lines, we
;;                          ;; set their color to that of the underlying background
;;                          (bg-mode-line-active bg-inactive)
;;                          (fg-mode-line-active fg-main)
;;                          (bg-mode-line-inactive bg-inactive)
;;                          (fg-mode-line-active fg-dim)
;;                          (border-mode-line-active bg-inactive)
;;                          (border-mode-line-inactive bg-main)
;;                             ;; line-numbers
;;                             (fg-line-number-active fg-main)
;;                             (bg-line-number-inactive bg-main)
;;                             (fg-line-number-inactive fg-dim)
;;                             ;; links
;;                             (underline-link unspecified)
;;                             (underline-link-visited unspecified)
;;                             (underline-link-symbolic unspecified)
;;                             ;; To hide the border around the active and inactive mode lines, we
;;                             ;; set their color to that of the underlying background
;;                             (bg-mode-line-active bg-inactive)
;;                             (fg-mode-line-active fg-main)
;;                             (bg-mode-line-inactive bg-inactive)
;;                             (fg-mode-line-active fg-dim)
;;                             (border-mode-line-active bg-inactive)
;;                             (border-mode-line-inactive bg-main)
;;                             ;; Change the background of matching parenthesis to a shade of magenta
;;                             ;; (bg-paren-match bg-magenta-subtle)
;;                             ;; Enable underlining matching parenthesis by applying a color to them
;;                             (underline-paren-match fg-main)
;;                             ;; Make the fringe invisible
;;                             (fringe unspecified)))

;;   (defun rune/modus-themes-init ()
;;     (load-theme (car modus-themes-to-toggle) t))
;;   :hook (after-init . rune/modus-themes-init))

;;; Defaults

(use-package no-littering
  :init
  (setq
   backup-directory-alist `(("." . ,(no-littering-expand-var-file-name "backups/")))
   auto-save-list-file-prefix (no-littering-expand-var-file-name "auto-saves/sessions/")
   auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-saves/") t))
   url-cookie-file (no-littering-expand-var-file-name "cookies/cookies")
   eww-bookmarks-directory (concat no-littering-var-directory "eww-bookmarks/")))

(use-package emacs
  :preface
  (defvar backup-dir
    (locate-user-emacs-file ".cache/backups")
    "Directory to store backups")
  (defvar auto-save-dir
    (locate-user-emacs-file ".cache/auto-save/")
    "Directory to store auto-save files.")
  :init
  (setq
   ;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
   ;; as a buffer is unsaved, backups create copies once, when the file is first
   ;; written, and never again until it is killed and reopened. This is better
   ;; suited to version control, and I don't want world-readable copies of
   ;; potentially sensitive material floating around our filesystem.
   create-lockfiles nil
   make-backup-files nil
   ;; But in case the user does enable it, some sensible defaults:
   version-control t ; number each backup file
   backup-by-copying t ; instead of renaming current file (clobbers links)
   delete-old-versions t ; clean up after itself
   kept-old-versions 5
   kept-new-versions 5
   backup-directory-alist
   `(("." . ,backup-dir))
   tramp-backup-directory-alist backup-directory-alist
   ;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
   ;; Use `recover-file' or `recover-session' to recover them.
   auto-save-default t
   ;; Don't auto-disable auto-save after deleting big chunks. This defeats
   ;; the purpose of a failsafe. This adds the risk of losing the data we
   ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
   auto-save-include-big-deletions t
   auto-save-file-name-transforms
   `((".*" ,auto-save-dir t))
   auto-save-no-message t
   auto-save-interval 100
   require-final-newline t
   ;; Disable the warning "X and Y are the same file". It's fine to ignore this
   ;; warning as it will redirect you to the existing buffer anyway.
   find-file-suppress-same-file-warnings t
   auto-revert-interval 1
   revert-without-query '(".*")
   ad-redefinition-action 'accept
   warning-suppress-log-types '((comp))
   create-lockfiles nil
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   inhibit-startup-message t
   scroll-preserve-screen-position 'always
   scroll-margin 2
   display-line-numbers-type t
   display-line-numbers-width 4
   display-line-numbers-width-start t
   display-line-numbers-grow-only t
   next-line-add-newlines t
   visible-bell nil
   ring-bell-function 'ignore
   enable-recursive-minibuffers t
   minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
   savehist-additional-variables '(kill-ring)
   recentf-max-menu-items 100
   recentf-max-saved-items 100
   tab-always-indent 'complete
   message-kill-buffer-on-exit t
   x-select-enable-clipboard t
   x-select-enable-primary t)
  (setq-default
   ;; Don't resize frames in steps; it looks weird, especially in tiling window
   ;; managers, where it can leave unseemly gaps.
   frame-resize-pixelwise t
   ;; But do not resize windows pixelwise, this can cause crashes in some cases
   ;; when resizing too many windows at once or rapidly.
   window-resize-pixelwise nil
   indent-tabs-mode sx-indent-tabs-mode
   tab-width sx-tab-width
   fill-column sx-fill-column)

  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Use UTF-8 everywhere.
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (if (boundp 'buffer-file-coding-system)
      (setq-default buffer-file-coding-system 'utf-8)
    (setq default-buffer-file-coding-system 'utf-8))
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  :custom-face (bookmark-face ((t nil))))

;; (after! general
;;   (major-mode-definer
;;     :major-modes '(emacs-lisp-mode)
;;     :keymaps '(emacs-lisp-mode-map)
;;     "b" 'eval-buffer
;;     "b" 'eval-buffer
;;     "d" 'eval-defun
;;     "e" 'eval-expression
;;     "s" 'eval-last-sexp)
;;   (global-definer
;;     "x" 'eval-expression))

(defun visiting-buffer-rename (file newname &optional _ok-if-already-exists)
  "Rename buffer visiting FILE to NEWNAME.
Intended as :after advice for `rename-file'."
  (when (called-interactively-p 'any)
    (when-let ((buffer (get-file-buffer file)))
      (with-current-buffer buffer
        (set-visited-file-name newname nil t)
        (when (derived-mode-p 'emacs-lisp-mode)
          (save-excursion
            (let* ((base (file-name-nondirectory file))
                   (sans (file-name-sans-extension base))
                   (newbase (file-name-nondirectory newname))
                   (newsans (file-name-sans-extension newbase)))
              (goto-char (point-min))
              (while (search-forward-regexp (format "^;;; %s" base) nil t)
                (replace-match (concat ";;; " newbase)))
              (goto-char (point-max))
              (when
                  (search-backward-regexp (format "^(provide '%s)" sans) nil t)
                (replace-match (format "(provide '%s)" newsans))))))))))

(defun visiting-buffer-kill (file &optional _trash)
  "Kill buffer visiting FILE.
Intended as :after advice for `delete-file'."
  (when (called-interactively-p 'any)
    (when-let ((buffer (get-file-buffer file)))
      (kill-buffer buffer))))

(add-hook! doom-first-input
  (pixel-scroll-precision-mode)
  (savehist-mode)
  (recentf-mode)
  (add-function :after after-focus-change-function #'(lambda () (save-some-buffers t)))
  (add-hook! 'minibuffer-setup-hook #'cursor-intangible-mode)

  (add-to-list 'recentf-exclude "\\.gpg\\")
  (dolist (dir (list (locate-user-emacs-file ".cache/")
                     (locate-user-emacs-file "workspace/.cache/")))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*")))

  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (advice-add 'rename-file :after 'visiting-buffer-rename)
  (advice-add 'delete-file :after 'visiting-buffer-kill))

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode pdf-view-mode olivetti-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
       Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(defun display-hl-line--turn-on ()
  "Turn on hl-line except for certain major modes.
       Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (hl-line-mode)))

(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  (prog-mode . hl-line-mode)
  (dired-mode . display-line-numbers-mode)
  (dired-mode . hl-line-mode)
  (org-mode . display-line-numbers-mode)
  (org-mode . hl-line-mode))

(use-package dabbrev
  :config (pushnew! dabbrev-ignored-buffer-modes 'pdf-view-mode))

;; (setq
;;  minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
;;  user-emacs-directory (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
;;  ;; Don't stretch the cursor to fit wide characters.
;;  x-stretch-cursor nil
;;  ring-bell-function 'ignore
;;  ;; mode-line-percent-position nil
;;  ;; A simple frame title
;;  frame-title-format "Emacs"
;;  icon-title-format frame-title-format
;;  hscroll-margin 2
;;  hscroll-step 1
;;  ;; Emacs spends too much effort recentering the screen if you scroll the
;;  ;; cursor more than N lines past window edges (where N is the settings of
;;  ;; `scroll-conservatively'). This is especially slow in larger files
;;  ;; during large-scale scrolling commands. If kept over 100, the window is
;;  ;; never automatically recentered. The default (0) triggers this too
;;  ;; aggressively, so I've set it to 10 to recenter if scrolling too far
;;  ;; off-screen
;;  scroll-conservatively 10
;;  scroll-margin 0
;;  scroll-preserve-screen-position t
;;  ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
;;  ;; for tall lines.
;;  auto-window-vscroll nil
;;  ;; Don't blink the paren matching the one at point, it's too distracting.
;;  blink-matching-paren nil
;;  ;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;;  ;; while we're in the minibuffer.
;;  enable-recursive-minibuffers t
;;  ;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;;  ;; useful information, like git-gutter and flymake
;;  indicate-buffer-boundaries t
;;  indicate-empty-lines nil
;;  ;; Favor vertical splits over horizontal ones.
;;  split-width-threshold 1600
;;  split-height-threshold nil))

;;; Dired

(use-package dired
  :general
  (global-definer "e" '(dired-jump :which-key "File explorer"))
  :init
  (setq
   dired-listing-switches sx-dired-listing-switches
   dired-use-ls-dired nil
   dired-kill-when-opening-new-dired-buffer t
   delete-by-moving-to-trash t
   dired-dwim-target t))

(use-package dired-single :after dired)

(after! dired
  (defun dired-home-directory ()
    (interactive)
    (dired-single-buffer (expand-file-name "~/")))

  (add-hook! dired-mode (general-nmap
                          :keymaps '(dired-mode-map)
                          "a" 'dired-create-empty-file
                          "~" 'dired-home-directory)))

(use-package all-the-icons-dired
  :init (setq all-the-icons-dired-monochrome nil)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :after dired
  :config
  (add-to-list 'dired-open-functions #'dired-open-xdg t))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))

;; Override evil-collection for "H"
(after! dired-hide-dotfiles
  (add-hook! 'evil-collection-setup-hook (general-nmap
                                           :keymaps '(dired-mode-map)
                                           "H" 'dired-hide-dotfiles-mode)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-preview
  :hook (dired-mode . dired-preview-mode)
  :general
  (general-nmap
    :keymaps '(dired-mode-map)
    "gp" 'dired-preview-mode))

(use-package dired-narrow
  :after dired
  :custom
  (dired-narrow-exit-when-1-left t))

(use-package dired-git-info
  :hook (dired-after-readin . dired-git-info-auto-enable)
  :custom
  (dgi-auto-hide-details-p nil)
  :general
  (general-nmap :keymaps 'dired-mode-map
    ")" 'dired-git-info-mode))

(use-package dired-filter :after dired)

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode)
  :general
  (general-nmap :keymaps '(dired-mode-map)
    "gc" 'dired-collapse-mode))

;;; Help

(use-package transient
  :general
  (:keymaps
   '(transient-base-map)
   "<escape>" 'transient-quit-one))

(use-package helpful
  :general
  (general-def
    [remap describe-function] 'helpful-callable
    [remap describe-symbol] 'helpful-symbol
    [remap describe-variable] 'helpful-variable
    [remap describe-command] 'helpful-command
    [remap describe-key] 'helpful-key)
  (general-nmap
    :keymaps '(helpful-mode-map)
    "gx" 'push-button)
  (+general-global-help
   "M" 'helpful-macro)
  :config
  (defun helpful--autoloaded-p (sym buf)
    "Return non-nil if function SYM is autoloaded."
    (-when-let (file-name (buffer-file-name buf))
      (setq file-name (s-chop-suffix ".gz" file-name))
      (help-fns--autoloaded-p sym)))

  (defun helpful--skip-advice (docstring)
    "Remove mentions of advice from DOCSTRING."
    (let* ((lines (s-lines docstring))
           (relevant-lines
            (--take-while
             (not (or (s-starts-with-p ":around advice:" it)
                      (s-starts-with-p "This function has :around advice:" it)))
             lines)))
      (s-trim (s-join "\n" relevant-lines)))))

;; which-key is a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup. For example,
;; after enabling the minor mode if you enter C-x and wait for the default of 1
;; second the minibuffer will expand with all of the available key bindings that
;; follow C-x (or as many as space allows given your settings).
(use-package which-key
  :init
  (setq which-key-sort-uppercase-first t)
  :hook (doom-first-input . (lambda ()
                              (which-key-setup-minibuffer)
                              (which-key-mode))))

(use-package eldoc-box
  :preface
  ;; Add scrolling support to the `eldoc-box' child frame.
  (defconst rune/lsp-help-buffer-keymap
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map t)
      (define-key map (kbd "C-j") #'rune/lsp-help-scroll-down)
      (define-key map (kbd "C-k") #'rune/lsp-help-scroll-up)
      (define-key map (kbd "C-u") #'rune/lsp-help-C-u)
      (define-key map (kbd "C-d") #'rune/lsp-help-C-d)
      map)
    "Keymap in `rune/lsp-help-at-point'")

  (defvar-local rune/lsp-control-deactivate-fn nil
    "non-nil if current buffer can be controlled by `rune/lsp-help-buffer-keymap'.")

  (defun rune/lsp-help-scroll-up ()
    "Scroll up in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (when rune/lsp-control-deactivate-fn
        (with-selected-frame eldoc-box--frame
          (scroll-down 3)))))
  (defun rune/lsp-help-scroll-down ()
    "Scroll down in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (when rune/lsp-control-deactivate-fn
        (with-selected-frame eldoc-box--frame
          (scroll-up 3)))))
  (defun rune/lsp-help-C-u ()
    "C-u (`evil-scroll-up') in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (when rune/lsp-control-deactivate-fn
        (with-selected-frame eldoc-box--frame
          (scroll-down (max 1 (/ (1- (window-height (selected-window))) 2)))))))
  (defun rune/lsp-help-C-d ()
    "C-u (`evil-scroll-down') in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (when rune/lsp-control-deactivate-fn
        (with-selected-frame eldoc-box--frame
          (scroll-up (max 1 (/ (1- (window-height (selected-window))) 2)))))))
  (defun rune/lsp-help-at-point ()
    "Display documentation of the symbol at point."
    (interactive)
    (eldoc-box-help-at-point)
    (with-current-buffer eldoc-box--buffer
      (setq-local rune/lsp-control-deactivate-fn
                  (set-transient-map rune/lsp-help-buffer-keymap t))))
  :hook
  ;; `evil-collection' sets bindings using `with-eval-after-load'. That means if
  ;; you define an overriding key in a config section that is defined before
  ;; `evil-collection-init' is called, it will be called first and then
  ;; evil-collection will overwrite it. The solution is to use the hook
  ;; `evil-collection-setup-hook' in order for evil-collection to tell you when
  ;; its done with the key binding setup.
  (evil-collection-setup . (lambda (&rest a)
                             (evil-define-key 'normal 'prog-mode-map
                               (kbd "K") 'rune/lsp-help-at-point))))
;; (kbd "K") 'eldoc-box-help-at-point)))

;;; Window

(use-package popper
  :hook
  ((doom-first-input . popper-mode)
   (doom-first-input . popper-echo-mode))
  :general
  (general-nmap
    "C-'" 'popper-toggle
    "M-'" 'popper-cycle
    "C-M-'" 'popper-toggle-type)
  :init
  (setq popper-window-height 0.33)
  (setq popper-mode-line '(:eval (propertize " POP " 'face 'mode-line-emphasis)))
  (setq popper-reference-buffers sx-popper-reference-buffers)
  (setq popper-group-function (lambda ()
                                (let ((tabs (funcall tab-bar-tabs-function)))
                                  (alist-get 'name (nth (tab-bar--current-tab-index tabs)
                                                        tabs))))))
;; Switch between windows using the keys in the home-row.
(use-package ace-window
  :general
  (+general-global-window
   "o" 'ace-window)
  :init
  (setq
   aw-keys sx-aw-keys
   aw-background nil))

;;; Completion

;; Vertico provides a minimalistic vertical completion UI, which is based on the default
;; completion system. By reusing the default system, Vertico achieves full compatibility
;; with built-in Emacs commands and completion tables. Vertico is pretty bare-bone and
;; comes with only a minimal set of commands.
(use-package vertico
  :hook (doom-first-buffer . vertico-mode)
  :init (setq vertico-cycle t)
  :general
  (general-nmap :keymaps 'vertico-map
    "M-RET" 'vertico-exit-input)
  ( :keymaps 'minibuffer-local-map
             ;; Scroll through the items with C-n and C-p instead
             "<up>" 'previous-history-element
             "<down>" 'next-history-element))

(use-package vertico-directory
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :general ( :keymaps 'vertico-map
                      "RET" 'vertico-directory-enter
                      "DEL" 'vertico-directory-delete-char
                      "M-DEL" 'vertico-directory-delete-word
                      "C-w" 'vertico-directory-delete-word))

(use-package all-the-icons-completion
  :hook
  (doom-first-buffer . all-the-icons-completion-mode)
  (marginalia-mode-hook . all-the-icons-completion-marginalia-setup))

;; Orderless is one of the same emacs packages that works modularly, using the basic emacs
;; API. This package provides an orderless completion style that divides the pattern
;; into space-separated components, and matches candidates that match all of the components
;; in any order. Each component can match in any one of several ways: literally, as a regexp,
;; as an initialism, in the flex style, or as multiple word prefixes. By default, regexp
;; and literal matches are enabled.
(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

;; Marginalia are marks or annotations placed at the margin of the page of a book
;; or in this case helpful colorful annotations placed at the margin of the minibuffer
;; for your completion candidates. Marginalia can only add annotations to be displayed
;; with the completion candidates. It cannot modify the appearance of the candidates themselves,
;; which are shown as supplied by the original commands.
(use-package marginalia
  :hook (doom-first-buffer . marginalia-mode)
  :general ( :keymaps 'minibuffer-local-map
                      "M-A" 'marginalia-cycle))

(use-package corfu
  :hook (doom-first-input . global-corfu-mode)
  :general ( :keymaps 'corfu-map
                      "C-n" 'corfu-next
                      "C-p" 'corfu-previous
                      ;; `TAB'-only completion.
                      "TAB" 'corfu-insert
                      ;; Unbind `RET' completely
                      "RET" 'nil
                      ;; TODO: change corfu-insert-separater keymap's M-SPC being bound on OS level
                      "M-SPC" 'corfu-insert-separator)
  :init
  (setq corfu-auto sx-corfu-auto ;; enable auto completion
        corfu-auto-delay sx-corfu-auto-delay
        corfu-auto-prefix sx-corfu-auto-prefix ; complete with less prefix keys
        ;; corfu-preselect 'directory ;; Select the first candidate, except for diretories
        corfu-cycle t
        corfu-scroll-margin 4
        corfu-quit-no-match t
        corfu-quit-at-boundary t
        corfu-max-width 100
        corfu-min-width 42
        corfu-count 9)
  ;; (setq eldoc-add-command #'corfu-insert)
  :config
  ;; (setq kind-icon-default-face 'corfu-default)
  (when sx-corfu-in-minibuffer
    (defun corfu-enable-in-minibuffer ()
      "Enable Corfu in the minibuffer if `completion-at-point' is bound."
      (when (where-is-internal #'completion-at-point
                               (list (current-local-map)))
        (corfu-mode t)))
    (add-hook! minibuffer-setup 'corfu-enable-in-minibuffer)))

(use-package corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode)
  :general
  ( :keymaps 'corfu-popupinfo-map
             "M-p" 'corfu-popupinfo-scroll-down
             "M-n" 'corfu-popupinfo-scroll-up)
  ( :keymaps 'corfu-map
             "C-h" 'corfu-popupinfo-toggle)
  :init
  (setq corfu-popupinfo-delay sx-corfu-popupinfo-delay)
  :custom-face
  (corfu-popupinfo ((t :height 1.0))))

(use-package kind-icon
  :demand t
  :after corfu
  :config
  ;; The icons appear too big in the Corfu completion childframe, you can
  ;; adjust height like this
  (plist-put kind-icon-default-style :height 0.6)
  ;; We can use VSCode icons provided by https://github.com/microsoft/vscode-icons
  ;; instead of the default Material icons set
  (setq kind-icon-mapping
        '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "vscode")
          (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode")
          (color          "#"   :icon "symbol-color"       :face success                          :collection "vscode")
          (command        "cm"  :icon "chevron-right"      :face default                          :collection "vscode")
          (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode")
          (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode")
          (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
          (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
          (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
          (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode")
          (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
          (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
          (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode")
          (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
          (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
          (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
          (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
          (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
          (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode")
          (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode")
          (param          "pa"  :icon "gear"               :face default                          :collection "vscode")
          (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode")
          (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "vscode")
          (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode")
          (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode")
          (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode")
          (text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "vscode")
          (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
          (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
          (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "vscode")
          (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
          (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
          (t              "."   :icon "question"           :face font-lock-warning-face           :collection "vscode")))

  ;; Enable for completion UIs with margin-formatters capability, i.e. Corfu
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; (use-package cape
;;   :hook
;;   ((text-mode prog-mode) . sx-update-completion-functions)
;;   (eglot-managed-mode . sx-update-completion-functions)
;;   :init
;;   (setq cape-dabbrev-min-length sx-cape-dabbrev-min-length)
;;   :config
;;   ;; (setq completion-at-point-functions '(cape-file))
;;   (defun sx-update-completion-functions ()
;;     "Add the file and dabbrev backends to `completion-at-point-functions'"
;;     (dolist (backend '(cape-file cape-dabbrev))
;;       (add-to-list 'completion-at-point-functions backend t)))
;;   (defun sx-update-lsp-completion-functions ()
;;     "Bust the capf for `eglot-managed-mode'. To be used in a hook."
;;     ;; (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
;;     (cl-nsubst (cape-capf-noninterruptible
;;                 (cape-capf-bufer #'eglot-completion-at-point #'string-prefix-p))
;;                'eglot-completion-at-point
;;                completion-at-point-functions)))

;; Use tempel instead of using yasnippet. It uses the local templates file, which I have
;; added to the `user-emacs-directory'. It's much more lightweight than yasnippet, and
;; we can always add more snippets if we need to.
;; (use-package tempel
;;                     :hook
;;                     (prog-mode . tempel-setup-capf)
;;                     (prog-mode . tempel-abbrev-mode)
;;                     (text-mode . tempel-setup-capf)
;;                     :general
;;                     (:keymaps
;;                      '(tempel-map)
;;                      "C-n" 'tempel-next
;;                      "C-p" 'tempel-previous)
;;                     :config
;;                     (defun tempel-setup-capf ()
;;                       (setq-local completion-at-point-functions
;;                                   (cons #'tempel-expand
;;                                         completion-at-point-functions))))

;;; Consult

;; Consult provides various practical commands based on the Emacs completion function
;; completing-read, which allows to quickly select an item from a list of candidates
;; with completion. Consult offers in particular an advanced buffer switching command
;; consult-buffer to switch between buffers and recently opened files.
(use-package consult
  ;; :commands (consult-completion-in-region)
  :commands consult--read consult-xref consult-register-format consult-register-window
  :general
  ( :keymaps 'minibuffer-local-map
             "C-r" 'consult-history
             ;; Scroll through the items with C-n and C-p instead
             "<up>" 'previous-history-element
             "<down>" 'next-history-element
             :keymaps 'consult-narrow-help
             "?" 'consult-narrow-help)
  (global-definer
    "b" '(consult-buffer :which-key "Buffer switcher")
    "l" '(consult-line :which-key "Search buffer lines")
    "x" '(consult-flymake :which-key "Diagnostics")
    "y" '(consult-yank-pop :which-key "Kill-ring")
    "/" '(consult-ripgrep :which-key "Ripgrep"))
  :init
  ;; (setq consult-narrow-key "<")
  (setq consult-ripgrep-args (concat sx-rg-command
                                     " --null"
                                     " --hidden"
                                     " --glob \"!**/.git/**\""
                                     " --line-buffered"
                                     " --color=never"
                                     " --max-columns=1000"
                                     " --path-separator /"
                                     " --smart-case"
                                     " --no-heading"
                                     " --line-number .")
        consult-find-args (concat sx-fd-command " --color=never" " -H" " .")
        register-preview-delay 0))

(after! consult
  ;; Hide all sources except normal buffers in `consult-buffers' by default.
  (dolist (src consult-buffer-sources)
    (unless (eq src 'consult--source-buffer)
      (set src (plist-put (symbol-value src) :hidden t))))
  ;; Hide all sources except normal buffers in `consult-project-buffers'
  ;; by default.
  (dolist (src consult-project-buffer-sources)
    (unless (eq src 'consult--source-project-buffer)
      (set src (plist-put (symbol-value src) :hidden t)))))

(after! (register consult)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window))

(after! (xref consult)
  ;; use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Consult-dir allows you to easily insert directory paths into the minibuffer
;; prompt in Emacs.  When using the minibuffer, you can switch - with completion
;; and filtering provided by your completion setup - to any directory you’ve
;; visited recently, or to a project or bookmarked directory. The minibuffer
;; prompt will be replaced with the directory you choose.
(use-package consult-dir
  :general
  (global-definer "z" '(consult-dir :which-key "Jump to directory"))
  ( :keymaps 'vertico-map
             "C-d" 'consult-dir
             "C-j" 'consult-dir-jump-file))

(after! consult-dir
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
  ;; adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t))

(use-package consult-project-extra
  :general
  (global-definer "f" '(consult-project-extra-find :which-key "File picker"))
  :config
  (add-to-list 'project-switch-commands
               '(consult-project-extra-find "Find file" ?f)))

(after! '(consult-dir consult-project-extra-find)
  (setq consult-dir-default-command #'consult-project-extra-find))

;;; Embark

;; Provides a sort of right-click contextual menu for Emacs, accessed through the
;; embark-act command (which you should bind to a convenient key), offering you relevant
;; actions to use on a target determined by the context:
;;
;; - In the minibuffer, the target is the current best completion candidate.
;; - In the *Completions* buffer the target is the completion at point.
;; - In a regular buffer, the target is the region if active, or else the file, symbol or URL at point.
(use-package embark
  :general
  ("M-o" 'embark-act)
  :config
  (after! consult
    (add-hook! 'embark-collect-mode-hook 'consult-preview-at-point-mode))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :init
  (setq
   wgrep-auto-save-buffer t
   wgrep-change-readonly-file t))

(use-package embark-consult :after consult)

;;; UI

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :commands all-the-icons-install-fonts)

(use-package doom-nano-modeline
  ;; :demand
  :hook (after-init . doom-nano-modeline-mode)
  :init
  (setq doom-nano-modeline-position 'bottom)
  :config
  (doom-nano-modeline-mode 1))

;;; Org
;;; Org Agenda
;;; Denote
;;; Git

;; Provides a convenient way of simultaneous browsing through the differences between
;; a pair (or a triple) of files or buffers. Nevertheless, it needs to be configured
;; to remove some of the defaults that are horrible. We will remove the frame that
;; ediff creates, opting for using a window with options if needed.

(use-package ediff
  :commands ediff
  :init
  (setq
   ediff-diff-options "-w"
   ediff-split-window-function (if (> (frame-width) 150)
                                   'split-window-horizontally
                                 'split-window-vertically))
  :config
  (advice-add 'ediff-window-display-p :override #'ignore)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
  ;; :hook (git-commit-mode . jinx-mode)
  :general
  (global-definer "g" '(magit-status :which-key "Git status"))
  (minor-mode-definer
    :keymaps 'git-commit-mode
    "f" 'with-editor-finish
    "c" 'with-editor-cancel)
  ;; (+general-global-magit
  ;;   "s" 'magit-status
  ;;   "f" 'magit-find-file
  ;;   "l" 'magit-log-buffer-file
  ;;   "d" 'magit-diff-buffer-file)
  (general-nmap :keymaps 'project-prefix-map
    "m" 'magit-project-status)
  :init
  (setq
   ;; (magit-ediff-dwim-show-on-hunks)
   ;; (magit-diff-refine-hunk 'all)
   sx-git-commit-summary-max-length sx-git-commit-summary-max-length
   magit-diff-hide-trailing-cr-characters t
   magit-diff-refine-ignore-whitespace t
   magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (add-hook! 'git-commit-mode-hook (set-fill-column sx-git-commit-fill-column))
  (add-hook! 'magit-status-mode (display-line-numbers-mode -1)))

;; (after! general
;;   (+general-global-magit
;;     "b" 'vc-region-history))

(use-package magit
  :after project
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit") t))

;;; Project

;; https://andreyor.st/posts/2022-07-16-project-el-enchancements
;; A great article about nice enhancements to `project.el'
;; One is a custom function that searches for a project root a bit differently
;; from the default. It searches for specific root-marker files which in
;; practice helps to not have to call `project-try-vc', giving us very
;; predictable project roots.
;; Another is a custom function to only save modified file-visiting buffers
;; in the current project and advices for `project-compile' and `recompile'
;; to prevent them from asking to save unrelated buffers.

(after! project
  (global-definer
    "t" '(project-eshell :which-key "Eshell")
    "j" '(project-switch-project :which-key "Switch project"))
  (+general-global-project
    "!" '(project-shell-command :which-key "shell-command")
    "&" '(project-async-shell-command :which-key "async shell-command")
    "D" 'project-dired)

  (defun rune/project-remember-projects-under (dir)
    "Index all projects below a directory DIR."
    (interactive "DDirectory: \nP")
    (project--ensure-read-project-list)
    (let ((project-dirs (mapcar (lambda (d) (concat d "/"))
                                (seq-filter #'file-directory-p
                                            (mapcar #'abbreviate-file-name
                                                    (directory-files dir t "^[^.]"))))))
      (dolist (dir project-dirs)
        (when-let ((proj (project-current nil dir)))
          (project-remember-project proj)))))

  (rune/project-remember-projects-under "~/projects/")
  (rune/project-remember-projects-under "~/nix-dotfiles/")
  (setq project-vc-extra-root-markers '(".git" ".project"))
  (setq project-switch-commands '((consult-ripgrep "Ripgrep" ?s)
                                  ;;(project-find-file "Find file" ?f)
                                  (project-dired "Dired" ?d)
                                  (magit-project-status "Git status" ?g)
                                  (consult-project-buffer "Switch buffer" ?r)
                                  (project-vc-dir "VC-Dir" ?G)
                                  (project-eshell "Eshell" ?!))))

(use-package projection
  :hook (doom-first-buffer . global-projection-hook-mode)
  :bind-keymap
  ("C-x P" . projection-map))

(use-package projection-multi
  :general
  (minor-mode-definer)
  (global-definer "m" '(projection-multi-compile :which-key "Multi-compile")))

(use-package projection-multi-embark
  :after embark
  :after projection-multi
  :demand t
  :config (projection-multi-embark-setup-command-map))

;;; Compile

(use-package compile
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :general
  (global-definer
    "," '(project-compile :which-key "Compile")
    "." '(recompile :which-key "Recompile"))
  :custom
  ;; Automatically scroll build output
  (compilation-scroll-output t)
  ;; Skip anything less than error
  (compilation-skip-threshold 2)
  ;; Automatically jump to the first error unconditionally during compilation
  (compilation-auto-jump-to-first-error t)
  ;; Don't hide any output lines
  (compilation-max-output-line-length nil)
  ;; Kill compilation process before starting another
  (compilarion-always-kill t)
  :commands (define-compilation-mode)
  :preface
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  ;; (autoload 'comint-truncate-buffer "comint" nil t)
  ;; (add-hook 'compilation-filter-hook #'comint-truncate-buffer)
  (cl-defun compile-add-error-syntax
      (mode name regexp &key file line col (level 'error) hyperlink highlight)
    "Register new compilation error syntax.

Add NAME symbol to `compilation-error-regexp-alist', and then add
REGEXP FILE LINE and optional COL LEVEL info to
`compilation-error-regexp-alist-alist'."
    (or file (error "Missing value for :file keyword"))
    (or line (error "Missing value for :line keyword"))
    (let ((faces '(compilation-info-face
                   compilation-warning-face
                   compilation-error-face))
          (level (cond ((eq level 'info) 0)
                       ((eq level 'warn) 1)
                       ((eq level 'error) 2)
                       (t (error "Mnsupported level type: %S" level))))
          (mode (symbol-name (or mode 'compilation))))
      (add-to-list (intern (concat mode "-error-regexp-alist")) name)
      (add-to-list (intern (concat mode "-error-regexp-alist-alist"))
                   (list name regexp file line col level hyperlink
                         (list highlight (nth level faces))))))
  (defmacro define-project-compilation-mode (base-name &rest body)
    (declare (indent 1))
    (let* ((name (symbol-name base-name))
           (doc-name (capitalize (replace-regexp-in-string "-compilation$" "" name)))
           (current-project-root (intern (concat name "-current-project")))
           (current-project-files (intern (concat name "-current-project-files")))
           (compilation-mode-name (intern (concat name "-mode"))))
      `(progn
         (defvar ,(intern (concat name "-error-regexp-alist")) nil
           ,(concat "Alist that specifies how to match errors in " doc-name " compiler output.
See `compilation-error-regexp-alist' for more information."))
         (defvar ,(intern (concat name "-error-regexp-alist-alist")) nil
           ,(concat "Alist of values for `" (downcase doc-name) "-compilation-error-regexp-alist'.
See `compilation-error-regexp-alist-alist' for more information."))
         (defvar-local ,current-project-root nil
           ,(concat "Current root of the project being compiled.
Set automatically by the `" (symbol-name compilation-mode-name) "'."))
         (defvar-local ,current-project-files nil
           ,(concat "Current list of files belonging to the project being compiled.
Set automatically by the `" (symbol-name compilation-mode-name) "'."))
         (define-compilation-mode ,compilation-mode-name
           ,(concat doc-name " Compilation")
           ,(concat "Compilation mode for " doc-name " output.")
           (setq-local ,current-project-root (project-current t))
           (setq-local ,current-project-files (project-files ,current-project-root))
           ,@body)
         (provide ',compilation-mode-name)))))

(use-package compile-multi
  :init
  (setq compile-multi-config '((rust-ts-mode ("cargo run" . "cargo run")
                                ("cargo run --release" . "cargo run --release")
                                ("cargo test" . "cargo test")))))

(use-package consult-compile-multi
  :after compile-multi
  :config (consult-compile-multi-mode))

(use-package compile-multi-all-the-icons
  :after all-the-icons-completion
  :demand t
  :after compile-multi)

(use-package compile-multi-embark
  :after embark
  :after compile-multi
  :demand t
  :config (compile-multi-embark-mode +1))

;;; Tab-bar

(after! tab-bar
  (setq
   tab-bar-close-button-show nil
   tab-bar-new-button-show nil))

(use-package tabspaces
  :hook (doom-first-buffer . tabspaces-mode)
  :general
  (+general-global-project
    "b" '(tabspaces-switch-to-buffer :which-key "tabspaces-switch-to-buffer")
    "d" '(tabspaces-close-workspace :which-key "tabspaces-close-workspace")
    "k" '(tabspaces-kill-buffers-close-workspace :which-key "tabspaces-kill-buffer-close-workspaces")
    "o" '(tabspaces-open-or-create-project-and-workspace :which-key "tabspaces-open-or-create-project-and-workspace")
    "r" '(tabspaces-remove-selected-buffer :which-key "tabspaces-remove-selected-buffer")
    "s" '(tabspaces-switch-or-create-workspace :which-key "tabspaces-switch-or-create-workspace")
    "t" '(tabspaces-switch-buffer-and-tab :which-key "tabspaces-switch-buffer-and-tab")
    "C" '(tabspaces-clear-buffers :which-key "tabspaces-clear-buffers"))
  :custom
  (tabspaces-session t)
  (tabspaces-project-switch-commands #'consult-project-extra-find)
  ;; (tabspaces-use-filtered-buffers-as-default t)
  (tab-bar-new-tab-choice "*scratch*")
  ;; (tabspaces-remove-to-default nil)
  ;; (tabspaces-session-auto-restore t)
  :preface
  (defun rune/tabspace-setup ()
    "Set up tabspace at startup"
    ;; Add *Messages* and *Welcome* to Tab `Home'
    (tabspaces-mode 1)
    (progn
      (tab-bar-rename-tab "Home")
      (when (get-buffer "*Messages*")
        (set-frame-parameter nil
                             'buffer-list
                             (cons (get-buffer "*Messages*")
                                   (frame-parameter nil 'buffer-list))))
      (when (get-buffer "*Welcome*")
        (set-frame-parameter nil
                             'buffer-list
                             (cons (get-buffer "*Welcome*")
                                   (frame-parameter nil 'buffer-list))))))
  (add-hook 'after-init-hook #'rune/tabspace-setup))

;; Sometimes we wish to switch to some open buffer in a tabspace and switch
;; to that tab as well. `tabspaces-switch-buffer-and-tab' achieves this.
;; If the buffer is open in more than one workspace the user will be prompted
;; to choose which tab to switch to. If there is no such buffer users will be
;; prompted on whether to create it in a new workspace or the current one.
;; Embark provides an elegant way to use `tabspaces-switch-buffer-and-tab'
;; while using `consult-buffer'. Narrow to the buffer you want to act on.
;; Run `embark-act' and then choose "B" to switch to the buffer and tab.
(after! '(tabspaces embark)
  (defvar-keymap embark-tabspaces-actions
    :doc "Keymap for actions with `tabspaces'."
    :parent embark-general-map
    "B" #'tab-bar-select-tab-by-name)
  (add-to-list 'embark-keymap-alist '(buffer . embark-tabspaces-actions)))

(after! '(tabspaces consult)
  ;; Have only workspace buffers appear in `consult-buffer' by default
  ;; Hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; Set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name "Workspace Buffers"
          :narrow ?w
          :history 'buffer-name-history
          :category 'buffer
          :state #'consult--buffer-state
          :default t
          :items #'(lambda () (consult--buffer-query
                               :predicate #'tabspaces--local-buffer-p
                               :sort 'visibility
                               :as #'buffer-name)))
    "Set workspace buffer list for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace)

  ;; Add a Consult source for all buffers where the action is
  ;;  `tabspaces-switch-buffer-and-tab'.
  (defvar consult--source-tab-buffer
    `(:name "All buffers"
      :narrow ?B
      :history buffer-name-history
      :category buffer
      :hidden t
      :face consult-buffer
      :items ,(lambda () (consult--buffer-query :sort 'visibility
                                                :as #'buffer-name))
      :action ,#'tabspaces-switch-buffer-and-tab)
    "All visible buffers list; switch to workspace when switching buffers.")
  (add-to-list 'consult-buffer-sources consult--source-tab-buffer))

;;; Lang

(use-package elisp-mode
  :hook (emacs-lisp-mode . eldoc-mode))

;; (use-package scheme)
;; (use-package geiser
;;   :hook (scheme-mode . geiser-mode)
;;   :custom
;;   (gesier-active-implementations '(guile))
;;   (geiser-default-implementation 'guile))
;; (use-package geiser-guile
;;   :after geiser)

(use-package rust-mode)

(use-package nix-mode
  :mode "\\.nix\\'")
;; (use-package nix-ts-mode
;;   :mode "\\.nix\\'")
;; (use-package nix-update)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Using the hl-todo package, we are able to highlight keywords
;; related to the working environment, like: TODO, FIXME and some
;; more.
(use-package hl-todo
  :hook ((prog-mode gfm-mode org-mode) . hl-todo-mode)
  :config
  (setq
   hl-todo-highlight-punctuation ":"
   hl-todo-keyword-faces
   `(("TODO"       (or org-todo error) bold)
     ("FIXME"      error bold)
     ("HACK"       font-lock-constant-face bold)
     ("REVIEW"     font-lock-keyword-face bold)
     ("NOTE"       success bold)
     ("DONE"       (or org-done success) bold)
     ("DEPRECATED" font-lock-doc-face bold))))

;; Markdown configuration, which I use specially often when editing README files
;; on Github. The are some interesting options like the change of the markdown-command
;; to pandoc which is way better at compiling html5.
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :hook (markdown-mode . visual-line-mode)
  :init (setq markdown-command "pandoc -t html5")
  :general
  (major-mode-definer
    :keymaps '(markdown-mode-map)
    :major-modes '(gfm-mode markdown-mode)
    "c" 'markdown-insert-code
    "l" 'markdown-insert-link))

;;; Mail
;;; Eglot

(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimization-init-p nil)
(defvar +lsp-defer-shutdown 3
  "If non-nil, defer shutdown of LSP servers for this many seconds after last
workspace buffer is closed.

This delay prevents premature server shutdown when a user still intends on
working on that project after closing the last buffer, or when programmatically
killing and opening many LSP/eglot-powered buffers.")

(define-minor-mode +lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not +lsp-optimization-mode)
      (setq-default read-process-output-max +lsp--default-read-process-output-max
                    gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                    +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq +lsp--default-read-process-output-max
            ;; DEPRECATED Remove check when 26 support is dropped
            (if (boundp 'read-process-output-max)
                (default-value 'read-process-output-max))
            +lsp--default-gcmh-high-cons-threshold
            (default-value 'gcmh-high-cons-threshold))
      ;; `read-process-output-max' is only available on recent development
      ;; builds of Emacs 27 and above.
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
      ;;        native JSON library, so we up the GC threshold to stave off
      ;;        GC-induced slowdowns/freezes. Doom uses `gcmh' to enforce its
      ;;        GC strategy, so we modify its variables rather than
      ;;        `gc-cons-threshold' directly.
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))

(use-package eglot
  :ensure nil
  :commands eglot eglot-ensure
  :hook
  (eglot-managed-mode . +lsp-optimization-mode)
  (eglot-managed-mode . rune/eglot-eldoc-manage)
  :custom
  (eglot-events-buffer-size 0) ;; disable eglot logging (improves performance)
  (eglot-autoshutdown t) ;; shutdown server when last managed buffer is killed
  :preface
  ;; Eglot specifically alters the `eldoc-documentation-strategy', so we
  ;; override it with a hook.
  (defun rune/eglot-eldoc-manage ()
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

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
      (rune/add-eglot-hooks
       eglot-server-programs
       ;; rune/lsp-bin-exists-p errors when it gets to rust but i can't figure out why
       ;; TODO: try exec-path-from-shell to fix rune/lsp-bin-exists-p
       ;; (seq-filter
       ;; #'rune/lsp-bin-exists-p
       ;; eglot-server-programs)
       )))
  :init
  (setq
   eglot-sync-connect 1
   eglot-connect-timeout 10
   eglot-autoshutdown t
   eglot-send-changes-idle-time 0.5
   eglot-auto-display-help-buffer nil
   eglot-report-progress nil
   rustic-lsp-client 'eglot)
  (defadvice! +lsp--defer-server-shutdown-a (fn &optional server)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    :around #'eglot--managed-mode
    (letf!
      (defun eglot-shutdown (server)
        (if (or (null +lsp-defer-shutdown)
                (eq +lsp-defer-shutdown 0))
            (prog1 (funcall eglot-shutdown server)
              (+lsp-optimization-mode -1))
          (run-at-time
           (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
           nil (lambda (server)
                 (unless (eglot--managed-buffers server)
                   (prog1 (funcall eglot-shutdown server)
                     (+lsp-optimization-mode -1))))
           server)))
      (funcall fn server)))

  (setq-default eglot-workspace-configuration
                '((:rust-analyzer
                   :completion (:callable (:snippets "fill_arguments"))
                   :checkOnSave (:command "clippy" :allTargets :json-false))))
  (eglot-auto-ensure-all))

(after! eglot
  (general-def
    :keymaps 'flymake-mode-map
    "M-n" 'flymake-goto-next-error
    "M-p" 'flymake-goto-prev-error)
  (minor-mode-definer
    :keymaps 'flymake-mode
    "n" 'flymake-goto-next-error
    "p" 'flymake-goto-prev-error)
  (minor-mode-definer
    :keymaps 'eglot--managed-mode
    "a" '(eglot-code-actions :which-key "Code actions")
    "d" '(consult-flymake :which-key "Diagnostics")
    "r" '(eglot-rename :which-key "Rename symbol"))
  (general-nmap
    :major-modes '(eglot--managed-mode)
    "gi" 'eglot-find-implementation))

;;; Eldoc

(use-package eldoc
  :defer t
  :custom
  ;; `eldoc-documentation-compose' and `eldoc-documentation-compose-eagerly'
  ;; help display information from multiple Eldoc sources at the same time.
  ;; The eager option displays results as they come in; the other collates all
  ;; the answers and displays them when they're all ready.
  ;; I like the eager option.
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  ;; Eldoc resizes the echo area display which is intrusive. Let's not do that.
  (eldoc-echo-area-use-multiline-p nil)
  ;; Skip showing documentation in the echo area and use an `eldoc-doc-buffer'
  ;; window if it is already displayed.
  (eldoc-echo-area-prefer-doc-buffer t)
  :preface
  ;; `eldoc-doc-buffer' opens a dedicated buffer for your documentation.
  ;; This displays what ever is at point. The only annoying thing about it
  ;; is that it prefers to pop open in an existing window.
  ;; Let's tweak its display to force the buffer to appear at the bottom
  ;; of the frame instead with a fixed window height of 4 lines.
  (add-to-list 'display-buffer-alist
               ;; The buffer name changes depending on the context,
               ;; and this display rule reflects that.
               '("^\\*eldoc" display-buffer-at-bottom
                 (window-height . 4)))
  ;; If you add a function to `eldoc-documentation-functions' (with add-hook)
  ;; then Eldoc will query the functions in the order they're in and source
  ;; documentation from them. Eglot and Flymake support this out of the box,
  ;; but as it's fairly other packages do not.
  ;; See https://masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  ;; for an example function to integrate Flycheck with Eldoc.
  ;; (add-hook 'eldoc-documentation-functions #'rune/flycheck-eldoc nil t)
  :config
  ;; Eldoc detects movement and uses its idle delay `eldoc-idle-delay' to
  ;; determine when to ask its backend documentation functions for information.
  ;; To improve performance, it won't trigger on every command; instead, it
  ;; maintains a list of common interactive commands. If you use things like
  ;; Paredit or Combobulate then it won't display if you interact with one of
  ;; those commands. Luckily there's the `eldoc-add-command-completion' command
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "combobulate-"))

;;; Flymake

(use-package flymake
  :hook ((prog-mode . flymake-mode)
         (text-mode . flymake-mode))
  :config
  (setq elisp-flymake-byte-compile-load-path (cons "./" load-path))

  (defun flymake-eldoc-function (report-doc &rest _)
    "Document diagnostics at point.
Intended for `eldoc-documentation-functions' (which see)."
    (let ((diags (flymake-diagnostics (point))))
      (when diags
        (funcall report-doc
                 (mapconcat (lambda (d)
                              (let ((level (flymake-diagnostic-type d)))
                                (pcase level
                                  ;; TODO: warning, error, and note don't work in non eglot modes (elisp)
                                  ('warning (propertize (flymake-diagnostic-text d) 'face 'flymake-warning-echo))
                                  ('error (propertize (flymake-diagnostic-text d) 'face 'flymake-error-echo))
                                  ('note (propertize (flymake-diagnostic-text d) 'face 'flymake-note-echo))
                                  ('eglot-warning (propertize (flymake-diagnostic-text d) 'face 'flymake-warning-echo))
                                  ('eglot-error (propertize (flymake-diagnostic-text d) 'face 'flymake-error-echo))
                                  ('eglot-note (propertize (flymake-diagnostic-text d) 'face 'flymake-note-echo))
                                  (_ (flymake-diagnostic-text d))))) diags "\n"))))))

(use-package flymake-collection
  :after flymake
  :hook (after-init . flymake-collection-hook-setup))

(use-package package-lint-flymake
  :init
  (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup))

(use-package flymake-flycheck
  :after flymake
  :hook (flymake-mode . flymake-flycheck-auto))

;;; Apheleia

(use-package apheleia
  :hook (doom-first-buffer . apheleia-global-mode)
  :preface
  ;; Sometimes we want to use `eglot-format-buffer' as the command to format the
  ;; buffer on save. The benefit of going through `eglot' rather than using the
  ;; formatter defined by Apheleia is two-fold. A language server's persistance
  ;; means that formats of the same project are reasonably fast in languages with
  ;; high JIT latency, like Julia. Adding a hook to `eglot-managed-mode' to call
  ;; eglot-format-buffer locks up Emacs while the formatter runs. Aphelia's
  ;; approach of running it in the background is much nicer.
  (require 'cl-lib)
  (cl-defun apheleia-indent-eglot-managed-buffer
      (&key buffer scratch callback &allow-other-keys)
    "Copy BUFFER to SCRATCH, then format scratch, then call CALLBACK."
    (with-current-buffer scratch
      (setq-local eglot--cached-server
                  (with-current-buffer buffer
                    (eglot-current-server)))
      (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
        (eglot-format-buffer))
      (funcall callback)))
  :config
  (add-to-list 'apheleia-formatters
               '(eglot-managed . apheleia-indent-eglot-manager-buffer))
  (add-to-list 'apheleia-mode-alist '(julia-mode . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(rust-mode . eglot-managed)))

;;; Eshell

(use-package esh-mode
  :preface
  ;; Eshell prompt
  (declare-function eshell-search-path "ext:esh-ext")
  (defun eshell-prompt ()
    (let* ((date (propertize (format-time-string "%a %H:%M") 'face '(:inherit shadow)))
           (path (abbreviate-file-name default-directory))
           (branch (when (and (eshell-search-path "git")
                              (locate-dominating-file default-directory ".git"))
                     (concat (propertize (propertize " on " 'face '(:inherit shadow)))
                             (propertize (string-trim (shell-command-to-string "git branch --show-current"))
                                         'face (if (string-empty-p (shell-command-to-string "git status --porcelain 2>/dev/null"))
                                                   '(:inherit shadow)
                                                 '(:inherit font-lock-builtin-face))))))
           (container (cond
                       ((file-exists-p "/run/.containerenv")
                        (format " in %s"
                                (with-temp-buffer
                                  (save-match-data
                                    (insert-file-contents "/run/.containerenv")
                                    (re-search-forward "^name=\"\\([^\"]+\\)\"" nil t)
                                    (switch-to-buffer (current-buffer))
                                    (or (match-string-no-properties 1) "podman")))))
                       ((file-exists-p "/.dockerenv") " in docker")))
           (ssh (when (getenv "SSH_CONNECTION") " via ssh"))
           (info (concat (or branch "")
                         (propertize (concat (or container "")
                                             (or ssh ""))
                                     'face '(:inherit shadow))))
           (prompt (if (= eshell-last-command-status 0)
                       "$"
                     (propertize "$" 'face '(:inherit error)))))
      (concat date " " path info "\n" prompt " ")))
  :custom
  (eshell-scroll-show-maximum-output nil)
  ;; (eshell-prompt-function 'eshell-prompt)
  (eshell-banner-message ""))

;; Custom functions for use in Eshell
(use-package eshell
  :defer
  :config
  (defalias 'eshell/v 'eshell-exec-visual)

  (defalias 'eshell/x #'eshell/exit)

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
                       (completing-read "cd: " eshell-dirs))))))))

;; Calling and exiting eshell
(use-package eshell
  :general
  ;; SPC-t<return>
  ("C-<return>" 'eshell-here)
  ;; SPC-t!
  ("C-!" 'eshell)
  :init
  (add-to-list 'display-buffer-alist
               '("^\\*eshell.*\\*$" display-buffer-at-bottom
                 (window-height . .33)))
  (add-to-list 'display-buffer-alist
               '("^\\*.*-eshell\\*$" display-buffer-at-bottom
                 (window-height . .33)))
  :config
  (advice-add 'eshell-life-is-too-much :after #'delete-window-if-not-single)
  (advice-add 'eshell-mark-output :after #'activate-mark)
  ;; From http://howardism.org/Technical/Emacs/eshell-fun.html
  (defun eshell-here ()
    "Opens up a new shell in the directory associated with the current buffer's
file. The eshell is renamed to match that directory to make multiple eshell
windows easier."
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
        (insert (concat "ls"))
        (eshell-send-input)))))

(use-package esh-module
  :after eshell
  :custom
  (eshell-modules-list
   (remove 'eshell-term eshell-modules-list)))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-global-mode))

(use-package eat
  :defer
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)
  :custom (eat-kill-buffer-on-exit t))

;;; Extra

(use-package messages
  :no-require
  :preface
  (provide 'messages)
  :general
  ( :keymaps 'messages-buffer-mode-map
             "gk" 'messages-clear-buffer)
  :config
  (defun messages-clear-buffer ()
    "Clear the *Messages* buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))))

(use-package font
  :no-require
  :hook (after-init . setup-fonts)
  :preface
  (defun font-installed-p (font-name)
    "Check if a font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))
  (defun setup-fonts ()
    (cond ((font-installed-p "JetBrains Mono Nerd Font")
           (set-face-attribute 'default nil
                               :family "JetBrains Mono Nerd Font"
                               :stipple nil
                               :box nil
                               :strike-through nil
                               :overline nil
                               :underline nil
                               :slant 'normal
                               :weight 'normal
                               :height 160
                               :width 'normal
                               :foundry "nil")))
    (when (font-installed-p "DejaVuSans")
      (set-face-attribute 'variable-pitch nil :font "DejaVu Sans"))))

(use-package frame
  :hook (after-init . window-divider-mode)
  :custom
  ;; The native border "consumes" a pixel of the fringe on righter-most
  ;; splits, `window-divider' does not.
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  :config
  ;; The blinking cursor is distracting.
  (blink-cursor-mode -1))

;; (use-package startup
;;   :no-require
;;   :custom
;;   (inhibit-splash-screen t))

;; (use-package menu-bar
;;   ;; :unless (display-graphic-p)
;;   :config
;;   (menu-bar-mode -1))

;; (use-package tooltip
;;   :config
;;   (tooltip-mode -1))

;; (use-package vc-hooks
;;   ;; :custom
;;   ;; Resolve symlinks when opening files, so that any operations are conducted
;;   ;; from the file's true directory (like `find-file')
;;   ;; (find-file-visit-truename t)
;;   ;; (vc-follow-symlinks t)
;;   :defer t)

;; (use-package ansi-color
;;   :custom
;;   (ansi-color-for-comint-mode))

;; (use-package comint
;;   :defer t
;;   :custom
;;   (comint-prompt-read-only t)
;;   (comint-buffer-maximum-size 2048)
;;   (comint-scroll-show-maximum-output nil)
;;   (comint-highlight-input nil)
;;   (comint-input-ignore-dups t))

;; (use-package profiler
;;   :general
;;   ("<f2>" 'profiler-start-or-report)
;;   :commands (profiler-report)
;;   :preface
;;   (defun profiler-start-or-report ()
;;     (interactive)
;;     (if (not (profiler-cpu-running-p))
;;         (profiler-start 'cpu)
;;       (profiler-report)
;;       (profiler-cpu-stop))))

;; ;; I want help windows to be selected automatically so I can close it with `q'
;; ;; after I've finished reading
;; (use-package help
;;   :general
;;   (global-definer "h" '(help-command :which-key "help"))
;;   :custom
;;   (help-window-select t))

;; ;; ;; for `pdf-tools'
;; ;; (use-package doc-view
;; ;;   :defer t
;; ;;   :custom
;; ;;   (doc-view-resolution 192))

;; (use-package autorevert
;;   :hook (after-init . global-auto-revert-mode))

;; (use-package repeat
;;   :hook (after-init . repeat-mode))

(use-package elec-pair
  :hook
  ((prog-mode . electric-pair-local-mode)
   (eval-expression-minibuffer-setup . electric-pair-local-mode)
   (smartparens-mode . (lambda () (electric-pair-local-mode -1)))))

(use-package puni
  :hook ((prog-mode . puni-mode)
         (eval-expression-minibuffer-setup . puni-mode))
  :general
  ( :states '(normal visual operator) :keymaps 'puni-mode-map
            "<return>" 'puni-expand-region
            "S-<return>" 'puni-contract-region)
  ;; paredit-like keys
  ( :states 'normal :keymaps 'puni-mode-map
            "C-M-f" 'puni-forward-sexp-or-up-list
            "C-M-b" 'puni-backward-sexp-or-up-list
            "C-M-t" 'puni-transpose
            ;; slurping & barfing
            "C-<left>" 'puni-barf-forward
            "C-}" 'puni-barf-forward
            "C-<right>" 'puni-slurp-forward
            "C-)" 'puni-slurp-forward
            "C-(" 'puni-slurp-backward
            "C-M-<left>" 'puni-slurp-backward
            "C-{" 'puni-barf-backward
            "C-M-<right>" 'puni-barf-backward
            ;; depth chaning
            "M-r" 'puni-raise
            "M-s" 'puni-splice
            "M-<up>" 'puni-splice-killing-backward
            "M-<down>" 'puni-splice-killing-forward
            "M-(" 'puni-wrap-round
            "M-{" 'puni-wrap-curly
            "M-?" 'puni-convolute
            "M-S" 'puni-split
            ;; :map region-bindings-mode-map
            ;; "(" 'puni-wrap-round
            ;; "[" 'puni-wrap-square
            ;; "{" 'puni-wrap-curly
            ;; "<" 'puni-wrap-angle
            ))
;; :init
;; (evil-define-key '(normal visual operator) 'puni-mode-map (kbd "<return>") 'puni-expand-region)
;; (evil-define-key '(normal visual operator) 'puni-mode-map (kbd "S-<return>") 'puni-contract-region))

;; (use-package yasnippet
;;   :defer t)

;; TODO: better theme for delta in git decorations.
;; copy from modus-theme magit diff faces
;; (use-package magit-delta
;;   :hook (magit-mode . magit-delta-mode))

;; ;; See `start-server-or-open-at-server' in `early-init.el' for an alternative.
;; (use-package server
;;   :commands (server-running-p)
;;   :init
;;   (unless (server-running-p)
;;     (server-start)))

(use-package envrc
  :functions envrc-global-mode
  :init (envrc-global-mode))

;; (use-package solaire-mode
;;   :hook (eshell-mode . solaire-mode))

;; ;; BUG: breaks tab-bar-mode
;; (use-package spacious-padding
;;   :hook (after-init . spacious-padding-mode))

;; (tooltip-mode -1)
;; (menu-bar-mode -1)
(set-fringe-mode 10)
(minibuffer-depth-indicate-mode)

;; (defun rune/show-welcome-buffer ()
;;   "Show *Welcome* buffer."
;;   (with-current-buffer (get-buffer-create "*Welcome*")
;;     (setq truncate-lines t)
;;     (let* ((buffer-read-only)
;;            (image-path "~/.emacs.d/xemacs_color.svg")
;;            (image (create-image image-path))
;;            (size (image-size image))
;;            (height (cdr size))
;;            (width (car size))
;;            (top-margin (floor (/ (- (window-height) height) 2)))
;;            (left-margin (floor (/ (- (window-width) width) 2)))
;;            (prompt-title "Welcome to Emacs!"))
;;       (erase-buffer)
;;       (setq mode-line-format nil)
;;       (goto-char (point-min))
;;       (insert (make-string top-margin ?\n ))
;;       (insert (make-string left-margin ?\ ))
;;       (insert-image image)
;;       (insert "\n\n\n")
;;       (insert (make-string (floor (/ (- (window-width) (string-width prompt-title)) 2)) ?\ ))
;;       (insert prompt-title))
;;     (setq cursor-type nil)
;;     (read-only-mode +1)
;;     (switch-to-buffer (current-buffer))
;;     (local-set-key (kbd "q") 'kill-this-buffer)))

;; (when (< (length command-line-args) 2)
;;   (add-hook 'emacs-startup-hook (lambda ()
;;                                   (when (display-graphic-p)
;;                                     (rune/show-welcome-buffer)))))

(use-package whitespace
  :hook
  (prog-mode . (lambda () (setq show-trailing-whitespace t)))
  (text-mode . (lambda () (setq show-trailing-whitespace t)))
  (conf-mode . (lambda () (setq show-trailing-whitespace t)))
  :custom
  ;; (whitespace-line-column nil)
  (whitespace-display-mappings
   '((tab-mark ?\t [?\u203A ?\t])
     (newline-mark ?\n [?\u2B90 ?\n])
     (space-mark ?\  [?\u00B7] [?.])))
  ;; (whitespace-style '(face trailing empty big-indent))
  (whitespace-style '(face trailing newline newline-mark tabs tab-mark))
  ;; (whitespace-style '(face indentation tabs tab-mark spaces space-mark newline
  ;;                          newline-mark trailing lines-tail))
  (whitespace-global-modes '(not erc-mode magit-mode))
  :config
  (global-whitespace-mode))

;; save place in file you are currently in
(use-package saveplace
  :hook (after-init . save-place-mode))

;; (use-package git-gutter
;;   :preface
;;   (defun modus-themes-custom-faces ()
;;     (modus-themes-with-colors
;;       (custom-set-faces
;;        `(git-gutter-fr:added ((,class :foreground ,green-fringe-bg)))
;;        `(git-gutter-fr:deleted ((,class :foreground ,red-fringe-bg)))
;;        `(git-gutter-fr:modified ((,class :foreground ,yellow-fringe-bg))))))
;;   :hook ((prog-mode . git-gutter-mode)
;;       (text-mode . git-gutter-mode)
;;       (modus-themes-after-load-theme . modus-themes-custom-faces))
;;   :config
;;   (setq git-gutter:update-interval 0.05))

;; (use-package git-gutter-fringe
;;   :config
;;   (setq git-gutter-fr:side 'right-fringe)
;;   (modus-themes-load-vivendi))

;; (setq custom-file "~/.emacs.d/var/custom.el")
;; (message "*** Emacs loaded in %s seconds with %d garbage collections."
;;          (format "%.2f seconds"
;;                  (float-time
;;                   (time-subtract after-init-time before-init-time))) gcs-done)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Profile emacs startup
;;; init.el ends here
