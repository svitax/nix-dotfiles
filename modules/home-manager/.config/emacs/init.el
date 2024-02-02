;; Always defer use-package packages. This means that if I really need a
;; a package, I will go to my config and edit the use-package recipe to lazy
;; load it. This reduces my startup time significantly
(setq use-package-always-defer t)
(require 'use-package)

;; Add modules directory
(add-to-list 'load-path (concat user-emacs-directory "modules"))

;;; Core -- the heart of the beast
(require 'core-lib)
(require 'on)

;; Function to print out the loading time and number of GCs during the loading.
(setq my/emacs-started nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
            (setq my/emacs-started t)))

;; Suppress "When done with this frame, type C-x 5 0" messages when opening a
;; new frame
(setq server-client-instructions nil)

;; Set the following to `t' to print debug information during the startup. This
;; will include the order in which the packages are loaded and the loading time
;; of individual packages.
;; (setq use-package-verbose t)

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

;;; General

(use-package general)

(after! general
  (general-evil-setup)

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
    "" '(:ignore t :which-key "localleader"))

  ;; Add an additional minor-mode-definer, for each of the modes.
  ;; It is key to remember that in this case, the :keymaps option refers to the minor-mode,
  ;; not the keymap.
  (general-create-definer minor-mode-definer
    :keymaps 'override
    :definer 'minor-mode
    :states '(normal hybrid motion visual operator)
    :prefix "SPC m")

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

  ;; Emulate different default Emacs keys, as I'm transitioning into evil.
  (global-definer
    ;; "SPC" '(execute-extended-command :which-key "M-x")
    ""     nil)

  ;; Since I let `evil-mode' take over `C-u' for buffer scrolling, I need to rebind the
  ;; `universal-argument' command to another key sequence.
  (general-nmap "C-M-u" 'universal-argument)

  ;; Fast and easy way to save buffers.
  (general-mmap "C-s" 'save-buffer)

  (global-definer "a" 'find-file)
  (global-definer "c" 'delete-window)
  (global-definer "e" 'dired-jump)
  (global-definer "f" 'project-find-file)
  (global-definer "k" 'kill-current-buffer)
  (+general-global-menu! "notes" "n")
  (+general-global-menu! "org" "o")
  (+general-global-menu! "project" "p")
  (global-definer "q" 'evil-quit-all)
  (+general-global-menu! "eshell" "t"))

;; (use-package which-key
;;   :hook (emacs-startup . which-key-mode)
;;   :init
;;   (setq which-key-sort-order #'which-key-key-order-alpha
;;         which-key-sort-uppercase-first nil
;;         which-key-add-column-padding 1
;;         which-key-max-display-columns nil
;;         which-key-min-display-lines 6
;;         which-key-side-window-slot -10)
;;   :config
;;   ;; general improvements to which-key readability
;;   (which-key-setup-side-window-bottom)
;;   (setq-hook! 'which-key-init-buffer-hook line-spacing 3))

;;; Evil

(use-package evil
  :hook (on-init-ui . evil-mode)
  :preface
  (setq evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; if the current state is obvious from the cursor's color/shape, then
        ;; we don't need superfluous indicators to do it instead
        evil-default-cursor '+evil-default-cursor-fn
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor '(box +evil-emacs-cursor-fn)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-echo-state nil
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        ;; It's infuriating that innocuous "beginning of line" or "end of line"
        ;; errors will abort macros, so suppress them:
        evil-kbd-macro-suppress-motion-error t
        evil-undo-system 'undo-redo
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-u-delete t
        evil-want-C-d-scroll t
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol t
        evil-respect-visual-line-mode t
        ;; evil-search-module 'evil-search
        ;; BUG: no evil-search module supports isearch-lazy-count
        ;; once fixed can deprecate evil-anzu
        ;; isearch-lazy-count t
        evil-vsplit-window-right t
        evil-split-window-below t)
  ;; Slow this down from 0.02 to prevent blocking in large or folded buffers
  ;; like magit while incrementally highlighting matches.
  (setq-hook! '(magit-mode-hook)
    evil-ex-hl-update-delay 0.25)
  :general
  (general-mmap
    "|" 'sx-indent-whole-buffer
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    "H" 'evil-first-non-blank
    "L" 'evil-end-of-line)
  (general-nmap
    "C-M-j" (concat ":m +1" (kbd "RET") "==")
    "C-M-k" (concat ":m -2" (kbd "RET") "=="))
  (general-vmap
    ;; BUG: drag doesn't work in visual mode
    "C-M-j" (concat ":m '>+1" (kbd "RET") "gv=gv")
    "C-M-k" (concat ":m '<-2" (kbd "RET") "gv=gv"))
  :config
  (defun sx-indent-whole-buffer ()
    "Indent buffer."
    (interactive)
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max)))

  (defun +evil-default-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-normal-color)))
  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

  ;; Stop copying each visual state move to the clipboard.
  (advice-add #'evil-visual-update-x-selection :override #'ignore)
  (fset 'evil-redirect-digit-argument 'ignore)

  ;; Ensure `evil-shift-width' always matches `tab-width'; evil does not police
  ;; this itself, so we must
  (setq-hook! 'after-change-major-mode-hook evil-shift-width tab-width)

  (after! eldoc
    ;; Allow ElDoc to trigger directly after changing modes
    (eldoc-add-command 'evil-normal-state
                       'evil-insert
                       'evil-change
                       'evil-delete
                       'evil-replace))

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'magit-status-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'insert))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :general
  (general-mmap :modes 'evil-org-mode
    "RET" 'evil-org-return))

;; TODO: evil-exchange
;; (use-package evil-exchange
;;   :commands evil-exchange)

;; TODO: evil-quick-diff
;; (use-package evil-quick-diff
;;   :commands (evil-quick-diff evil-quick-diff-cancel))

;; TODO: evil-anzu
;; (use-package evil-anzu)

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general ([remap comment-line] #'evilnc-comment-or-uncomment-lines
            "M-;" 'comment-line))

(use-package evil-matchit
  :hook ((prog-mode text-mode eshell-mode) . evil-matchit-mode)
  :commands evilmi-jump-items
  :general
  (general-mmap :keymaps '(evil-matchit-mode-map)
    "<tab>" 'evilmi-jump-items))

(use-package evil-snipe
  :hook (on-first-input . evil-snipe-override-mode)
  :commands evil-snipe-local-mode evil-snipe-override-local-mode
  :init (setq evil-snipe-smart-case t
              evil-snipe-scope 'line
              evil-snipe-repeat-scope 'visible
              evil-snipe-char-fold t))

(use-package evil-surround
  :hook (on-first-input . global-evil-surround-mode)
  :general
  (general-omap :keymaps 'evil-surround-mode-map
    "z" 'evil-surround-edit
    "Z" 'evil-Surround-edit
    "s" nil)
  (general-vmap :keymaps 'evil-surround-mode-map
    "Z" 'evil-surround-region))

(use-package evil-lion
  :commands evil-lion-left evil-lion-right
  :general
  (general-nmap
    "ga" 'evil-lion-left
    "gA" 'evil-lion-right)
  (general-vmap
    "ga" 'evil-lion-left
    "gA" 'evil-lion-right))

(use-package avy
  :commands evil-avy-goto-char-timer
  :init (setq avy-timeout-seconds 0.25)
  :general
  (general-nmap "s" 'evil-avy-goto-char-timer))

(use-package smart-tab-over
  :hook (on-first-input . smart-tab-over-global-mode))

;; `buffer-hop' so C-M-o/C-M-i jumps between visited buffers
;; (use-package buffer-hop
;;   :hook (doom-first-input . global-buffer-hop-mode)
;;   :init (setq buffer-hop-ignored-buffers-patterns '("^\\*" "^magit" "^COMMIT_EDITMSG")
;;               buffer-hop-disabled-modes '(minibuffer-mode dired-mode vertico-mode))
;;   :general
;;   (general-nmap
;;     "C-M-i" 'buffer-hop-next
;;     "C-M-o" 'buffer-hop-prev))

;; `better-jumper' so C-i/C-o only jumps to locations inside a buffer.
;; (use-package better-jumper
;;   :hook (doom-first-input . better-jumper-mode)
;;   :commands doom-set-jump-a doom-set-jump-maybe-a doom-set-jump-h
;;   :init
;;   ;; Maintain jump lists for each individual buffer.
;;   (setq better-jumper-context 'buffer)
;;   (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
;;   (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
;;   (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
;;   (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
;;   (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)
;;   :config
;;   (defun doom-set-jump-a (fn &rest args)
;;     "Set a jump point and ensure fn doesn't set any new jump points."
;;     (better-jumper-set-jump (if (markerp (car args)) (car args)))
;;     (let ((evil--jumps-jumping t)
;;           (better-jumper--jumping t))
;;       (apply fn args)))

;;   (defun doom-set-jump-maybe-a (fn &rest args)
;;     "Set a jump point if fn actually moves the point."
;;     (let ((origin (point-marker))
;;           (result
;;            (let* ((evil--jumps-jumping t)
;;                   (better-jumper--jumping t))
;;              (apply fn args)))
;;           (dest (point-marker)))
;;       (unless (equal origin dest)
;;         (with-current-buffer (marker-buffer origin)
;;           (better-jumper-set-jump
;;            (if (markerp (car args))
;;                (car args)
;;              origin))))
;;       (set-marker origin nil)
;;       (set-marker dest nil)
;;       result))

;;   (defun doom-set-jump-h ()
;;     "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
;;     (better-jumper-set-jump)
;;     nil)

;;   ;; Creates a jump point before killing a buffer. This allows you to undo
;;   ;; killing a buffer easily (only works with file buffers though; it's not
;;   ;; possible to resurrect special buffers).
;;   ;;
;;   ;; I'm not advising `kill-buffer' because I only want this to affect
;;   ;; interactively killed buffers.
;;   (advice-add #'kill-current-buffer :around #'doom-set-jump-a)

;;   ;; Create a jump point before jumping with imenu.
;;   (advice-add #'imenu :around #'doom-set-jump-a))

;;; Theme

(setq sx-themes-list nil)
(setq sx-themes-index 1)

(defun sx-cycle-theme ()
  "Change the theme to the next index in the `qk-themes-list'. I would normally
use this for switching between light and dark modes."
  (interactive)
  (setq sx-themes-index (% (1+ sx-themes-index) (length sx-themes-list)))
  (sx-load-indexed-theme))

(defun sx-load-theme (theme)
  "Load theme without slip-through, disable and enable the theme completely. I
believe this should be modified in the next version of Emacs; keeping this for now."
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme :no-confirm)))))

(defun sx-load-indexed-theme ()
  "Load the active indexed theme in the `sx-themes-list'"
  (sx-load-theme (nth sx-themes-index sx-themes-list)))

;; Emacs does not have an "after-load-theme-hook", which I find essential for
;; adding or changing some of the faces dynamically. We can advise the
;; `change-theme' function to define a hook.
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
;; (defadvice consult-theme (after run-after-load-theme-hook activate)
;;   "Run `after-load-theme-hook'."
;;   (run-hooks 'after-load-theme-hook))
(defadvice sx-load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(use-package doom-themes
  :hook (on-init-ui . (lambda () (load-theme 'my-doom-gruvbox t)))
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-gruvbox-dark-variant "hard")
  (add-to-list 'sx-themes-list 'my-doom-gruvbox)
  ;; (add-to-list 'sx-themes-list 'modus-operandi-tinted)
  (sx-load-indexed-theme)
  :config (doom-themes-org-config))

(use-package modus-themes
  ;;   :hook (on-init-ui . (lambda () (load-theme 'modus-vivendi t)))
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-to-toggle '(modus-operandi-tinted))
  (setq modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        ;; Control the style of command prompts (e.g. minibuffer, shell, IRC clients).
        ;; `modus-themes-prompts' are either nil (the default), or a list of
        ;; properties that may include any of those symbols: `italic',;; `WEIGHT'
        modus-themes-prompts '(bold))
  ;; Define some palette overrides using the presets
  (customize-set-variable 'modus-themes-common-palette-overrides
                          `(;; To hide the border around the active and inactive mode lines, we
                            ;; set their color to that of the underlying background
                            (bg-mode-line-active bg-inactive)
                            (fg-mode-line-active fg-main)
                            (bg-mode-line-inactive bg-inactive)
                            (fg-mode-line-active fg-dim)
                            (border-mode-line-active bg-inactive)
                            (border-mode-line-inactive bg-main)
                            ;; line-numbers
                            (fg-line-number-active fg-main)
                            (bg-line-number-inactive bg-main)
                            (fg-line-number-inactive fg-dim)
                            ;; links
                            (underline-link unspecified)
                            (underline-link-visited unspecified)
                            (underline-link-symbolic unspecified)
                            ;; To hide the border around the active and inactive mode lines, we
                            ;; set their color to that of the underlying background
                            (bg-mode-line-active bg-inactive)
                            (fg-mode-line-active fg-main)
                            (bg-mode-line-inactive bg-inactive)
                            (fg-mode-line-active fg-dim)
                            (border-mode-line-active bg-inactive)
                            (border-mode-line-inactive bg-main)
                            ;; Change the background of matching parenthesis to a shade of magenta
                            ;; (bg-paren-match bg-magenta-subtle)
                            ;; Enable underlining matching parenthesis by applying a color to them
                            (underline-paren-match fg-main)
                            ;; Make the fringe invisible
                            (fringe unspecified)))

  ;;   (add-to-list 'sx-themes-list 'modus-vivendi)
  (add-to-list 'sx-themes-list 'modus-operandi-tinted)
  ;;   (sx-load-indexed-theme))
  )

;; (use-package ef-themes)

;;; Defaults

(setq create-lockfiles nil
      make-backup-files nil
      version-control t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 5
      kept-old-versions 5
      auto-save-default t
      auto-save-include-big-deletions t
      auto-save-no-message t
      auto-save-interval 100
      inhibit-startup-message t
      scroll-margin 2
      next-line-add-newlines t
      enable-recursive-minibuffers t
      tab-always-indent 'complete
      select-enable-clipboard t
      select-enable-primary t)

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file')
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook! 'find-file-not-found-functions
  (defun doom-create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook! on-first-input
  (savehist-mode)
  (recentf-mode)
  (electric-pair-mode)
  (global-whitespace-mode)
  (global-auto-revert-mode))

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse (with `consult-yank-pop')
(setq kill-do-not-save-duplicates t)

(use-package no-littering
  :init
  (setq backup-directory-alist `(("." . ,(no-littering-expand-var-file-name "backups/")))
        auto-save-list-file-prefix (no-littering-expand-var-file-name "auto-saves/sessions/")
        auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-saves/") t)))
  (setq custom-file (no-littering-expand-var-file-name "custom.el")
        custom-theme-directory (no-littering-expand-var-file-name "themes/")))

(after! (no-littering recentf)
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory)))

(use-package recentf
  :config
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 200)
  (defun doom--recentf-file-truename-fn (file)
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
        (abbreviate-file-name (file-truename (tramp-file-name-localname file)))
      file))
  ;; Exclude anything in runtime folders
  (add-to-list 'recentf-exclude
               (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
                                             "/run"))))
  ;; Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
  ;; abbreviate $HOME -> ~ in filepaths (more portable, more reaadable, & saves
  ;; space)
  (add-to-list 'recentf-filename-handlers #'doom--recentf-file-truename-fn)
  ;; Text properties inflate the size of recentf's files, and there is no
  ;; purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  (add-hook! '(on-switch-window-hook write-file-functions)
    (defun doom--recentf-touch-buffer-h ()
      "Bump file in recent file list when it is switched or written to."
      (when buffer-file-name
        (recentf-add-file buffer-file-name))
      ;; Return nil for `write-file-functions'
      nil))

  (add-hook! 'dired-mode-hook
    (defun doom--recentf-add-dired-directory-h ()
      "Add Dired directories to recentf file list."
      (recentf-add-file default-directory)))

  ;; The most sensible time to clean up your recent files list is when you quit
  ;; Emacs (unless this is a long-running daemon session).
  (setq recent-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup)

  ;; Otherwise `load-file' calls in `recentf-load-list' pollute *Messages*
  ;; (advice-add #'recentf-load-list :around #'doom-shut-up-a)
  )

(use-package savehist
  :init
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables '(kill-ring
                                        register-alist
                                        mark-ring global-mark-ring
                                        search-ring regexp-search-ring)))

(use-package saveplace
  :hook (doom-first-input . save-place-mode))

;; (use-package display-line-numbers
;;   :hook ((prog-mode org-mode) . display-line-numbers-mode))

(use-package whitespace
  :hook
  ((prog-mode text-mode conf-mode) . (lambda () (setq show-trailing-whitespace t)))
  :init
  (setq-default whitespace-display-mappings '((tab-mark ?\t [?\u21E5 ?\t])
                                              (newline-mark ?\n [?\u21A9 ?\n])
                                              (space-mark ?\  [?\u00B7] [?.]))
                ;; whitespace-style '(face trailing newline newline-mark tabs tab-mark)
                whitespace-style '(face empty trailing tabs tab-mark)
                whitespace-action '(cleanup auto-cleanup)
                whitespace-global-modes '(not shell-mode magit-mode magit-diff-mode
                                          ibuffer-mode dired-mode occur-mode erc-mode)))

;;; Fonts

(defun sx/set-font-faces ()
  (set-face-attribute 'default nil :family "JetBrains Mono Nerd Font" :weight 'regular :height 160)
  (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono Nerd Font" :weight 'regular :height 1.0)
  (set-face-attribute 'variable-pitch nil :family "JetBrains Mono Nerd Font" :height 160))

(if (daemonp)
    ;; (add-hook 'after-make-frame-functions
    ;;           (lambda (frame)
    ;;             ;; (setq doom-modeline-icon t)
    ;;             (with-selected-frame frame
    ;;               (sx/set-font-faces))))
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                ;; (setq doom-modeline-icon t)
                (sx/set-font-faces)))
  (add-hook 'on-init-ui-hook 'sx/set-font-faces))

;;; Help

;; TODO: transient
;; (use-package transient)
;; TODO: devdocs https://github.com/astoff/devdocs.el
;; (use-package devdocs)
;; TODO: repeat-help https://github.com/karthink/repeat-help
;; (use-package repeat-help)

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
  :init
  ;; Make `apropos' et co search more extensively. Might be slower but more useful.
  (setq apropos-do-all t)
  (setq helpful-max-buffers 1))

;; I want help windows to be selected automatically so I can close it with `q'
;; after I've finished reading
(use-package help
  :general
  (global-definer "h" '(help-command :which-key "help"))
  (general-def :keymaps 'help-map
    "C-h" 'embark-prefix-help-command
    "A" 'describe-face
    "M" 'describe-keymap)
  :init
  (setq help-window-select t))

(use-package eldoc
  :general
  (general-nmap "K" 'eldoc-doc-buffer)
  :init
  ;; `eldoc-documentation-compose' and `eldoc-documentation-compose-eagerly'
  ;; help display information from multiple Eldoc sources at the same time.
  ;; The eager option displays results as they come in; the other collates all
  ;; the answers and displays them when they're all ready.
  ;; I like the eager option.
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  ;; Eldoc resizes the echo area display which is intrusive. Let's not do that.
  (setq eldoc-echo-area-use-multiline-p nil)
  ;; Skip showing documentation in the echo area and use an `eldoc-doc-buffer'
  ;; window if it is already displayed.
  (setq eldoc-echo-area-prefer-doc-buffer t)
  ;; `eldoc-doc-buffer' opens a dedicated buffer for your documentation.
  ;; This displays what ever is at point. The only annoying thing about it
  ;; is that it prefers to pop open in an existing window.
  ;; Let's tweak its display to force the buffer to appear at the bottom
  ;; of the frame instead with a fixed window height of 4 lines.
  ;; I use `popper' to manage my `eldoc' buffers.
  (add-to-list 'display-buffer-alist
               ;; The buffer name changes depending on the context,
               ;; and this display rule reflects that.
               '("^\\*eldoc" display-buffer-at-bottom
                 (window-height . 4)))
  ;; If you add a function to `eldoc-documentation-functions' (with add-hook)
  ;; then Eldoc will query the functions in the order they're in and source
  ;; documentation from them. Eglot and Flymake support this out of the box,
  ;; but as it's fairly new other packages do not.
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

;; Extra coloring for Emacs' Info-mode
(use-package info-colors
  :commands info-colors-fontify-node
  :hook (Info-selection . info-colors-fontify-node))

;;; Dired

;; TODO: dired-atool https://github.com/HKey/dired-atool
;; (use-package dired-atool)
;; TODO: dired-rsync https://github.com/stsquad/dired-rsync
;; (use-package dired-rsync)
;; TODO: dired-gitignore https://github.com/mueller/dired-gitignore.el
;; (use-package dired-gitignore)
;; TODO: sudo-edit https://github.com/nflath/sudo-edit
;; (use-package sudo-edit)

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :general
  (global-definer "e" '(dired-jump :which-key "File explorer"))
  :init (setq dired-listing-switches "-lAXhv --group-directories-first"
              dired-use-ls-dired nil
              dired-kill-when-opening-new-dired-buffer t
              dired-hide-details-hide-symlink-targets nil
              dired-dwim-target t
              ;; don't prompt to revert, just do it
              dired-auto-revert-buffer #'dired-buffer-stale-p
              auto-revert-remote-files nil
              ;; Always copy/delete recursively
              dired-recursive-copies 'always
              dired-recursive-deletes 'top
              ;; Ask whether destination dirs should get created when copying/removing files
              dired-create-destination-dirs 'ask
              delete-by-moving-to-trash t))

(after! dired
  (defvar prot-dired--limit-hist '()
    "Minibuffer history for `prot-dired-limit-regexp'.")

  (defun prot-dired-limit-regexp (regexp omit)
    "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
    (interactive
     (list
      (read-regexp
       (concat "Files "
               (when current-prefix-arg
                 (propertize "NOT " 'face 'warning))
               "matching PATTERN: ")
       nil 'prot-dired--limit-hist)
      current-prefix-arg))
    (dired-mark-files-regexp regexp)
    (unless omit (dired-toggle-marks))
    (dired-do-kill-lines)
    (add-to-history 'prot-dired--limit-hist regexp))

  (general-nmap
    :keymaps '(dired-mode-map)
    "/" 'prot-dired-limit-regexp))

(after! dired
  (general-nmap
    :keymaps '(dired-mode-map)
    "l" 'dired-find-file
    "h" 'dired-up-directory))

(use-package dired-single :after dired)

(after! dired
  (defun dired-home-directory ()
    (interactive)
    (dired-single-buffer (expand-file-name "~/")))

  (add-hook! dired-mode (general-nmap
                          :keymaps '(dired-mode-map)
                          "C-<return>" 'dired-find-file-other-window
                          "a" 'dired-create-empty-file
                          "~" 'dired-home-directory)))

(use-package all-the-icons-dired
  :init (setq all-the-icons-dired-monochrome nil)
  :hook (dired-mode . all-the-icons-dired-mode))

;; TODO: I have a feeling find-dired doesn't work because
;; I need to wrap it with inheritenv-add-advice
(use-package find-dired
  :general
  (major-mode-definer
    :major-modes '(dired-mode)
    "f" 'find-lisp-find-dired)
  :init (setq find-ls-option '("-exec ls -ldh {} +" . "-ldh")))

(use-package dired-open
  :after dired
  :config
  (add-to-list 'dired-open-functions #'dired-open-xdg t))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))

(after! dired-hide-dotfiles
  (add-hook! dired-mode (general-nmap
                          :keymaps '(dired-mode-map)
                          "." 'dired-hide-dotfiles-mode)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-preview
  :general (general-nmap :keymaps '(dired-mode-map)
             "gp" 'dired-preview-global-mode))

(use-package dired-subtree
  :general (general-nmap :keymaps '(dired-mode-map)
             [tab] 'dired-subtree-toggle
             [C-tab] 'dired-subtree-cycle)
  :init (setq dired-subtree-line-prefix "  ├"))

;;; Window

(global-definer "w" '(evil-window-map :which-key "window"))
(general-def :keymaps 'evil-window-map "C-h" 'embark-prefix-help-command)

(use-package popper
  :hook
  (doom-first-buffer . popper-mode)
  (doom-first-buffer . popper-echo-mode)
  :general
  (general-nmap
    "C-'" 'popper-toggle
    "M-'" 'popper-cycle
    "C-M-'" 'popper-toggle-type)
  (general-iemap
    "C-'" 'popper-toggle
    "M-'" 'popper-cycle
    "C-M-'" 'popper-toggle-type)
  :init
  (setq popper-window-height 0.33)
  (setq popper-echo-lines 1)
  (setq popper-reference-buffers '("\\*Messages\\*"
                                   "\\*Warnings\\*"
                                   "Output\\*$"
                                   "\\*Async Shell Command\\*"
                                   "\\*Shell Command Output\\*"
                                   compilation-mode
                                   jupyter-repl-mode
                                   "^\\*eshell.*\\*$" "^\\*.*-eshell\\*$" eshell-mode
                                   "^\\*shell.*\\*$" "^\\*.*-shell\\*$" shell-mode
                                   "^\\*term.*\\*$" term-mode
                                   "^\\*eat.*\\*$" eat-mode
                                   help-mode helpful-mode
                                   "^\\*eldoc.*\\*$"))
  (defvar popper-echo--propertized-names nil
    "Alist of popup buffer names and their shortened, propertized
display names.")

  (defun popper-message-shorten (full-name)
    (let ((name (file-name-nondirectory full-name)))
      (or (alist-get name popper-echo--propertized-names nil nil #'string=)
          (let ((short-name
                 (cond
                  ((string= "*Messages*" name)
                   (concat (propertize "LOG " 'face 'default)
                           (propertize name 'face 'popper-echo-area-buried)))
                  ((string-match "^\\*[hH]elpful.*?: \\(.*\\)\\*$" name)
                   (concat (propertize "HLP " 'face '(:inherit link :underline nil))
                           (propertize (match-string 1 name) 'face 'popper-echo-area-buried)))
                  ((string-match "^\\*Help:?\\(.*\\)\\*$" name)
                   (concat (propertize "HLP" 'face
                                       '(:inherit link :underline nil))
                           (propertize (match-string 1 name)
                                       'face 'popper-echo-area-buried)))
                  ((string-match "^\\*\\(e?\\)shell:? ?\\(.*\\)\\*$" name)
                   (concat (if (string-empty-p (match-string 1 name))
                               (propertize "SH" 'face 'success)
                             (propertize "ESH" 'face 'success))
                           (unless (string-empty-p (match-string 2 name)) " ")
                           (propertize (match-string 2 name)
                                       'face 'popper-echo-area-buried)))
                  ((string-match "^\\*\\(.*?\\)-\\(e?\\)shell\\*$" name)
                   (concat (if (string-empty-p (match-string 2 name))
                               (propertize "SH" 'face 'success)
                             (propertize "ESH" 'face 'success))
                           (unless (string-empty-p (match-string 1 name)) " ")
                           (propertize (match-string 1 name)
                                       'face 'popper-echo-area-buried)))
                  ((string-match "^[*]?\\(.*?\\) *\\(?:[Oo]utput\\|Command\\)\\*$" name)
                   (concat (propertize "OUT "
                                       'face '(:inherit warning))
                           (propertize (match-string 1 name)
                                       'face 'popper-echo-area-buried)))
                  ((string-match "^\\*\\(.*?\\)[ -][Ll]og\\*$" name)
                   (concat (propertize "LOG "
                                       ;; '(:inherit link-visited :underline nil)
                                       'face 'default)
                           (propertize (match-string 1 name)
                                       'face 'popper-echo-area-buried)))
                  ((or (string-match "^\\*[Cc]ompil\\(?:e\\|ation\\)\\(.*\\)\\*$" name)
                       (string-match
                        "^\\*\\(.*?\\)[ -]?\\(?:byte\\)?[ -]?[Cc]ompil\\(?:e\\|ation\\)\\*$" name))
                   (concat (propertize "COM "
                                       'face '(:inherit link-visited :underline nil :weight normal))
                           (propertize (match-string 1 name) 'face 'popper-echo-area-buried)))
                  ((string-match "^[*]?\\(?:e?shell.*\\|v?term\\).*\\*$" name)
                   (concat (propertize "RPL " 'face 'success) name))
                  (t (propertize name 'face 'popper-echo-area-buried)))))
            (cdar (push (cons name short-name) popper-echo--propertized-names))))))

  (setq popper-echo-transform-function #'popper-message-shorten)

  (after! doom-modeline
    (setq popper-mode-line
          '(:eval (let ((face (if (doom-modeline--active)
                                  'doom-modeline-emphasis
                                'doom-modeline)))
                    (if (and (doom-modeline-icon-displayable-p)
                             (bound-and-true-p doom-modeline-icon)
                             (bound-and-true-p doom-modeline-mode))
                        (format " %s " (all-the-icons-octicon "pin" :face face :v-adjust 0.05))
                      (propertize " POP " 'face face))))))
  ;; (setq popper-group-function #'popper-group-by-project)
  ;; Group popups by tab-bar
  ;; (setq popper-group-function (lambda ()
  ;;                            (let ((tabs (funcall tab-bar-tabs-function)))
  ;;                              (alist-get 'name (nth (tab-bar--current-tab-index tabs)
  ;;                                                    tabs)))))
  )

(use-package ace-window
  :init (setq aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f ?g)
              aw-background nil
              aw-scope 'frame)
  :general ("M-o" 'ace-window))

;;; Completion

;; TODO: tempel
;; (use-package tempel)
;; TODO: eglot-tempel
;; (use-package eglot-tempel)
;; TODO: tempel-collection
;; (use-package tempel-collection)
;; or
;; (use-package yasnippet)
;; (use-package yasnippet-capf)
;; (use-package yasnippet-snippets)
;; (use-package consult-yasnippet)

;; NOTE: (on-init-ui . vertico-mode) adds extra garbage collection
(use-package vertico
  :hook (on-init-ui . vertico-mode)
  :init (setq vertico-count 10
              vertico-cycle t
              vertico-resize 'grow-only)
  :general
  (:keymaps
   '(minibuffer-local-map
     minibuffer-local-ns-map
     minibuffer-local-completion-map
     minibuffer-local-must-match-map
     minibuffer-local-isearch-map)
   [escape] 'keyboard-escape-quit)
  ( :keymaps 'vertico-map
             "M-RET" 'vertico-exit-input
             "M-n" 'vertico-next-group
             "M-p" 'vertico-previous-group)
  ( :keymaps 'minibuffer-local-map
             "C-j" 'vertico-next
             "C-k" 'vertico-previous
             ;; Scroll through the items with C-n/C-p or C-j/C-k instead
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

(use-package vertico-multiform
  :after vertico
  :hook (vertico-mode . vertico-multiform-mode)
  :init
  (setq vertico-multiform-categories
        '((consult-grep buffer)
          (imenu buffer)
          (buffer)
          ;; (file buffer)
          ;; (project-file buffer)
          (info-menu buffer)
          (consult-org-heading buffer)
          (consult-history buffer)
          (consult-eglot-symbols buffer)
          (consult-xref buffer)
          (embark-keybinding buffer); not convinced
          (consult-locate buffer)))
  (setq vertico-multiform-commands
        '((magit:--author flat)
          (consult-notes buffer)
          (Info-goto-node buffer)
          (Info-lookup-symbol buffer)
          (Info-follow-reference buffer)
          (consult-yank-pop buffer))))

(use-package all-the-icons-completion
  :hook
  (doom-first-buffer . all-the-icons-completion-mode)
  (marginalia-mode-hook . all-the-icons-completion-marginalia-setup))

(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        ;; basic is required for /ssh: completion to work, but keep the same
        ;; values for project-file too.
        completion-category-overrides '((project-file (styles . (partial-completion basic orderless)))
                                        (file (styles . (partial-completion basic orderless)))))
  (setq orderless-component-separator 'orderless-escapable-split-on-space))

(use-package marginalia
  :hook (doom-first-buffer . marginalia-mode)
  :init (setq marginalia-max-relative-age 0))

(use-package corfu
  :hook (doom-first-buffer . global-corfu-mode)
  :general ( :keymaps 'corfu-map
                      "C-n" 'corfu-next
                      "C-p" 'corfu-previous
                      "C-g" 'corfu-quit
                      ;; `TAB'-only completion.
                      "TAB" 'corfu-insert
                      ;; Unbind `RET' completely
                      "RET" nil
                      ;; TODO: change corfu-insert-separater keymap's M-SPC being bound on OS level
                      "M-SPC" 'corfu-insert-separator)
  :init
  (setq corfu-auto t
        corfu-auto-delay 0.25
        corfu-auto-prefix 1
        corfu-echo-delay 0.25
        corfu-cycle t
        ;; corfu-scroll-margin (/ corfu-count 2)
        corfu-scroll-margin 5
        corfu-count 10
        corfu-quit-no-match t
        corfu-quit-at-boundary t
        ;; corfu-max-width 100
        ;; corfu-min-width 42
        ;; Increase right-margin-width otherwise gets cut off because of kind-icon
        corfu-right-margin-width 1.0)
  (setq corfu-popupinfo-delay 0.25)
  :config
  (defvar sx-corfu-minibuffer-exclude-modes (list read-passwd-map)
    "Minibuffer-local keymaps for which Corfu should be disabled.")
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (or (bound-and-true-p vertico--input)
                (memq (current-local-map)
                      sx-corfu-minibuffer-exclude-modes))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point
                             (list (current-local-map)))
      (corfu-mode t)))
  (add-hook! minibuffer-setup 'corfu-enable-in-minibuffer)

  (defun sx-corfu-shell-settings ()
    (setq-local corfu-quit-no-match t
                corfu-auto nil)
    (setq-local corfu-map (copy-keymap corfu-map)
                completion-cycle-threshold nil)
    (define-key corfu-map "\r" #'corfu-insert-and-send)
    (corfu-mode))
  (defun corfu-insert-and-send ()
    (interactive)
    ;; 1. First insert the completed candidate
    (corfu-insert)
    ;; 2. Send the entire prompt input to the shell
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((derived-mode-p 'comint-mode)
      (comint-send-input))))
  (add-hook! (shell-mode eshell-mode) 'sx-corfu-shell-settings)

  ;; In evil-insert-state, pressing ESC will always invoke evil-normal-state even when
  ;; Corfu's UI is displaying, thus causing the key to bring me back to normal state
  ;; without discarding Corfu's popup completion list.
  ;; Use emulation-mode-map-alists to close Corfu first and then invoke evil-normal-state
  (defvar my-override-keymap-alist '())
  (add-to-ordered-list 'emulation-mode-map-alists 'my-override-keymap-alist 0)
  (add-hook
   'my-override-keymap-alist
   `(completion-in-region-mode . ,(define-keymap "<escape>" (lambda ()
                                                              (interactive)
                                                              (corfu-quit)
                                                              (unless (minibuffer-window-active-p (selected-window))
                                                                (evil-normal-state)))))))

(use-package corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode)
  :general (:keymaps 'corfu-popupinfo-map
            "M-p" 'corfu-popupinfo-scroll-down
            "M-n" 'corfu-popupinfo-scroll-up
            :keymaps 'corfu-map
            "C-h" 'corfu-popupinfo-toggle)
  :custom-face
  (corfu-popupinfo ((t :height 1.0)))
  :init
  (setq corfu-popupinfo-delay 0.25))

(use-package corfu-history
  :hook (corfu-mode . corfu-history-mode)
  :init (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package kind-icon
  :commands kind-icon-margin-formatter
  :init (setq kind-icon-default-face 'corfu-default)
  :config
  ;; The icons appear too big in the Corfu completion childframe, you can
  ;; adjust height like this
  (plist-put kind-icon-default-style :height 0.6))

(after! corfu
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :hook
  (jupyter-repl-interaction-mode . sx-update-jupyter-eglot-capfs)
  ((text-mode prog-mode) . sx-update-cape-capfs)
  :config
  (defun sx-update-cape-capfs ()
    "Add general Cape backends to `completion-at-point-functions'"
    (dolist (backend '(cape-file
                       ;; cape-dabbrev
                       ))
      (add-to-list 'completion-at-point-functions backend t)))
  (defun sx-update-jupyter-eglot-capfs ()
    ;; But the capf for `eglot-completion-at-point'
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    ;; Jupyter's completion-at-point function overshadows Eglot's.
    ;; We use a Cape super capf to have them at the same time.
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'jupyter-completion-at-point
                       #'python-completion-at-point)))))

;; (use-package tempel)
;; (use-package eglot-tempel)
;; (use-package tempel-collection)
;; or
;; (use-package yasnippet)
;; (use-package yasnippet-capf)
;; (use-package yasnippet-snippets)
;; (use-package consult-yasnippet)

;;; Consult

(use-package consult
  :general
  (:keymaps 'minibuffer-local-map
   "C-r" 'consult-history
   :keymaps 'consult-narrow-help
   "?" 'consult-narrow-help)
  (global-definer
    "b" 'consult-buffer
    "i" 'consult-imenu
    "k" 'consult-bookmark
    "l" 'consult-line
    "y" 'consult-yank-pop
    "/" 'consult-grep))

;; Use Consult to select xref locations with preview
(after! (consult)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; (use-package consult-dir)

;;; Embark

;; TODO: embark evil actions
(use-package embark
  :general
  (general-nmap
    "C-." 'embark-act
    "M-." 'embark-dwim)
  ("C-." 'embark-act
   "M-." 'embark-dwim)
  ([remap describe-bindings] 'embark-bindings)
  :init
  (setq embark-indicators '(embark-minimal-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the modeline of Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

;;; UI

(use-package hl-todo
  :hook (on-first-buffer . global-hl-todo-mode)
  :init (setq hl-todo-require-punctuation t
              hl-todo-highlight-punctuation ":"))

(use-package consult-todo
  :general (global-definer
             "T" 'consult-todo))

(use-package rainbow-mode)

;; (use-package highlight-thing
;;   :hook (prog-mode . highlight-thing-mode)
;;   ;; Restrict the highlighting to lines surrounding points, e.g. to reduce the
;;   ;; load of highlighting in large buffers.
;;   :init (setq highlight-thing-limit-to-region-in-large-buffers-p nil
;;               highlight-thing-narrow-region-lines 15
;;               highlight-thing-large-buffer-limit 5000))

(use-package pulsar
  :hook
  (doom-first-input . pulsar-global-mode))

(after! (pulsar doom-themes)
  (custom-set-faces
   `(pulsar-generic ((t (:background ,(doom-color 'base4)))))
   `(pulsar-red ((t (:background ,(doom-darken (doom-color 'red) 0.50)))))
   `(pulsar-blue ((t (:background ,(doom-darken (doom-color 'blue) 0.50)))))
   `(pulsar-cyan ((t (:background ,(doom-darken (doom-color 'cyan) 0.50)))))
   `(pulsar-green ((t (:background ,(doom-darken (doom-color 'green) 0.50)))))
   `(pulsar-magenta ((t (:background ,(doom-darken (doom-color 'violet) 0.50)))))
   `(pulsar-yellow ((t (:background ,(doom-darken (doom-color 'yellow) 0.50)))))))

(after! (pulsar consult)
  (setq consult-after-jump-hook nil) ;reset to avoid conflicts
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry))

(after! (pulsar evil)
  (defvar highlight-functions '(evil-goto-line
                                evil-goto-first-line
                                evil-scroll-down
                                evil-scroll-up
                                evil-window-down
                                evil-window-up
                                evil-window-left
                                evil-window-right
                                evil-window-next))

  (dolist (func highlight-functions)
    (add-to-list 'pulsar-pulse-functions func)))

;; Feels too slow
(after! (pulsar evil avy)
  (add-to-list 'pulsar-pulse-functions 'evil-avy-goto-char-timer))

(after! (pulsar ace-window)
  (add-to-list 'pulsar-pulse-functions 'ace-window))

(after! (ace-window)
  (custom-set-faces
   '(aw-leading-char-face ((t (:inherit avy-lead-face))))))

;; TODO: breadcrumb
;; (use-package breadcrumb)
;; TODO: indent-bars
;; (use-package indent-bars)
;; TODO: ligature
;; (use-package ligature)
;; TODO: ts-fold
;; (use-package ts-fold)
;; TODO: spacious-padding
;; (use-package spacious-padding)
;; TODO: lin
;; (use-package lin)
;; TODO: nano-vertico
;; (use-package nano-vertico)

;; Implement `centered-cursor-mode' with standard configuration settings.
;; Scrolling not as smooth, but more performant and doesn't suffer from the
;; annoying readjustments when opening minibuffer and which-key
(setq scroll-preserve-screen-position t
      scroll-conservatively 0
      maximum-scroll-margin 0.5
      scroll-margin 99999)

(use-package all-the-icons
  :commands all-the-icons-install-fonts)

;; NOTE: (after-init . doom-modeline-mode) adds extra garbage collection
(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode)
         (doom-modeline-mode . setup-custom-doom-modeline))
  :init
  ;; Show column number in modeline
  (column-number-mode)

  (setq doom-modeline-height 30
        doom-modeline-lsp nil
        doom-modeline-lsp-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-percent-position nil
        ;; doom-modeline-position-line-format nil
        doom-modeline-buffer-modification-icon nil
        doom-modeline-highlight-modified-buffer-name nil
        doom-modeline-irc-buffers t
        ;; doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-time-icon nil
        doom-modeline-mode-alist nil)
  ;; (setq-default mode-line-buffer-identification "%b")
  :config
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my-modeline 'default))
  (doom-modeline-def-modeline
    'my-modeline
    '(eldoc matches buffer-info remote-host)
    '(compilation irc debug repl input-method buffer-position major-mode process vcs pdf-pages)))

(use-package diff-hl
  :hook
  (doom-first-buffer . global-diff-hl-mode)
  (doom-first-buffer . diff-hl-flydiff-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (setq-default left-fringe-width 5))

;;; Org

(setq org-return-follows-link t)
(setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                             (vm-imap . vm-visit-imap-folder-other-frame)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file)
                             (wl . wl-other-frame)))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

;; TODO: book-mode
;; (use-package book-mode)
;; TODO: polymode
;; (use-package polymode)
;; TODO: ox-ipynb
;; (use-package ox-ipynb)
;; TODO: openwith-mode
;; (use-package openwith-mode)
;; TODO: org-transclusion
;; (use-package org-transclusion)
;; TODO: olivetti
;; (use-package olivetti)

;;; Org-Agenda

;; TODO: org-agenda
;; (use-package org-agenda)
;; (setq org-agenda-span 1
;;       org-agenda-start-day "+0d"
;;       org-agenda-skip-timestamp-if-done t
;;       org-agenda-skip-deadline-if-done t
;;       org-agenda-skip-scheduled-if-done t
;;       org-agenda-skip-scheduled-if-deadline-is-shown t
;;       org-agenda-skip-timestamp-if-deadline-is-shown t
;;       org-agenda-current-time-string ""
;;       org-agenda-time-grid '((daily) () "" "")
;;       org-agenda-prefix-format '((agenda . "  %?-2i %t ")
;;                                  (todo . " %i %-12:c")
;;                                  (tags . " %i %-12:c")
;;                                  (search . " %i %-12:c"))
;;       ;; requires you to set categories in your org-agenda file
;;       org-agenda-category-icon-alist
;;       `(("Teaching.p" ,(list (all-the-icons-faicon "graduation-cap" :height 0.8)) nil nil :ascent center)
;;         ("Family.s" ,(list (all-the-icons-faicon "home" :v-adjust 0.005)) nil nil :ascent center)
;;         ("Producer.p" ,(list (all-the-icons-faicon "youtube-play" :height 0.9)) nil nil :ascent center)
;;         ("Bard.p" ,(list (all-the-icons-faicon "music" :height 0.9)) nil nil :ascent center)
;;         ("Stories.s" ,(list (all-the-icons-faicon "book" :height 0.9)) nil nil :ascent center)
;;         ("Author.p" ,(list (all-the-icons-faicon "pencil" :height 0.9)) nil nil :ascent center)
;;         ("Gamedev.s" ,(list (all-the-icons-faicon "gamepad" :height 0.9)) nil nil :ascent center)
;;         ("Knowledge.p" ,(list (all-the-icons-faicon "database" :height 0.8)) nil nil :ascent center)
;;         ("Personal.p" ,(list (all-the-icons-material "person" :height 0.9)) nil nil :ascent center)))
;; (org-agenda-earlier)
;; (org-agenda-later)

;; TODO: org-super-agenda
;; (use-package org-super-agenda)
;; LibrePhoenix 'Making Org Agenda Look Beautiful'
;; (setq org-super-agenda-groups
;;       ;; Each group has an implicit boolean OR operator between its selectors.
;;       '(;; Optionally specify section name
;;         (:name " Overdue " :scheduled past :order 2 :face 'error)
;;         (:name "Personal " :and(:file-path "Personal.p" :not (:tag "event")) :order 3)
;;         (:name "Family " :and(:file-path "Family.s" :not (:tag "event")) :order 3)
;;         (:name "Teaching " :and(:file-path "Teaching.p" :not (:tag "event")) :order 3)
;;         (:name "Gamedev " :and(:file-path "Gamedev.s" :not (:tag "event")) :order 3)
;;         (:name "Youtube " :and(:file-path "Producer.p" :not (:tag "event")) :order 3)
;;         (:name "Music " :and(:file-path "Bard.p" :not (:tag "event")) :order 3)
;;         (:name "Storywriting " :and(:file-path "Stories.s" :not (:tag "event")) :order 3)
;;         (:name "Writing " :and(:file-path "Author.p" :not (:tag "event")) :order 3)
;;         (:name "Learning " :and(:file-path "Knowledge.p" :not (:tag "event")) :order 3)
;;         ;; Optionally specify section name
;;         (:name " Today " :time-grid t :date today :scheduled today :order 1 :face 'warning)))

;; TODO: org-timeblock
;; (use-package org-timeblock)
;; TODO: org-hyperscheduler
;; (use-package org-hyperscheduler)

;;; Denote

(use-package denote
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
  :init
  (setq denote-directory (expand-file-name "~/OneDrive/notes/")
        denote-known-keywords '("emacs" "denote" "testing")))

(use-package consult-notes
  :hook (doom-first-buffer . consult-notes-denote-mode)
  :init
  ;; Search only for text files in Denote dir
  (setq consult-notes-denote-files-function (function denote-directory-text-only-files))
  :general
  (+general-global-notes
    "f" 'consult-notes
    "/" 'consult-notes-search-in-all-notes))

(use-package denote-explore
  :general
  (+general-global-notes
    "S" 'denote-explore-single-keywords
    "Z" 'denote-explore-zero-keywords
    "R" 'denote-explore-rename-keyword
    "s" 'denote-explore-sort-keywords))

;;; Citar

(use-package citar
  ;; so my `sx-biblio-bibtex-lookup' function loads in citar
  :commands citar--bibliography-files
  :general (+general-global-notes
             "p" 'citar-open-files
             "o" 'citar-open)
  :init
  (setq citar-bibliography '("~/OneDrive/docs/lib.bib"))
  (setq citar-library-paths '("~/OneDrive/docs/books/"))
  (setq citar-templates
        '((main . "${title:55} ${author editor:55} ${date year issued:4}")
          (suffix . "  ${tags keywords keywords:40}")
          (preview . "${author editor} ${title}, ${journal publisher container-title collection-title booktitle} ${volume} (${year issued date}).\n")
          (note . "#+title: Notes on ${author editor}, ${title}"))))

(after! all-the-icons
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))
  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01)
     :function #'citar-has-links :padding "  " :tag "has:links"))

  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3)
     :function #'citar-has-notes :padding "  " :tag "has:notes"))

  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon "circle-o" :face 'all-the-icon-green) :function #'citar-is-cited
     :padding "  " :tag "is:cited"))

  (setq citar-indicators
        (list citar-indicator-files-icons ; citar-indicator-links-icons
              citar-indicator-notes-icons ; citar-indicator-cited-icons
              )))

(use-package citar-embark
  :hook (doom-first-buffer . citar-embark-mode))

(use-package citar-denote
  :hook (doom-first-buffer . citar-denote-mode)
  :general
  (+general-global-notes
    "a" 'citar-denote-add-citekey
    "A" 'citar-denote-remove-citekey))

;;; PDF / EPUB

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (setq-default pdf-view-display-size 'fit-page)
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (blink-cursor-mode -1)))
  (pdf-tools-install :no-query))

(use-package nov
  :init
  (add-to-list 'auto-mode-alist `("\\.epub\\'" . nov-mode)))

;;; Git

(use-package magit
  :general
  (global-definer "g" '(magit-status :which-key "Magit"))
  (general-nmap :keymaps 'project-prefix-map
    "m" 'magit-project-status)
  :init
  (setq magit-diff-hide-trailing-cr-characters t
        magit-diff-refine-ignore-whitespace t
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; TODO: ediff
;; (use-package ediff)
;; TODO: forge
;; (use-package forge)
;; TODO: blamer
;; (use-package blamer)
;; TODO: consult-gh
;; (use-package consult-gh)

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
  (setq project-switch-commands #'project-find-file)

  (global-definer
    ;; "t" '(project-eshell :which-key "Eshell")
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
  (rune/project-remember-projects-under "~/nix-dotfiles/"))

;; (use-package consult-project-extra
;;   :commands consult-project-extra-find
;;   :general
;;   (global-definer "f" '(consult-project-extra-find :which-key "File picker")))

;; (after! (project)
;;   (setq project-switch-commands #'consult-project-extra-find))

;; TODO: projection
;; (use-package projection)
;; TODO: projection-multi
;; (use-package projection-multi)

;;; Workspaces

(use-package beframe
  :hook (doom-first-input . beframe-mode))

(after! (consult beframe)
  ;; Hide recent files list (still available with "f" prefix)
  (consult-customize consult--source-recent-file :hidden t)
  ;; Hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)

  ;; Set consult-beframe buffer list
  (defface beframe-buffer
    '((t :inherit font-lock-string-face))
    "Face for `consult' framed buffers.")

  (defun sx-beframe-buffer-names-sorted (&optional frame)
    "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
    (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

  (defvar beframe-consult-source
    `( :name "Frame-specific buffers (current frame)"
       :narrow ?F
       :category buffer
       :face beframe-buffer
       :history beframe-history
       :default t
       :items ,#'sx-beframe-buffer-names-sorted
       :action ,#'switch-to-buffer
       :state ,#'consult--buffer-state))

  (add-to-list 'consult-buffer-sources 'beframe-consult-source))

;; (use-package tab-bar)
;; (after! '(consult tab-bar)
;;   consult tab bar)

;;; Elisp

(require 'elisp-fontification)
(require 'elisp-indentation)

(use-package eros
  :hook (emacs-lisp-mode . eros-mode)
  :init
  (add-hook 'eros-inspect-hooks (lambda () (flymake-mode -1)))
  (add-hook 'eros-inspect-hooks (lambda () (general-nmap
                                             :keymaps 'local
                                             "q" 'kill-buffer-and-window)))
  :config (major-mode-definer emacs-lisp-mode-map
            "i" 'eros-inspect-last-result))

(add-hook! emacs-lisp-mode 'flymake-mode)

(after! general
  (general-nmap
    :keymaps 'emacs-lisp-mode-map
    :major-modes 'emacs-lisp-mode
    "K" 'helpful-at-point)
  (major-mode-definer emacs-lisp-mode-map
    "b" 'eval-buffer
    "d" 'eval-defun
    "e" 'eval-last-sexp
    "s" 'eval-expression
    "x" 'consult-flymake))

(use-package elisp-demos
  :init
  (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; Many major modes do no highlighting of number literals, so we do it for them.
(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;; Python

(after! general
  (major-mode-definer python-base-mode-map
    ;; "a" taken
    "c" 'jupyter-connect-repl
    ;; "f" taken
    "g" 'jupyter-repl-associate-buffer
    "j" 'jupyter-run-repl
    "k" 'jupyter-repl-restart-kernel
    "l" 'jupyter-server-list-kernels
    ;; "r" taken
    ;; "x" taken
    "'" 'jupyter-repl-pop-to-buffer)

  (minor-mode-definer
    :keymaps 'jupyter-repl-interaction-mode
    "b" 'jupyter-eval-buffer
    "d" 'jupyter-eval-defun
    "e" 'jupyter-eval-line-or-region))

;; Remove guess indent python message
(setq python-indent-guess-indent-offset-verbose nil)

(use-package flymake-ruff
  :after flymake
  :hook (eglot-managed-mode . flymake-ruff-load))

(use-package jupyter
  ;; `jupyter-completion-at-point' has a hard dependency on `company-doc-buffer'
  ;; to provide completion documentation popups
  :init
  (setq jupyter-repl-echo-eval-p t))

;;; Bibtex

(use-package bibtex
  :init
  (setq bibtex-dialect 'biblatex
        bibtex-user-optional-fields
        `(("keywords" "Keywords to describe the entry" "")
          ("file" "Link to a document file" ""))
        bibtex-align-at-equal-sign t))

(use-package biblio
  :general
  (+general-global-notes "L" 'sx-biblio-bibtex-lookup)
  :config
  ;; Integrating Biblio and Citar
  (defun sx-biblio-lookup ()
    "Combines biblio-lookup and biblio-doi-insert-bibtex."
    (interactive)
    (let* ((dbs (biblio--named-backends))
           (db-list (append dbs '(("DOI" . biblio-doi-backend))))
           (db-selected (biblio-completing-read-alist
                         "Database:"
                         db-list)))
      (if (eq db-selected 'biblio-doi-backend)
          (let ((doi (read-string "DOI: ")))
            (biblio-doi-insert-bibtex doi))
        (biblio-lookup db-selected))))

  (defun sx-biblio-bibtex-lookup ()
    "Select BibTeX file, perform a lookup with Biblio and insert entry."
    (interactive)
    (let ((bibfile (completing-read
                    "BibTeX file:"
                    (citar--bibliography-files))))
      (find-file bibfile)
      (goto-char (point-max))
      (sx-biblio-lookup)
      (save-buffer))))

;; `biblio-gscholar' isn't in (M)ELPA so we bring in dependencies manually
;; (use-package gscholar-bibtex) ; dependency for biblio-gscholar
;; (require 'biblio-gscholar)

;;; Lang

;; NOTE: make sure to install Aspell or Hunspell dictionaries.
;; Leaving jinx-mode on is really taxing on my CPU. I only activate it when I
;; really need it.
(use-package jinx
  ;; :hook ((text-mode prog-mode) . jinx-mode)
  :general
  ( "M-$" 'jinx-correct
    "C-M-$" 'jinx-languages))

(use-package treesit-auto
  :hook (doom-first-buffer . global-treesit-auto-mode)
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package rust-mode)

(use-package nix-mode)

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

(use-package expand-region
  :init
  (setq expand-region-show-usage-message nil)
  :general
  (general-mmap
    "M-e" 'er/expand-region
    "C-M-e" 'er/contract-region))

;; (use-package pet
;;   :hook (python-base-mode-hook . pet-mode))

;; TODO: code-cells
;; (use-package code-cells)
;; TODO: poetry
;; (use-package poetry)
;; TODO: puni
;; (use-package puni)
;; TODO: numpydoc
;; (use-package numpydoc)
;; TODO: docstring-mode
;; (use-package docstring-mode)
;; TODO: python-mls
;; (use-package python-mls)
;; TODO: vundo
;; (use-package vundo)
;; TODO: undo-fu-session
;; (use-package undo-fu-session)

;;; Compile

(use-package envrc
  :hook (doom-first-buffer . envrc-global-mode))

(use-package inheritenv
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
  (inheritenv-add-advice 'jupyter-run-repl)
  (inheritenv-add-advice 'flymake-ruff--run-checker))

(use-package compile
  :hook (compilation-filter . ansi-color-compilation-filter)
  :general (global-definer
             "," '(project-compile :which-key "Compile")
             "." '(recompile :which-key "Recompile"))
  :init (setq compilation-scroll-output t ;; Automatically scroll build output
              compilation-skip-threshold 2 ;; Skip anything less than error
              compilation-auto-jump-to-first-error t ;; Automatically jump to the first error unconditionally during compilation
              compilation-max-output-line-length nil ;; Don't hide any output lines
              ;; Kill compilation process before starting another
              compilation-always-kill t))

;; TODO: compile-multi
;; (use-package compile-multi)
;; TODO: consult-compile-multi
;; (use-package consult-compile-multi)
;; TODO: compile-multi-all-the-icons
;; (use-package compile-multi-all-the-icons)
;; TODO: compile-multi-embark
;; (use-package compile-multi-embark)

;;; Eglot

(use-package eglot
  :hook
  (doom-first-input . eglot-auto-ensure-all)
  (eglot-managed-mode . flymake-mode)
  (eglot-managed-mode . rune/eglot-eldoc-manage)
  :init
  (setq eglot-events-buffer-size 0 ;; disable eglot logging (improves performance)
        eglot-autoshutdown t ;; shutdown server when last managed buffer is killed
        eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-auto-display-help-buffer nil
        eglot-report-progress nil
        eglot-workspace-configuration '((:rust-analyzer
                                         :completion (:callable (:snippets "fill_arguments"))
                                         :checkOnSave (:command "clippy" :allTargets :json-false))))
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
                     (not (eq 'lisp-mode mode))    ; prefer sly/slime
                     (not (eq 'scheme-mode mode))  ; prefer geiser
                     )
            (let ((hook-name (format "%s-hook" (symbol-name mode))))
              (message "adding eglot to %s" hook-name)
              (add-hook (intern hook-name) #'eglot-ensure))))))))

  (defun eglot-auto-ensure-all ()
    "Add `eglot-ensure' to major modes that offer LSP support.

Major modes are only selected if the major mode's associated LSP
binary is detected on the system."
    (when (require 'eglot nil :noerror)
      (rune/add-eglot-hooks eglot-server-programs)))
  :config
  ;; Ask Eglot to stay away from completely taking over Flymake.
  ;; Just add it as another item.
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)

  (defadvice! +lsp--defer-server-shutdown-a (fn &optional server)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    :around #'eglot-managed-mode
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

  (add-to-list 'eglot-server-programs '(toml-ts-mode . ("taplo" "lsp" "stdio"))))

(after! eglot
  (minor-mode-definer
    ;; :keymaps refers to the minor mode, not the keymap
    :keymaps 'eglot--managed-mode
    "a" 'eglot-code-actions
    "r" 'eglot-rename
    "f" 'eglot-format-buffer
    "x" 'consult-flymake)

  (minor-mode-definer
    ;; :keymaps refers to the minor mode, not the keymap
    :keymaps 'flymake-mode
    "n" 'flymake-goto-next-error
    "p" 'flymake-goto-prev-error)

  (general-nmap
    :major-modes '(eglot--managed-mode)
    "gi" 'eglot-find-implementation
    "gt" 'eglot-find-typeDefinition
    "gD" 'eglot-find-declaration))

;; TODO: consult-eglot
;; (use-package consult-eglot)

;; (use-package eldoc-box
;;   :hook (eglot-managed-mode . sx-add-eglot-keys)
;;   :config
;;   (defun sx-add-eglot-keys ()
;;     "Add eglot bindings after a buffer has been managed."
;;     (general-nmap
;;       :keymaps 'eglot-mode-map
;;       "K" 'eldoc-box-eglot-help-at-point)))

;;; Xref

(after! xref
  (setq xref-auto-jump-to-first-definition 'move
        xref-auto-jump-to-first-xref 'move
        xref-prompt-for-identifier '(not
                                     xref-find-references
                                     xref-find-definitions-other-window
                                     xref-find-definitions-other-frame)))

;;; Flymake

(after! flymake
  (setq-default flymake-fringe-indicator-position 'right-fringe)

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

;; (use-package package-lint-flymake
;;   :hook (emacs-lisp-mode . package-lint-flymake-setup))

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
  ;; Replace black with ruff in Python
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff))

  (add-to-list 'apheleia-formatters
               '(eglot-managed . apheleia-indent-eglot-manager-buffer))
  (defvar apheleia-eglot-managed-modes '(julia-mode julia-ts-mode rust-mode rust-ts-mode))
  (dolist (mode apheleia-eglot-managed-modes)
    (add-to-list 'apheleia-mode-alist '(mode . eglot-managed))))

;;; Dape

;; TODO: dape
;; (use-package dape)

;;; Eshell

(use-package eshell
  :general
  (+general-global-eshell
    "e" 'eshell
    "h" 'eshell-here
    "t" 'project-eshell)
  (general-nmap :keymaps 'eshell-mode-map
    "C-r" #'consult-history)
  :init (setq eshell-history-size 4096
              eshell-hist-ignoredups t
              eshell-buffer-maximum-lines 10000
              eshell-command-aliases-list
              '(("q" "exit")
                ("c" "clear")
                ("ll" "eza --no-user --header --group-directories-first --all --binary")
                ("lla" "eza --no-user --header --group-directories-first --all --binary --long")
                ("llg" "eza --no-user --header --group-directories-first --all --binary --long --git-ignore")
                ("llt" "eza --no-user --header --group-directories-first --tree --level 2")
                ("e" "find-file")
                ("sw" "nh os switch --nom .")))
  :config
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

(use-package esh-mode
  :preface
  (defun shortened-path (path max-len)
    "Return a potentially trimmed-down version of the directory PATH.
Replacing parent directories with their initial characters to try to
get the character length of PATH (sans directory slashes) down to
MAX-LEN."
    (require 'cl-lib)
    (let* ((components (split-string (abbreviate-file-name path) "/"))
           (len (+ (1- (length components))
                   (cl-reduce '+ components :key 'length)))
           (str ""))
      (while (and (> len max-len) (cdr components))
        (setq str (concat str (if (= 0 (length (car components)))
                                  "/" (string (elt (car components) 0) ?/)))
              len (- len (1- (length (car components))))
              components (cdr components)))
      (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

  ;; Tramp info
  (defun sx/eshell-remote-p ()
    "If you are in a remote machine."
    (tramp-tramp-file-p default-directory))
  (defun sx/eshell-remote-user ()
    "Return remote user name."
    (tramp-file-name-user (tram-dissect-file-name default-directory)))
  (defun sx/eshell-remote-host ()
    "Return remote host."
    ;; `tramp-file-name-real-host' is removed and replaced by
    ;; `tramp-file-name-host' in Emacs 26.
    (tramp-file-name-host (tram-dissect-file-name default-directory)))

  (defun sx/eshell-status-formatter (timestamp duration)
    "Return the status display for `sx/eshell-status'.
TIMESTAMP is the value returned by `current-time' and DURATION is the floating
time the command took to complete in seconds."
    ;; (format "#[STATUS] End time %s, duration %.3fs\n"
    ;;         (format-time-string "%F %T" timestamp)
    ;;         duration)
    (format " 󰁫 %.1fs" duration))

  (defcustom sx/eshell-status-min-duration 1
    "If a command takes more time than this, display its status with `epe-status'."
    :group 'sx
    :type 'number)

  (defvar sx/eshell-status-last-command-time nil)
  (make-variable-buffer-local 'sx/eshell-status-last-command-time)

  (defun sx/eshell-status-record ()
    "Record the time of the current command."
    (setq sx/eshell-status-last-command-time (current-time)))

  (defun sx/eshell-status (&optional formatter min-duration)
    "Termination timestamp and duration of command.
Status is only returned if command duration was longer than MIN-DURATION \(defaults to `sx/eshell-status-min-duration').
FORMATTER is a function of two arguments, TIMESTAMP and DURATION, that returns a string."
    (if sx/eshell-status-last-command-time
        (let ((duration (time-to-seconds
                         (time-subtract (current-time) sx/eshell-status-last-command-time))))
          (setq sx/eshell-status-last-command-time nil)
          (if (> duration (or min-duration
                              sx/eshell-status-min-duration))
              (funcall (or formatter
                           #'sx/eshell-status-formatter)
                       (current-time)
                       duration)
            ""))
      (progn
        (add-hook 'eshell-pre-command-hook #'sx/eshell-status-record)
        "")))

  (defun sx/eshell-prompt ()
    (concat
     (when (and (package-installed-p 'tramp) (sx/eshell-remote-p))
       (propertize (concat (sx/eshell-remote-user) "@" (sx/eshell-remote-host) " ")
                   'face 'font-lock-comment-face))
     (when (package-installed-p 'envrc)
       (propertize (if (string= envrc--status 'none)
                       "" (concat "󱄅 " (replace-regexp-in-string "-env$" "" (getenv "name"))))
                   'face 'font-lock-comment-face))
     (propertize (concat " " (shortened-path (eshell/pwd) 40)) 'face 'eshell-prompt)
     (propertize (if (car (vc-git-branches))
                     (concat "  " (car (vc-git-branches)))
                   "")
                 'face 'diff-header)
     (propertize (concat (sx/eshell-status))
                 'face 'font-lock-comment-face)
     (propertize " \n 𝝺 " 'face (if (zerop eshell-last-command-status) 'success 'error))))

  :custom
  ;; (eshell-prompt-regexp "^.* ❯")
  (eshell-prompt-regexp "^.* 𝝺 ") ;; Match last output of prompt -> prevents ~read-only~
  (eshell-prompt-function #'sx/eshell-prompt)
  (eshell-scroll-show-maximum-output nil)
  (eshell-banner-message ""))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-global-mode))

(use-package eat
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)
  :init (setq eat-kill-buffer-on-exit t))

(use-package pcmpl-args
  :hook
  (eshell-mode . sx-pcmpl-args-eshell-settings)
  ((eshell-mode shell-mode) . sx-pcmpl-args-capf-ensure)
  :config
  (defun sx-pcmpl-args-prepare ()
    (let ((pfunc
           (thread-first
             "pcomplete/"
             (concat (car (pcomplete-parse-arguments)))
             (intern))))
      (unless (fboundp pfunc)
        (defalias pfunc 'pcmpl-args-pcomplete-on-man)))
    (list nil :exclusive 'no))

  (defun sx-pcmpl-args-capf-ensure ()
    (add-hook 'completion-at-point-functions
              'sx-pcmpl-args-prepare -90 t))

  (defun sx-pcmpl-args-eshell-settings ()
    (setq-local pcomplete-try-first-hook
                '(eshell-complete-host-reference
                  eshell-complete-history-reference
                  eshell-complete-user-reference
                  ;; eshell-complete-variable-assignment
                  eshell-complete-variable-reference
                  eshell-complete-lisp-symbols
                  t))))

;; TODO: load-bash-alias
;; (use-package load-bash-alias)
;; TODO: eshell-z with consult-dir
;; (after! eshell
;;   eshell-z (consult-dir))

;;; Tramp

(use-package tramp
  :commands tramp-tramp-file-p tramp-file-name-user tramp-file-name-host)

;;; Elfeed

;; TODO: elfeed
;; (use-package elfeed)
;; TODO: elfeed-tube
;; (use-package elfeed-tube)
;; TODO: elfeed-summary
;; (use-package elfeed-summary)
;; idk if this works
;; (after! (consult elfeed)
;;   (add-to-list 'consult-bookmark-narrow
;;             `(?e "Elfeed" ,#'elfeed-search-bookmark-handler)))

;;; Notmuch

;; TODO: notmuch
;; (use-package notmuch)
;; TODO: notmuch-bookmarks
;; (use-package notmuch-bookmarks)
;; idk if this works
;; (after! (consult notmuch)
;;   (add-to-list 'consult-bookmark-narrow
;;                 `((?n "Notmuch" ,#'notmuch-bookmarks-jump-handler))))

;;; Bookmarks

;; TODO: bookmark+
(use-package bookmark-plus
  ;; :straight '(bookmark-plus :type git :host github :repo "emacsmirror/bookmark-plus")
  :init (require 'bookmark+)
  :general
  ;; Add URL bookmark with leader-K
  (global-definer "K" (lambda ()
                        (interactive)
                        (let ((current-prefix-arg '(4)))
                          (call-interactively #'bmkp-url-target-set)))))

(after! (consult bookmark+)
  ;; Hide bookmark list (still available with "m" prefix)
  (consult-customize consult--source-bookmark :hidden t)
  (defun bookmark-url-handler (bm)
    "Handler for web bookmarks, opens bookmark BM with default browser."
    (browse-url (assoc-default 'filename (cdr bm))))
  (add-to-list 'consult-bookmark-narrow
               `(?u "Bmkp-Url-Browse" ,#'bmkp-jump-url-browse)))

;;; AI

;; TODO: gptel
;; (use-package gptel)

;;; Extra

;; TODO: verb
;; (use-package verb)
;; TODO: ement
;; (use-package ement)
;; TODO: emacs-everywhere
;; (use-package emacs-everywhere)
;; TODO: docker
;; (use-package docker)
;; TODO: prodigy
;; (use-package prodigy)
;; TODO: reverso
;; (use-package reverso)
;; TODO: pomm
;; (use-package pomm)
;; TODO: turbo-log
;; (use-package turbo-log)
;; TODO: macrursors
;; (use-package macrursors)
;; TODO: persistent-kmacro
;; (use-package persistent-kmacro)
;; TODO: daemons
;; (use-package daemons)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
