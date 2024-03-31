;;; Early-init

;; (use-package early-init
;;   :no-require
;;   :unless (featurep 'early-init)
;;   :config
;;   (load-file (locate-user-emacs-file "early-init.el")))

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
(require 'on)

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
;; (setq initial-major-mode 'fundamental-mode)

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

;;; Constants

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

  (general-mmap "C-s" 'save-buffer)

  (global-definer "a" '(find-file :which-key "File browser"))
  (global-definer "c" '(delete-window :which-key "Close window"))
  (global-definer "e" '(dired-jump :which-key "Dired"))
  (global-definer "f" '(project-find-file :which-key "File picker"))
  (global-definer "k" '(kill-current-buffer :which-key "Kill buffer"))
  (+general-global-menu! "notes" "n")
  (+general-global-menu! "org" "o")
  (+general-global-menu! "project" "p")
  (global-definer "q" '(kill-emacs :which-key "Quit Emacs"))
  (+general-global-menu! "eshell" "t")
  (+general-global-menu! "window" "w"
    "=" '(balance-windows :which-key "Balance windows")
    "s" '(split-window-vertically :which-key "Split window vertically")
    "v" '(split-window-horizontally :which-key "Split window horizontally")
    "m" '(delete-other-windows :which-key "Delete other windows")
    "c" '(delete-window :which-key "Close window")))

;;; Which-Key

(use-package which-key
  :hook (emacs-startup . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3))

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
    "gh" 'evil-first-non-blank
    "gl" 'evil-end-of-line)
  (general-nmap
    [escape] 'sx-nohl-and-quit
    "C-j" (concat ":m +1" (kbd "RET") "==")
    "C-k" (concat ":m -2" (kbd "RET") "=="))
  (general-vmap
    [escape] 'sx-nohl-and-quit
    ;; BUG: drag doesn't work in visual mode
    "C-j" (concat ":m '>+1" (kbd "RET") "gv=gv")
    "C-k" (concat ":m '<-2" (kbd "RET") "gv=gv"))
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

;; (use-package evil-collection
;;   :after evil
;;   :init
;;   (evil-collection-init))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :general
  (general-mmap :modes 'evil-org-mode
    "RET" 'evil-org-return))

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general ([remap comment-line] #'evilnc-comment-or-uncomment-lines
            "M-;" 'comment-line))

(use-package evil-matchit
  :hook (on-first-input . global-evil-matchit-mode)
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
    "g=" 'evil-lion-left
    "g+" 'evil-lion-right)
  (general-vmap
    "g=" 'evil-lion-left
    "g+" 'evil-lion-right))

;; (use-package evil-cleverparens
;;   :hook (emacs-lisp-mode . evil-cleverparens-mode)
;;   :init
;;   (setq
;;    ;; evil-cleverparens-complete-parens-in-yanked-region t
;;    evil-cleverparens-use-s-and-S nil)
;;   :config
;;   (setq evil-cleverparens-use-additional-movement-keys nil)
;;   :general
;;   (general-nmap :keymaps '(evil-cleverparens-mode-map)
;;     "M-o" nil
;;     "M-O" nil
;;     "H" 'evil-first-non-blank
;;     "L" 'evil-end-of-line
;;     ;; "L" 'evil-cp-forward-sexp
;;     ;; "H" 'evil-cp-backward-sexp
;;     ;; "M-l" 'evil-cp-end-of-defun
;;     ;; "M-h" 'evil-cp-beginning-of-defun
;;     ))

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

;; TODO: evil-lion https://github.com/edkolev/evil-lion
;; (use-package evil-lion) ;; g=
;; TODO: evil-multiedit
;; (use-package multiedit)
;; TODO: evil-space https://github.com/linktohack/evil-space
;; (use-package evil-space)

(use-package avy
  :commands evil-avy-goto-char-timer
  :init (setq avy-timeout-seconds 0.25)
  :general
  (general-nmap "s" 'evil-avy-goto-char-timer))

;; (use-package smart-tab-over
;;   :hook (doom-first-input . smart-tab-over-global-mode))

;;; Theme

;; sourcehut is down so nix build fails
;; (use-package modus-themes :demand)

(use-package doom-themes
  :hook (on-init-ui . (lambda () (load-theme 'doom-gruvbox t)))
  :init (setq doom-themes-enable-bold t
              doom-themes-enable-italic t
              doom-gruvbox-dark-variant "hard"))

(after! (doom-themes pulsar)
  (custom-set-faces
   `(pulsar-generic ((t (:background ,(doom-color 'base4)))))
   `(pulsar-red ((t (:background ,(doom-darken (doom-color 'red) 0.50)))))
   `(pulsar-blue ((t (:background ,(doom-darken (doom-color 'blue) 0.50)))))
   `(pulsar-cyan ((t (:background ,(doom-darken (doom-color 'cyan) 0.50)))))
   `(pulsar-green ((t (:background ,(doom-darken (doom-color 'green) 0.50)))))
   `(pulsar-magenta ((t (:background ,(doom-darken (doom-color 'violet) 0.50)))))
   `(pulsar-yellow ((t (:background ,(doom-darken (doom-color 'yellow) 0.50)))))))

(after! (doom-themes evil-goggles)
  (custom-set-faces
   '(evil-goggles-default-face ((t (:inherit pulsar-generic))))
   '(evil-goggles-delete-face ((t (:inherit pulsar-red))))
   '(evil-goggles-change-face ((t (:inherit pulsar-red))))
   '(evil-goggles-indent-face ((t (:inherit pulsar-cyan))))
   '(evil-goggles-yank-face ((t (:inherit pulsar-blue))))
   '(evil-goggles-join-face ((t (:inherit pulsar-cyan))))
   '(evil-goggles-fill-and-move-face ((t (:inherit pulsar-yellow))))
   '(evil-goggles-paste-face ((t (:inherit pulsar-green))))
   '(evil-goggles-shift-face ((t (:inherit pulsar-cyan))))
   '(evil-goggles-surround-face ((t (:inherit pulsar-magenta))))
   '(evil-goggles-commentary-face ((t (:inherit pulsar-magenta))))
   '(evil-goggles-nerd-commenter-face ((t (:inherit pulsar-magenta))))
   '(evil-goggles-replace-with-register-face ((t (:inherit pulsar-green))))
   '(evil-goggles-set-marker-face ((t (:inherit pulsar-yellow))))
   '(evil-goggles-undo-face ((t (:inherit pulsar-red))))
   '(evil-goggles-redo-face ((t (:inherit pulsar-green))))
   '(evil-goggles-record-macro-face ((t (:inherit pulsar-yellow))))))

(after! (doom-themes ace-window)
  (custom-set-faces
   '(aw-leading-char-face ((t (:inherit avy-lead-face))))))

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
  (global-whitespace-mode))

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
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

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
  :hook (on-first-input . save-place-mode))

(use-package display-line-numbers
  :hook ((prog-mode org-mode) . display-line-numbers-mode))

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

;;; Dired

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :general
  (global-definer "e" '(dired-jump :which-key "File explorer"))
  :init
  (setq
   dired-listing-switches "-lAXhv --group-directories-first"
   dired-use-ls-dired nil
   dired-kill-when-opening-new-dired-buffer t
   delete-by-moving-to-trash t
   dired-hide-details-hide-symlink-targets nil
   dired-dwim-target t))

(use-package dired-single :after dired)

(after! dired
  (general-nmap
    :keymaps '(dired-mode-map)
    "l" 'dired-find-file
    "h" 'dired-up-directory))

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

(after! (dired dired-hide-dotfiles)
  (add-hook! dired-mode (general-nmap
                          :keymaps '(dired-mode-map)
                          "." 'dired-hide-dotfiles-mode)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-preview
  :general
  (general-nmap
    :keymaps '(dired-mode-map)
    "gp" 'dired-preview-global-mode))

;; (use-package dired-narrow
;;   :after dired
;;   :init
;;   (setq dired-narrow-exit-when-1-left t)
;;   :general
;;   (general-nmap :keymaps '(dired-mode-map)
;;     "s" 'dired-narrow-regexp))

;; (use-package dired-git-info
;;   :init
;;   (setq dgi-auto-hide-details-p nil)
;;   :general
;;   (general-nmap :keymaps 'dired-mode-map
;;     ")" 'dired-git-info-mode))

;; (use-package dired-filter :after dired
;;   :general
;;   (general-nmap :keymaps 'dired-mode-map
;;     "*"  dired-filter-mark-map
;;     "g/" dired-filter-map))

;; (use-package dired-collapse
;;   :general
;;   (general-nmap :keymaps '(dired-mode-map)
;;     "gc" 'dired-collapse-mode))

;; TODO: dired-atool https://github.com/HKey/dired-atool
;; (use-package dired-atool)
;; TODO: dired-rsync https://github.com/stsquad/dired-rsync
;; (use-package dired-rsync)
;; TODO: dired-gitignore https://github.com/mueller/dired-gitignore.el
;; (use-package dired-gitignore)
;; TODO: sudo-edit https://github.com/nflath/sudo-edit
;; (use-package sudo-edit)

;;; Help

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
  ;; NOTE: I use `popper' to manage my `eldoc' buffers.
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

;; TODO: transient
;; (use-package transient)
;; TODO: devdocs https://github.com/astoff/devdocs.el
;; (use-package devdocs)
;; TODO: repeat-help https://github.com/karthink/repeat-help
;; (use-package repeat-help)

;;; Window

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
  ;; (setq popper-group-function #'popper-group-by-project)
  ;; Group popups by tab-bar
  ;; (setq popper-group-function (lambda ()
  ;;                            (let ((tabs (funcall tab-bar-tabs-function)))
  ;;                              (alist-get 'name (nth (tab-bar--current-tab-index tabs)
  ;;                                                    tabs)))))
  )

(use-package ace-window
  :init
  (setq
   aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f ?g)
   aw-background nil)
  :general ("M-o" 'ace-window))

;;; Completion

(use-package vertico
  :hook (on-init-ui . vertico-mode)
  :init (setq vertico-count 10
              vertico-cycle t
              vertico-resize 'grow-only)
  :general
  (general-nmap :keymaps 'vertico-map
    "M-RET" 'vertico-exit-input)
  (:keymaps 'minibuffer-local-map
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

(use-package all-the-icons-completion
  :hook
  (on-first-input . all-the-icons-completion-mode)
  (marginalia-mode-hook . all-the-icons-completion-marginalia-setup))

(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package marginalia
  :hook (on-first-input . marginalia-mode)
  :init
  (setq marginalia-max-relative-age 0) ; absolute time
  :general ( :keymaps 'minibuffer-local-map
                      "M-A" 'marginalia-cycle))

(use-package corfu
  :hook (on-first-input . global-corfu-mode)
  ;; (doom-first-input . corfu-echo-mode)
  :general ( :keymaps 'corfu-map
                      "C-n" 'corfu-next
                      "C-p" 'corfu-previous
                      "C-g" 'corfu-quit
                      ;; `TAB'-only completion.
                      "TAB" 'corfu-insert
                      ;; Unbind `RET' completely
                      "RET" 'nil
                      "ESC" (lambda () (corfu-quit) (evil-esc))
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
        corfu-right-margin-width 1.0
        ))

(use-package corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode)
  :general ( :keymaps 'corfu-popupinfo-map
                      "M-p" 'corfu-popupinfo-scroll-down
                      "M-n" 'corfu-popupinfo-scroll-up
                      :keymaps 'corfu-map
                      "C-h" 'corfu-popupinfo-toggle)
  :custom-face
  (corfu-popupinfo ((t :height 1.0)))
  :init
  (setq corfu-popupinfo-delay 0.25))

(use-package kind-icon
  ;; :commands kind-icon-margin-formatter
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
    "b" '(consult-buffer :which-key "Buffer switcher")
    "l" '(consult-line :which-key "Search buffer lines")
    "y" '(consult-yank-pop :which-key "Kill-ring")
    "/" '(consult-ripgrep :which-key "Ripgrep")))

;; Use Consult to select xref locations with preview
(after! (consult)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; (use-package consult-dir)

;;; Embark

(use-package embark
  :general
  (general-nmap
    "C-." 'embark-act
    "M-." 'embark-dwim)
  ("C-." 'embark-act
   "M-." 'embark-dwim)
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

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :init (setq wgrep-auto-save-buffer t
              wgrep-change-readonly-file t))

;;; Fonts

(add-hook 'on-init-ui-hook (lambda ()
                             (set-face-attribute 'default nil :family "JetBrains Mono Nerd Font" :weight 'regular :height 160)
                             (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono Nerd Font" :weight 'regular :height 1.0)
                             (set-face-attribute 'variable-pitch nil :family "JetBrains Mono Nerd Font" :height 160)))

;;; UI

(use-package all-the-icons
  :commands all-the-icons-install-fonts)

(use-package lambda-line
  :hook (doom-first-buffer . lambda-line-mode)
  :init
  (setq
   lambda-line-space-top 0.15
   lambda-line-space-bottom -0.15
   lambda-line-abbrev t
   lambda-line-hspace "  "
   lambda-line-git-diff-mode-line nil
   lambda-line-vc-symbol "#"
   lambda-line-visual-bell nil
   lambda-line-symbol-position -0.11
   lambda-line-gui-rw-symbol " "
   lambda-line-gui-mod-symbol " "
   lambda-line-gui-ro-symbol " "))

(after! lambda-line
  (customize-set-variable 'flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-modeline-note-counter ""))
  (customize-set-variable 'flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters))

  (custom-set-faces
   `(mode-line ((t (:foreground ,(doom-color 'fg) :background ,(doom-color 'my-black)
                    :box (:line-width 1 :color ,(doom-color 'base3) :style nil)))))
   `(mode-line-inactive ((t (:foreground ,(doom-color 'base6) :background ,(doom-color 'bg) :box (:line-width 1 :color ,(doom-color 'bg-alt2) :style nil)))))
   `(lambda-line-active ((t (:foreground ,(doom-lighten (doom-color 'fg) 0.25) :background unspecified
                             :box (:line-width 1 :color ,(doom-color 'bg-alt2) :style nil)))))
   `(lambda-line-inactive ((t (:foreground ,(doom-color 'base6) :background unspecified
                               :box (:line-width 1 :color ,(doom-darken (doom-color 'modeline-bg) 0.15) :style nil)))))
   `(lambda-line-active-name ((t (:inherit 'variable-pitch :foreground ,(doom-color 'fg)))))
   `(lambda-line-inactive-name ((t (:inherit 'variable-pitch :foreground ,(doom-color 'base6)))))
   `(lambda-line-active-primary ((t (:inherit 'variable-pitch :foreground ,(doom-color 'base6) :weight light))))
   `(lambda-line-inactive-primary ((t (:inherit 'variable-pitch :foreground ,(doom-color 'base6) :weight light))))
   `(lambda-line-active-secondary ((t (:inherit fixed-width :foreground ,(doom-color 'fg)))))
   `(lambda-line-inactive-secondary ((t (:inherit fixed-width :foreground ,(doom-color 'base6)))))
   `(lambda-line-active-tertiary ((t (:inherit fixed-width :foreground ,(doom-color 'fg)))))
   `(lambda-line-inactive-tertiary ((t (:inherit fixed-width))))
   `(lambda-line-active-status-RW ((t (:foreground ,(doom-color 'green)
                                       :inverse-video ,(if lambda-line-status-invert t nil)
                                       :box ,(if lambda-line-status-invert `(:line-width 1 :color ,(doom-color 'green)
                                                                             :style nil)
                                               `(:line-width 1 :color ,(doom-color 'bg-alt2) :style nil))))))
   `(lambda-line-inactive-status-RW ((t (:foreground ,(doom-color 'base6) :inverse-video ,(if lambda-line-status-invert t nil)
                                         :box ,(if lambda-line-status-invert `(:line-width 1 :color ,(doom-color 'bg-alt2)
                                                                               :style nil)
                                                 `(:line-width 1 :color ,(doom-color 'bg-alt2) :style nil))))))
   `(lambda-line-active-status-MD ((t (:foreground ,(doom-color 'red) :inverse-video ,(if lambda-line-status-invert t nil)
                                       :box ,(if lambda-line-status-invert `(:line-width 1 :color ,(doom-color red)
                                                                             :style nil)
                                               `(:line-width 1 :color ,(doom-color 'bg-alt2) :style nil))))))
   `(lambda-line-inactive-status-MD ((t (:foreground ,(doom-color 'base6)   :inverse-video ,(if lambda-line-status-invert t nil)
                                         :box ,(if lambda-line-status-invert `(:line-width 1 :color ,(doom-color 'bg-alt2)
                                                                               :style nil)
                                                 `(:line-width 1 :color ,(doom-color 'bg-alt2) :style nil))))))
   `(lambda-line-active-status-RO ((t (:foreground ,(doom-color 'yellow) :inverse-video ,(if lambda-line-status-invert t nil)
                                       :box ,(if lambda-line-status-invert `(:line-width 1 :color ,(doom-color 'yellow)
                                                                             :style nil)
                                               `(:line-width 1 :color ,(doom-color 'bg-alt2) :style nil))))))
   `(lambda-line-inactive-status-RO ((t (:foreground ,(doom-color 'base6) :inverse-video ,(if lambda-line-status-invert t nil)
                                         :box ,(if lambda-line-status-invert `(:line-width 1 :color ,(doom-color 'bg-alt2)
                                                                               :style nil)
                                                 `(:line-width 1 :color ,(doom-color 'bg-alt2) :style nil))))))
   `(lambda-line-visual-bell ((t (:background ,(doom-color 'modeline-bg)))))
   ))

(use-package evil-goggles
  :hook
  (doom-first-input . evil-goggles-mode)
  :init
  (setq evil-goggles-duration 0.125)
  :config
  (setq evil-goggles--commands
        (append evil-goggles--commands
                '((evil-commentary-line
                   :face evil-goggles-commentary-face :switch evil-goggles-enable-yank
                   :advice evil-goggles--generic-async-advice)
                  (evil-cp-delete
                   :face evil-goggles-delete-face :switch evil-goggles-enable-delete
                   :advice evil-goggles--generic-blocking-advice)
                  (evil-cp-delete-line
                   :face evil-goggles-delete-face :switch evil-goggles-enable-delete
                   :advice evil-goggles--delete-line-advice)
                  (evil-cp-yank
                   :face evil-goggles-yank-face :switch evil-goggles-enable-yank
                   :advice evil-goggles--generic-async-advice)
                  (evil-cp-yank-line
                   :face evil-goggles-yank-face :switch evil-goggles-enable-yank
                   :advice evil-goggles--generic-async-advice)
                  (evil-cp-change
                   :face evil-goggles-change-face :switch evil-goggles-enable-change
                   :advice evil-goggles--generic-blocking-advice)
                  (evil-cp-change-line
                   :face evil-goggles-change-face :switch evil-goggles-enable-change
                   :advice evil-goggles--generic-blocking-advice)))))

(use-package pulsar
  :hook
  (doom-first-input . pulsar-global-mode))

(after! (pulsar consult)
  (setq consult-after-jump-hook nil) ;reset to avoid conflicts
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry))

(after! (pulsar evil)
  (defvar highlight-functions '(sx-center-scroll-half-page-down
                                sx-center-scroll-half-page-up
                                evil-goto-line
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

;; (use-package centered-cursor-mode
;;   ;; BUG: `centered-cursor-mode' adjusts position and window too much when opening
;;   ;; minibuffer and which-key
;;   :hook (doom-first-input . global-centered-cursor-mode))
;; Implement `centered-cursor-mode' with standard configuration settings.
;; Scrolling not as smooth, but more performant and doesn't suffer from the
;; annoying readjustments when opening minibuffer and which-key
(setq scroll-preserve-screen-position t
      scroll-conservatively 0
      maximum-scroll-margin 0.5
      scroll-margin 99999)

;; (use-package hl-todo)
;; (use-package consult-todo)
;; (use-package breadcrumb)
;; (use-package indent-bars)
;; (use-package ligature)
;; (use-package ts-fold)
;; (use-package spacious-padding)
;; (use-package lin)
;; (use-package nano-vertico)

;;; Org

(setq org-return-follows-link t)
(setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                             (vm-imap . vm-visit-imap-folder-other-frame)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file)
                             (wl . wl-other-frame)))

;; (use-package org-modern)
;; (use-package book-mode)
;; (use-package polymode)
;; (use-package ox-ipynb)
;; (use-package openwith-mode)
;; (use-package org-transclusion)
;; (use-package olivetti)

;;; Org-Agenda

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

;; (use-package org-timeblock)
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
  (setq
   magit-diff-hide-trailing-cr-characters t
   magit-diff-refine-ignore-whitespace t
   magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package ediff)
;; (use-package forge)
;; (use-package consult-gh)
;; (use-package blamer)

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

;; (use-package projection)
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

(require 'elisp-cider-overlays)
(require 'elisp-fontification)
(require 'elisp-indentation)

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
    "d" 'jupter-eval-defun
    "e" 'jupyter-eval-line-or-region))

;; Remove guess indent python message
(setq python-indent-guess-indent-offset-verbose nil)

(use-package flymake-ruff
  :after flymake
  :hook (eglot-managed-mode . flymake-ruff-load))

(use-package jupyter
  ;; NOTE: `jupyter-completion-at-point' has a hard dependency on `company-doc-buffer'
  ;; to provide completion documentation popups
  :init
  (setq jupyter-repl-echo-eval-p t))

(use-package poetry
  :general
  (major-mode-definer python-base-mode-map
    "P" 'poetry
    "A" 'poetry-add-dep
    "R" 'poetry-remove-dep))

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
(use-package gscholar-bibtex) ; dependency for biblio-gscholar
(require 'biblio-gscholar)

;;; Lang

;; `dumb-jump' is great for languages without a language server.
(use-package dumb-jump
  :hook (emacs-lisp-mode . setup-dumb-jump-xref-backend)
  :init
  (setq dumb-jump-quiet t)
  (defun setup-dumb-jump-xref-backend ()
    (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t)))

;; ? (use-package smartparens)

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

;; (use-package code-cells)
;; (use-package poetry)
;; (use-package puni)
;; (use-package numpydoc)
;; (use-package docstring-mode)
;; (use-package python-mls)
;; (use-package jinx)
;; (use-package vundo)
;; (use-package undo-fu-session)

;;; Compile

(use-package envrc
  :hook (doom-first-buffer . envrc-global-mode))

;; Envrc sets environment variables in Emacs buffer-locally. This allows users
;; to have different buffer-local paths for executables in different projects.
;; However when Emacs libraries run background processes on behalf of a user,
;; they often run processes in temporary buffers that do not inherit the calling
;; buffer's environment. This results in executables not being found, or the
;; wrong versions of executables being picked up.
;; `inheritenv' provides the macro `inheritenv-add-advice' which wraps any
;; command with an advice function so it inherits buffer-local variables.
;; This is useful for when we discover problems we can't patch upstream.
(use-package inheritenv
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

;; (use-package compile-multi)
;; (use-package consult-compile-multi)
;; (use-package compile-multi-all-the-icons)
;; (use-package compile-multi-embark)

;;; Mail

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

;; (use-package consult-eglot)

;; (use-package eldoc-box
;;   :hook (eglot-managed-mode . sx-add-eglot-keys)
;;   :config
;;   (defun sx-add-eglot-keys ()
;;     "Add eglot bindings after a buffer has been managed."
;;     (general-nmap
;;       :keymaps 'eglot-mode-map
;;       "K" 'eldoc-box-eglot-help-at-point)))

;;; Flymake

(after! flymake
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

(use-package package-lint-flymake
  :hook (emacs-lisp-mode . package-lint-flymake-setup))

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

;; (use-package dape)

;;; Eshell

(use-package eshell
  :general
  (+general-global-eshell
    "e" 'eshell
    "h" 'eshell-here
    "t" 'project-eshell)
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

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-global-mode))

(use-package eat
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)
  :init (setq eat-kill-buffer-on-exit t))

;; (after! eshell
;;   eshell-z (consult-dir))
;; (use-package pcmpl-args)
;; (use-package load-bash-alias)

;;; AI

;; (use-package gptel)

;;; Elfeed

;; (use-package elfeed)
;; (use-package elfeed-tube)
;; (use-package elfeed-summary)

;;; Extra

;; (use-package verb)
;; (use-package ement)
;; (use-package emacs-everywhere)
;; (use-package docker)
;; (use-package prodigy)
;; (use-package reverso)
;; (use-package pomm)
;; (use-package turbo-log)
;; (use-package macrursors)
;; (use-package persistent-kmacro)
;; (use-package daemons)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     ;; (emacs-init-time "%.2f")
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; (require 'splash-screen)
