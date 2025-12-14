;;; init.el --- This is my init. -*- lexical-binding: t; -*-

;;; Commentary:

;;;; init is where my Emacs config starts.

;;; Code:

;;;;;;;;;;;;;;;;;;;;
;;;; early-init ;;;;

(use-package early-init
  :no-require
  :init
  ;; These are some general settings for frames and the basics of the
  ;; toolkit. In short, I want to keep things minimal. Notice the
  ;; `frame-resize-pixelwise' and `frame-inhibit-implied-resize': by default
  ;; Emacs will resize the frame if you adjust the font size, which I never want.
  (setopt frame-resize-pixelwise t
          frame-inhibit-implied-resize t
          frame-title-format '("%b")
          ring-bell-function 'ignore
          use-file-dialog nil
          inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name
          initial-buffer-choice t
          initial-major-mode 'lisp-interaction-mode)

  ;; The default setting for reporting native compilation errors is set to a
  ;; verbose value which is confusing: it produces warnings for compilation
  ;; issues that only the developer of the given package needs to deal
  ;; with. These include innocuous facts like docstrings being wider than a
  ;; certain character count. To make things even worse, the buffer that shows
  ;; these warnings uses the stop sign character, resulting in a long list of
  ;; lines with red spots everywhere, as if we have totally broken Emacs.
  (when (native-comp-available-p)
    (setq native-comp-async-report-warnings-errors nil)
    ;; HACK: see <https://emacs.stackexchange.com/a/82011>. Bug still exists in
    ;; Emacs 30.2. Remove this hack when fixed.
    (defun +native--compile-async-skip-p
        (native--compile-async-skip-p file load selector)
      (let* ((naive-elc-file (file-name-with-extension file "elc"))
             (elc-file       (replace-regexp-in-string
                              "\\.el\\.elc$" ".elc" naive-elc-file)))
        (or (gethash elc-file comp--no-native-compile)
            (funcall native--compile-async-skip-p file load selector))))
    (advice-add 'native--compile-async-skip-p
                :around '+native--compile-async-skip-p))

  ;; I like starting with a scratch buffer. I know that a lot of users specify a
  ;; dashboard or an Org agenda view, but I prefer to keep things generic in
  ;; this regard. Besides, I would rather not accidentally divulge any potential
  ;; sensitive informatifn if I am sharing my screen.
  ;;
  ;; I keep the `initial-major-mode' to its default `lisp-interaction-mode' even
  ;; though my configurations for `emacs-lisp-mode' make that mode surplus to
  ;; its requirements. Those who aren't familiar with `lisp-interaction-mode'
  ;; will quickly learn that they can type "C-j" (M-x `eval-print-last-sexp') to
  ;; evaluate and print the return value of the form before point. When
  ;; programming in Elisp, this comes in handy quite frequently.
  (setopt initial-buffer-choice t
          initial-major-mode 'lisp-interaction-mode
          initial-scratch-message
          (format ";; This is `%s'. Type `%s' to evaluate and print results.\n\n"
                  'lisp-interaction-mode
                  (propertize
                   (substitute-command-keys
                    "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")
                   'face 'help-key-binding)))

  ;; I do not use these graphical elements by default.
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;
;;;; use-package ;;;;

(use-package use-package
  :init
  ;; `use-package-enable-imenu-support' must be set before requiring
  ;; use-package
  (setopt use-package-enable-imenu-support t))

;; (use-package use-package-xdg
;;   ;; Many packages leave crumbs in `user-emacs-directory' or even
;;   ;; $HOME. `no-littering';; is a solution to this, but I want to adhere
;;   ;; to the XDG Base Directory Specification which `no-littering' has
;;   ;; eschewed.
;;   ;;
;;   ;; We implement here two custom use-package keywords, `:xdg-state' and
;;   ;; `xdg-cache'. They give each package a directory of their own, per
;;   ;; the XDG specification, in which to store state or cache data.
;;   ;;
;;   ;; For example:
;;   ;; (use-package bookmark
;;   ;;   :xdg-state
;;   ;;   (bookmark-default-file "bookmarks.eld"))
;;   ;; 1. On load of 'bookmark', create an $XDG_STATE_HOME/emacs/bookmark directory
;;   ;;    if it doesn't already exist.
;;   ;; 2. Set `bookmark-default-file' to $XDG_STATE_HOME/emacs/bookmark/bookmark.eld
;;   )

;;;;;;;;;;;;;;
;;;; lisp ;;;;

;; Additional load paths
(eval-when-compile
  ;; "plugins/" contains downloaded packages or plugins I've written.
  (add-to-list 'load-path (concat user-emacs-directory "plugins")))

(use-package common
  :no-require
  :config
  (defun +alist-set (key val alist-symbol)
    "Set KEY to VAL in the alist stored in ALIST-SYMBOL.
If KEY exists, update its value. If not, add a new entry."
    (let ((entry (assoc key (symbol-value alist-symbol))))
      (if entry
          (setf (cdr entry) val)
        (set alist-symbol (cons (cons key val) (symbol-value alist-symbol))))))

  (defun +common-empty-buffer-p ()
    "Test whether the buffer is empty."
    (or (= (point-min) (point-max))
        (save-excursion
          (goto-char (point-min))
          (while (and (looking-at "^\\([a-zA-Z]+: ?\\)?$")
                      (zerop (forward-line 1))))
          (eobp))))

  ;; The `+common-line-regexp-p' and `+common--line-regexp-alist'
  ;; are contributed by Gabriel: <https://github.com/gabriel376>.  They
  ;; provide a more elegant approach to using a macro, as shown further
  ;; below.
  (defvar +common--line-regexp-alist
    '((empty . "[\s\t]*$")
      (indent . "^[\s\t]+")
      (non-empty . "^.+$")
      (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
      (heading . "^[=-]+"))
    "Alist of regexp types used by `+common-line-regexp-p'.")

  (defun +common-line-regexp-p (type &optional n)
    "Test for TYPE on line.
TYPE is the car of a cons cell in
`+common--line-regexp-alist'.  It matches a regular
expression.

With optional N, search in the Nth line from point."
    (save-excursion
      (goto-char (line-beginning-position))
      (and (not (bobp))
           (or (beginning-of-line n) t)
           (save-match-data
             (looking-at
              (alist-get type +common--line-regexp-alist))))))

  (defvar +fundamental-mode-hook nil
    "Normal hook for `fundamental-mode' (which is missing by default).")

  (defvar +common-url-regexp
    (concat
     "~?\\<\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]*\\)"
     "[.@]"
     "\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]+\\)\\>/?")
    "Regular expression to match (most?) URLs or email addresses.")

  (defun +common-completion-table (category candidates)
    "Pass appropriate metadata CATEGORY to completion CANDIDATES.

This is intended for bespoke functions that need to pass
completion metadata that can then be parsed by other
tools (e.g. `embark')."
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (category . ,category))
        (complete-with-action action candidates string pred))))

  (defun +common-completion-table-no-sort (category candidates)
    "Pass appropriate metadata CATEGORY to completion CANDIDATES.
Like `+common-completion-table' but also disable sorting."
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (category . ,category)
            (display-sort-function . ,#'identity))
        (complete-with-action action candidates string pred))))

  (defun +fundamental-mode-run-hook (&rest args)
    "Apply ARGS and then run `+fundamental-mode-hook'."
    (apply args)
    (run-hooks '+fundamental-mode-hook))

  (advice-add #'fundamental-mode :around #'+fundamental-mode-run-hook))

;;;;;;;;;;;;;;;;;;;
;;;; bindings ;;;;;

(use-package prefix
  :no-require
  :config
  ;; Define a set of keymaps with commonly used commands and put them behind the
  ;; mode-specific-map (`C-c') or the ctl-x-map (`C-x'). The idea is to hit a
  ;; series of keys to get the desired command. Keymaps are organised
  ;; thematically and rely on strong mnemonics, such as `b' for buffers, `w' for
  ;; windows, and so on.
  (defvar-keymap +prefix-map
    :doc "Prefix keymap"
    :prefix '+prefix-map
    "h" help-map)
  (defvar-keymap +bib-prefix-map
    :doc "Prefix keymap for bibliography."
    :prefix '+bib-prefix-map)
  (defvar-keymap +dap-prefix-map
    :doc "Prefix keymap for debugging."
    :prefix '+dap-prefix-map)
  (defvar-keymap +goto-prefix-map
    :doc "Prefix keymap for goto."
    :prefix '+goto-prefix-map)
  (defvar-keymap +guix-prefix-map
    :doc "Prefix keymap for Guix commands."
    :prefix '+guix-prefix-map)
  (defvar-keymap +mail-prefix-map
    :doc "Prefix keymap for mail."
    :prefix '+mail-prefix-map)
  (defvar-keymap +narrow-prefix-map
    :doc "Prefix keymap for narrowing."
    :prefix '+narrow-prefix-map)
  (defvar-keymap +notes-prefix-map
    :doc "Prefix keymap for notes commands."
    :prefix '+notes-prefix-map)
  (defvar-keymap +project-prefix-map
    :doc "Prefix map for project."
    :prefix '+project-prefix-map)
  (defvar-keymap +registers-prefix-map
    :doc "Prefix map for registers."
    :prefix '+registers-prefix-map)
  (defvar-keymap +search-prefix-map
    :doc "Prefix map for search."
    :prefix '+search-prefix-map)
  (defvar-keymap +tab-prefix-map
    :doc "Prefix map for tabs."
    :prefix '+tab-prefix-map)
  (defvar-keymap +window-prefix-map
    :doc "Prefix map for windows."
    :prefix '+window-prefix-map)
  (defvar-keymap +toggle-prefix-map
    :doc "Prefix map for minor mode toggles."
    :prefix '+toggle-prefix-map
    "h" #'hl-line-mode
    ;; "l" #'logos-focus-map
    ;; "s" #'spacious-padding-mode
    ;; "r" #'rainbow-mode
    )

  (bind-keys :map global-map
             ("C-x" . +prefix-map)
             ("M-s" . +search-prefix-map)
             ("M-g" . +goto-prefix-map)
             :map +prefix-map
             ;; ("a" . org-agenda-custom) ; abbrev-keymap ("C-a" . ) ; edebug
             ;; ("b" . consult-buffer) ; switch-to-buffer ("C-b" . ibuffer)  ; list-buffers
             ;; ("c" . org-capture) ; ("C-c" . +kill-terminal-or-restart) ; save-buffers-kill-emacs
             ;; ("d" . dired) ("C-d" . consult-dir) ; list-directory
             ("e" . kmacro-end-and-call-macro) ; ("C-e" . eval-last-sexp)
             ;; ("f" . ) ; set-fill-column ("C-f" . find-file)
             ;; ("g" . +guix-prefix-map) ("C-g" . )
             ;; ("h" . mark-whole-buffer) ("C-h" . help-map)
             ;; ("i" . +org-capture-inbox) ; insert-file ("C-i" . indent-rigidly)
             ;; ("j" . ) ; ("C-j" . dired-jump)
             ;; ("k" . +kill-this-buffer) ("C-k" . kmacro-keymap)
             ("l" . +bib-prefix-map) ; count-lines-page ; ("C-l" . ) ; downcase-region ; "lib" mnemonic
             ("m" . +mail-prefix-map) ; ("C-m" . ) ; mule-keymap
             ("n" . +narrow-prefix-map) ; ("C-n" . +narrow-or-widen-dwim) ; set-goal-column
             ;; ("o" . other-window) ("C-o" . guix) ; delete-blank-lines ; "os" mnemonic
             ("p" . +project-prefix-map) ; ("C-p" . ) ; mark-page
             ("q" . kbd-macro-query) ("C-q" . read-only-mode)
             ("r" . +registers-prefix-map) ; ("C-r" . consult-recent-file) ; find-file-read-only
             ("s" . save-some-buffers) ("C-s" . save-buffer)
             ("t" . +tab-prefix-map) ("C-t" . transpose-lines)
             ("u" . undo) ; vundo? ("C-u" . ) ; upcase-region ; "undo" mnemonic?
             ;; ("v" . magit-status) ; vc-prefix-map ("C-v" . find-alternate-file)
             ("w" . +window-prefix-map) ("C-w" . write-file)
             ("x" . +toggle-prefix-map) ("C-x" . exchange-point-and-mark)
             ;; ("y" . dape-global-map) ; ("C-y" . ) ; "why" mnemonic
             ;; ("z" . vertico-repeat) ("C-z" . +switch-to-shell-buffer)
             ("(" . kmacro-start-macro) (")" . kmacro-end-macro)
             ("TAB" . indent-rigidly)))

;; NOTE document repeat
(use-package repeat
  :config
  (repeat-mode)
  (setopt repeat-exit-key "C-g"
          repeat-exit-timeout 10)
  ;; `repeat-mode' is great for many things, but `other-window' keeps
  ;; causing me trouble. I keep trying to switch to another window and
  ;; typing a word that begins with o or O. Let's disable repeat-mode
  ;; for other-window.
  (put 'other-window 'repeat-map nil)
  ;; `undo' doesn't need a repeat map either.
  (put 'undo 'repeat-map nil)
  ;; I always forget I'm in repeat-mode because I'm usually looking at point and
  ;; not at the echo area. This gives me a clue by updating the cursor color if
  ;; repeat-mode is active.
  (add-function :after repeat-echo-function
                (let ((default-cursor-color (face-background 'cursor)))
                  (lambda (map)
                    "Color the cursor while repeat-map is active"
                    (let ((cursor-indicator-color (face-foreground 'error))
                          (cursor-current-color (face-background 'cursor)))
                      (unless (equal cursor-current-color cursor-indicator-color)
                        (setq default-cursor-color cursor-current-color))
                      (set-cursor-color (if map
                                            cursor-indicator-color
                                          default-cursor-color)))))
                '((name . "colorful-cursor-while-repeating"))))

;;;;;;;;;;;;;;;;
;;;; themes ;;;;

(use-package modus-themes
  :init
  (load-theme 'modus-vivendi)
  :config
  (setopt modus-themes-common-palette-overrides
          `(;; Hide the border around the active and inactive mode lines.
            (border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)
            ;; Leave foreground color intact when region is highlighted.
            (fg-region unspecified)
            ;; With `modus-themes-preset-overrides-faint' the grays are toned
            ;; down, gray backgrounds are removed from some contexts, and almost
            ;; all accent colors are desaturated. It makes the themes less
            ;; attention-grabbing.
            ,@modus-themes-preset-overrides-faint))

  (setopt modus-operandi-palette-overrides
          `(;; (bg-mode-line-active bg-blue-intense)
            ;; (fg-mode-line-active fg-main)
            (cursor yellow-warmer)
            (bg-region bg-sage)))

  (setopt modus-vivendi-palette-overrides
          `((fg-main "#e4e4ef")
            (bg-main "#181818")
            ;; (bg-mode-line-active bg-lavender)
            ;; (fg-mode-line-active fg-main)
            (cursor yellow-warmer)
            (bg-region bg-lavender)))

  (set-fringe-bitmap-face 'right-arrow 'shadow)

  ;; BUG 2025-10-25 `modus-themes-with-colors' was changed in commit
  ;; https://github.com/protesilaos/modus-themes/commit/27558488efabc3cd164a6f7fee979de0bc9ed42b
  ;; and the following configuration is broken as of version 5.0.0-dev

  ;; We use the `enable-theme-functions' hook to ensure that these values are
  ;; updated after we switch themes. This special hook available in Emacs 29+
  ;; works with anything that uses the basic `enable-theme' function.
  (defun +modus-themes-customize-faces (&rest _)
    (modus-themes-with-colors
      (custom-set-faces
       ;; By default, the background of the `region' face extends from the end
       ;; of the line to the edge of the window. To limit it to the end of the
       ;; line, we need to override the face's `:extend' attribute.
       '(region ((t :extend nil)))
       ;; The `diff-hl' package defaults to drawing bitmaps for the indicators
       ;; they display (e.g. bitmap of a plus sign for added lines). I replace
       ;; these bitmaps with contiguous lines which look nicer, but require a
       ;; change to the foreground of the relevant faces to yield the desired
       ;; color combinations.
       `(diff-hl-insert ((,c :foreground ,bg-added-fringe :background ,fringe)))
       `(diff-hl-delete ((,c :foreground ,bg-removed-fringe :background ,fringe)))
       `(diff-hl-change ((,c :foreground ,bg-changed-fringe :background ,fringe)))
       ;; I don't need to be reminded by ignored items in version control.
       `(diff-hl-dired-ignored ((,c :foreground ,bg-main)))
       `(diff-hl-dired-unknown ((,c :foreground ,bg-main)))
       `(keycast-key ((,c :inherit bold :foreground ,fg-mode-line-active :background ,bg-mode-line-active)))
       `(keycast-command ((,c :inherit mode-line :foreground ,fg-mode-line-active :background ,bg-mode-line-active))))))
  (add-hook 'enable-theme-functions #'+modus-themes-customize-faces))

;;;;;;;;;;;;
;;;; ui ;;;;

(use-package fontaine
  :config
  ;; Define detailed font configurations and set them on command.
  (bind-keys :map +toggle-prefix-map
             ("f" . fontaine-set-preset))

  (setopt x-underline-at-descent-line nil
          text-scale-remap-header-line t
          fontaine-presets '((regular)
                             (small
                              :default-height 120)
                             (presentation
                              :default-height 260)
                             (t
                              :default-family "Aporetic Sans Mono"
                              ;; font height is 1/10pt.
                              :default-height 140
                              :fixed-pitch-family "Aporetic Sans Mono"
                              :variable-pitch-family "Aporetic Sans")))
  ;; Themes re-apply face definitions when they are loaded. This is necessary to
  ;; render the theme. For certain faces, such as `bold' and `italic', it means
  ;; that their font family may be reset (depending on the particularities of
  ;; the theme.)
  ;;
  ;; To avoid such a potential problem, we can arrange to restore the current
  ;; font preset which was applied by `fontaine-set-preset'. Fontaine provides
  ;; the command `fontaine-apply-current-preset'. It can be called interactively
  ;; after loading a theme or be assigned to a hook that is ran at the post
  ;; `load-theme' phase.
  ;;
  ;; `fontaine-mode' does this automatically, persisting the latest font preset
  ;; when closing/starting Emacs and while switching between themes.
  (fontaine-mode 1)

  ;; Set the last preset or fall back to desired style from `fontaine-presets'
  ;; (the `regular' in this case).
  (fontaine-set-preset 'regular))

;; (use-package show-font)

(use-package variable-pitch
  :no-require
  :config
  ;; The built-in `variable-pitch-mode' makes the current buffer use a
  ;; proportionately spaced font. I want to activate it in all buffers where I
  ;; normally focus on prose. Exceptions to these major modes that I do not
  ;; consider related to prose (and which in my opinion should not be derived
  ;; from text-mode): these are excluded in the function
  ;; `+enable-variable-pitch'.
  (defun +enable-variable-pitch ()
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode 'yaml-ts-mode
                            'toml-ts-mode 'conf-toml-mode)
      (variable-pitch-mode 1)))
  (add-hook 'text-mode-hook #'+enable-variable-pitch)
  (add-hook 'notmuch-show-mode-hook #'+enable-variable-pitch)
  (add-hook 'elfeed-show-mode-hook #'+enable-variable-pitch)

  (bind-keys :map +toggle-prefix-map
             ("v" . variable-pitch-mode)))

(use-package cursory
  :config
  ;; Cursory provides a thin wrapper around built-in variables that affect the
  ;; style of the Emacs cursor on graphical terminals. The intent is to allow the
  ;; user to define preset configurations such as 'block with slow blinking' or
  ;; 'bar with fast blinking' and set them on demand. The use-case for such
  ;; presets is to adapt to evolving interface requirements and concomitant levels
  ;; of expected comfort, such as in the difference between writing and reading.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'box-no-blink))
  (cursory-mode 1)
  (bind-keys :map +toggle-prefix-map
             ("c" . cursory-set-preset)))

(use-package pulsar
  :config
  ;; Temporarily highlight the current line after a given function is invoked.
  ;; The affected functions are defined in the user option
  ;; `pulsar-pulse-functions'. What Pulsar does is set up an advice so that
  ;; those functions run a hook after they are called. The pulse effect is added
  ;; there (`pulsar-after-function-hook').
  (setopt pulsar-face 'pulsar-green
          pulsar-region-face 'pulsar-yellow
          pulsar-region-change-face 'pulsar-yellow
          pulsar-highlight-face 'pulsar-magenta
          pulsar-iterations 15)

  (dolist (func '(beginning-of-buffer
                  end-of-buffer
                  beginning-of-defun
                  end-of-defun
                  backward-sexp
                  forward-sexp
                  forward-list
                  backward-list
                  backward-up-list
                  down-list
                  forward-sentence
                  backward-sentence))
    (add-to-list 'pulsar-pulse-functions func))
  ;; There are convenience functions/commands which pulse the line using a
  ;; specific color: `pulsar-pulse-line-green' is one of them.
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-blue)

  (add-hook 'next-error-hook #'pulsar-pulse-line-red)
  (add-hook 'next-error-hook #'pulsar-reveal-entry)

  ;; HACK `pulse-flag' is being incorrectly set to nil after updating to Emacs
  ;; 30.2. Remove this hack after upgrading to Pulsar 1.4.0.
  (defun pulsar--create-pulse (locus face)
    "Create a pulse spanning the LOCUS using FACE.
LOCUS is a cons cell with two buffer positions."
    (let ((pulse-delay pulsar-delay)
          (pulse-flag t)
          (pulse-iterations pulsar-iterations)
          (overlay (make-overlay (car locus) (cdr locus))))
      (overlay-put overlay 'pulse-delete t)
      (overlay-put overlay 'window (frame-selected-window))
      (pulse-momentary-highlight-overlay overlay face)))

  (pulsar-global-mode 1))

;; (use-package lin)

;; (use-package rainbow-mode)

;; Highlight numbers in source code
(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode)
  (add-hook 'conf-mode-hook #'highlight-numbers-mode)
  (setq highlight-numbers-generic-regexp (rx (and symbol-start (one-or-more digit)) (optional "." (* digit)) symbol-end))
  ;; Define the right format for numbers in `dts-mode'
  (puthash 'dts-mode
           (rx (and symbol-start (or (+ digit) (+ hex-digit) (and "0" (any "xX") (+ hex-digit))) symbol-end))
           highlight-numbers-modelist))

(use-package druid-modeline
  ;; I use a custom mode line that is close in spirit to the `nano-modeline'
  ;; package.
  :config
  (setopt druid-modeline-padding '(0.0 . 0.0))

  (druid-modeline-text-mode 1)
  (add-hook 'prog-mode-hook #'druid-modeline-prog-mode)
  (add-hook 'org-mode-hook #'druid-modeline-org-mode)
  (add-hook 'org-capture-mode-hook #'druid-modeline-org-capture-mode)
  (add-hook 'org-agenda-mode-hook #'druid-modeline-org-agenda-mode)
  (add-hook 'gptel-mode-hook #'druid-modeline-gptel-mode)
  (add-hook 'shell-mode-hook #'druid-modeline-shell-mode)
  (add-hook 'eshell-mode-hook #'druid-modeline-shell-mode)
  (add-hook 'term-mode-hook #'druid-modeline-shell-mode)
  (add-hook 'vterm-mode-hook #'druid-modeline-shell-mode)
  (add-hook 'mistty-mode-hook #'druid-modeline-shell-mode)
  (add-hook 'inferior-python-mode-hook #'druid-modeline-shell-mode)
  (add-hook 'inferior-emacs-lisp-mode-hook #'druid-modeline-shell-mode)
  (add-hook 'inferior-ess-mode-hook #'druid-modeline-shell-mode)
  (add-hook 'pdf-view-mode-hook #'druid-modeline-pdf-mode)
  (add-hook 'nov-mode-hook #'druid-modeline-nov-mode)
  (add-hook 'eww-mode-hook #'druid-modeline-eww-mode)
  (add-hook 'Info-mode-hook #'druid-modeline-info-mode)
  (add-hook 'elpher-mode-hook #'druid-modeline-elpher-mode))

(use-package keycast
  ;; This is a helpful package by Jonas Bernoulli that echoes the key presses
  ;; and corresponding commands on the mode line, tab bar, header line, or a
  ;; special buffer.
  ;;
  ;; I usually enable `keycast-mode-line-mode' (or rather my special
  ;; `druid-modeline-keycast-mode') when I do a presentation. It shows an
  ;; indicator on the focused mode line.
  :config
  (define-minor-mode druid-modeline-keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if druid-modeline-keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" keycast-mode-line)))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" keycast-mode-line) global-mode-string))))

  (setopt keycast-mode-line-format "%2s%k%c%R"
          keycast-mode-line-window-predicate 'mode-line-window-selected-p
          keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '( mouse-event-p mouse-movement-p mwheel-scroll handle-select-window
                    mouse-set-point mouse-drag-region))
    (add-to-list 'keycast-substitute-alist `(,event nil)))

  (defun store-action-key+cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys)
          keycast--this-command cmd)
    cmd)
  (defun store-action-key-no-cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys)
          keycast--this-command cmd))
  (defun keycast-capture-avy-dispatch (char)
    (if-let ((cmd (assoc char avy-dispatch-alist)))
        (setq keycast--this-command-keys (make-vector 1 char)
              keycast--this-command (cdr cmd))))

  ;; Make keycast capture keys used in Embark and Avy
  (advice-add 'embark-keymap-prompter :filter-return #'store-action-key+cmd)
  (advice-add 'avy-goto-char-timer :filter-return #'store-action-key+cmd)
  (advice-add 'avy-handler-default :before #'keycast-capture-avy-dispatch)

  (bind-keys :map +toggle-prefix-map
             ("k" . druid-modeline-keycast-mode)))

;; (use-package spacious-padding)

(use-package fill
  :no-require
  :config
  ;; `auto-fill-mode' automatically breaks long lines so that they wrap at the
  ;; `fill-column' length: it happens as you type. This way, a paragraph is not
  ;; a single long line, but several shorter lines with newline characters
  ;; between them. I find this much more pleasant to work with instead of having
  ;; to rely on `visual-line-mode' to visually wrap long lines. I want my text
  ;; to be readable even if I do not use Emacs (e.g. if I use cat or less on the
  ;; command-line). Some relevant programs strip away the newlines inside a
  ;; paragraph, but there are some that do not. For those I might rely upon
  ;; `virtual-line-mode' combined with `virtual-auto-fill-mode'.
  ;;
  ;; To manually fill a region of text, mark it and type `M-q'. Or do `M-q' to
  ;; operate on the current paragraph without marking it. Depending on the major
  ;; mode you are in, this key binding calls a different command. The generic
  ;; one is `fill-paragraph'. I use `M-Q' to "unfill" text.

  (defun +unfill-dwim (&optional beg end)
    "Unfill paragraph or, when active, the region.
Join all lines in region delimited by BEG and AND, if active, while
respecting any empty lines (so multiple paragraphs are not joined, just
unfilled). If no region is active, operate on the paragraph. The idea is
to produce the opposite effect of `fill-paragraph' and `fill-region'."
    (interactive "r")
    (let ((fill-column most-positive-fixnum))
      (if (use-region-p)
          (fill-region beg end)
        (fill-paragraph))))

  (add-hook 'text-mode-hook #'auto-fill-mode)
  (add-hook 'prog-mode-hook #'auto-fill-mode)

  ;; TODO display-fill-column-indicator only if a line is currently passing it
  ;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

  (define-minor-mode +auto-fill-or-visual-line-mode
    "Enable `visual-line-mode' and disable `auto-fill-mode' in the current
buffer."
    :global nil
    (if +auto-fill-or-visual-line-mode
        (progn
          (auto-fill-mode -1)
          (visual-line-mode 1))
      (auto-fill-mode 1)
      (visual-line-mode -1)))

  (setopt fill-column 80)

  (bind-keys :map global-map
             ;; TODO I don't like using shift in the binding for +unfill-dwim
             ;; NOTE 2025-06-26 I also never use this so do I really need it?
             ("M-Q" . +unfill-dwim)
             :map +prefix-map
             ("f" . set-fill-column)
             :map +toggle-prefix-map
             ("q" . +auto-fill-or-visual-line-mode)))

(use-package visual-fill-column
  :config
  ;; I normally do not use `visual-line-mode'. What it does is to break long
  ;; lines to span multiple lines without actually affecting the underlying
  ;; text. In other words, we still have one long line only its visualisation is
  ;; as a paragraph. Unfortunately the standard behavior does not respect
  ;; `fill-column' and wraps lines at the window edge. `visual-fill-column'
  ;; makes it so it wraps at `fill-column' (or `visual-fill-column-width').
  ;;
  ;; For the cases where I am fine with `visual-line-mode', I enable the mode by
  ;; adding it to these mode hooks.
  (dolist (mode '(help-mode-hook
                  helpful-mode-hook
                  Custom-mode-hook
                  epa-info-mode-hook))
    (add-hook mode #'visual-line-mode))

  (add-hook 'visual-line-mode-hook #'visual-fill-column-for-vline))

(use-package truncate-lines
  :no-require
  :config
  ;; This here is the opposite of what we saw above. Unlike `visual-line-mode'
  ;; where long lines are made to look like paragraphs, "truncation" means to
  ;; let the line cover its natural length and simply cut it off screen.

  ;; I redefine the `toggle-truncate-lines' function which performs line
  ;; truncation to not display a message about the fact. Why do we need this?
  ;; Check the output of `M-x calendar' in a tiny window and you will see the
  ;; reason. In short, it is better to have lines not show their full contents
  ;; than to have something that looks completely broken. Note that this
  ;; function may also be set up elsewhere and described in that context. Here I
  ;; only cover the basic parent modes.

  (defun toggle-truncate-lines (&optional arg)
    "Toggle truncating of long lines for the current buffer.
When truncating is off, long lines are folded.
With prefix argument ARG, truncate long lines if ARG is positive,
otherwise fold them.  Note that in side-by-side windows, this
command has no effect if `truncate-partial-width-windows' is
non-nil."
    (interactive "P")
    (setq truncate-lines
          (if (null arg)
              (not truncate-lines)
            (> (prefix-numeric-value arg) 0)))
    (force-mode-line-update)
    (unless truncate-lines
      (let ((buffer (current-buffer)))
        (walk-windows (lambda (window)
                        (if (eq buffer (window-buffer window))
                            (set-window-hscroll window 0)))
                      nil t))))

  (dolist (mode '(text-mode-hook
                  prog-mode-hook
                  dired-mode-hook
                  +fundamental-mode-hook
                  hexl-mode-hook
                  comint-mode-hook
                  compilation-mode-hook
                  grep-mode-hook
                  log-view-mode-hook))
    (add-hook mode #'toggle-truncate-lines)))

(use-package display-line-numbers
  :config
  ;; I do not like to see line numbers by default and seldom use
  ;; `display-line-numbers-mode'. They do not help me navigate a buffer, nor are
  ;; they relevant in most cases. I enable the mode only when I need to compare
  ;; buffers or get a sense of how far apart two relevant sections are in a
  ;; file.
  ;;
  ;; Use absolute numbers in narrowed buffers.
  (setq-default display-line-numbers-widen t)

  (bind-keys :map +toggle-prefix-map
             ("n" . global-display-line-numbers-mode)))

(use-package whitespace
  :config
  ;; Emacs has very comprehensive whitespace rendering capabilities. I do not
  ;; render newline and space characters (see my tab configuration) because they
  ;; are easy to infer in most cases, but also because `whitespace-mode'
  ;; highlights each whitespace with a face which can cripple performance in
  ;; larger files. Since I only render trailing whitespace, empty lines, and tab
  ;; characters to draw attention to fix these mistakes, this ends up not
  ;; mattering as much.
  (setopt whitespace-style '(face tabs tab-mark trailing empty))

  ;; `whitespace-mode' provides the actions feature which allows us to
  ;; automatically run a series of actions after a buffer is written. I'm
  ;; interested in the cleanup actions which perform different operations based
  ;; on the defined whitespace style. For my defined whitespace style, it will
  ;; remove all empty lines at beginning and/or end of the buffer (`empty'), and
  ;; all trailing tabs and spaces (`trailing'). Lookup `whitespace-cleanup' for
  ;; all the available cleanup operations.
  (setq-default whitespace-action '(auto-cleanup))

  ;; We can enable whitespace mode globally by calling
  ;; `global-whitespace-mode'. The downside of this is that whitespace will be
  ;; rendered inside of every Emacs buffer and this is not really necessary. For
  ;; example, I don't need whitespace to be rendered in shell, occur, or ibuffer
  ;; windows. Luckily there's an option to control which modes should enable
  ;; whitespace mode when `global-whitespace-mode' is enabled. And it's aptly
  ;; named `whitespace-global-modes'. This option takes a list of major mode
  ;; symbol names, that when matched, will enable `whitespace-mode'. We can also
  ;; negate the list, by prefixing it with `not', causing global whitespace mode
  ;; to be disabled for the listed major mode symbols.
  (setq-default whitespace-global-modes '(prog-mode text-mode))
  (global-whitespace-mode))

;;;;;;;;;;;;;;;;;;;;;
;;;; environment ;;;;

;; TODO document envrc
(use-package envrc
  ;; Buffer-local "direnv" integration for Emacs
  :config
  ;; By default, when I call `project-compile', `project-shell-command', or
  ;; `project-async-shell-command' and select another project that has a .envrc
  ;; file, the environment does not get applied.  This happens because there is
  ;; no buffer in the destination ("other") project in which `envrc-mode' could
  ;; be activated before the process is started.  See the discussion in
  ;; <https://github.com/purcell/envrc/issues/59> for why the following advice
  ;; is necessary to work around this:
  (defun +envrc-ensure-current-project (fn &rest args)
    (let ((default-directory (project-root (project-current t))))
      (with-temp-buffer
        (envrc-mode 1)
        (apply fn args))))
  (advice-add 'project-compile :around #'+envrc-ensure-current-project)
  (advice-add 'project-shell-command :around #'+envrc-ensure-current-project)
  (advice-add 'project-async-shell-command
              :around #'+envrc-ensure-current-project)

  (setopt envrc-show-summary-in-minibuffer nil)
  (envrc-global-mode)

  (bind-keys :map global-map
             ("C-c e" . envrc-command-map)))

(use-package inheritenv
  :config
  ;; `envrc' sets environment variables in Emacs buffer-locally. This allows
  ;; users to have different buffer-local paths for executables in different
  ;; projects. However, when Emacs libraries run background processes on behalf
  ;; of a user, they often run processes in temporary buffers that do not
  ;; inherit the calling buffer's environment. This results in executables not
  ;; being found, or the wrong versions of executables being picked up.
  ;;
  ;; `inheritenv' provides the macro `inheritenv-add-advice' which wraps any
  ;; command with an advice function so it inherits buffer-local variables. This
  ;; is useful for when we discover problems we can't patch upstream.
  (inheritenv-add-advice 'process-lines))

;;;;;;;;;;;;;;;
;;;; files ;;;;

(use-package files
  :config
  (setopt y-or-n-p-use-read-key t
          use-short-answers t
          confirm-kill-processes nil
          confirm-kill-emacs 'yes-or-no-p
          large-file-warning-threshold nil)

  (bind-keys :map +prefix-map
             ("C-f" . find-file)))

(use-package backup
  :no-require
  :config
  ;; By default, Emacs tries to keep backups (i.e. some-file.el~). I do not need
  ;; this feature because all the files I care about are either under version
  ;; control or backed up to a flash drive.
  (setopt backup-inhibited t
          make-backup-files nil))

(use-package lockfiles
  :no-require
  :config
  ;; By default, Emacs tries to lock down files so that they are not modified by
  ;; other programs (i.e. .#some-file.el). I do not need this feature because if
  ;; I am ever modifying my files externally, then I know what I am doing.
  (setopt create-lockfiles nil
          auto-save-default nil))

(use-package autorevert
  :config
  ;; The "auto-revert" facility makes Emacs update the contents of a saved
  ;; buffer when its underlying file is changed externally. This can happen, for
  ;; example, when a "git pull" modifies the file we are already displaying in a
  ;; buffer. Emacs thus automatically reverts the buffer to reflect the new file
  ;; contents.
  (global-auto-revert-mode))

(use-package recentf
  :config
  ;; Emacs can keep track of recently visited files. Then we can revisit them
  ;; with the command `recent-open', which provides minibuffer completion.

  ;; Recent files are also available in the `consult-buffer' interface, which
  ;; makes it a one-stop-shop for opening buffers, recent files, or
  ;; bookmarks. This can be better than having to remember if something is a
  ;; buffer or is stored by bookmarks/recentf. Same idea for using one command
  ;; instead of three (or more).

  ;; I generally do not rely on `recentf-mode', as most of my work is done in
  ;; projects, which I switch to directly. Though I sometimes need to revisit a
  ;; file that I do not need to keep track of.

  (setopt recentf-max-saved-items 100)

  (recentf-mode))

(use-package saveplace
  :config
  ;; Tell Emacs to record where we were in the file, so we resume there on next
  ;; visit. Turn on place saving globally.
  (save-place-mode))

;;;;;;;;;;;;;;;
;;;; dired ;;;;

(use-package dired
  :config
  ;; Dired is probably my favorite Emacs tool. It exemplifies how I see Emacs as
  ;; a whole: a layer of interactivity on top of Unix. The `dired' interface
  ;; wraps -- and puts synergy -- to standard commands like 'ls', 'cp', 'mv',
  ;; 'rm', 'mkdir', 'chmod', and related. All while granting access to many
  ;; other conveniences, such as (i) marking files to operate on (individually,
  ;; with a regexp, etc.), (ii) bulk renaming files by making the buffer
  ;; writeable and editing it like a regular file, (iii) showing only files you
  ;; want, (iv) listing the contents of any subdirectory, such as to benefit
  ;; from the bulk-renaming capability, (v) running a keyboard macro that edits
  ;; file contents while using Dired to navigate the file listing, (vi) open
  ;; files in an external application, and more.
  ;;
  ;; Dired lets us work with our files in a way that still feels close to the
  ;; command-line, yet has more powerful interactive features than even fully
  ;; fledged, graphical file managers.

  ;; I add two settings which make all copy, rename/move, and delete operations
  ;; more intuitive. I always want to perform those actions in a recursive
  ;; manner, as this is the intent I have when I am targeting directories.
  ;;
  ;; The `delete-by-moving-to-trash' is a deviation from the behaviour of the
  ;; 'rm' program, as it sends the file into the virtual trash folder. Depending
  ;; on the system, files in the trash are either removed automatically after a
  ;; few days, or we still have to permanently delete them manually. I prefer
  ;; this extra layes of safety. Plus, we have the `trashed' package to navigate
  ;; the trash folder in a Dired-like way.
  (setopt dired-recursive-copies 'always
          dired-recursive-deletes 'always
          delete-by-moving-to-trash t)

  ;; As I already explained, Dired is a layes of interactivity on top of
  ;; standard Unix tools. We can see this in how Dired produces the file listing
  ;; and how we can affect it. The 'ls' program accepts an '-l' flag for a
  ;; "long", detailed list of files. This is what Dired uses. But we can pass
  ;; more flags by setting the value of `dired-listing-switches'. Do 'M-x man'
  ;; and then search for the 'ls' manpage to learn about what I have here. In
  ;; short:
  ;;
  ;; '-A'
  ;; Show hidden files ("dotfiles"), such as '.bashrc', but omit the implied '.'
  ;; and '..' targets. The latter two refer to the present and parent directory,
  ;; respectively.
  ;;
  ;; '-G'
  ;; Do not show the group name in the long listing. Only show the owner of the
  ;; file.
  ;;
  ;; '-F'
  ;; Differentiate regular from special files by appending a character to
  ;; them. The '*' is for executables, the '/' is for directories, the '|' is
  ;; for a named pipe, the '=' is for a socket, the '@' and the '>' are for
  ;; stuff I have never seen.
  ;;
  ;; '-h'
  ;; Make file sizes easier to read, such as '555k' instead of '568024'.
  ;;
  ;; '-l'
  ;; Produce a long, detailed listing. Dired requires this.
  ;;
  ;; '-v'
  ;; Sort files by version numbers, such that 'file1', 'file2', and 'file10'
  ;; appear in this order instead of 1, 10, 2. The latter is called
  ;; "lexicographic" and I have not found a single case where it is useful to me.
  ;;
  ;; '--group-directories-first'
  ;; Does what it says to place all directoris before files in the listing. I
  ;; prefer this over a strict sorting that does not differentiate between files
  ;; and directories.
  ;;
  ;; '--time-style=long-iso'
  ;; Uses the international standart for time representation in the file
  ;; listing. So we have something like '2023-12-30 06:38' to show the last
  ;; modified time.
  (setopt dired-listing-switches
          "-AGFhlv --group-directories-first --time-style=long-iso")

  ;; I often have two Dired buffers open side-by-side and want to move files
  ;; between them. By setting `dired-dwim-target' to a 't' value, we get the
  ;; other buffer as the default target of the current rename or copy
  ;; operation. This is exactly what I want.
  ;;
  ;; If there are more than two windows showing Dired buffers, the default
  ;; target is the previously visited window.
  ;;
  ;; Note that this only affects how quickly we can access the default value, as
  ;; we can always type 'M-p' (`previous-history-element') and 'M-n'
  ;; (`next-history-element') to cycle the minibuffer history.
  (setopt dired-dwim-target t)

  ;; From inside a Dired buffer, we can type '!' (`dired-do-shell-command') or
  ;; '&' (`dired-do-async-shell-command') to run an arbitrary command with the
  ;; given file (or marked files) as an argument. These commands will produce a
  ;; minibuffer prompt, which expects us to type in the name of the command.
  ;; Emacs already tries to guess some relevant defaults, though we can make it
  ;; do what we want by configuring the `dired-guess-shell-alist-user' user
  ;; option.
  ;;
  ;; This variable takes an alist value: a list of lists. Each element (each
  ;; list) has the first item in the list as a regular expression to match file
  ;; names. We normally want to have file type extensions here, though we can
  ;; also target the full name of a file. The remaining entries in the list are
  ;; strings that specify the name of the external program to use. We can have
  ;; as many as we want and cycle between them using the familiar 'M-p' and
  ;; 'M-n' keys inside the minibuffer.
  ;;
  ;; On Linux, the generic "open with default app" call is `xdg-open', so we
  ;; always want that as a fallback.
  ;;
  ;; Note that in Emacs 30, we have the command `dired-do-open', which is the
  ;; equivalent of typing '&' and then specifying the `xdg-open' command.
  (setopt dired-guess-shell-alist-user
          '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
            ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
            (".*" "xdg-open")))

  ;; These are some minor tweaks that I do not really care about. The only one
  ;; which is really nice in my opinion is the hook that involves the
  ;; `dired-hide-details-mode'. This is the command that hides the noisy output
  ;; of the 'ls -l' flag, leaving only the file names in the list. We can toggle
  ;; this effect at any time with the '(' key, by default.
  (setopt dired-auto-revert-buffer #'dired-directory-changed-p
          dired-free-space nil
          dired-kill-when-opening-new-dired-buffer t)

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)

  ;; Dired is excellent out-of-the-box. I provide a few minor commands that make
  ;; it more convenient for me to perform common actions. Chief among them is
  ;; `+dired-limit-regexp' (bound to 'C-c C-l'), which is an easier way to do
  ;; this in standard dired
  ;;
  ;; - Type '% m' (`dired-mark-files-regexp') to mark files you want to keep
  ;; seeing. Provide a regular expression or simply a common word.
  ;;
  ;; - Toggle the mark so that you now cover everything you do not want to see.
  ;;
  ;; - Invoke `dired-do-kill-lines' (bound to 'k' by default) to remove the
  ;; marked files from the view until the buffer is generated again with
  ;; `revert-buffer' (bound to 'g' by default).
  ;;
  ;; All this is file, but with `+dired-limit-regexp' I simply provide the
  ;; regexp I want to see.

  (defvar +dired--limit-hist '()
    "Minibuffer history for `+dired-limit-regexp'.")

  (defun +dired-limit-regexp (regexp omit)
    "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]), exclude files
matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
    (interactive
     (list
      (read-regexp
       (concat "Files "
               (when current-prefix-arg
                 (propertize "NOT " 'face 'warning))
               "matching PATTERN: ")
       nil '+dired--limit-hist)
      current-prefix-arg))
    (dired-mark-files-regexp regexp)
    (unless omit (dired-toggle-marks))
    (dired-do-kill-lines)
    (add-to-history '+dired--limit-hist regexp))

  ;; Another common use-case for me is to create a flat listing of all files
  ;; that match a regular expression, found recursively from the current
  ;; directory. I do this with `+dired-search-flat-list'.
  (defvar +dired-regexp-history nil
    "Minibuffer history of `+dired-regexp-prompt'.")

  (defun +dired-regexp-prompt ()
    (let ((default (car +dired-regexp-history)))
      (read-regexp
       (format-prompt "Files matching REGEXP" default)
       default '+dired-regexp-history)))

  (defun +dired--get-files (regexp)
    "Return files matching REGEXP, recursively from `default-directory'."
    (directory-files-recursively default-directory regexp nil))

  (defun +dired-search-flat-list (regexp)
    "Return a Dired buffer for files matching REGEXP.
Perform the search recursively from the current directory."
    (interactive (list (+dired-regexp-prompt)))
    (if-let* ((files (+dired--get-files regexp))
              (relative-paths (mapcar #'file-relative-name files)))
        (dired (cons (format "*flat-dired for `%s'*" regexp) relative-paths))
      (error "No files matching `%s'" regexp)))

  ;; The other commands have situational uses. For example, the
  ;; `+dired-grep-marked-files' is something I have only used a few times where
  ;; `consult-grep' would have produced too many results in a given directory.
  ;; (defvar +dired--find-grep-hist '()
  ;;   "Minibuffer history for `+dired-grep-marked-files'.")
  (defvar +dired-grep-marked-files-history nil
    "Minibuffer history for `+dired-grep-marked-files-prompt'.")
  (defun +dired-grep-marked-files-prompt ()
    "Prompt for string to search for with `+dired-grep-marked-files'."
    (read-string
     "grep for PATTERN in marked files: "
     nil '+dired-grep-marked-files-history))

  (defun +dired-grep-marked-files (files regexp)
    "Run `find' with `grep' for REGEXP on marked FILES."
    (interactive
     (if-let* ((marks (dired-get-marked-files 'no-dir)))
         (list marks (+dired-grep-marked-files-prompt))
       (user-error "No files marked")))
    (let* ((buffer-name (format "*+dired-grep-marked for `%s'*" regexp))
           (quoted-files (mapconcat #'shell-quote-argument files " "))
           (cmd (concat
                 "find " quoted-files
                 " -not " (shell-quote-argument "(")
                 " -wholename " (shell-quote-argument "*/.git*")
                 " -prune " (shell-quote-argument ")")
                 " -type f"
                 " -exec grep -nHER --color=auto "
                 (shell-quote-argument regexp) " "
                 (shell-quote-argument "{}") " "
                 (shell-quote-argument ";"))))
      (compilation-start cmd 'grep-mode (lambda (_mode) buffer-name) t)))

  ;; Jump to next and previous subdirectories headings.
  (defvar +dired--directory-header-regexp "^ +\\(.+\\):\n"
    "Pattern to match Dired directory headings.")

  (defun +dired-subdirectory-next (&optional arg)
    "Move to next or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
    (interactive "p")
    (let ((pos (point))
          (subdir +dired--directory-header-regexp))
      (goto-char (line-end-position))
      (if (re-search-forward subdir nil t (or arg nil))
          (progn
            (goto-char (match-beginning 1))
            (goto-char (line-beginning-position)))
        (goto-char pos))))

  (defun +dired-subdirectory-previous (&optional arg)
    "Move to previous or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
    (interactive "p")
    (let ((pos (point))
          (subdir +dired--directory-header-regexp))
      (goto-char (line-beginning-position))
      (if (re-search-backward subdir nil t (or arg nil))
          (goto-char (line-beginning-position))
        (goto-char pos))))

  ;; Insert subdirectory heading for all marked subdirectories.
  (defun +dired-remove-inserted-subdirs ()
    "Remove all inserted Dired subdirectories."
    (interactive)
    (goto-char (point-max))
    (while (and (+dired-subdirectory-previous)
                (not (equal (dired-current-directory)
                            (expand-file-name default-directory))))
      (dired-kill-subdir)))

  (defun +dired--dir-list (list)
    "Filter out non-directory file paths in LIST."
    (cl-remove-if-not
     (lambda (dir)
       (file-directory-p dir))
     list))

  (defun +dired--insert-dir (dir &optional flags)
    "Insert DIR using optional FLAGS."
    (dired-maybe-insert-subdir (expand-file-name dir) (or flags nil)))

  (defun +dired-insert-subdir (&optional arg)
    "Generic command to insert subdirectories in Dired buffers.

When items are marked, insert those which are subsirectories of
the current directory.  Ignore regular files.

If no marks are active and point is on a subdirectory line,
insert it directly.

If no marks are active and point is not on a subdirectory line,
prompt for a subdirectory using completion.

With optional ARG as a single prefix (`\\[universal-argument]')
argument, prompt for command line flags to pass to the underlying
ls program.

With optional ARG as a double prefix argument, remove all
inserted subdirectories."
    (interactive "p")
    (let* ((name (dired-get-marked-files))
           (flags (when (eq arg 4)
                    (read-string "Flags for `ls' listing: "
                                 (or dired-subdir-switches dired-actual-switches)))))
      (cond  ; NOTE 2021-07-20: `length>', `length=' are from Emacs28
       ((eq arg 16)
        (+dired-remove-inserted-subdirs))
       ((and (length> name 1) (+dired--dir-list name))
        (mapc (lambda (file)
                (when (file-directory-p file)
                  (+dired--insert-dir file flags)))
              name))
       ((and (length= name 1) (file-directory-p (car name)))
        (+dired--insert-dir (car name) flags))
       (t
        (let ((selection (read-directory-name "Insert directory: ")))
          (+dired--insert-dir selection flags)))))) ; override `dired-maybe-insert-subdir'

  ;; Jump to subdirectory headings with Imenu.
  (defun +dired--imenu-prev-index-position ()
    "Find the previous file in the buffer."
    (let ((subdir +dired--directory-header-regexp))
      (re-search-backward subdir nil t)))

  (defun +dired--imenu-extract-index-name ()
    "Return the name of the file at point."
    (file-relative-name
     (buffer-substring-no-properties (+ (line-beginning-position) 2)
                                     (1- (line-end-position)))))

  (defun +dired-setup-imenu ()
    "Configure Imenu for the current Dired buffer.
Add this to `dired-mode-hook'."
    (set (make-local-variable 'imenu-prev-index-position-function)
         '+dired--imenu-prev-index-position)
    (set (make-local-variable 'imenu-extract-index-name-function)
         '+dired--imenu-extract-index-name))

  (add-hook 'dired-mode-hook #'+dired-setup-imenu)

  ;; `dired-jump' doesn't need a repeat map.
  (put 'dired-jump 'repeat-map nil)

  ;; Reuse current buffer in Dired by pressing 'a' (`dired-find-alternate-file')
  (put 'dired-find-alternate-file 'disabled nil)

  ;; `+dired-hist' is a minor mode that keeps track of visited Dired buffers and
  ;; lets you go back and forward across them. This is similar to the facility
  ;; provided in other Emacs major modes, such as Info and EWW.
  (defvar +dired-hist-stack nil
    "The stack of previously visited Dired buffers.")

  (defvar +dired-hist-forward-stack nil
    "Forward history of previously visited Dired buffers.")

  (defun +dired-hist--match (stack)
    (equal (cdr-safe (car-safe stack)) default-directory))

  (defun +dired-hist-go-back ()
    "Go backward in the visited Dired buffer history."
    (interactive)
    (+dired-hist--update)
    (when-let ((_ (cdr-safe +dired-hist-stack))
               (elm (pop +dired-hist-stack)))
      (unless (+dired-hist--match +dired-hist-forward-stack)
        (push elm +dired-hist-forward-stack))
      (+dired-hist--visit (car +dired-hist-stack))))

  (defun +dired-hist-go-forward ()
    "Go forward in the visited Dired buffer history."
    (interactive)
    (when +dired-hist-forward-stack
      (+dired-hist--visit (pop +dired-hist-forward-stack))
      (+dired-hist--update)))

  (defun +dired-hist--visit (item)
    "Visit Dired buffer or directory specified in ITEM.

ITEM is a cons cell of the form (marker . directory)."
    (let* ((last-buffer (marker-buffer (car item)))
           (alive-p (buffer-live-p last-buffer))
           (win (and alive-p
                     (get-buffer-window last-buffer))))
      (cond
       (win (select-window win))
       (alive-p (switch-to-buffer last-buffer))
       (t (dired--find-possibly-alternative-file (cdr item))))))

  (defun +dired-hist--update ()
    "Update the Dired buffer history stack."
    (unless (+dired-hist--match +dired-hist-stack)
      (push (cons (point-marker) default-directory) +dired-hist-stack)))

  (define-minor-mode +dired-hist-mode
    "Keep track of visited Dired buffers and switch between them."
    :group '+dired-hist
    :global t
    :lighter nil
    (if +dired-hist-mode
        (add-hook 'dired-mode-hook #'+dired-hist--update)
      (remove-hook 'dired-mode-hook #'+dired-hist--update)
      (setq +dired-hist-stack nil
            +dired-hist-forward-stack nil)))

  (+dired-hist-mode 1)

  (bind-keys
   :map +prefix-map
   ("d" . dired)
   ("C-j" . dired-jump)
   :map dired-mode-map
   ("e" . wdired-change-to-wdired-mode)
   ("i" . +dired-insert-subdir) ; override `dired-maybe-insert-subdir'
   ("l" . +dired-hist-go-back)
   ("r" . +dired-hist-go-forward)
   ("/" . +dired-limit-regexp)
   ("M-e" . +dired-subdirectory-next)
   ("M-a" . +dired-subdirectory-previous)
   ("C-c C-n" . +dired-subdirectory-next)
   ("C-c C-p" . +dired-subdirectory-previous)
   ("C-c C-l" . +dired-limit-regexp)
   ("M-s f" . +dired-search-flat-list) ; alt. `consult-find'
   ("M-s M-f" . +dired-search-flat-list) ; alt. `consult-find'
   ("M-s g" . +dired-grep-marked-files) ; alt. `consult-grep'
   ("M-s M-g" . +dired-grep-marked-files) ; alt. `consult-grep'
   ))

(use-package dired-aux
  :after dired
  :config
  ;; `dired-aux' is a built-in library that provides useful extras for
  ;; Dired. The highlights from what I have here are:
  ;;
  ;; The user option `dired-create-destination-dirs' and
  ;; `dired-create-destination-dirs-on-trailing-dirsep', which offer to create
  ;; the specified directory path if it is missing.
  ;;
  ;; The key binding for `dired-do-open', which opens the file or directory
  ;; externally.
  (setopt dired-isearch-filenames 'dwim
          dired-create-destination-dirs 'ask
          dired-create-destination-dirs-on-trailing-dirsep t
          dired-vc-rename-file t
          dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))

  (bind-keys :map dired-mode-map
             ("C-<return>" . dired-do-open) ; Emacs 30
             ("F" . dired-create-empty-file) ; mirror `dired-find-file'
             ("M-s f" . nil)))

;;;;;;;;;;;;;;;;;;;
;;;; bookmarks ;;;;

(use-package bookmark
  :config
  ;; Bookmarks are compartments that store arbitrary information about a file or
  ;; buffer. The records are used to recreate that file/buffer inside of
  ;; Emacs. Put differently, we can easily jump back to a file or directory (or
  ;; anything that has a bookmark recorder+handler, really). Use the
  ;; `bookmark-set' command (`C-x r m' by default) to record a bookmark and then
  ;; visit one of your bookmarks with `bookmark-jump' (`C-x r b' by default).
  (setopt bookmark-use-annotations nil
          bookmark-automatically-show-annotations nil
          bookmark-fringe-mark nil ; Emacs 29 to hide bookmark fringe icon
          ;; Write changes to the bookmark file as soon as 1 modification is
          ;; made (addition or deletion). Otherwise Emacs will only save the
          ;; bookmarks when it closes, which may never happen properly
          ;; (e.g. power failure).
          bookmark-save-flag 1)

  (bind-keys :map +registers-prefix-map
             ("b" . bookmark-jump)
             ("l" . bookmark-bmenu-list)
             ("m" . bookmark-set)))

;;;;;;;;;;;;;;;;;;;
;;;; registers ;;;;

(use-package register
  :config
  ;; Much like bookmarks, registers store data that we can reinstate quickly. A
  ;; common use-case is to write some text to a register and then insert that
  ;; text by calling the given register. This is much better than relying on the
  ;; `kill-ring', because registers are meant to be overwritten by the user,
  ;; whereas the `kill-ring' accumulates lots of text that we do not necessarily
  ;; need.
  ;;
  ;; To me, registers are essential for keyboard macros. By default, registers
  ;; do not persist between Emacs sessions, though I do need to re-use them from
  ;; time to time, hence the arrangement to record them with `savehist-mode'.
  (setopt register-preview-delay 0.8)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'register-alist)))

;;;;;;;;;;;;;;;;;;;;
;;;; completion ;;;;

(use-package minibuffer
  :config
  ;; The term "completion" describes a process where user input is assisted by
  ;; pattern matching algorithms to type out incomplete items. The most basic
  ;; way of this model of interaction is what we get in a command-line prompt,
  ;; where we can hit "TAB" to expand the word before point to something the
  ;; program already knows about (e.g. ema followed by TAB may complete to
  ;; emacs).
  ;;
  ;; In Emacs, completion encompasses user interfaces that show the available
  ;; candidates (the likely options) right away, as well as provide more advanced
  ;; capabilities for storing the history of previous inputs, displaying helpful
  ;; annotations next to each candidate, and "completion styles" to control how
  ;; user input is matched to candidates. Because we use the minibuffer for most
  ;; common interactions, completion is an integral part of any setup.

  ;; The `completion-styles' are pattern matching algorithms. They interpret
  ;; input and match candidates accordingly.
  ;;
  ;; `emacs-22': prefix completion that only operates on the text before
  ;; point. If we are in "prefix|suffix", with "|" representing the cursor, it
  ;; will consider everything that expands "prefix" and then add back to it the
  ;; "suffix".
  ;;
  ;; `basic': prefix completion that also accounts for the text after
  ;; point. Using the above example, this one will consider patterns that match
  ;; all of `emacs22' as well as anything thet completes "suffix".
  ;;
  ;; `partial-completion': this is used for file navigation. Instead of typing
  ;; out a full path like "~/.local/share/fonts", we do "~/.l/s/f" or variants
  ;; thereof to make the matches unique such as ~/.l/sh/fon. It is a joy to
  ;; navigate the file system in this way.
  ;;
  ;; `substring': matches the given sequence of characters literally regardless
  ;; of where it is in a word. So "pro" will match "professional" as well as
  ;; "reproduce".
  ;;
  ;; `flex': completion of an in-order subset of characters. It does not matter
  ;; where the characters are in the word, so long as they are encountered in
  ;; the given order. The input "lad" will thus match "list-faces-display" as
  ;; well as "pulsard-highlight-dwim".
  ;;
  ;; `initials': completion of acronyms and initialisms. Typing "lfd" will thus
  ;; match "list-faces-display". This completion style can also be used for file
  ;; system navigation, though I prefer to only have `partial-completion' handle
  ;; that task.
  ;;
  ;; `orderless': this is the only completion style I use which is not built
  ;; into Emacs and which I tweak further in a separate section. It matches
  ;; patterns out-of-order. Patterns are typically words separated by spaces,
  ;; though they can also be regular expressions, and even styles that are the
  ;; same as the aforementioned `flex' and `initials'.

  ;; Now that you know about the completions styles I use, take a look at the
  ;; value of my `completion-styles'. You will notice that `orderless', which is
  ;; the most powerful/flexible is placed last. I do this because Emacs tries
  ;; the styles in the given order from left to right, moving the next one until
  ;; it finds a match. As such, I usually want to start with tight matches
  ;; (e.g. "li-fa-di" for "list-faces-display") and only widen the scope of the
  ;; search as I need to. This is easy to do because none of the built-in
  ;; completion styles parses the empty space (the default
  ;; `orderless-component-separator'), so as soon as I type a space after some
  ;; characters I am using `orderless'.
  (setopt completion-styles '(basic orderless))

  ;; The `completion-styles' is the fallback option in case there is no
  ;; provision for the given completion category. The completion category is a
  ;; piece of metada that is associated with the completion table we are
  ;; matching against while using the minibuffer. For example, the "find-file"
  ;; command has the "file" category, while the "switch-to-buffer" command uses
  ;; the "buffer" category. The defaults for those are specified in the variable
  ;; `completion-category-defaults', while overrides for them can be set in the
  ;; `completion-category-overrides'.
  ;;
  ;; While we can override the categories we care about, the presence of those
  ;; `completion-category-defaults' will surprise us in some cases because we
  ;; will not be using what we specified in the `completion-styles'. As such, I
  ;; set the `completion-category-defaults' to nil, to always fall back to my
  ;; preferred `completion-styles' and then I further configure overrides where
  ;; those make sense to me.
  (setopt completion-category-defaults nil)

  ;; We can opt for per-category styles by configuring the user option
  ;; `completion-category-overrides'.
  (setopt completion-category-overrides
          ;; In order to narrow our Citar searches not only using citation keys
          ;; (i.e. using authors, titles, etc.), we need a completion style that
          ;; is order independent.
          '((citar-candidate (styles . (orderless basic)))))

  ;; This makes it so that the minibuffer prompt is not accessible with regular
  ;; motions to avoid mistakes.
  (setopt minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Make it so prompts where more than one candidate can be provided using
  ;; completion show an indicator about this fact. Each candidate is separated
  ;; by the `crm-separator'. We display [`completing-read-multiple': <separator>],
  ;; e.g., [`completing-read-multiple': ,] if the separator is a comma.
  ;; (setq crm-prompt (format "%s %%p" (propertize "[%d]" 'face 'shadow))) ; Emacs 31
  (defun crm-indicator (args)
    (cons (format "[`completing-read-multiple': %s]  %s"
                  (propertize
                   (replace-regexp-in-string
                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                    crm-separator)
                   'face 'error)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

(use-package mb-depth
  :config
  ;; The need to have multiple (i.e. "recursive") minibuffers arises when you
  ;; initiate a command, such as M-x followed by some incomplete command where
  ;; remember that you forgot to perform another command before confirming the
  ;; first one. An example is the combination of M-x (execute-extended-command)
  ;; and M-: (eval-expression).
  (setopt enable-recursive-minibuffers t)
  ;; Shows a number next to the minibuffer prompt, indicating the level of depth
  ;; in the recursion, starting with 2
  (minibuffer-depth-indicate-mode))

(use-package minibuf-eldef
  :config
  ;; Minibuffer prompts often have a default value. This is used when the user
  ;; types `RET' without inputing anything. The out-of-the-box behaviour of
  ;; Emacs is to append informative text to the prompt like
  ;; `\(default some-default-value\)'. With this tweak to
  ;; `minibuffer-default-prompt-format' we get a more compact style of
  ;; `\[some-default-value\]', which looks better to me.
  (setopt minibuffer-default-prompt-format " [%s]")
  ;; Display the default value next to the prompt only if `RET' will actually
  ;; use the default in that situation. This means that while you start typing
  ;; in the minibuffer, the `[some-default-value]' indicator disappears, since
  ;; it is no longer applicable. Without this mode, the indicator stays there at
  ;; all times, which can be annoying or distracting.
  (minibuffer-electric-default-mode))

(use-package rfn-eshadow
  :config
  ;; `file-name-shadow-mode' is a neat little feature to dim or remove the
  ;; "shadowed" part of a file prompt while using something like find-file. File
  ;; name shadowing happens when we invoke find-file and instead of first
  ;; deleting the contents of the minibuffer, we start typing out the file
  ;; system path we wish to visit. For example, I am in `~/Git/Projects' and
  ;; type directly after it something like `~/.local/share/fonts/', so Emacs
  ;; displays `~/Git/Projects/~/.local/share/fonts/'.  With
  ;; `file-name-shadow-mode' the original "shadowed" part will be greyed out.
  ;; We can remove it altogether by applying the invisible property. This is
  ;; especially nice with the completion style called `partial-completion'.
  (setopt file-name-shadow-properties
          '(face file-name-shadow field shadow invisible t intangible t))
  (file-name-shadow-mode 1))

(use-package savehist
  :config
  ;; Minibuffer prompts can have their own history. When they do not, they share
  ;; a common history of user inputs. Emacs keeps track of that history in the
  ;; current session, but loses it as soon as we close it. With `savehist-mode'
  ;; enabled, all minibuffer histories are written to a file and are restored
  ;; when we start Emacs again.
  ;;
  ;; Histories are useful in two ways:
  ;;
  ;; 1. Recent choices appear at the top, so we can find them more easily.
  ;;
  ;; 2. The `M-p' (`previous-history-element') and `M-n'
  ;; (`next-history-element') commands it the minibuffer will be useful right
  ;; away upon restoring Emacs.
  ;;
  ;; Since we are already recording minibuffer histories, we can instruct
  ;; `savehist-mode' to also keep track of additional variables and restore them
  ;; next time we use Emacs. Hence `savehist-additional-variables'. I do this in
  ;; a few places: `corfu', `register', and `shell'.
  ;;
  ;; Note that the user option `history-length' applies to each individual
  ;; history variable: it is not about all histories combined.
  ;;
  ;; Overall, I am happy with this feature and benefit from it on a daily basis.
  (setopt history-length 100
          history-delete-duplicates t
          savehist-save-minibuffer-history t
          savehist-file (locate-user-emacs-file "savehist"))

  (add-to-list 'savehist-additional-variables 'kill-ring)

  (savehist-mode))

(use-package marginalia
  :config
  ;; The `marginalia' package, co-authored by Daniel Mendler and Omar Antolin
  ;; Camarena, provides helpful annotations to the side of completion
  ;; candidates. We see its effect, for example, when we call `M-x': each
  ;; command has a brief description next to it (taken from its doc string) as
  ;; well as a key binding, if it has one.
  ;;
  ;; Annotations are provided on a per-category basis. Categories are metadata
  ;; associated with the completion table, which describe what the candidates
  ;; are about.
  ;;
  ;; The out-of-the-box settings of `marginalia' are perfectly usable. We can
  ;; always write our own functions or opt out of some annotations by modifying
  ;; the user option `marginalia-annotator-registry'. I used to do that, but
  ;; ultimately decided that I did not need to make any changes.
  (setopt marginalia-max-relative-age 0) ; absolute time
  (bind-keys :map minibuffer-local-map
             ("M-]" . marginalia-cycle))
  (marginalia-mode))

(use-package orderless
  ;; The `orderless' package by Omar Antolin Camarena provides one of the
  ;; completion styles that I use. It is a powerful pattern matching algorithm
  ;; that parses user input and interprets it out-of-order, so that `in pa' will
  ;; cover "insert-pair" as well as "package-install". Components of the search
  ;; are space-separated, by default, though we can modify the user option
  ;; `orderless-component-separator' to have something else (but I cannot think
  ;; of a better value). In the section about completion styles, I explain how I
  ;; use `orderless' and why its power does not result in lots of false
  ;; positives.
  ;;
  ;; With orderless we can also define so-called "style-dispatchers". These are
  ;; characters attached to the input which instruct `orderless' to use a
  ;; specific pattern for that component.
  ;;
  ;; I used to have my own style dispatchers, but realised that I was not using
  ;; them enough. The default method has also updated since I did my
  ;; configuration to support an affixation method (prefix OR suffix) for style
  ;; dispatching. This method reads the `orderless-affix-dispatch-alist' to
  ;; determine how to interpret the input. From that list, the most obvious
  ;; advantage to me is he `!', which is a logical `NOT': it is a very easy way
  ;; to remove something from the list of candidates while typing in the
  ;; minibuffer. The `&' is potentially useful because it matches the
  ;; annotations displayed by `marginalia'. The remaining dispatch characters
  ;; affect how the input is treated as a literal string, initialism, etc.,
  ;; which is not as useful to me.
  ;;
  ;; Remember to check my `completion-styles' and the
  ;; `completion-category-overrides'.
  )

(use-package vertico
  ;; The `vertico' package by Daniel Mendler displays the minibuffer in a
  ;; vertical layout. Under the hood, it takes care to be responsive and to
  ;; handle even massive completion tables gracefully. Whereas, say, the
  ;; built-in completion user interface will suffer from a noticeable
  ;; performance penalty.
  ;;
  ;; All we need to get a decent experience with `vertico' is to enable the
  ;; `vertico-mode'. For most users this is enough. In my case though, I have to
  ;; use the "multiform" mechanism of this package to make it not show up
  ;; eagerly.
  ;;
  ;; Beside what I am using it for, the "multiform" mechanism allows us to
  ;; change the layout of `vertico' on a per-command or per-category basis. We
  ;; can, for instance, have a horizontal presentation for some items. I have
  ;; tried this for a while, but ultimately decided to go with a more
  ;; predictable scheme.
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  ;; I don't like it when the minibuffer shows up eagerly. I find it disorienting
  ;; and motion sickness inducing when there are many things jumping around on my
  ;; screen at once, such as when the minibuffer constantly resizes itself or
  ;; pushes my windows around.
  ;;
  ;; The "multiform" mechanism of the `vertico' package allows us to change the
  ;; layout on a per-command or per-category basis. We can use this mechanism to
  ;; make the minibuffer not show up eagerly.

  (defvar +vertico-multiform-minimal
    '(unobtrusive
      (vertico-flat-format . ( :multiple ""
                               :single ""
                               :prompt ""
                               :separator ""
                               :ellipsis ""
                               :no-match ""))
      (vertico-preselect . prompt))
    "List of configurations for minimal Vertico multiform.
The minimal view is intended to be less eager or less revealing
for general usage.

Toggle the vertical view with the `vertico-multiform-vertical'
command or use the commands `+vertico-minimal-next' and
`+vertico-minimal-previous', which toggle the vertical view
automatically.")

  (defvar +vertico-multiform-maximal
    '((vertico-count . 10)
      (vertico-preselect . directory))
    "List of configurations for maximal Vertico multiform.")

  (defvar +vertico-multiform-grid
    '(grid
      (vertico-grid-annotate . 20)
      (vertico-count . 4))
    "List of configurations for grid Vertico multiform.")

  (defun +vertico--match-directory (str)
    "Match directory delimiter in STR."
    (string-suffix-p "/" str))

  (defun +vertico-sort-directories-first (files)
    "Sort directories before FILES."
    (setq files (vertico-sort-alpha files))
    (nconc (seq-filter #'+vertico--match-directory files)
           (seq-remove #'+vertico--match-directory files)))

  (defun +vertico-minimal-next ()
    "Like `vertico-next' but toggle vertical view if needed.
This is done to accommodate `+vertico-multiform-minimal'."
    (interactive)
    (if vertico-unobtrusive-mode
        (progn
          (vertico-multiform-vertical)
          (vertico-next 1))
      (vertico-next 1)))

  (defun +vertico-minimal-previous ()
    "Like `vertico-previous' but toggle vertical view if needed.
This is done to accommodate `+vertico-multiform-minimal'."
    (interactive)
    (if vertico-unobtrusive-mode
        (let ((vertico--index 0))
          (vertico-multiform-vertical)
          (vertico-previous 1))
      (vertico-previous 1)))

  (defun +vertico-minimal-complete ()
    "Expand contents and show remaining candidates, if needed.
This is dote to accommodate `+vertico-multiform-minimal'."
    (interactive)
    (if (and vertico-unobtrusive-mode
             (> vertico--total 1))
        (progn
          (minibuffer-complete)
          (+vertico-minimal-next))
      (vertico-insert)))

  (defun +vertico-minimal-exit ()
    "Exit with the candidate if `+vertico-multiform-minimal'.
If there are more candidates that match the given input, expand
the minibuffer to show the remaining candidates and select the
first one. Else do `vertico-exit'."
    (interactive)
    (cond
     ((and (= vertico--total 1)
           (not (eq 'file (vertico--metadata-get 'category))))
      (minibuffer-complete)
      (vertico-exit))
     ((and vertico-unobtrusive-mode
           (not minibuffer--require-match)
           (or (string-empty-p (minibuffer-contents))
               minibuffer-default
               (eq vertico-preselect 'directory)
               (eq vertico-preselect 'prompt)))
      (vertico-exit-input))
     ((and vertico-unobtrusive-mode (> vertico--total 1))
      (minibuffer-complete-and-exit)
      (+vertico-minimal-next))
     (t
      (vertico-exit))))

  (setopt vertico-multiform-categories `(;; Maximal
                                         (embark-keybinding
                                          ,@+vertico-multiform-maximal)
                                         ;; Grid
                                         (jinx
                                          ,@+vertico-multiform-grid)
                                         ;; Minimal
                                         (file
                                          ,@+vertico-multiform-minimal
                                          (vertico-sort-function
                                           . +vertico-sort-directories-first))
                                         (t ,@+vertico-multiform-minimal))
          vertico-multiform-commands `(("citar-\\(.*\\)"
                                        ,@+vertico-multiform-maximal))
          vertico-cycle t
          vertico-count 5)

  ;; The `completion-in-region-function' handles in-buffer text completion using
  ;; Emacs' underlying infrastructure for `completion-at-point-functions'. A
  ;; popular interface for this is `corfu', which provides in-buffer popups. I
  ;; find these too visually distracting, especially when they appear
  ;; automatically. I don't want things eagerly popping in and out of my view. I
  ;; want manual completion. Pop up only when I say so.
  ;;
  ;; Since I have already configured `vertico' to behave this way, it makes
  ;; sense to leverage that interface here. When Vertico is active, use
  ;; `consult-completion-in-region' to handle in-buffer completions through the
  ;; minibuffer. Otherwise use the default `completion--in-region' function.
  (setopt completion-in-region-function
          (lambda (&rest args)
            (apply (cond ((and (minibufferp)
                               minibuffer-completion-table)
                          #'completion--in-region)
                         (t
                          #'consult-completion-in-region))
                   args)))

  (with-eval-after-load 'rfn-eshadow
    ;; This works with `file-name-shadow-mode' enabled. When you are in
    ;; a sub-directory and use, say, find-file to go to your home '~/'
    ;; or root '/' directory, Vertico will also clear the old path to
    ;; keep only your current input.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

  ;; Setup vertico-repeat
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)

  (bind-keys :map vertico-map
             ("TAB" . +vertico-minimal-complete)
             ("DEL" . vertico-directory-delete-char)
             ("M-<backspace>" . vertico-directory-delete-word)
             ;; TODO should i disable C-DEL in Emacs so i train myself to use
             ;; M-DEL to delete words? or should i use C-w to delete words,
             ;; reserving C-DEL for vertico-directory-up?
             ("C-<backspace>" . vertico-directory-up)
             :map vertico-multiform-map
             ("C-n" . +vertico-minimal-next)
             ("C-p" . +vertico-minimal-previous)
             ("C-v" . vertico-scroll-up)
             ("M-v" . vertico-scroll-down)
             ("<down>" . +vertico-minimal-next)
             ("<up>" . +vertico-minimal-previous)
             ("C-l" . vertico-multiform-vertical)
             ("C-M-l" . vertico-multiform-buffer)
             ("RET" . +vertico-minimal-exit)
             ("<return>" . +vertico-minimal-exit)
             ("M-a" . vertico-previous-group)
             ("M-e" . vertico-next-group)
             :map +prefix-map
             ("z" . vertico-repeat)))

(use-package consult
  ;; Provides a number of commands that turbocharge the minibuffer with advanced
  ;; capabilities for filtering, asynchronous input, and previewing of the
  ;; current candidate's context.
  ;;
  ;; A case where filtering is in use is the `consult-buffer' command. Anything
  ;; that defines a source for this interface can be filtered by typing the
  ;; appropriate narrow key and space in the empty minibuffer e.g. `b SPC' the
  ;; filter specific to buffers. Delete back to remove the `[Buffer]' filter and
  ;; insert another filter. Every multi-source command from `consult' relies on
  ;; this paradigm.
  ;;
  ;; Asynchronous input pertains to the intersection between Emacs and external
  ;; search programs. A case in point is `consult-grep', which calls the
  ;; system's `grep' program. The prompt distinguishes between what is sent to
  ;; the external program and what is only shown to Emacs by wrapping the former
  ;; inside of `consult-async-split-style' (`#' be default). So the input
  ;; `#my-#completion' will send `my-' to the `grep' program and then use
  ;; `completion' inside of the minibuffer to perform the subsequent
  ;; pattern-matching (e.g. with help from `orderless'. The part that is sent to
  ;; the external program does not block Emacs. It is handled asynchronously, so
  ;; everything stays responsive.
  ;;
  ;; As for previewing, `consult' commands show the context of the current match
  ;; and update the window as we move between completion candidates in the
  ;; minibuffer. For example, the `consult-line' command performs an in-buffer
  ;; search and lets us move between matches in the minibuffer while seeing in
  ;; the window above what the surrounding text looks like. This is an excellent
  ;; feature when we are trying to find something and do not quite remember all
  ;; the search terms to narrow down to it simply by typing at the minibuffer
  ;; prompt.  Unfortunately, the eager previewing can be disorienting when
  ;; moving quickly between candidates. The `consult-preview-key' variable
  ;; allows us to configure this behavior. I have set it so `C-M-n' and `C-M-p'
  ;; + `S-<down>' and `S-<up>' always scrolls over the list of candidates while
  ;; doing preview. The `consult-customize' macro allows us to configure the the
  ;; preview on a per-command basis.

  :config
  (bind-keys :map vertico-map
             ("C-M-n" . vertico-next)
             ("C-M-p" . vertico-previous)
             ("S-<down>" . vertico-next)
             ("S-<up>" . vertico-previous))

  (setopt consult-preview-key '("M-." "C-M-n" "C-M-p" "S-<down>" "S-<up>"))
  (consult-customize
   consult-theme :preview-key (list :debounce 0.3
                                    "M-." "C-M-n" "C-M-p" "S-<down>" "S-<up>")
   consult-mark :preview-key 'any)

  ;; When I call `consult-buffer', I usually instantly narrow to the buffer
  ;; subset, although I sometimes want to select a recent file or a shell
  ;; buffer. Typing "f SPC" or "s SPC" on those occasions is easier than almost
  ;; always typing "b SPC". I hide all sources, except normal buffers by
  ;; default.
  (dolist (src consult-buffer-sources)
    (unless (or (null (symbol-value src))
                (eq src 'consult-source-buffer)
                (plist-member (symbol-value src) :hidden))
      (set src (plist-put (symbol-value src) :hidden t))))

  ;; Always work from the current directory (use `project-*' commands or `cd' to
  ;; switch directories).
  ;; NOTE 2025-12-05: I use C-u to select the directory from which to launch the
  ;; consult function.
  ;; (setopt consult-project-function nil)

  ;; Use `consult-find-args' to specify slow directories to skip, like .git/,
  ;; .cache/, and node-modules.
  ;; NOTE 2025-05-28: It would be better to use `fd' which respects my
  ;; .gitignore file, making it so I don't have to add these extra args.
  (setopt consult-find-args (concat "find . -not ( "
                                    "-path */.git* -prune "
                                    "-or -path */.cache* -prune "
                                    "-or -path */.direnv* -prune "
                                    "-or -path */.node_modules* -prune "
                                    ")"))

  ;; Use `consult-grep-arg' to make sure all files are read and symbolic links
  ;; are followed `-R'.
  ;; NOTE 2025-05-28: -R isn't always desirable. I mainly want it for grepping
  ;; inside the /nix/store. We can avoid some of the issues of using the -R flag
  ;; with `consult-git-grep', which respects my .gitignore.
  ;; (setopt consult-grep-args '("grep" (consult--grep-exclude-args)
  ;;                             "--null" "--line-buffered" "--color=never"
  ;;                             "--ignore-case" "--with-filename" "--line-number"
  ;;                             "-I" "-R"))

  ;; Make sure all files are read and symbolic links are followed with the
  ;; `--follow' argument to ripgrep.
  (setopt consult-ripgrep-args
          "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
 --smart-case --no-heading --with-filename --line-number --search-zip --follow")

  ;; NOTE document pulsar and consult integration
  (with-eval-after-load 'pulsar
    (setq consult-after-jump-hook nil)
    (dolist (fn '(pulsar-recenter-center pulsar-reveal-entry))
      (add-hook 'consult-after-jump-hook fn)))

  ;; Consult source for shell-related buffers.
  (defvar +consult--source-shell-buffer
    `( :name "Shells"
       :narrow   ?s
       :category buffer
       :state    ,#'consult--buffer-state
       :items    ,(lambda ()
                    (mapcar #'buffer-name
                     (seq-filter (lambda (buf)
                                   (with-current-buffer buf
                                    (derived-mode-p
                                     'shell-mode
                                     'eshell-mode
                                     'mistty-mode
                                     'comint-mode)))
                      (buffer-list))))))
  (add-to-list 'consult-buffer-sources '+consult--source-shell-buffer :append)

  (defun +consult-tab (tab)
    "Switch to TAB by name."
    (interactive
     (if-let* ((tabs (mapcar (lambda (tab)
                               (cdr (assq 'name tab)))
                             (tab-bar-tabs))))
         (list (consult--read tabs :prompt "Tabs: " :category 'tab))
       (user-error "No tabs found")))
    (tab-bar-select-tab-by-name tab))

  ;; Add a `consult' command to visualize `xref' history, adaptation of
  ;; `consult-mark'
  (defvar +consult--xref-history nil
    "History for the `+consult-xref-history' results.")
  (defun +consult-xref-history ()
    "Jump to a marker in `xref--history'.

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history."
    (interactive)
    (consult--read
     (consult--global-mark-candidates
      (flatten-list xref--history))
     :prompt "Go to Xref: "
     ;; Despite `+consult-xref-history' formatting the candidates in grep-like
     ;; style, we are not using the `consult-grep' category, since the
     ;; candidates have location markers attached.
     :category 'consult-location
     :sort nil
     :require-match t
     :lookup #'consult--lookup-location
     :history '(:input +consult--xref-history)
     :add-history (thing-at-point 'symbol)
     :state (consult--jump-state)))

  ;; Start `consult-line' search with active region, if available.
  (defalias '+consult-line-dwim 'consult-line)
  (consult-customize
   consult-line +consult-line-dwim
   :prompt "Search: "
   +consult-line-dwim
   :initial (when (use-region-p)
              (deactivate-mark)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))))

  ;; TODO `+consult-grep-dwim' that uses `(locate-dominating-file ".git")' to
  ;; determine whether to use consult-grep or consult-git-grep.
  ;; NOTE ripgrep already respects .gitignore

  ;; Start `consult-grep' search with active region, if available.
  (defalias '+consult-grep-dwim 'consult-grep)
  (defalias '+consult-ripgrep-dwim 'consult-ripgrep)
  (consult-customize
   +consult-grep-dwim +consult-ripgrep-dwim
   :initial (when (use-region-p)
              (deactivate-mark)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))))

  (bind-keys :map global-map
             ("M-y" . consult-yank-pop)
             :map +prefix-map
             ("b" . consult-buffer) ; orig. `switch-to-buffer'
             ("C-r" . consult-recent-file) ; orig. `find-file-read-only'
             :map ctl-x-4-map
             ("b" . consult-buffer-other-window) ; orig. `switch-to-buffer-other-window'
             :map ctl-x-5-map
             ("b" . consult-buffer-other-frame) ; orig. `switch-to-buffer-other-frame'
             :map +project-prefix-map
             ("b" . consult-project-buffer) ; orig. `project-switch-to-buffer'
             :map +registers-prefix-map
             ("b" . consult-bookmark) ; orig. `bookmark-jump'
             :map +tab-prefix-map
             ("b" . consult-buffer-other-tab) ; orig. `switch-to-buffer-other-tab'
             :map +goto-prefix-map
             ("b" . consult-bookmark) ("M-b" . consult-bookmark)
             ("e" . consult-compile-error) ("M-e" . consult-compile-error)
             ("f" . consult-flymake) ("M-f" . consult-flymake)
             ("g" . consult-goto-line) ("M-g" . consult-goto-line) ; orig. `goto-line'
             ("i" . consult-imenu) ("M-i" . consult-imenu)
             ("I" . consult-imenu-multi)
             ("k" . consult-kmacro) ("M-k" . consult-kmacro)
             ("m" . consult-mark) ("M-m" . consult-mark)
             ("M" . consult-global-mark)
             ("o" . consult-outline) ("M-o" . consult-outline) ; alt. `consult-org-heading'
             ("r" . consult-register) ("M-r" . consult-register)
             ("t" . +consult-tab) ("M-t" . +consult-tab)
             ("," . +consult-xref-history) ("M-," . +consult-xref-history)
             :map +search-prefix-map
             ("e" . consult-isearch-history)
             ("f" . consult-fd) ("M-f" . consult-fd)
             ("g" . +consult-ripgrep-dwim) ("M-g" . +consult-ripgrep-dwim)
             ("k" . consult-keep-lines) ("M-k" . consult-keep-lines)
             ("l" . +consult-line-dwim) ("M-l" . +consult-line-dwim)
             ("L" . consult-line-multi)
             ("u" . consult-focus-lines) ("M-u" . consult-focus-lines) ; C-u to unfocus
             :map +toggle-prefix-map
             ("t" . consult-theme)
             :map consult-narrow-map
             ;; Available filters are displayed with the `consult-narrow-help'
             ;; command at the prompt
             ("?" . consult-narrow-help)
             :map help-map
             ("C-i" . consult-info)
             :map isearch-mode-map
             ("M-s l" . consult-line) ("M-s M-l" . consult-line) ; needed by consult-line to detect Isearch
             ("M-s L" . consult-line-multi) ; needed by consult-line to detect Isearch
             ("M-e" . consult-isearch-history) ; orig. `isearch-edit-string'
             ("M-s e" . consult-isearch-history) ; orig. `isearch-edit-string'
             :map minibuffer-local-map
             ("M-s" . consult-history) ; orig. `next-matching-history-element'
             ("M-r" . consult-history) ; orig. `previous-matching-history-element'
             ))

(use-package consult-dir
  ;; `consult-dir' allows you to easily insert directory paths into the
  ;; minibuffer prompt in Emacs.

  ;; When using the minibuffer, you can switch -- with completion and filtering
  ;; provided by your completion setup -- to any directory you've visited
  ;; recently, or to a project, a bookmarked directory, or even a remote host
  ;; via TRAMP. The minibuffer prompt will be replaced with the directory you
  ;; choose.

  ;; Why would you want to do this?
  ;; To avoid navigating long distances when picking a file or directory in any
  ;; Emacs command that requires one.

  ;; It works with all Emacs commands that require you to specify file paths,
  ;; and with `embark' actions on files. The directory candidates are collected
  ;; from user bookmarks, `project' roots (if available), `projectile' roots (if
  ;; available), and `recentf' file locations. The `default-directory' variable
  ;; is not changed in the process. I provide custom sources that collect
  ;; candidates from the directories visited in the `zoxide' shell tool, and
  ;; docker containers.

  ;; You can use it to select a distant directory when copying a file with
  ;; `dired'. Think of it like the shell tools zoxide, fasd, or autojump but
  ;; for Emacs.

  ;; Call `consult-dir' when in the minibuffer to choose a directory with
  ;; completion and insert it into the minibuffer prompt, shadowing or replacing
  ;; the directory path showing currently. The file name part of the text is
  ;; retained. This lets the user switch to distant directories very quickly
  ;; when finding files, for instance.
  ;;
  ;; Call `consult-dir' from a regular buffer to choose a directory with
  ;; completion and then interactively find a file in that directory. The
  ;; command run with this directory is configurable via
  ;; `consult-dir-default-command' and defaults to `find-file'.
  ;;
  ;; Call `consult-dir-jump-file' from the minibuffer to asynchronously find a
  ;; file anywhere under the directory that is currently in the prompt. This can
  ;; be used with `consult-dir' to quickly switch directories and find files at
  ;; an arbitrary depth under them. It uses `consult-find' under the hood.

  :config
  ;; A function that returns a list of directories
  (defun +consult-dir--zoxide-dirs ()
    "Return list of zoxide dirs."
    (mapcar
     (lambda (dir)
       (file-name-as-directory (abbreviate-file-name dir)))
     (split-string (shell-command-to-string "zoxide query --list") "\n" t)))
  ;; A consult source that calls this function
  (defvar +consult-dir--source-zoxide
    `( :name     "Zoxide dirs"
       :narrow   ?z
       :category file
       :face     consult-file
       :history  file-name-history
       :enabled  ,(lambda () (executable-find "zoxide"))
       :items    ,#'+consult-dir--zoxide-dirs)
    "Zoxide directory source for `consult-dir'.")
  ;; Adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources '+consult-dir--source-zoxide t)

  ;; BUG Changing the `consult-dir-default-command' to (+shell--insert-and-send
  ;; "cd" default-directory) in +shell-mode makes `shell-dirtrack-mode' stop
  ;; working (cd commands issued to the shell no longer set the buffer's
  ;; default-directory). Is the `shell-directory-tracker' function in
  ;; `comint-input-filter-functions' no longer respected?
  ;;
  ;; This is why I have chosen to use the following function instead which
  ;; passes the selected directory directly to cd, meant to be used in
  ;; `shell-mode'.
  (defun +consult-dir-shell-cd ()
    "Choose a directory and cd to it."
    (declare (interactive-only t))
    (interactive)
    (+shell--insert-and-send "cd" (consult-dir--pick "In directory: ")))

  ;; TODO can i define a function in bash that calls consult-dir for
  ;; shell-mode. i know i can probably do this for vterm, but what about shell?
  ;; https://github.com/karthink/consult-dir/wiki#directories-visited-in-eshell
  ;; TODO add consult-dir source for docker containers using TRAMP
  ;; https://github.com/karthink/consult-dir/wiki#docker-containers-using-tramp

  (setopt consult-dir-shadow-filenames nil)

  (bind-keys :map +prefix-map
             ("C-d" . consult-dir)
             :map minibuffer-local-completion-map
             ("C-x C-d" . consult-dir)
             ("C-x C-j" . consult-dir-jump-file)
             :map vertico-map
             ("C-x C-j" . consult-dir-jump-file)
             :map shell-mode-map
             ("C-x C-d" . +consult-dir-shell-cd)))

(use-package embark
  ;; The `embark' package by Omar Antolin Camarena provides a mechanism to
  ;; perform relevant actions via keybindings in the given context. What
  ;; constitutes "the given context" depends on where the cursor is, such as if
  ;; it is at the end of a symbolic expression in Lisp code or inside the
  ;; minibuffer. The single point of entry is the `embark-act' command or
  ;; variants like `embark-dwim'.

  ;; With `embark-act' we gain access to a customisable list of commands for the
  ;; given context. If we are over a lisp symbol, one possible action is to
  ;; describe it (i.e. produce documentation about it). If we are browsing files
  ;; in the minibuffer, possible actions include file operations such as to
  ;; delete or rename the file. And so on for everything. The `embark-dwim'
  ;; command always performs the default action for the given context. It is
  ;; like invoking `embark-act' and then typing the `RET' key.

  ;; A killer feature of `embark' is how it acts on mibibuffer completion
  ;; candidates: it can collect a snapshot of them or place them in a buffer
  ;; whose major mode is appropriate for whatever they are (e.g. dired for files
  ;; and ibuffer for buffers). We can thus type something in the minibuffer to
  ;; narrow the results, then use `embark-collect' or `embark-export' to capture
  ;; the results and operate on them accordingly.

  ;; For example, if we are reading documentation about `embark-' and have 10
  ;; items there, we can "collect" the results in their own buffer and then
  ;; navigate it as if it were the minibuffer: `RET' will perform the action
  ;; that the actual minibuffer would have carried out (to show documentation,
  ;; in this case). Similarly, the "export" mechanism takes the completion
  ;; candidates out of the minibuffer, though it also puts them in a major mode
  ;; that is appropriate for them.

  ;; Depending on the configurations about the "indicator", the `embark-act'
  ;; command will display an informative buffer with keys and their
  ;; corresponding commands. We can control its placement the same way we do
  ;; with all well-behaved buffers (`display-buffer-alist').

  ;; One downside of `embark' is that it is hard to know what the context is. I
  ;; have had this experience myself several times, where I thought I was
  ;; targeting the URL at point while the actions were about Org source blocks,
  ;; headings, and whatnot. Embark is probably correct in such a case, though I
  ;; cannot make my brain think the way it expects.

  ;; Another downside, which is also true for `which-key', is the sheer number
  ;; of options for each context. I feel that the defaults should be more
  ;; conservative, to have 3-4 actions per context to make it easier to find
  ;; stuff. The package would then provide an easy way for users to opt in to
  ;; what they need (e.g. I do not care about acting on Org heading) rather than
  ;; having to opt out of a ton of contexts and their corresponding key
  ;; bindings.

  ;; Personaly, I would be content with a package that does the equivalent of
  ;; "collect" and "export". The rest is about organising keybindings and how
  ;; you approach a given task (workflow as opposed to core functionality).

  :config
  ;; Replace the key help with a completing-read interface.
  (setopt prefix-help-command #'embark-prefix-help-command)

  ;; Truncate lines in `embark-collect-mode'
  (add-hook 'embark-collect-mode-hook #'toggle-truncate-lines)

  (setopt embark-confirm-act-all nil
          embark-mixed-indicator-both nil
          embark-mixed-indicator-delay 1.0
          embark-indicators '(embark-minimal-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))

  (defun +embark-isearch-backward ()
    "Prompt for string in the minibuffer and start isearch backwards.
Unlike isearch, this command reads the string from the
minibuffer, which means it can be used as an Embark action."
    (interactive)
    (isearch-mode nil)
    (isearch-edit-string)
    ;; Make sure isearch-lazy-count is updated on first invocation of
    ;; embark-isearch commands
    (when isearch-lazy-count
      (run-at-time 0 nil #'isearch-update)))

  (defun +embark-isearch-forward ()
    "Prompt for string in the minibuffer and start isearch forwards.
Unlike isearch, this command reads the string from the
minibuffer, which means it can be used as an Embark action."
    (interactive)
    (isearch-mode t)
    (isearch-edit-string)
    ;; Make sure isearch-lazy-count is updated on first invocation of
    ;; embark-isearch commands
    (when isearch-lazy-count
      (run-at-time 0 nil #'isearch-update)))

  (defun +embark-select-next-line ()
    "Like `embark-select' but also moves to next line."
    (interactive)
    (embark-select)
    (next-line))

  (bind-keys :map global-map
             ("C-." . embark-act)
             ("C-*" . embark-act-all)
             :map minibuffer-local-map
             ("C-*" . embark-act-all)
             ("C-c C-c" . embark-collect)
             ("C-c C-e" . embark-export)
             ("C-c C-i" . embark-select)
             :map help-map
             ("b" . embark-bindings)
             :map embark-general-map
             ("C-s" . +embark-isearch-forward)
             ("C-r" . +embark-isearch-backward)
             :map embark-collect-mode-map
             ("m" . +embark-select-next-line)))

;; Needed for correct exporting while using Embark with Consult commands.
(use-package embark-consult)

(use-package corfu
  ;; `corfu' handles in-buffer text completion splendidly using Emacs'
  ;; underlying infrastructure for `completion-at-point-functions'.
  ;;
  ;; However, automatic in-buffer text completion distracts me. I don't want
  ;; things eagerly popping in and out of my view. I want manual completion. Pop
  ;; up only when I say so. So completion is triggered with the `TAB' key,
  ;; producing a popup where the cursor is. See my `tabs' configuration.
  ;;
  ;; On that note, I set `corfu-preview-current' to nil because I don't want the
  ;; selected candidates to insert themselves into my buffer without my
  ;; direction, once again with the `TAB' key (`corfu-complete') while a `corfu'
  ;; popup is active.
  ;;
  ;; Since I am doing manual completion, that lets me use `SPC' for separator
  ;; insertion while a corfu popup is active. This means I get all the benefits
  ;; of `orderless' with `corfu'.
  ;;
  ;; `corfu-popupinfo-mode' will show a secondary documentation popup if we move
  ;; over a candidate but do not to anything with it.
  :config
  ;; (global-corfu-mode)
  (corfu-popupinfo-mode 1)

  (setopt corfu-cycle t
          corfu-preview-current nil
          corfu-min-width 20
          corfu-popupinfo-delay '(0.25 . 0.25))

  ;; Sort by input history
  (with-eval-after-load 'savehist
    (cl-pushnew 'corfu-history savehist-additional-variables))
  (corfu-history-mode 1)

  (defun +corfu-quit-and-newline ()
    "Quit Corfu completion and insert newline or submit input."
    (interactive)
    (progn
      (corfu-quit)
      (cond
       ((and (derived-mode-p 'eshell-mode)
             (fboundp 'eshell-send-input))
        (eshell-send-input))
       ((derived-mode-p 'comint-mode)
        (comint-send-input))
       ((derived-mode-p 'minibuffer-mode)
        (exit-minibuffer))
       (t
        (default-indent-new-line)))))

  (defun +corfu-completion-at-point ()
    "Perform completion on the text around point, using `corfu'.

The completion method is determined by completion-at-point-functions."
    (interactive)
    (let ((completion-in-region-function #'corfu--in-region))
      (completion-at-point)))
  (bind-keys :map global-map
             ("C-M-i" . +corfu-completion-at-point)
             :map emacs-lisp-mode-map
             ("C-M-i" . +corfu-completion-at-point))

  (bind-keys :map corfu-map
             ("C-h" . corfu-info-documentation)
             ("C-v" . corfu-popupinfo-scroll-up)
             ("M-v" . corfu-popupinfo-scroll-down)
             ("M-." . corfu-info-location)
             ("SPC" . corfu-insert-separator)
             ("TAB" . corfu-complete)
             ("RET" . +corfu-quit-and-newline)))

(use-package cape
  ;; Cape provides completion-at-point extensions. That is, it provides
  ;; `completion-at-point' functions which you can add to the
  ;; `completion-at-point-functions' list, which makes the backends available
  ;; for completion.
  ;;
  ;; Notable capfs are `cape-line' for completion of a line from the current
  ;; buffer, `cape-history' for history completion in shell or Comint modes,
  ;; `cape-file' for completion of file names, and `cape-elisp-symbol' +
  ;; `cape-elisp-block' for completion of Elisp symbols anywhere.
  ;;
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'. The order of the functions matters, as the
  ;; first capf returning a result wins and the later capfs may not get a chance
  ;; to run. One must distinguish the buffer-local and the global value of the
  ;; `completion-at-point-functions' variable. The buffer-local value of the
  ;; list takes precedence, but if the buffer-local list contains the symbol `t'
  ;; at the end, it means that the functions specified in the global list should
  ;; be executed afterwards. The special meaning of the value `t' is a feature
  ;; of the `run-hooks' function, see the section "Running Hooks" it the Elisp
  ;; manual for further information.
  :config
  ;; In order to merge capfs you can try the functions `cape-capf-super'. It is
  ;; only necessary if you want to combine multiple capfs, such that the
  ;; candidates from multiple sources appear together in the completion list at
  ;; the same time. `cape-capf-super' is not needed if multiple capfs should be
  ;; tried one after the other, for example you can use `cape-file' together
  ;; with programming capfs by adding `cape-file' to the
  ;; `completion-at-point-functions' list. File completion will then be
  ;; available in comments and string literals, but not in normal code.
  (defun +cape-dabbrev-dict-keyword ()
    "Merges the dabbrev, dict, and keyword cape capfs to display candidates
together."
    (cape-wrap-super #'cape-dabbrev #'cape-dict #'cape-keyword))

  (bind-keys :map global-map
             ("C-<tab>" . cape-prefix-map)))

;; TODO https://protesilaos.com/emacs/dotemacs#h:fd84b79a-351e-40f0-b383-bf520d77834b
;; (use-package abbrev)

(use-package dabbrev
  :config
  ;; The built-in `dabbrev' package provides a text completion method that reads
  ;; the contents of a buffer and expands the text before the cursor to match
  ;; possible candidates. This is done with `M-/' (`dabbrev-expand') which is
  ;; what I use most of the time to perform in-buffer completions.
  ;;
  ;; I like `dabbrev' because it is minimal. It does not produce any popup or
  ;; affect the window layout so it is keeping me focused on what I am doing. I
  ;; wish it had a behaviour where we could initial it and at any point demand a
  ;; fully fledged minibuffer presentation of what it is trying to match,
  ;; instead of cycling through the candidates with repeated `M-/'. Granted, I
  ;; normally do not cycle in that way, as I typically type out enough to get an
  ;; exact match or be one `M-/' away from it.
  ;;
  ;; Apart from the `dabbrev-expand' command, we have `dabbrev-completion'. I do
  ;; not use it because it does not feel natural while typing prose to stop,
  ;; check the minibuffer for some text, select it, and go back to typing.

  (setopt dabbrev-abbrev-skip-leading-regexp "[$*/=~']"
          dabbrev-case-fold-search nil
          dabbrev-upcase-means-case-search t
          dabbrev-ignored-buffer-modes
          '(archive-mode image-mode docview-mode pdf-view-mode)))

;;;;;;;;;;;;;;;;;;
;;;; snippets ;;;;

(use-package tempel
  ;; This is yet another nimble package from Daniel Mendler. It lets us define a
  ;; template which we can insert at any point and fill in the empty fields with
  ;; whatever values we want.

  ;; I integrate `tempel-complete' with my completion setup through `cape' so
  ;; that templates appear in the same interface as other completions and expand
  ;; with the same key. Having a single entry point for snippet expansion and
  ;; context-based completion makes the overall experience more coherent. Even
  ;; though snippet expansion takes priority in this arrangement, the trade-off
  ;; feels worthwile.
  ;;
  ;; By contrast, I keep `dabbrev' outside of this system. I use it as a
  ;; complementary mechanism, not as another source within the completion
  ;; pipeline. Including all of its candidates in the main completion list would
  ;; drown out more relevant suggestions and make the interface noisy.
  :config
  (defalias '+capf-eglot-tempel
    (cape-capf-super #'eglot-completion-at-point
                     :with (cape-capf-prefix-length #'tempel-complete 2)))
  (defalias '+capf-elisp-tempel
    (cape-capf-super #'elisp-completion-at-point
                     :with (cape-capf-prefix-length #'tempel-complete 2)))

  (defun +capf-setup-eglot (&optional global)
    "Add capfs to GLOBAL hook if non-nil, else local."
    (when (eglot-managed-p)
      (let ((local (not global)))
        (add-hook 'completion-at-point-functions
                  #'+capf-eglot-tempel -10 'local)
        (add-hook 'completion-at-point-functions
                  #'cape-file -10 'local)
        (when local
          ;; Do not run the global hook; everything is already added locally.
          (delq t completion-at-point-functions)))))
  (add-hook 'eglot-managed-mode-hook #'+capf-setup-eglot)

  (defun +capf-setup-elisp (&optional global)
    "Add capfs to GLOBAL hook if non-nil, else local."
    (let ((local (not global)))
      (add-hook 'completion-at-point-functions
                #'+capf-elisp-tempel -10 'local)
      (add-hook 'completion-at-point-functions
                #'cape-file -10 'local)
      (when local
        ;; Do not run the global hook; everything is already added locally.
        (delq t completion-at-point-functions))))
  (add-hook 'emacs-lisp-mode-hook #'+capf-setup-elisp)

  (defun +capf-setup (&optional global)
    "Add capfs to GLOBAL hook if non-nil, else local."
    (let ((local (not global)))
      (add-hook 'completion-at-point-functions #'tempel-complete -10 'local)
      (add-hook 'completion-at-point-functions #'cape-file -10 'local)
      (when local
        ;; Do not run the global hook; everything is already added locally.
        (delq t completion-at-point-functions))))
  (+capf-setup t)

  (bind-keys :map tempel-map
             ("C-g" . tempel-done)
             ("M-n" . tempel-next)
             ("M-p" . tempel-previous)))

;; TODO document tempel-collection
(use-package tempel-collection)

;;;;;;;;;;;;;;;;;
;;;; project ;;;;

(use-package project
  ;; In Emacs parlance, a "project" is a collection of files and/or directories
  ;; that share the same root. The root of a project is identified by a special
  ;; file or directory, with `.git/' being one of the defaults as it is a
  ;; version control system supported by the built-in `vc.el'.

  :config
  ;; We can specify more project roots as a list of strings in the user option
  ;; `project-vc-extra-root-markers'. I work exclusively with Git repositories,
  ;; so I add there a `.project' file in case I ever need to register a project
  ;; without it being controlled by `git'. In that case, the `.project' file is
  ;; just and empty file in a directory that I want to treat as the root of this
  ;; project.
  (setopt project-vc-extra-root-markers '(".project"))

  ;; The common way to switch to a project is to type `C-x p p', which calls the
  ;; command `project-switch-project'. It lists all registered projects and also
  ;; includes a `... (choose a dir)' option. By choosing a new directory, we
  ;; register it in our project list if it has a recognizable root. Once we
  ;; select a project, we are presented with a list of common actions to start
  ;; working on the project. These are defined in the user option
  ;; `project-switch-commands' and are activated by an assigned key.
  (setopt project-switch-commands '((project-switch-to-buffer "Buffer" ?b)
                                    (project-dired "Dired" ?d)
                                    (project-find-file "File" ?f)
                                    (+project-consult-grep "Grep" ?g)
                                    (+shell-pop-to-buffer "Shell" ?s)
                                    (magit-project-status "VC" ?v)
                                    (project-compile "Compile" ?,)
                                    (project-async-shell-command "Async Command" ?&)
                                    (project-shell-command "Command" ?!)
                                    (keyboard-quit "Cancel" ?\C-g)))

  ;; While inside a project, we have many commands that operate on the project
  ;; level. For example, `project-find-file' searches for a file across the
  ;; project, while `project-switch-to-buffer' switches to a buffer that is
  ;; specific to the project.
  ;;
  ;; If any of the `project.el' commands is called from outside a project, it
  ;; first prompts for a project and then carries out its action. For example,
  ;; `project-find-file' will ask for a project to use, then switch to it, then
  ;; prompt for a file inside of the specified project.

  (defun +project-consult-grep (&optional dir initial)
    "Search with `grep' for files in DIR with INITIAL input with
`consult-project-function' set to the default project function."
    (interactive)
    (let ((consult-project-function 'consult--default-project-function))
      (consult-grep dir initial)))

  (bind-keys :map +project-prefix-map
             ;; ("b" . project-switch-to-buffer) ; alt. `consult-project-buffer'
             ("d" . project-dired)
             ("e" . project-eshell)
             ("f" . project-find-file)
             ("g" . +project-consult-grep)
             ("k" . project-kill-buffers)
             ("p" . project-switch-project)
             ("r" . project-query-replace-regexp)
             ("v" . project-vc-dir)
             ("," . project-compile)
             ("&" . project-async-shell-command)
             ("!" . project-shell-command)))

;; TODO <https://github.com/MatthewTromp/list-projects>
;; (use-package list-projects
;;   :config
;;   (bind-keys :map +project-prefix-map
;;              ("l" . list-projects)))

;;;;;;;;;;;;;;;;;
;;;; buffers ;;;;

;; NOTE document buffer
(use-package buffers
  :no-require
  :config
  (defun +kill-this-buffer (&optional arg)
    "Kill the current buffer.

With optional prefix ARG (\\[universal-argument]) delete the buffer's
window as well."
    (interactive "P")
    (let ((proc (get-buffer-process (current-buffer))))
      (when (and proc (process-live-p proc))
        (kill-process proc)))
    (let ((kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions)))
      (if (and arg (not (one-window-p)))
          (kill-buffer-and-window))
      (kill-this-buffer)))

  (defun +rename-file-and-buffer (name)
    "Apply NAME to current file and rename its buffer."
    (interactive
     (list (read-file-name "Rename current file: " (buffer-file-name))))
    (let ((file (buffer-file-name)))
      (if (vc-registered file)
          (vc-rename-file file name)
        (rename-file file name))
      (set-visited-file-name name t t)))

  (defun +buffers--major-mode-prompt ()
    "Prompt of `+buffers-major-mode'.
Limit list of buffers to those matching the current
`major-mode' or its derivatives."
    (let ((read-buffer-function nil)
          (current-major-mode major-mode))
      (read-buffer
       (format "Buffer for %s: " major-mode)
       nil
       :require-match
       (lambda (pair) ; pair is (name-string . buffer-object)
         (with-current-buffer (cdr pair)
           (derived-mode-p current-major-mode))))))

  (defun +buffers-major-mode ()
    "Select BUFFER matching the current one's major mode."
    (interactive)
    (switch-to-buffer (+buffers--major-mode-prompt)))

  (bind-keys :map global-map
             ("M-s b" . +buffers-major-mode)
             ("C-<next>" . end-of-buffer)
             ("C-<prior>" . beginning-of-buffer)
             :map +prefix-map
             ("k" . +kill-this-buffer)
             ("C-b" . ibuffer)
             ("<right>" . next-buffer)
             ("<left>" . previous-buffer)
             ;; :map +buffer-prefix-map
             ;; ("c" . clone-indirect-buffer-other-window)
             ;; ("g" . revert-buffer-quick)
             ;; ("k" . +kill-this-buffer)
             ;; ("m" . +buffers-major-mode) ; (prot) if i can filter in consult-buffer by major mode i don't need this
             ;; ("r" . +rename-file-and-buffer)
             ;; ("v" . +buffers-vc-root) ; (prot) if i can filter in consult-buffer by vc root i don't need this
             ))

(use-package uniquify
  ;; When a buffer name is reserved, Emacs tries to produce the new buffer by
  ;; finding a suitable variant of the original name. The doc string of the
  ;; variable `uniquify-buffer-name-style' does a good job at explaining the
  ;; various patterns:
  ;;
  ;; For example, the files /foo/bar/mumble/name and /baz/quux/mumble/name would
  ;; have the following buffer names in the various styles:
  ;;
  ;;   forward                       bar/mumble/name    quux/mumble/name
  ;;   reverse                       name\mumble\bar    name\mumble\quux
  ;;   post-forward                  name|bar/mumble    name|quux/mumble
  ;;   post-forward-angle-brackets   name<bar/mumble>   name<quux/mumble>
  ;;   nil                           name               name<2>
  ;;
  ;; I use the forward style, which is the closest to the actual file name.
  :config
  (setopt uniquify-buffer-name-style 'forward))

;;;;;;;;;;;;;;;;;
;;;; windows ;;;;

(use-package window
  :no-require
  :config
  (defun +window-toggle-split ()
    "Toggle between horizontal and vertical window splits, preserving buffer
  layout."
    (interactive)
    (let ((current-buffers (mapcar #'window-buffer (window-list))) ; List of buffers in current windows
          (split-direction (if (= (window-width) (frame-width))
                               'vertical
                             'horizontal)))
      (delete-other-windows)
      ;; Toggle the split direction
      (if (eq split-direction 'horizontal)
          (split-window-vertically)
        (split-window-horizontally))
      ;; Restore buffers to the new windows
      (let ((windows (window-list)))
        (cl-loop for buffer in current-buffers
                 for window in windows
                 do (set-window-buffer window buffer)))))

  ;; Make `other-window' split the frame when there's only one window, giving
  ;; the command a use when it has none.
  (advice-add 'other-window :before
              (defun +other-window-split-if-single (&rest _)
                "Split the frame if there is a single window."
                (when (one-window-p) (split-window-sensibly))))

  (bind-keys
   :map global-map
   ("M-o" . other-window)
   :map +prefix-map
   ("o" . other-window)
   ("0" . delete-window) ; `s-k' or `s-0'
   ("1" . delete-other-windows) ; `s-K' or `s-1'
   ("2" . split-window-below) ; `s-s' or `s-2'
   ;; ("M-2" . +split-window-below-and-focus) ; lambda emacs
   ("3" . split-window-right) ; `s-v' or `s-3'
   ;; ("M-3" . +split-window-right-and-focus) ; lambda emacs
   ("4" . ctl-x-4-prefix)
   ("5" . ctl-x-5-prefix)
   ;; ("6" . )
   ;; ("7" . )
   ;; ("8" . )
   ;; ("9" . )
   ;; ("*" . )
   ;; ("&" . )
   ("+" . balance-windows-area) ; `s-+'
   ("#" . server-edit) ; `s-#'
   :map +window-prefix-map
   ;; TODO make all these into s-* keybinds with sxhkd
   ("-" . fit-window-to-buffer) ; `s--'
   ("^" . tear-off-window) ; ^ should be tear or detach
   ("0" . delete-windows-on)
   ("1" . delete-other-windows-vertically) ; `s-!'
   ("2" . split-root-window-below) ; `s-S' or `s-@'
   ("3" . split-root-window-right) ; `s-V' or `s-\#'
   ("d" . toggle-window-dedicated)
   ("q" . quit-window)
   ("s" . window-toggle-side-windows)
   ;; windmove
   ("a" . windmove-up)
   ("A" . windmove-swap-states-up)
   ("e" . windmove-right)
   ("E" . windmove-swap-states-right)
   ("h" . windmove-down)
   ("H" . windmove-swap-states-down)
   ("o" . other-window)
   ("r" . +window-toggle-split)
   ;; ("r" . window-layout-transpose) ; Emacs 31
   ;; ("R" . rotate-windows-back) ; Emacs 31
   ("y" . windmove-left)
   ("Y" . windmove-swap-states-left)))

(use-package display-buffer
  :no-require
  :config
  ;; The `display-buffer-alist' is a powerful user option and somewhat hard to
  ;; get started with. The reason for its difficulty comes from the knowledge
  ;; required to understand the underlying `display-buffer' mechanism.

  ;; Here is the gist of what we do with it:
  ;;
  ;; - The alist is a list of lists.
  ;; - Each element of the alist (i.e. one of the lists) is of the following
  ;;   form:
  ;;       (BUFFER-MATCHER
  ;;        FUNCTIONS-TO-DISPLAY-BUFFER
  ;;        OTHER-PARAMETERS)
  ;; - The `BUFFER-MATCHER' is either a regular expression to match the buffer
  ;;   by its name or a method to get the buffer whose major mode is the one
  ;;   specified. In the latter case, you will see the use of cons cells (like
  ;;   (one . two)) involving the `derived-mode' symbol.
  ;; - The `FUNCTIONS-TO-DISPLAY-BUFFER' is a list of `display-buffer' functions
  ;;   that are tried in the order they appear in until one works. The list can
  ;;   be of one element, as you will notice with some of my entries.
  ;; - The `OTHER-PARAMETERS' are enumerated in the Emacs Lisp Reference
  ;;   Manual. Evaluate `(info "(elisp) Buffer Display Action Alists")'

  ;; Here are some commonly used `FUNCTIONS-TO-DISPLAY-BUFFER' functions
  ;; (non-exhaustive):
  ;;
  ;;  `display-buffer-same-window': Display the buffer in the currently selected
  ;;     window.
  ;;  `display-buffer-reuse-window': Reuse any window already displaying the
  ;;     buffer.
  ;;  `display-buffer-reuse-mode-window': Reuse a window showing a buffer in the
  ;;     same major mode as the one to display.
  ;;  `display-buffer-in-previous-window': Reuse the window that most recently
  ;;     displayed the buffer, if it still exists.
  ;;  `display-buffer-use-some-window': Try to reuse any existing window that
  ;;     is suitable.
  ;;  `display-buffer-pop-up-window': Pop up a new window for displaying the
  ;;     buffer.
  ;;  `display-buffer-below-selected': Use or create a window below the
  ;;     currently selected one.
  ;;  `display-buffer-at-bottom': Use or create a window at the bottom of the
  ;;     current frame.
  ;;  `display-buffer-pop-up-frame': Display the buffer in a new top-level
  ;;     frame.
  ;;  `display-buffer-in-child-frame': Display the buffer in a child frame
  ;;     attached to the current one.
  ;;  `display-buffer-no-window': Do not display the buffer at all and return
  ;;     nil immediately.

  (defun +display-buffer-use-some-other-window (buffer alist)
    "Display BUFFER in some other existing window.

Like `display-buffer-use-some-window', but never reuse the selected
window if it's the only one."
    (when (cdr (window-list))
      (display-buffer-use-some-window buffer alist)))

  ;; Here are some commonly used `OTHER-PARAMETERS' (non-exhaustive):
  ;;
  ;;  `inhibit-same-window': A non-nil value prevents the same
  ;;     window from being used for display.
  ;;  `inhibit-switch-frame': A non-nil value prevents any frame
  ;;     used for showing the buffer from being raised or selected.
  ;;  `reusable-frames': The value specifies the set of frames to
  ;;     search for a window that already displays the buffer.
  ;;     Possible values are nil (the selected frame), t (any live
  ;;     frame), visible (any visible frame), 0 (any visible or
  ;;     iconified frame) or an existing live frame.
  ;;  `pop-up-frame-parameters': The value specifies an alist of
  ;;     frame parameters to give a new frame, if one is created.
  ;;  `window-height': The value specifies the desired height of the
  ;;     window chosen and is either an integer (the total height of
  ;;     the window), a floating point number (the fraction of its
  ;;     total height with respect to the total height of the frame's
  ;;     root window) or a function to be called with one argument -
  ;;     the chosen window.  The function is supposed to adjust the
  ;;     height of the window; its return value is ignored.  Suitable
  ;;     functions are `shrink-window-if-larger-than-buffer' and
  ;;     `fit-window-to-buffer'.
  ;;  `window-width': The value specifies the desired width of the
  ;;     window chosen and is either an integer (the total width of
  ;;     the window), a floating point number (the fraction of its
  ;;     total width with respect to the width of the frame's root
  ;;     window) or a function to be called with one argument - the
  ;;     chosen window.  The function is supposed to adjust the width
  ;;     of the window; its return value is ignored.
  ;;  `preserve-size': The value should be either (t . nil) to
  ;;     preserve the width of the chosen window, (nil . t) to
  ;;     preserve its height or (t . t) to preserve its height and
  ;;     width in future changes of the window configuration.
  ;;  `window-parameters': The value specifies an alist of window
  ;;     parameters to give the chosen window.
  ;;  `allow-no-window': A non-nil value means that `display-buffer'
  ;;     may not display the buffer and return nil immediately.

  (setq display-buffer-base-action
        '((display-buffer-reuse-mode-window
           display-buffer-reuse-window
           +display-buffer-use-some-other-window
           display-buffer-pop-up-window)
          (some-window . mru)
          (reusable-frames . nil)))

  (add-to-list 'display-buffer-alist
               '("^\\*Org Links\\*$"
                 (display-buffer-no-window)
                 (allow-no-window . t)))

  (add-to-list 'display-buffer-alist
               '("\\*\\(Org \\(Select\\|Note\\)\\|Agenda Commands\\)\\*\\'"
                 (display-buffer-in-side-window)
                 (window-height . 0.3)
                 (dedicated . t)
                 (side . bottom)
                 (slot . 0)))

  ;; The following settings are relevant for the `display-buffer-alist' we saw
  ;; right above. Notice, in particular, the `split-height-threshold' and
  ;; `split-width-threshold' which determine when to split the frame by height
  ;; or width. These are relevant for `+window-display-buffer-below-or-pop' and
  ;; the other more basic functions I have defined for this purpose.

  (setopt window-combination-resize t
          even-window-sizes 'height-only
          window-sides-vertical nil
          switch-to-buffer-in-dedicated-window 'pop
          split-height-threshold 40
          split-width-threshold 155 ; should be above half of `window-width'
          window-min-height 4
          window-min-width 10)

  ;; Respects display actions when switching buffers
  (setopt switch-to-buffer-obey-display-actions t)
  ;; Ensure Org src buffers are opened using display-buffer
  (setopt org-src-window-setup 'plain))

(use-package horizontal-splits
  :no-require
  :config
  ;; This is a port from Emacs 31
  (defcustom split-window-preferred-direction 'horizontal
    "The first direction tried when Emacs needs to split a window.
This variable controls in which order `split-window-sensibly' will try to
split the window.  That order specially matters when both dimensions of
the frame are long enough to be split according to
`split-width-threshold' and `split-height-threshold'.  If this is set to
`vertical' (the default), `split-window-sensibly' tries to split
vertically first and then horizontally.  If set to `horizontal' it does
the opposite.  If set to `longest', the first direction tried
depends on the frame shape: in landscape orientation it will be like
`horizontal', but in portrait it will be like `vertical'.  Basically,
the longest of the two dimension is split first.

If both `split-width-threshold' and `split-height-threshold' cannot be
satisfied, it will fallback to split vertically.

See `split-window-preferred-function' for more control of the splitting
strategy."
    :type '(radio
            (const :tag "Try to split vertically first"
             vertical)
            (const :tag "Try to split horizontally first"
             horizontal)
            (const :tag "Try to split along the longest edge first"
             longest))
    :version "31.1"
    :group 'windows)

  (defun window--try-vertical-split (window)
    "Helper function for `split-window-sensibly'"
    (when (window-splittable-p window)
      (with-selected-window window
        (split-window-below))))

  (defun window--try-horizontal-split (window)
    "Helper function for `split-window-sensibly'"
    (when (window-splittable-p window t)
      (with-selected-window window
        (split-window-right))))

  (defun split-window-sensibly (&optional window)
    "Split WINDOW in a way suitable for `display-buffer'.
The variable `split-window-preferred-direction' prescribes an order of
directions in which Emacs should try to split WINDOW.  If that order
mandates starting with a vertical split, and `split-height-threshold'
specifies an integer that is at least as large a WINDOW's height, split
WINDOW into two windows one below the other and return the lower one.
If that order mandates starting with a horizontal split, and
`split-width-threshold' specifies an integer that is at least as large
as WINDOW's width, split WINDOW into two windows side by side and return
the one on the right.

In either case, if the first attempt to split WINDOW fails, try to split
the window in the other direction in the same manner as described above.
If that attempt fails too, and WINDOW is the only window on its frame,
try splitting WINDOW into two windows, one below the other, disregarding
the value of `split-height-threshold' and return the window on the
bottom.

By default `display-buffer' routines call this function to split
the largest or least recently used window.  To change the default
customize the option `split-window-preferred-function'.

You can enforce this function to not split WINDOW horizontally,
by setting (or binding) the variable `split-width-threshold' to
nil.  If, in addition, you set `split-height-threshold' to zero,
chances increase that this function does split WINDOW vertically.

In order to not split WINDOW vertically, set (or bind) the
variable `split-height-threshold' to nil.  Additionally, you can
set `split-width-threshold' to zero to make a horizontal split
more likely to occur.

Have a look at the function `window-splittable-p' if you want to
know how `split-window-sensibly' determines whether WINDOW can be
split."
    (let ((window (or window (selected-window))))
      (or (if (or
               (eql split-window-preferred-direction 'horizontal)
               (and (eql split-window-preferred-direction 'longest)
                    (> (frame-width) (frame-height))))
              (or (window--try-horizontal-split window)
                  (window--try-vertical-split window))
            (or (window--try-vertical-split window)
                (window--try-horizontal-split window)))
          (and
           ;; If WINDOW is the only usable window on its frame (it is
           ;; the only one or, not being the only one, all the other
           ;; ones are dedicated) and is not the minibuffer window, try
           ;; to split it vertically disregarding the value of
           ;; `split-height-threshold'.
           (let ((frame (window-frame window)))
             (or
              (eq window (frame-root-window frame))
              (catch 'done
                (walk-window-tree (lambda (w)
                                    (unless (or (eq w window)
                                                (window-dedicated-p w))
                                      (throw 'done nil)))
                                  frame nil 'nomini)
                t)))
           (not (window-minibuffer-p window))
           (let ((split-height-threshold 0))
             (window--try-vertical-split window)))))))

;;;;;;;;;;;;;;
;;;; tabs ;;;;

(use-package tab-bar
  ;; Emacs comes with two distinct built-in notions of "tabs": (i) standalone
  ;; window+buffer layouts which we may also call "workspaces", and (ii) buffers
  ;; as buttons for a visual representation and navigation tool to switch
  ;; between buffers. The latter is provided by the `tab-line-mode', which I
  ;; have no use for. I rely on `switch-to-buffer' and to a lesser extent
  ;; `previous-buffer' and `next-buffer' (the latter two move back and forth in
  ;; the given window's history of visible buffers).
  ;;
  ;; The `tab-bar-mode', however, fills a special niche. It is useful when I
  ;; cannot rely on separate frames to keep a sense of context or order to what
  ;; I am working on. For me, the most efficient workflow involves a singular
  ;; maximised frame, rather than many frames distributed across the desktop[s].
  ;; (That is until I decide to further integrate my window manager with Emacs
  ;; and have every Emacs window as a separate frame managed by the window
  ;; manager, but tab-bar-mode will still be useful).
  ;; TODO: <https://sqrtminusone.xyz/posts/2021-10-04-emacs-i3/> and
  ;; <https://github.com/davidshepherd7/frames-only-mode>
  ;;
  ;; I enforce each tab holding its own project with `otpp'. I enforce
  ;; separation between tabs such that each tab operates on its own distinct
  ;; list of buffers with `bufferlo'. The same also offers lightweight Emacs
  ;; bookmarks-based persistence for these sets of tabs.
  :config
  (setopt tab-bar-new-button nil
          tab-bar-close-button-show nil))

;; TODO: document modern-tab-bar. this can probably just live under tab-bar
;; <https://github.com/aaronjensen/emacs-modern-tab-bar>
;; (use-package modern-tab-bar
;;   :config
;;   (modern-tab-bar-mode))

;; TODO: document one-tab-per-project
;; <https://github.com/abougouffa/one-tab-per-project>
;; (use-package otpp
;;   :config
;;   (otpp-mode 1)
;;   ;; I want to advice the commands in `otpp-override-commands' to be run in the
;;   ;; current tab's root directory.
;;   (otpp-override-mode 1))

;; TODO: document bufferlo <https://github.com/florommel/bufferlo>
;; (use-package bufferlo)

;;;;;;;;;;;;;;;;;;;;
;;;; navigation ;;;;

(use-package isearch
  ;; Emacs provides lots of useful facilities to search the contents of buffers
  ;; or files. The most common scenario is to type `C-s' (`isearch-forward') to
  ;; perform a search forward from point or `C-r' (`isearch-backward') to do so
  ;; in reverse. These commands pack a ton of functionality and they integrate
  ;; nicely with related facilities, such as those of (i) permanently
  ;; highlighting the thing being searched, (ii) putting all results in a buffer
  ;; that is useful for navigation purposes, among others, and (iii) replacing
  ;; the given matching items with another term.

  ;; Here I summarise the functionality:

  ;; `C-s' (`isearch-forward')
  ;; Search forward from point (incremental search); retype `C-s' to move forth.
  ;;
  ;; `C-r' (`isearch-backward')
  ;; Search backward from point (incremental); retype `C-r' to move back. While
  ;; using either `C-r' and `C-r' you can move in the opposite direction with
  ;; either of those keys when performing a repeat.
  ;;
  ;; `C-M-s' (`isearch-forward-regexp')
  ;; Same as `C-s' but matches a regular expression. The `C-s' and `C-r' motions
  ;; are the same after the matches are found.
  ;;
  ;; `C-M-r' (`isearch-backward-regexp')
  ;; The counterpart of the above `C-M-s' for starting in reverse.
  ;;
  ;; `C-s C-w' (`isearch-yank-word-or-char')
  ;; Search forward for word at point. Again, `C-s' and `C-r' move forth and
  ;; back, respectively.
  ;;
  ;; `C-r C-w' (`isearch-yank-word-or-char')
  ;; Same as above, but backward.
  ;;
  ;; `M-s o' (`occur')
  ;; Search for the given regular expression throughout the buffer and collect
  ;; the matches in an `*occur*' buffer.
  ;;
  ;; `C-u 5 M-s o' (`occur')
  ;; Like the above, but give it N lines of context where N is the prefix
  ;; numeric argument (5 in this example).
  ;;
  ;; `C-s SEARCH' followed by `M-s o' (`isearch-forward' -> `occur')
  ;; Like `C-s' but then put the matches in an `*occur*' buffer.
  ;;
  ;; `C-s SEARCH' followed by `C-u 5 M-s o' (`isearch-forward' -> `occur')
  ;; Same as above, but now with N lines of context (5 in this example).
  ;;
  ;; `M-%' (`query-replace')
  ;; Prompt for target to replace and then prompt for its replacement
  ;;
  ;; `C-M-%' (`query-replace-regexp')
  ;; Same as above, but matches a regular expression.
  ;;
  ;; `C-s SEARCH' followed by `M-%' (`isearch-forward' -> `query-replace')
  ;; Search with `C-s' and then perform a query-replace for the following
  ;; matches.
  ;;
  ;; `C-M-s SEARCH' followed by `M-%' (`isearch-forward' ->
  ;; `query-replace-regexp')
  ;; As above, but regexp aware.
  ;;
  ;; `C-s SEARCH C-M-%' (`isearch-forward' -> `query-replace-regexp')
  ;; Same as above.
  ;;
  ;; For starters, just learn: `C-s', `C-r', `M-s o', and `M-%'
  :config
  ;; The first thing I want to do for Isearch is make it more convenient for me
  ;; to match words that occur in sequence but are not necessarily following
  ;; each other. By default, we can do that with something like
  ;; `isearch-forward-regexp' followed by `one.*two'. Though it is inconvenient
  ;; to be a regexp-aware search mode when all we want is to type "one two" and
  ;; have the space be interpreted as "intermediate characters" rather than a
  ;; literal space. The following does exactly this for regular
  ;; `isearch-forward' and `isearch-backward'.
  (setopt search-whitespace-regexp ".*?"
          isearch-lax-whitespace t
          isearch-regexp-lax-whitespace nil)

  ;; Here I am just tweaking the delay that affects when deferred highlights are
  ;; applied. The current match is highlighted immediately. The rest are done
  ;; after `lazy-highlight-initial-delay' unless they are longer in character
  ;; count than `lazy-highlight-no-delay-length'.
  (setopt search-highlight t
          isearch-lazy-highlight t
          lazy-highlight-initial-delay 0.5
          lazy-highlight-no-delay-length 4)

  ;; I think the following options should be enabled by default. They produce a
  ;; counter next to the Isearch prompt that shows the position of the current
  ;; match relative to the total count (like 5/20). As we move to the
  ;; next/previous match, the counter is updated accordingly. We have the option
  ;; to place this information after the search terms, though I prefer to have
  ;; them as a prefix so as not to interfere with what I am typing.
  (setopt isearch-lazy-count t
          lazy-count-prefix-format "(%s/%s) "
          lazy-count-suffix-format nil)

  ;; With the default settings, when we are repeating an Isearch in the opposite
  ;; direction, Emacs does not move directly to the next/previous
  ;; match. Instead, it places the cursor at the opposite end of the current
  ;; match. So, if we start with `C-s' and search for "word" we now see "word|"
  ;; where the bar represents the cursor. With `C-r' we now have "|word" on the
  ;; same match we were on. I do not like this behaviour so I configure
  ;; `isearch-repeat-on-direction-change' accordingly. Furthermore, I can always
  ;; control where the cursor is left after exiting the search by performing the
  ;; given motion (e.g. `M-f' (`forward-word')) or by using my custom command to
  ;; exit on the opposite end with `C-RET' while in an Isearch
  ;; (`+isearch-other-end').

  ;; If you are using keyboard macros that rely on Isearch, DO NOT set
  ;; `isearch-wrap-pause' to the `no-ding' value. That disables the error
  ;; Isearch produces when it reaches the end of the matches. This error exits
  ;; the keyboard macro, which is exactly what you want if you are calling it
  ;; with a 0 numeric argument (to run from point until the end of the buffer).
  (setopt isearch-wrap-pause t ; `no-ding' makes keyboard macros never quit
          isearch-repeat-on-direction-change t)

  ;; Here I am making some minor tweaks to the `occur' buffer (remember to read
  ;; the introduction to the section). I always want (i) the cursor to be at the
  ;; top of the buffer, (ii) the current line to be highlighted, as it is easier
  ;; for selection purposes, and (iii) for long lines to be truncated, meaning
  ;; to stretch beyond the visible portion of the window without wrapping below,
  ;; and for this to be done silently without messaging me about it. The latter
  ;; depends on my custom `toggle-truncate-lines'.
  (setopt list-matching-lines-jump-to-current-line nil)

  (add-hook 'occur-mode-hook #'toggle-truncate-lines)
  (add-hook 'occur-mode-hook #'hl-line-mode)

  ;; Scrolling shouldn't cancel search
  (setopt isearch-allow-scroll 'unlimited)

  ;; Move to the next/previous match in Isearch with the down/up arrow
  ;; keys. (`C-s' and `C-r' still work though).
  (defun +isearch-repeat-forward (&optional arg)
    "Move forward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
    (interactive "p")
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end))
    (isearch-repeat-forward (or arg 1)))

  (defun +isearch-repeat-backward (&optional arg)
    "Move backward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
    (interactive "p")
    (when (and (not isearch-forward) isearch-other-end)
      (goto-char isearch-other-end))
    (isearch-repeat-backward (or arg 1)))

  ;; Place the cursor on the opposite end of an Isearch when exitting. Do this
  ;; with `C-RET' while in Isearch.
  (defun +isearch-other-end ()
    "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
    (interactive)
    (isearch-done)
    (when isearch-other-end
      (goto-char isearch-other-end)))

  ;; Delete the non-matching portion of a query in Isearch with a single
  ;; backspace instead of doing it character-by-character.
  (defun +isearch-abort-dwim ()
    "Delete failed `isearch' input, single char, or cancel search.

This is a modified variant of `isearch-abort' that allows us to
perform the following, based on the specifics of the case: (i)
delete the entirety of a non-matching part, when present; (ii)
delete a single character, when possible; (iii) exit current
search if no character is present and go back to point where the
search started."
    (interactive)
    (if (eq (length isearch-string) 0)
        (isearch-cancel)
      (isearch-del-char)
      (while (or (not isearch-success) isearch-error)
        (isearch-pop-state)))
    (isearch-update))

  ;; Exit `isearch-mode' once I run `isearch-occur'.
  (defun +isearch-occur ()
    (interactive)
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (call-interactively #'isearch-occur))

  ;; Do a project search out of the current search word.
  (defun +isearch-project-grep ()
    (interactive)
    (let ((query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (project-find-regexp query)))

  ;; Type `M-s M-<' (`+isearch-beginning-of-buffer') or `M-s M->'
  ;; (`+isearch-end-of-buffer') to search for the symbol at point starting from
  ;; the beginning/end of the buffer.
  (defmacro +isearch-occurrence (name edge &optional doc)
    "Construct function for moving to `isearch' occurrence.
NAME is the name of the function.  EDGE is either the beginning
or the end of the buffer.  Optional DOC is the resulting
function's docstring."
    `(defun ,name (&optional arg)
      ,doc
      (interactive "p")
      (let ((x (or arg 1))
            (command (intern (format "isearch-%s-of-buffer" ,edge))))
       (isearch-forward-symbol-at-point)
       (funcall command x))))

  (+isearch-occurrence
   +isearch-beginning-of-buffer
   "beginning"
   "Run `isearch-beginning-of-buffer' for the symbol at point.
With numeric ARG, move to ARGth occurrence counting from the
beginning of the buffer.")

  (+isearch-occurrence
   +isearch-end-of-buffer
   "end"
   "Run `isearch-end-of-buffer' for the symbol at point.
With numeric ARG, move to ARGth occurrence counting from the
end of the buffer.")

  (bind-keys
   :map +search-prefix-map
   ("c" . count-matches)
   ("o" . occur)
   ("M-o" . multi-occur)
   ("." . isearch-forward-thing-at-point)
   ("M-<" . +isearch-beginning-of-buffer)
   ("M->" . +isearch-end-of-buffer)
   :map isearch-mode-map
   ;; The change to `C-g' is important for me as I want to exit the search
   ;; altogether, not resume the search of the previous succesful match.
   ("C-g" . isearch-cancel) ; instead of `isearch-abort'
   ("<up>" . +isearch-repeat-backward)
   ("<down>" . +isearch-repeat-forward)
   ("<backspace>" . isearch-del-char)
   ("<C-backspace>" . +isearch-abort-dwim)
   ("<M-backspace>" . +isearch-abort-dwim)
   ("<C-return>" . +isearch-other-end)
   ("M-/" . isearch-complete)
   ("M-s o" . +isearch-occur)
   ("M-s g" . +isearch-project-grep)
   ("M-s M-g" . +isearch-project-grep)
   :map minibuffer-local-isearch-map
   ("M-/" . isearch-complete-edit)
   :map occur-mode-map
   ("t" . toggle-truncate-lines)))

(defvar +ripgrep (or (executable-find "rg") (executable-find "ripgrep"))
  "Store path to ripgrep executable, else nil.")

(use-package grep
  ;; `grep' is a wrapper for the Unix program of the same name. Not much to add
  ;; there. Note the use of the `let' to decide wether I use the `grep' or `rg'
  ;; (`ripgrep') program: this covers Xref as well.

  ;; Starting with Emacs 31, buffers using `grep-mode' can now be edited
  ;; directly. For older versions of Emacs, we have the `wgrep' package by
  ;; Masahiro Hayashi. The idea is to collect the results of a search in one
  ;; place and quickly apply a change across all or some of them. We have the
  ;; same concept with `occur' as well as with Dired buffers (see `wdired'). It
  ;; uses key bindings like those of the occur edit mode.

  :config
  (setopt grep-save-buffers nil
          grep-use-headings t ; Emacs 30
          grep-program (or +ripgrep (executable-find "grep"))
          grep-template (if +ripgrep
                            "rg -nH --null -e <R> <F>"
                          "grep <X> <C> -nH --null -e <R> <F>")))

;; TODO recursive project search https://www.youtube.com/watch?v=1jBbVUnNbDU
;; TODO +xref-find-apropos-documentation (searches doc string)

(use-package xref
  :config
  ;; `xref' provides infrastructure to jump to and from a definition. For
  ;; example, with point over a function, call `xref-find-definitions' will jump
  ;; to the file where the function is defined or provide an option to pick one
  ;; among multiple definitions, where applicable.

  (setopt xref-search-program (if +ripgrep 'ripgrep 'grep))

  ;; Use Consult to select xref locations with preview.
  (with-eval-after-load 'consult
    (setopt xref-show-xrefs-function #'consult-xref
            xref-show-definitions-function #'consult-xref))

  ;; Record location with `xref-push-marker-stack' before navigating with
  ;; `find-function' and related.
  (defun +xref-push-marker-stack-a (&rest _args)
    "Advice function for `xref-push-marker-stack'."
    (xref-push-marker-stack))
  (dolist (fn '(find-face-definition
                find-function
                find-function-on-key
                find-library
                find-variable))
    (advice-add fn :before #'+xref-push-marker-stack-a))

  (with-eval-after-load 'pulsar
    (dolist (func '(xref-go-back
                    xref-go-forward
                    xref-find-definitions))
      (add-to-list 'pulsar-pulse-functions func))))

(use-package re-builder
  ;; `re-builder' defines a command that lets us write a regexp that matches
  ;; against the current buffer, allowing us to test it live.
  :config

  (defun +reb-query-replace (to-string)
    "Replace current RE from point with `query-replace-regexp'."
    (interactive
     (let ((regexp (with-current-buffer reb-target-buffer
                     reb-regexp)))
       (barf-if-buffer-read-only)
       (list (query-replace-read-to regexp "Query replace" t))))
    (with-current-buffer reb-target-buffer
      (query-replace-regexp reb-regexp to-string)))

  (bind-keys :map reb-mode-map
             ("M-%" . +reb-query-replace)))

;; TODO dumbjump

(use-package avy
  ;; The `avy' package lets you select a location on the screen to move the cursor
  ;; to. It does so by producing an overlay with characters that need to be typed
  ;; to specify the location. By default, the overlay covers the candidate, though
  ;; I change the `avy-style' to have it appear as a prefix instead.
  ;;
  ;; There are several commands on offer which narrow down the candidates. My
  ;; favorite is `avy-goto-char-timer' (closely followed by `avy-goto-char-2' and
  ;; `avy-goto-word-1'). It prompts for a character and then has a time window
  ;; lasting `avy-timeout-seconds' during which it can read more characters. Once
  ;; Avy receives the input, it overlays every word that contains those characters
  ;; in succession. By default if there is a single match, it jumps directly to
  ;; it.
  ;;
  ;; Avy has the ability to act on the candidate rather than simply jump to
  ;; it. Karthik Chikmagalur has a comprehensive essay on the matter, which I
  ;; consider essential reading for anyone wanting to make best use of this
  ;; package: https://karthinks.com/software/avy-can-do-anything/ (2021-10-21). I
  ;; still am not sure whether I need all that power though, as in my workflow I
  ;; jump to a point and then invoke `embark-act'.
  :config
  (setopt avy-keys '(?n ?r ?t ?s ?h ?a ?e ?i ?g ?y) ; Graphite keyboard layout
          avy-timeout-seconds 0.27
          avy-single-candidate-jump t ; nil if i want to make use of avy actions
          avy-all-windows t ; all windows
          avy-all-windows-alt nil ; only the current window with C-u
          )
  (bind-keys :map global-map
             ("C-j" . avy-goto-char-timer)
             :map isearch-mode-map
             ("C-j" . avy-isearch)))

(use-package link-hint
  :config
  ;; The `link-hint' package offers a convenient way to act on visible links
  ;; through Avy, much like the hinting interfaces of modal browsers
  ;; (qutebrowser, vimium, pentadactyl, etc.) A "link" here is intentionally
  ;; broad, covering URLs, file paths, buttons, Org references, Info nodes, mail
  ;; addresses and more all qualify.
  ;;
  ;; The commands can follow a link or copy it and even operate on several links
  ;; in succession. Additional link types and actions can be defined by the
  ;; user. For example, I created an action that moves the point to the selected
  ;; link.
  (defun +link-hint--nop () nil)

  (dolist (type link-hint-types)
    (let* ((name (symbol-name type))
           (stripped (intern (string-remove-prefix "link-hint-" name))))
      (link-hint-define-type stripped
        :jump #'+link-hint--nop)))

  (defun +link-hint-jump-link ()
    (interactive)
    (let ((link-hint-restore nil))
      (avy-with +link-hint-jump-link
        (link-hint--one :jump))))

  (setopt link-hint-message nil)

  (bind-keys :map global-map
             ("C-c j" . link-hint-open-link)
             ("C-c J" . +link-hint-jump-link))

  (dolist (spec '((elpher elpher-mode-map)
                  (eww eww-mode-map)
                  (help help-mode-map)
                  (helpful helpful-mode-map)
                  (nov nov-mode-map)
                  (info Info-mode-map)))
    (let ((feature (car spec))
          (maps (cdr spec)))
      (with-eval-after-load feature
        (dolist (map maps)
          (bind-keys :map (symbol-value map)
                     ("f" . link-hint-open-link)
                     ("j" . +link-hint-jump-link)))))))

(use-package paragraphs
  :no-require
  :config
  ;; For a long time, I was using double spaces after a sentence, as this is the
  ;; Emacs default. I don't have a strong preference for or against it, though I
  ;; reverted to the single space convention as it is the norm nowadays.
  ;;
  ;; The technical benefit of double spaces is that it makes sentence navigation
  ;; less ambiguous as you do not get false positives like "Dr.". Though I
  ;; realized I seldom use such language so why type more spaces for a
  ;; theoretical advantage?
  ;;
  ;; I still need to use double spaces for Elisp programming, otherwise the byte
  ;; compiler produces warnings. It is annoyingly pedantic, but here we are...
  (setopt sentence-end-double-space nil
          sentence-end-without-period nil)

  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local sentence-end-double-space t)))

  (with-eval-after-load 'pulsar
    (dolist (func '(forward-paragraph
                    backward-paragraph
                    org-forward-paragraph
                    org-backward-paragraph))
      (add-to-list 'pulsar-pulse-functions func))))

(use-package logos
  ;; This package provides a simple approach to setting up a "focus mode". It
  ;; uses the `page-delimiter' (typically `^L') or the outline together with some
  ;; commands to move between pages whether narrowing is in effect or not. It
  ;; also provides some optional aesthetic tweaks which come into effect when
  ;; the buffer-local `logos-focus-mode' is enabled. The manual shows how to
  ;; extend the code to achieve the desired result.
  ;;
  ;; I use `logos' to do video presentations that involve "slides". Each
  ;; heading/section becomes its own "slide" simply by narrowing to it.
  :config
  (setopt logos-outlines-are-pages t
          logos-outline-regexp-alist
          `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos-page-delimiter))
            (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos-page-delimiter))
            (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos-page-delimiter))
            (conf-toml-mode . "^\\[")))

  ;; Place point at the top when changing pages, but not in `prog-mode'.
  (defun +logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top
  (add-hook 'logos-page-motion-hook #'+logos--recenter-top)

  (bind-keys
   :map +prefix-map
   ("]" . logos-forward-page-dwim)
   ("[" . logos-backward-page-dwim)
   :map +narrow-prefix-map
   ("p" . logos-narrow-dwim)))

;; NOTE document golden-ratio-scroll
(use-package golden-ratio-scroll
  :no-require
  :config
  (bind-keys
   :map global-map
   ("C-v" . +golden-ratio-scroll-screen-down)
   ("M-v" . +golden-ratio-scroll-screen-up)
   ("<next>" . +golden-ratio-scroll-screen-down)
   ("<prior>" . +golden-ratio-scroll-screen-up))

  (setq scroll-preserve-screen-position t
        scroll-conservatively 1
        scroll-margin 0
        next-screen-context-lines 0)

  (defcustom +golden-ratio-scroll-recenter nil
    "Recenter or not after scroll"
    :type 'boolean)
  (defcustom +golden-ratio-scroll-screen-ratio 1.618
    "Forward or backward (window-text-height)/<this-value> lines"
    :type 'number)
  (defvar +golden-ratio-scroll-screen-previous-point (point-marker))
  (defun +golden-ratio-scroll-screen-down ()
    "Scroll half screen down."
    (interactive)
    (let ((old-marker +golden-ratio-scroll-screen-previous-point)
          (scroll-line-count (round (/ (window-text-height)
                                       +golden-ratio-scroll-screen-ratio))))
      (setq +golden-ratio-scroll-screen-previous-point (point-marker))
      (if (and (not (and (equal (current-buffer) (marker-buffer old-marker))
                         (equal (marker-position old-marker) (point))))
               (equal last-command '+golden-ratio-scroll-screen-up))
          (goto-char (marker-position old-marker))
        (forward-visible-line scroll-line-count))
      (when (and (member major-mode '(dired-mode wdired-mode))
                 (equal (point-max) (point)))
        (dired-previous-line 1))
      (when +golden-ratio-scroll-recenter
        (recenter (+ scroll-line-count (/ (- (window-text-height) scroll-line-count) 2)))))
    (setq this-command 'scroll-up-command))
  (put '+golden-ratio-scroll-screen-down 'isearch-scroll t)
  (defun +golden-ratio-scroll-screen-up ()
    "Scroll half screen up."
    (interactive)
    (let ((old-marker +golden-ratio-scroll-screen-previous-point)
          (scroll-line-count (round (/ (window-text-height)
                                       +golden-ratio-scroll-screen-ratio))))
      (setq +golden-ratio-scroll-screen-previous-point (point-marker))
      (if (and (not (and (equal (current-buffer) (marker-buffer old-marker))
                         (equal (marker-position old-marker) (point))))
               (equal last-command '+golden-ratio-scroll-screen-down))
          (goto-char (marker-position old-marker))
        (forward-visible-line (- 0 scroll-line-count)))
      (when (and (member major-mode '(dired-mode wdired-mode))
                 (equal (point-min) (point)))
        (dired-next-line 2))
      (when +golden-ratio-scroll-recenter
        (recenter (/ (- (window-text-height) scroll-line-count) 2))))
    (setq this-command 'scroll-down-command))
  (put '+golden-ratio-scroll-screen-up 'isearch-scroll t)

  (with-eval-after-load 'pulsar
    (dolist (func '(+golden-ratio-scroll-screen-up
                    +golden-ratio-scroll-screen-down))
      (add-to-list 'pulsar-pulse-functions func))))

;; TODO document narrow
(use-package narrow
  :no-require
  :config

  (defun +narrow-or-widen-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun, whichever
applies first. Narrowing to org-src-block actually calls
`org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
          ((and (bound-and-true-p org-src-mode) (not p))
           (org-edit-src-exit))
          ((derived-mode-p 'org-mode)
           (or (ignore-errors (org-edit-src-code))
               (ignore-errors (org-narrow-to-block))
               (org-narrow-to-subtree)))
          ((derived-mode-p 'diff-mode)
           (diff-restrict-view p))
          ((region-active-p)
           (narrow-to-region (region-beginning) (region-end)))
          (t (narrow-to-defun))))

  (defun +narrow-to-sexp ()
    "Narrow to sexp containing point."
    (interactive)
    (narrow-to-region
     (save-excursion (up-list -1 t t) (point))
     (save-excursion (up-list +1 t t) (point))))

  (put 'narrow-to-page 'disabled nil)

  (bind-keys :map +prefix-map
             ("n" . +narrow-prefix-map)
             ("C-n" . +narrow-or-widen-dwim)
             :map +narrow-prefix-map
             ("d" . narrow-to-defun)
             ;; ("g" . goto-line-relative) ; if narrowed, make "M-g g" do goto-line-relative instead
             ("r" . narrow-to-region)
             ("l" . +narrow-to-sexp) ; alias for Org mode
             ("n" . +narrow-or-widen-dwim)
             ("p" . narrow-to-page)
             ("s" . +narrow-to-sexp)
             ("w" . widen))

  (with-eval-after-load 'org
    (bind-keys
     ;; :map org-mode-map
     ;; ("C-c n" . +narrow-prefix-map)
     ;; ("C-c C-n" . +narrow-or-widen-dwim)
     :map +narrow-prefix-map
     ("b" . org-narrow-to-block)
     ("e" . org-narrow-to-element)
     ("s" . org-narrow-to-subtree))))

;; (use-package poi) ;; or better-jumper

;;;;;;;;;;;;;;;;;
;;;; editing ;;;;

(use-package editing
  :no-require
  :config

  (defun +duplicate--buffer-substring (boundaries)
    "Duplicate buffer substring between BOUNDARIES.
BOUNDARIES is a cons cell representing buffer positions."
    (unless (consp boundaries)
      (error "`%s' is not a cons cell" boundaries))
    (let ((beg (car boundaries))
          (end (cdr boundaries)))
      (goto-char end)
      (newline)
      (insert (buffer-substring-no-properties beg end))))

  (defun +duplicate-dwim ()
    "Duplicate the current line or active region."
    (interactive)
    (unless mark-ring ; needed when entering a new buffer
      (push-mark (point) t nil))
    (+duplicate--buffer-substring
     (if (region-active-p)
         (cons (region-beginning) (region-end))
       (cons (line-beginning-position) (line-end-position))))
    (setq this-command 'yank))

  (defun +yank-replace-dwim ()
    "Replace region with latest kill if active, otherwise replace line.
This command can then be followed by the standard `yank-pop' (default is
bound to \\[yank-pop])."
    (interactive)
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (delete-region (line-beginning-position) (line-end-position)))
    (yank)
    (setq this-command 'yank))

  (defun +comment-line-dwim (n)
    "Comment N lines, defaulting to the current one.
When the region is active, comment its lines instead."
    (interactive "p")
    (if (use-region-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-line n)))

  (defun +uncomment-sexp (&optional n)
    "Uncomment a sexp around point."
    (interactive "P")
    (let* ((initial-point (point-marker))
           (inhibit-field-text-motion t)
           (p)
           (end (save-excursion
                  (when (elt (syntax-ppss) 4)
                    (re-search-backward comment-start-skip
                                        (line-beginning-position)
                                        t))
                  (setq p (point-marker))
                  (comment-forward (point-max))
                  (point-marker)))
           (beg (save-excursion
                  (forward-line 0)
                  (while (and (not (bobp))
                              (= end (save-excursion
                                       (comment-forward (point-max))
                                       (point))))
                    (forward-line -1))
                  (goto-char (line-end-position))
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t)
                  (ignore-errors
                    (while (looking-at-p comment-start-skip)
                      (forward-char -1)))
                  (point-marker))))
      (unless (= beg end)
        (uncomment-region beg end)
        (goto-char p)
        ;; Indentify the "top-level" sexp inside the comment.
        (while (and (ignore-errors (backward-up-list) t)
                    (>= (point) beg))
          (skip-chars-backward (rx (syntax expression-prefix)))
          (setq p (point-marker)))
        ;; Re-comment everything before it.
        (ignore-errors
          (comment-region beg p))
        ;; And everything after it.
        (goto-char p)
        (forward-sexp (or n 1))
        (skip-chars-forward "\r\n[:blank:]")
        (if (< (point) end)
            (ignore-errors
              (comment-region (point) end))
          ;; If this is a closing delimiter, pull it up.
          (goto-char end)
          (skip-chars-forward "\r\n[:blank:]")
          (when (eq 5 (car (syntax-after (point))))
            (delete-indentation))))
      ;; Without a prefix, it's more useful to leave point where
      ;; it was.
      (unless n
        (goto-char initial-point))))

  (defun +comment-sexp--raw ()
    "Comment the sexp at point or ahead of point."
    (pcase (or (bounds-of-thing-at-point 'sexp)
               (save-excursion
                 (skip-chars-forward "\r\n[:blank:]")
                 (bounds-of-thing-at-point 'sexp)))
      (`(,l . ,r)
       (goto-char r)
       (skip-chars-forward "\r\n[:blank:]")
       (save-excursion
         (comment-region l r))
       (skip-chars-forward "\r\n[:blank:]"))))

  (defun +comment-sexp-dwim (&optional n)
    "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
    (interactive "P")
    (if (or (elt (syntax-ppss) 4)
            (< (save-excursion
                 (skip-chars-forward "\r\n[:blank:]")
                 (point))
               (save-excursion
                 (comment-forward 1)
                 (point))))
        (+uncomment-sexp n)
      (dotimes (_ (or n 1))
        (+comment-sexp--raw))))

  ;; TODO possibly replace +mark-sexp with +er/expand-region-dwim?
  (defun +mark (bounds)
    "Mark between BOUNDS as a cons cell of beginning and end positions."
    (push-mark (car bounds))
    (goto-char (cdr bounds))
    (activate-mark))

  (defun +mark-sexp ()
    "Mark symbolic expression at or near point.
Repeat to extend the region forward to the next symbolic
expression."
    (interactive)
    (if (and (region-active-p)
             (eq last-command this-command))
        (ignore-errors (forward-sexp 1))
      (when-let* ((thing (cond
                          ((thing-at-point 'url) 'url)
                          ((thing-at-point 'sexp) 'sexp)
                          ((thing-at-point 'string) 'string)
                          ((thing-at-point 'word) 'word))))
        (+mark (bounds-of-thing-at-point thing)))))

  (defun +kill-region (&optional beg end)
    "Kill region if active, else kill symbol at point"
    (interactive
     (when (region-active-p)
       (list
        (region-beginning)
        (region-end))))
    (if (and beg end)
        (kill-region beg end)
      (+mark-sexp)
      (kill-region (region-beginning) (region-end)))
    (setq this-command 'kill-region))

  (defun +kill-ring-save (&optional beg end)
    "Save region if active, else save symbol at point."
    (interactive
     (when (region-active-p)
       (list
        (region-beginning)
        (region-end))))
    (if (and beg end)
        (kill-ring-save beg end)
      (+mark-sexp)
      (kill-ring-save (region-beginning) (region-end)))
    (setq this-command 'kill-ring-save))

  (defcustom +date-specifier "%F"
    "Date specifier for `format-time-string'.
Used by `+insert-date'."
    :type 'string
    :group 'simple)
  (defcustom +time-specifier "%R %z"
    "Time specifier for `format-time-string'.
Used by `insert-date'."
    :type 'string
    :group 'simple)
  (defun +insert-date (&optional arg)
    "Insert the current date as `+date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `+time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
    (interactive "P")
    (let* ((date +date-specifier)
           (time +time-specifier)
           (format (if arg (format "%s %s" date time) date)))
      (when (use-region-p)
        (delete-region (region-beginning) (region-end)))
      (insert (format-time-string format))))

  (defun +escape--pos-url-on-line (char)
    "Return position of `+common-url-regexp' at CHAR."
    (when (integer-or-marker-p char)
      (save-excursion
        (goto-char char)
        (re-search-forward +common-url-regexp (line-end-position) :noerror))))
  (defun +escape-url-line (char)
    "Escape all URLs or email addresses on the current line.
When called from Lisp CHAR is a buffer position to operate from
until the end of the line.  In interactive use, CHAR corresponds
to `line-beginning-position'."
    (interactive
     (list
      (if current-prefix-arg
          (re-search-forward
           +common-url-regexp
           (line-end-position) :no-error
           (prefix-numeric-value current-prefix-arg))
        (line-beginning-position))))
    (when-let* ((regexp-end (+escape--pos-url-on-line char)))
      (goto-char regexp-end)
      (unless (looking-at ">")
        (insert ">")
        (when (search-backward "\s" (line-beginning-position) :noerror)
          (forward-char 1))
        (insert "<"))
      (+escape-url-line (1+ regexp-end)))
    (goto-char (line-end-position)))
  ;; Thanks to Bruno Boal for the original `+escape-url-region'.
  ;; Check Bruno's Emacs config: <https://github.com/BBoal/emacs-config>.
  (defun +escape-url-region (&optional beg end)
    "Apply `+escape-url-line' on region lines between BEG and END."
    (interactive
     (if (region-active-p)
         (list (region-beginning) (region-end))
       (error "There is no region!")))
    (let ((beg (min beg end))
          (end (max beg end)))
      (save-excursion
        (goto-char beg)
        (setq beg (line-beginning-position))
        (while (<= beg end)
          (+escape-url-line beg)
          (beginning-of-line 2)
          (setq beg (point))))))
  (defun +escape-url-dwim ()
    "Escape URL on the current line or lines implied by the active region.
Call the commands `+escape-url-line' and `+escape-url-region'."
    (interactive)
    (if (region-active-p)
        (+escape-url-region (region-beginning) (region-end))
      (+escape-url-line (line-beginning-position))))

  ;; C-x C-c does the usual killing, whereas C-u C-x C-c restarts the Emacs
  ;; systemd service. This will come in handy when I switch Home Manager
  ;; profiles because I tend to restart Emacs after that and being able to do it
  ;; quickly is a great thing.
  (defun +restart-emacs ()
    (shell-command "systemctl --user restart emacs.service"))
  (defun +kill-terminal-or-restart (&optional restart)
    "Quit emacsclient or restart it with RESTART."
    (interactive "P")
    (if restart
        (+restart-emacs)
      (save-buffers-kill-terminal)))

  (defun +transpose-chars ()
    "Always transposes the two chars before point.
There is no dragging the character forward. This is the behavior of
`transpose-chars' when point is at the end of the line."
    (interactive)
    (if (eq (point) (line-end-position))
        (transpose-chars 1)
      (transpose-chars -1)
      (forward-char)))

  (defun +indent-dwim ()
    "Indent the current defun in `prog-mode' or paragraph in `text-mode'."
    (interactive)
    (save-excursion
      (cond ((derived-mode-p 'prog-mode)
             (mark-defun))
            ((derived-mode-p 'text-mode)
             (mark-paragraph)))
      (indent-for-tab-command)
      (deactivate-mark)))

  (setopt kill-whole-line t)

  (bind-keys :map global-map
             ("M-c" . capitalize-dwim)
             ("M-l" . downcase-dwim)
             ("M-u" . upcase-dwim)
             ("M-=" . count-words)

             ;; Make `transpose-chars' always act like when point is at end of
             ;; the line.
             ("C-t" . +transpose-chars)

             ;; Make `kill-region' and `kill-ring-save' act on symbol at point
             ;; if no active region.
             ("C-w" . +kill-region)
             ("M-w" . +kill-ring-save)
             ;; `+save-next-kill' causes the following command, if it kills, to
             ;; save in the kill ring instead. With prefix argument has same
             ;; behavior as `append-next-kill', which adds to previous kill.
             ;; ("C-M-w" . +save-next-kill)

             ;; `+duplicate-dwim' will duplicate the region if active, otherwise
             ;; the current line.
             ("C-M-w" . +duplicate-dwim)
             ("C-M-y" . +yank-replace-dwim)

             ;; The default `delete-char' doesn't respect the values of
             ;; `delete-active-region'. Make it so `C-d' deletes the region if
             ;; active.
             ("C-d" . delete-forward-char)

             ;; Kills up to a char similar to Vim's dt command.
             ("M-z" . zap-up-to-char) ; More useful than original `zap-to-char'

             ;; Escape urls and insert dates.
             ("C-<" . +escape-url-dwim)
             ("C-=" . +insert-date)

             ;; The `+comment-line-dwim' command is like the built-in
             ;; `comment-dwim', but toggles line wise commenting instead of
             ;; appending them by default.
             ("M-;" . +comment-line-dwim)
             ("C-M-;" . +comment-sexp-dwim)

             ;; `+indent-dwim' will indent the current defun or paragraph.
             ("C-M-q" . +indent-dwim)
             ("C-M-\\" . +indent-dwim)

             :map +prefix-map
             ("C-c" . +kill-terminal-or-restart)
             ("h" . mark-whole-buffer)
             ("C-o" . delete-blank-lines)))

;; TODO document mark-command
(use-package mark-command
  :no-require
  :config
  ;; A problem with the `mark-ring' is that sometimes it gets filled with
  ;; repeated entries, so I find myself hitting `C-u C-SPC' 2 to 4 times in the
  ;; same place. The following advice tries to make `pop-to-mark-command' and
  ;; `+unpop-to-mark-command' pop multiple times until it moves the point.
  (defun +mark-pop-until-move (orig-fun &rest args)
    "Call ORIG-FUN until the point moves.
Try the repeated popping up to 10 times."
    (let ((p (point)))
      (dotimes (i 10)
        (when (= p (point))
          (apply orig-fun args)))))
  (advice-add 'pop-to-mark-command :around #'+mark-pop-until-move)
  (advice-add '+unpop-to-mark-command :around #'+mark-pop-until-move)

  (defun +marker-is-point-p (marker)
    "Test if MARKER is current point."
    (and (eq (marker-buffer marker) (current-buffer))
         (= (marker-position marker) (point))))

  (defun +push-mark-maybe (ring)
    "Push mark into RING if its head is not the current point."
    (if (not ring)
        (error (format "mark ring is empty"))
      (unless (or (+marker-is-point-p (car ring))
                  (+marker-is-point-p (car (reverse ring))))
        (push-mark))))

  ;; Do the reverse of `pop-to-mark-command' (C-u C-SPC)

  (defun +set-mark-command (arg)
    "Set the mark where point is, and activate it; or jump to the mark.
Setting the mark also alters the region, which is the text
between point and mark; this is the closest equivalent in
Emacs to what some editors call the \"selection\".

With no prefix argument, set the mark at point, and push the
old mark position on local mark ring.  Also push the new mark on
global mark ring, if the previous mark was set in another buffer.

When Transient Mark Mode is off, immediately repeating this
command activates `transient-mark-mode' temporarily.

With prefix argument (e.g., \\[universal-argument] \\[set-mark-command]), \
jump to the mark, and set the mark from
position popped off the local mark ring (this does not affect the global
mark ring).  Use \\[pop-global-mark] to jump to a mark popped off the global
mark ring (see `pop-global-mark').

If `set-mark-command-repeat-pop' is non-nil, repeating
the \\[set-mark-command] command with no prefix argument pops the next position
off the local (or global) mark ring and jumps there.

With \\[universal-argument] \\[universal-argument] as prefix
argument, unconditionally set mark where point is, even if
`set-mark-command-repeat-pop' is non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
    (interactive "P")
    (let (do-it)
      (cond ((and (equal arg '(4))
                  (not (eq this-command real-last-command)))
             (+push-mark-maybe mark-ring)
             (call-interactively 'pop-to-mark-command)
             (setq do-it t))
            (t
             (setq do-it t)))
      (when do-it
        (let ((set-mark-cmd (if cua-mode
                                'cua-set-mark
                              'set-mark-command)))
          (setq this-command set-mark-cmd)
          (funcall set-mark-cmd arg)))))

  (defun +unpop-to-mark-command ()
    "Unpop off mark ring. Does nothing if mark ring is empty."
    (interactive)
    (when mark-ring
      (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
      (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
      (when (null (mark t)) (ding))
      (setq mark-ring (nbutlast mark-ring))
      (goto-char (marker-position (car (last mark-ring))))))

  ;; Do the reverse of `pop-global-mark' (C-x C-SPC)

  (defun +pop-global-mark ()
    "Pop off global mark ring and jump to the top location."
    (interactive)
    (+push-mark-maybe global-mark-ring)
    (when (+marker-is-point-p (car global-mark-ring))
      (call-interactively 'pop-global-mark))
    (call-interactively 'pop-global-mark))

  (defun +unpop-global-mark ()
    "Unpop off `global-mark-ring'."
    (interactive)
    (+push-mark-maybe global-mark-ring)
    (setq global-mark-ring (nreverse global-mark-ring))
    (when (+marker-is-point-p (car global-mark-ring))
      (call-interactively 'pop-global-mark))
    (call-interactively 'pop-global-mark)
    (setq global-mark-ring (nreverse global-mark-ring)))

  ;; Pulsar integration
  (with-eval-after-load 'pulsar
    (dolist (func '(pop-global-mark
                    +unpop-global-mark
                    pop-to-mark-command
                    +unpop-to-mark-command))
      (add-to-list 'pulsar-pulse-functions func)))

  (bind-keys :map global-map
             ("C-SPC" . +set-mark-command)
             :map +prefix-map
             ("C-SPC" . +pop-global-mark)
             ;; Make Emacs repeat the `pop-to-mark-command' and
             ;; `pop-global-mark' commands.
             :repeat-map pop-global-mark-repeat-map
             ("C-SPC" . +pop-global-mark)
             ("SPC" . +unpop-global-mark)
             :repeat-map pop-to-mark-command-repeat-map
             ("C-SPC" . pop-to-mark-command)
             ("SPC" . +unpop-to-mark-command))
  ;; BUG: can't add repeat hints to commands bound to C-SPC.
  ;; too lazy to file an issue in core Emacs
  ;; (put 'pop-to-mark-command 'repeat-hint "pop")
  ;; (put 'pop-global-mark 'repeat-hint "pop")

  (put '+unpop-to-mark-command 'repeat-hint "unpop")
  (put '+unpop-global-mark 'repeat-hint "unpop"))

(use-package scratch-plus
  ;; This package provides the means to create unkillable scratch buffers,
  ;; persistent scratch buffers, per-project scratch buffers, and per-major-mode
  ;; scratch buffers.
  :init
  (setopt scratch-plus-initial-message
          (lambda (majormode)
            (format "This is a scratch buffer for `%s'." majormode)))
  :config
  (scratch-plus-mode)

  ;; TODO use :xdg-state keyword to set scratch-plus-save-directory
  ;; TODO +scratch-pop-to-buffer modeled after +shell, +ielm, and +python ones
  (setopt scratch-plus-save-directory user-emacs-directory
          scratch-plus-prevent-kill nil
          scratch-plus-display-action
          '((+display-buffer-use-some-other-window)
            (dedicated . t)
            (body-function . select-window)))

  (bind-keys :map scratch-plus-mode-map
             ("C-x M-s" . nil) ; unmap `scratch-plus-switch'
             ("C-c s" . scratch-plus-switch)))

(use-package move-text
  ;; The package `move-text' provides utility functions to easily move the
  ;; current line or region up and down.
  :config
  ;; Re-indent the text in and around a text move.
  (defun +indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  (advice-add 'move-text-up :after '+indent-region-advice)
  (advice-add 'move-text-down :after '+indent-region-advice)

  ;; TODO what should I bind move-text to in org-mode?
  (bind-keys :map global-map
             ("M-<up>" . move-text-up)
             ("M-<down>" . move-text-down)))

(use-package tabs
  :no-require
  :config
  ;; `TAB' in Emacs tries to be smart. Instead of inserting tabs, or spaces, it
  ;; tries to indent the current line to where it should be given the context
  ;; and depending on the major mode. This works best when we mark a region of
  ;; text and hit `TAB' there.
  ;;
  ;; If we need to forcefully indent, we can use `indent-rigidly' (`C-x\ C-i' by
  ;; default). This command allows us to shift a region left or right using the
  ;; arrow keys. A common use-case for me is to paste some text I want to
  ;; indent, and then do `C-u C-x C-i', which indents by four spaces the
  ;; implicit region.
  ;;
  ;; `tab-always-indent' makes the `TAB' key assume the dual role of indenting
  ;; text as well as triggering completion at point. (See my `corfu'
  ;; configuration). When it can perform indentation, it does that, otherwise it
  ;; starts a completion loop. The `tab-first-completion' determines when not to
  ;; complete. In my case complete unless the next character is part of a word.
  ;; Typing `TAB' a second time always results in completion.
  (setopt tab-always-indent 'complete
          tab-first-completion 'word)

  ;; `tab-width' and `indent-tabs-mode' are about the use of tabs. I never want
  ;; them, as I only use spaces.
  (setq-default tab-width 4
                indent-tabs-mode nil))

(use-package elec-pair
  :config
  ;; Emacs describes as "electric" any behaviour that tries to be smart about
  ;; how to handle a given action. The `electric-pair-mode', for example,
  ;; automatically inserts a closing parenthesis.
  (electric-pair-mode)

  ;; You can also create your own pairs.
  (defun +electric-pair-add-fn (pairs)
    "Add PAIRS to `electric-pair-pairs'."
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  (defun +electric-pair-markdown-h ()
    (+electric-pair-add-fn '((?* . ?*)
                             (?_ . ?_)
                             (?` . ?`))))
  (add-hook 'markdown-mode-hook #'+electric-pair-markdown-h))

(use-package paren
  ;; The built-in `show-paren-mode' highlights the parenthesis on the opposite
  ;; end of the current symbolic expression. It also highligts matching terms of
  ;; control flow in programming languages that are not using parentheses like
  ;; Lisp: for instance, in a `bash' shell script it highlights the `if' and
  ;; `fi' keywords. This mode also works for prose and I use it globally. Simple
  ;; and effective!
  :config
  (show-paren-mode 1)
  (setopt show-paren-context-when-offscreen nil
          show-paren-when-point-in-periphery t))

;; (use-package insert-pair)

(use-package delsel
  :config
  ;; Every graphical application I have ever used will delete the selected text
  ;; upon the insertion of new text. Emacs does not do this by default. With
  ;; `delete-selection-mode' we get it.
  (delete-selection-mode +1))

(use-package expand-region
  :config
  ;; The `expand-region' package expands the region from smallest to the largest
  ;; syntactic unit in the give context. If Emacs is built with tree-sitter
  ;; support and we are running a major mode that is designed for tree-sitter,
  ;; it will use the tree-sitter framework to determine the syntactic units.
  ;;
  ;; I almost never use `er/contract-region'. I often find myself just
  ;; aborting and starting over again with `er/expand-region' in the
  ;; scenario where I widened the selection more than I should.

  (defun +er/expand-region-dwim ()
    "Do-What-I-Mean `er/expand-region' to start with symbol or word.
If over a real symbol, mark that directly, else start with a word.  Fall
back to regular `er/expand-region'"
    (interactive)
    (let ((symbol (bounds-of-thing-at-point 'symbol)))
      (cond
       ((equal (bounds-of-thing-at-point 'word) symbol)
        (er/expand-region 1))
       (symbol (er/expand-region 2))
       (t (er/expand-region)))))

  (bind-keys
   :map global-map
   ("C-M-SPC" . +er/expand-region-dwim) ; overrides mark-sexp
   )

  (setopt expand-region-fast-keys-enabled nil))

(use-package multiple-cursors
  :config
  ;; The `multiple-cursors' package offers a comprehensive list of commands to
  ;; edit text in several points of the screen at once. One could rely on
  ;; keyboard macros and `query-replace' to perform complex edits in numerous
  ;; parts of a buffer or set thereof, it is sometimes easier to just pop a few
  ;; extra cursors and type directly, especially for quick, one-time operations.

  ;; Known limitations
  ;;
  ;; + isearch-forward and isearch-backward aren't supported with multiple
  ;;   cursors. You should feel free to add a simplified version that can work
  ;;   with it, or maybe use phisearch.
  ;; + Commands run with `M-x' won't be repeated for all cursors.
  ;; + All key bindings that refer to lambdas are always run for all cursors. If
  ;;   you need to limit it, you will have to give it a name.
  ;; + Redo might screw with your cursors. Undo works very well.

  ;; So learn keyboard macros and `query-replace' and let this package handle
  ;; more straightforward workflows.

  ;; The approach that `multiple-cursors' uses to create fake cursors doesn't
  ;; play nice with `cursory'. I suppose this might be a technical limitation of
  ;; Emacs, but the fake cursors don't inherit directly from my cursor type, and
  ;; instead place an overlay with either the "|" or " " characters.
  ;; Unfortunately if you set your cursor type to bar, it tries to render the
  ;; fake cursors as literal "|" characters with the mc/cursor-bar-face. This
  ;; approach distorts lines and just plainly doesn't work, so I disable it.
  (setopt mc/match-cursor-style t)

  (defun +mc/mark-sexps (num-sexps direction)
    (dotimes (i (if (= num-sexps 0) 1 num-sexps))
      (mc/save-excursion
       (let ((furthest-cursor (cl-ecase direction
                                (forwards  (mc/furthest-cursor-after-point))
                                (backwards (mc/furthest-cursor-before-point)))))
         (when (overlayp furthest-cursor)
           (goto-char (overlay-get furthest-cursor 'point))
           (when (= num-sexps 0)
             (mc/remove-fake-cursor furthest-cursor))))
       (cl-ecase direction
         (forwards (forward-sexp 2) (backward-sexp 1))
         (backwards (backward-sexp 2) (forward-sexp 1)))
       (mc/create-fake-cursor-at-point))))

  (defun +mc/mark-next-sexps (arg)
    "Mark next ARG sexps."
    (interactive "p")
    (+mc/mark-sexps arg 'forwards)
    (mc/maybe-multiple-cursors-mode))
  (add-to-list 'mc--default-cmds-to-run-once '+mc/mark-next-sexps)

  (defun +mc/mark-previous-sexps (arg)
    (interactive "p")
    (+mc/mark-sexps 'backwards)
    (mc/maybe-multiple-cursors-mode))
  (add-to-list 'mc--default-cmds-to-run-once '+mc/mark-previous-sexps)

  (defun +mc/remove-current-cursor ()
    "Remove the current cursor by replacing the next fake cursor."
    (interactive)
    (let ((next-cursor
           (or (mc/next-fake-cursor-after-point)
               (mc/prev-fake-cursor-before-point)
               (error "This is the only cursor."))))
      (mapc 'mc/remove-fake-cursor
            (cl-remove-if-not 'mc/fake-cursor-p
                              (overlays-at (point))))
      (mc/pop-state-from-overlay next-cursor)))
  (add-to-list 'mc--default-cmds-to-run-once '+mc/remove-current-cursor)

  (defvar-keymap mc-mark-map
    :doc "multiple-cursors mark map."
    :prefix 'mc-mark-map)
  (defvar-keymap mc-mark-repeat-map
    :repeat (:hints ((mc/mark-next-like-this-symbol . "next")
                     (mc/mark-previous-like-this-symbol . "prev")
                     (mc/mark-next-lines . "next line")
                     (mc/mark-previous-lines . "prev line")
                     (+mc/mark-next-sexps . "next sexp")
                     (+mc/mark-previous-sexps . "prev sexp")
                     (mc/skip-to-next-like-this . "skip next")
                     (mc/skip-to-previous-like-this . "skip prev")
                     (+mc/remove-current-cursor . "del"))))
  (bind-keys
   :map global-map
   ("C-'" . mc-mark-map) ; C-c m
   :map mc/keymap
   ("C-'" . nil) ; orig. `mc-hide-unmatched-lines-mode'
   ("<return>" . nil) ; orig. `multiple-cursors-mode'
   :map mc-mark-map
   ("." . mc/mark-all-like-this-dwim)
   ("C-a" . mc/edit-beginnings-of-lines)
   ("C-d" . +mc/remove-current-cursor)
   ("C-e" . mc/edit-ends-of-lines)
   ("k" . mc-hide-unmatched-lines-mode) ; "keep" mnemonic
   ("C-k" . mc-hide-unmatched-lines-mode) ; "keep" mnemonic
   ("n" . mc/mark-next-like-this-symbol)
   ("p" . mc/mark-previous-like-this-symbol)
   ("C-n" . mc/mark-next-lines)
   ("C-p" . mc/mark-previous-lines)
   ("C-M-n" . +mc/mark-next-sexps)
   ("C-M-p" . +mc/mark-previous-sexps)
   ("C-SPC" . mc/mark-pop)
   (">" . mc/skip-to-next-like-this)
   ("<" . mc/skip-to-previous-like-this)
   :repeat-map mc-mark-repeat-map
   ("C-d" . +mc/remove-current-cursor)
   ("n" . mc/mark-next-like-this-symbol)
   ("p" . mc/mark-previous-like-this-symbol)
   ("C-n" . mc/mark-next-lines)
   ("C-p" . mc/mark-previous-lines)
   ("C-M-n" . +mc/mark-next-sexps)
   ("C-M-p" . +mc/mark-previous-sexps)
   ("C-SPC" . mc/mark-pop)
   (">" . mc/skip-to-next-like-this)
   ("<" . mc/skip-to-previous-like-this)))

(use-package symbol-overlay
  :config
  (defun +symbol-overlay-mc-mark-all ()
    "Mark all symbol overlays using `multiple-cursors'."
    (interactive)
    (when-let* ((overlays (symbol-overlay-get-list 0))
                (point (point))
                (point-overlay (seq-find
                                (lambda (overlay)
                                  (and (<= (overlay-start overlay) point)
                                       (<= point (overlay-end overlay))))
                                overlays))
                (offset (- point (overlay-start point-overlay))))
      (setq deactivate-mark t)
      (mapc (lambda (overlay)
              (unless (eq overlay point-overlay)
                (mc/save-excursion
                 (goto-char (+ (overlay-start overlay) offset))
                 (mc/create-fake-cursor-at-point))))
            overlays)
      (mc/maybe-multiple-cursors-mode)))

  (defvar-keymap +symbol-overlay-active-map
    :doc "Keymap automatically actived when there are overlays from
`symbol-overlay'."
    "C-g" #'symbol-overlay-remove-all)

  (define-minor-mode +symbol-overlay-active-mode
    "Minor mode for when overlays from `symbol-overlay' exist."
    :keymap +symbol-overlay-active-map
    :global nil)

  (defun +symbol-overlay-dwim ()
    "Toggle all overlays of symbol at point.

If point is already on an overlayed symbol, select them all with
`multiple-cursors'."
    (interactive)
    (let* ((overlays (symbol-overlay-get-list 0))
           (overlay-at-point (and overlays
                                  (seq-find
                                   (lambda (overlay)
                                     (and (<= (overlay-start overlay) (point))
                                          (<= (point) (overlay-end overlay))))
                                   overlays))))
      (cond (overlay-at-point
             (+symbol-overlay-mc-mark-all))
            (t
             (call-interactively #'symbol-overlay-put)
             (+symbol-overlay-active-mode 1)))))

  (advice-add 'symbol-overlay-remove-all :after
              (lambda (&rest _)
                (when (not (symbol-overlay-get-list 0))
                  (+symbol-overlay-active-mode -1))))

  (bind-keys :map global-map
             ("C-," . +symbol-overlay-dwim)
             :map symbol-overlay-map
             ;; I don't like that symbol-overlay binds one zillion keys.
             ("<" . nil) ; orig. `symbol-overlay-jump-first'
             (">" . nil) ; orig. `symbol-overlay-jump-last'
             ("d" . nil) ; orig. `symbol-overlay-jump-to-definition'
             ("e" . nil) ; orig. `symbol-overlay-echo-mark'
             ("h" . nil) ; orig. `symbol-overlay-map-help'
             ("i" . nil) ; orig. `symbol-overlay-put'
             ("n" . nil) ; orig. `symbol-overlay-jump-next'
             ("p" . nil) ; orig. `symbol-overlay-jump-prev'
             ("q" . nil) ; orig. `symbol-overlay-query-replace'
             ("r" . nil) ; orig. `symbol-overlay-rename'
             ("s" . nil) ; orig. `symbol-overlay-isearch-literally'
             ("t" . nil) ; orig. `symbol-overlay-toggle-in-scope'
             ("w" . nil) ; orig. `symbol-overlay-save-symbol'
             ("M-<" . symbol-overlay-jump-first)
             ("M->" . symbol-overlay-jump-last)
             ("M-%" . symbol-overlay-query-replace)
             ("C-g" . symbol-overlay-remove-all)
             ("M-n" . symbol-overlay-jump-next)
             ("M-p" . symbol-overlay-jump-prev)
             ("C-s" . symbol-overlay-isearch-literally)))

(use-package vundo
  :disabled t
  ;; The `vundo' package by Yuan Fu (aka "casouri") builds on top of the
  ;; standard `undo' infrastructure to provide a tree view of the undo steps. It
  ;; is an essential complement to what is otherwise a powerful mechanism.

  ;; I personally like minimalist interfaces by default, meaning that I prefer
  ;; nothing to pop up unless it is necessary. To this end, my command
  ;; `+vundo-if-repeat-undo' produces a visualisation of the undo steps only
  ;; after I repeat the `undo' command. The assumption is that if I am
  ;; repeating, I am already interested in something further back in the
  ;; history, at which point having a representation of it is helpful. I am
  ;; implementing this using the advice mechanism, so that (i) the command calls
  ;; the original function if needed, and (ii) I can extend the functionality to
  ;; many functions without needing to rebind any keys.

  ;; To make this feel natural, I bind keys in the `vundo-mode-map' tha are
  ;; consistent with the defaults for `undo' and `undo-redo'. This way, I can
  ;; keep operating on the buffer without switching contexts. The visualization
  ;; is a nice extra.
  :config
  (setopt vundo-glyph-alist vundo-unicode-symbols)

  (defvar +vundo-undo-functions '(undo undo-only undo-redo)
    "List of undo functions to check if we need to visualise the undo ring.")

  (defvar +vundo-undo-command #'undo
    "Command to call if we are not going to visualise the undo ring.")

  (defun +vundo-if-repeat-undo (&rest args)
    "Use `undo' if the last command is among `+vundo-undo-functions'.
In other words, start visualising the undo ring if we are going to be cycling
through the edits."
    (interactive)
    (if (and (member last-command +vundo-undo-functions)
             (not undo-in-region))
        (call-interactively 'vundo)
      (apply args)))

  (mapc
   (lambda (fn)
     (advice-add fn :around #'+vundo-if-repeat-undo))
   +vundo-undo-functions)

  (with-eval-after-load 'pulsar
    (add-hook 'vundo-post-exit-hook #'pulsar-pulse-line-green))

  (bind-keys
   :map global-map
   ("C-!" . undo) ; ? and ! are on the same key in my keyboard layout.
   ("C-?" . undo-redo)
   :map +prefix-map
   ("u" . vundo)
   :map vundo-mode-map
   ("C-/" . vundo-backward)
   ("C-!" . vundo-backward)
   ("C-?" . vundo-forward)))

;;;;;;;;;;;;;;;;;
;;;; linting ;;;;

(use-package flymake
  :config
  ;; The built-in `flymake' feature defines an interface for viewing the output
  ;; of linter programs. A "linter" parses a file and reports possible
  ;; notes/warnings/errors in it. With `flymake' we get these diagnostics in the
  ;; form of a standalone buffer as well as inline highlights (typically
  ;; underlines combined with fringe indicators) for the portion of text in
  ;; question. The linter report is displayed with the command
  ;; `flymake-show-buffer-diagnostics', or `flymake-show-project-diagnostics'.
  ;; Highlights are shown in the context of the file.
  ;;
  ;; The built-in `eglot' feature uses `flymake' internally to handle the LSP
  ;; linter output.
  ;;
  ;; I see no need to constantly check the buffer for changes in order to update
  ;; the linting report, so I set `flymake-no-changes-timeout' to nil. The other
  ;; essential user options for me are `flymake-start-on-save-buffer' and
  ;; `flymake-start-on-flymake-mode' as they make the linter update its report
  ;; when the buffer is saved and when `flymake-mode' is started,
  ;; respectively. Otherwise, we have to run it manually, which is cumbersome.
  (setopt flymake-no-changes-timeout nil
          flymake-start-on-save-buffer t
          flymake-start-on-flymake-mode t)

  ;; Interesting feature but I do not want to use it.
  (setopt flymake-show-diagnostics-at-end-of-line nil) ; Emacs 31

  ;; By default, `flymake-mode' doesn’t override the `next-error' and related
  ;; navigation commands. When Flymake is active, though, I prefer `M-g n' and
  ;; similar bindings to move among diagnostics annotated in the buffer.
  ;;
  ;; I achieves this with the following bindings, but it overrides regular
  ;; compilation mechanisms which I still rely on frequently. That trade-off is
  ;; acceptable for me: I can always disable Flymake temporarily and return to
  ;; the usual next-error workflow.
  (bind-keys :map flymake-mode-map
             ("M-g n" . flymake-goto-next-error)
             ("M-g M-n" . flymake-goto-next-error)
             ("M-g p" . flymake-goto-prev-error)
             ("M-g M-p" . flymake-goto-prev-error)
             :map flymake-diagnostics-buffer-mode-map
             ("n" . next-error-no-select)
             ("p" . previous-error-no-select)
             :repeat-map flymake-goto-error-repeat-map
             ("n" . flymake-goto-next-error)
             ("p" . flymake-goto-prev-error))

  (bind-keys :map global-map
             ("C-c C-v" . flymake-mode)))

;; TODO document flymake-collection
(use-package flymake-collection
  :config
  ;; Change :flymake-hook use-package keyword to :lint
  (defvar flymake-collection-hook-config)

  (declare-function use-package-concat "use-package-core")
  (declare-function use-package-process-keywords "use-package-core")
  (defvar use-package-keywords)
  (defvar use-package-deferring-keywords)

  (if (member :flymake-hook use-package-keywords)
      (setq use-package-keywords (remove :flymake-hook use-package-keywords)))

  ;; Add to use-package-keywords, just after :custom.
  (unless (member :lint use-package-keywords)
    (let ((tail (nthcdr (cl-position :custom use-package-keywords)
                        use-package-keywords)))
      (setcdr tail (cons :lint (cdr tail)))))

  (defun use-package-normalize/:lint (_name _keyword args)
    args)

  (defun use-package-handler/:lint (name-symbol _ hooks rest state)
    (let ((body (use-package-process-keywords name-symbol rest state)))
      (use-package-concat
       (cl-loop for it in hooks
                collect `(push (quote ,it) flymake-collection-hook-config))
       body)))

  (add-hook 'after-init-hook #'flymake-collection-hook-setup))

;;;;;;;;;;;;;;;;;;;;
;;;; formatting ;;;;

(use-package apheleia
  ;; Apheleia is a code auto-formatter. Its main selling point is that it does
  ;; all its formatting without moving the point. It comes pre-configured with a
  ;; few formatters, but it's possible to add more by configuring the
  ;; `apheleia-formatters' alist and `apheleia-mode-alist' to automatically
  ;; activate these formatters for major modes and filetypes. Check their
  ;; docstring for more details.
  :config
  (bind-keys :map global-map
             ("C-c C-f" . apheleia-format-buffer)))

(use-package apheleia-use-package
  ;; Adds :format keyword for use-package forms
  ;;
  ;; Possible variations:
  ;;
  ;; Specification of a new formatter
  ;; (use-package nix-mode
  ;;   :format
  ;;   (nixpkgs-fmt . ("nixpkgs-fmt" file))
  ;;   nix-mode other-nix-mode)
  ;;
  ;; Add an existing formatter to other modes
  ;; (use-package graphql-mode
  ;;   :format
  ;;   (prettier)
  ;;   graphql-mode)
  ;;
  ;; Short version of the above
  ;; (use-package graphql-mode
  ;;   :format
  ;;   prettier graphql-mode)
  :no-require
  :config
  (eval-when-compile
    (require 'cl-lib))

  (defvar use-package-keywords)
  (defvar eglot-workspace-configuration)
  (defvar eglot-server-programs)
  (declare-function use-package-concat "use-package-core")
  (declare-function use-package-process-keywords "use-package-core")

  (push :format use-package-keywords)

  (defun use-package-normalize/:format (_name _keyword args)
    "Normalizer for `:format' keyword in `use-package' forms.
The parameter ARGS is explained in the `use-package' documentation."
    (let ((format-spec (car args))
          (modes (cdr args)))
      (when (symbolp format-spec)
        (setq format-spec (list format-spec)))
      (cons format-spec modes)))
  (defun use-package-handler/:format (name _keyword args rest state)
    "Handler for `:format' keyword in `use-package' forms.
The parameter NAME, ARGS, REST, and STATE are explained in the
`use-package' documentation."
    (let ((format-spec (car args))
          (modes (cdr args))
          eval-form)
      (when (cdr format-spec)
        (push `(setf (alist-get ',(car format-spec) apheleia-formatters)
                ',(cdr format-spec))
              eval-form))
      (dolist (mode modes)
        (push `(setf (alist-get ',mode apheleia-mode-alist)
                ',(car format-spec))
              eval-form))
      (use-package-concat
       (use-package-process-keywords name rest state)
       `((eval-after-load 'apheleia-formatters ',(macroexp-progn (nreverse eval-form))))))))

;;;;;;;;;;;;;;;;
;;;; syntax ;;;;

(use-package treesit
  :disabled t
  :config
  ;; Emacs uses the dlopen function in libdl to load tree-sitter grammar
  ;; libraries (`libtree-sitter-*-.so'). Guix does not put these files in Emacs'
  ;; default search path, so we use `treesit-extra-load-path' to tell Emacs
  ;; where to look for them.
  (setopt treesit-extra-load-path '("~/.guix-home/profile/lib/tree-sitter")))

(use-package hl-todo
  ;; This tool by Jonas Bernoulli will apply highlighting to keywords that are
  ;; normally used in code comments. Simple and effective.
  :config
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package consult-todo
  :config
  (bind-keys :map +search-prefix-map
             ("t" . consult-todo)
             ("M-t" . consult-todo)))

;;;;;;;;;;;;;
;;;; lsp ;;;;

(use-package eglot
  ;; The built-in `eglot' feature, developed and maintained by João Távora, is
  ;; Emacs' own client for the Language Server Protocol (LSP). The LSP
  ;; technology is all about enhancing the ability of a text editor to work with
  ;; a given programming language. This works by installing a so-called
  ;; "language-server" on your computer, which the "LSP client" (i.e. `eglot')
  ;; will plug into. A typical language server provides the following
  ;; capabilities:
  ;;
  ;; Code completion
  ;;     This can be visualized for in-buffer automatic expansion of function
  ;;     calls, variables, and the like.
  ;; Code linting
  ;;     To display suggestions, warnings, or errors. These are highlighted in
  ;;     the buffer, usually with an underline, and can also be displayed in a
  ;;     standalone buffer with the commands `flymake-show-buffer-diagnostics',
  ;;     `flymake-show-project-diagnostics'.
  ;; Code navigation and cross-referencing
  ;;     While over a symbol, use a command to jump directly to its definition.
  ;;     The default key bindings for going forth and then back are `M-.'
  ;;     (`xref-find-definitions') and `M-,' (`xref-go-back').
  ;;     [Features such as the definition of the outline should, in principle,
  ;;     be implemented by the major mode though I see no reason why a language
  ;;     server cannot be involved in this task. You can use the built-in
  ;;     `outline-minor-mode' to provide Org-like folding capabilities for
  ;;     outline headings. I usually navigate the outline using minibuffer
  ;;     completion, with the help of `consult-outline'.]
  ;;
  ;; Assuming the language server is installed, to start using the LSP client in
  ;; a given file, do `M-x eglot'. To make this happen automatically for every
  ;; newly visited file, add a hook like this:
  ;;
  ;; (add-hook 'SOME-MAJOR-MODE #'eglot-ensure)
  ;;
  ;; My `eglot-use-package' package adds the `:lsp-ensure' keyword for
  ;; use-package forms to simplify this.
  :init
  (with-eval-after-load 'pulsar
    (dolist (func '(eglot-find-typeDefinition
                    eglot-find-declaration
                    eglot-find-implementation))
      (add-to-list 'pulsar-pulse-functions func)))
  :config
  ;; Ask Eglot to stay away from completely taking over Flymake. Just add it as
  ;; another item.
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)))
  ;; Stay away from ElDoc as well.
  (add-to-list 'eglot-stay-out-of 'eldoc)

  ;; Eglot automatically adds `eglot--mode-line-format' to `mode-line-misc-info'
  ;; I don't like that, so let's remove it.
  (defun +eglot-remove-mode-line-misc-info ()
    (setq mode-line-misc-info
          (assoc-delete-all 'eglot--managed-mode mode-line-misc-info)))
  (add-hook 'eglot-managed-mode-hook #'+eglot-remove-mode-line-misc-info)

  (setopt eglot-extend-to-xref t)

  (bind-keys
   :map eglot-mode-map
   ("C-c M-." . eglot-find-declaration)
   ("C-c C-." . eglot-find-typeDefinition)
   ("C-c M-?" . eglot-find-implementation)
   ("C-c C-a" . eglot-code-actions)
   ;; TODO: make a `+refactor-map' and put `eglot-rename' under it
   ("C-c C-r" . eglot-rename)))

(use-package eglot-use-package
  ;; Adds :lsp-ensure, :lsp-server, and :lsp-config keywords for use-package
  ;; forms.
  ;;
  ;; Possible variations
  ;;
  ;; (use-package go-ts-mode
  ;;   :lsp-ensure (go-ts-mode go-mod-ts-mode)
  ;;   :lsp-server ((go-ts-mode go-mod-ts-mode) . ("gopls"))
  ;;   :lsp-config '(gopls: . (:ui.completion.usePlaceholders t
  ;;                           :hints (:assignVariableTypes t
  ;;                                   :constantValues t
  ;;                                   :parameterNames t
  ;;                                   :rangeVariableTypes t)
  ;;                           :hoverKind "FullDocumentation")))
  :no-require
  :config
  (eval-when-compile
    (require 'cl-lib))

  (defvar use-package-keywords)
  (defvar eglot-workspace-configuration)
  (defvar eglot-server-programs)
  (declare-function use-package-concat "use-package-core")
  (declare-function use-package-process-keywords "use-package-core")

  (push :lsp-ensure use-package-keywords)
  (push :lsp-server use-package-keywords)
  (push :lsp-config use-package-keywords)

  (defun use-package-normalize/:lsp-ensure (_name _keyword args)
    "Normalizer for `:lsp-ensure' in `use-package' forms.
The parameter ARGS is explained in the `use-package' documentation."
    (setq args (flatten-list args))
    (cond
     ((symbolp args) (list args))
     ((and (listp args) (cl-every #'symbolp args)) args)
     (t (error "Invalid :lsp-ensure value: %S" args))))
  (defun use-package-handler/:lsp-ensure (name _keyword args rest state)
    "Handler for `:lsp-ensure' in `use-package' forms.
The parameters NAME, ARGS, REST, and STATE are explained in the
`use-package' documentation."
    (let (eval-forms)
      (dolist (mode args)
        (push `(add-hook ',(intern (concat (symbol-name mode) "-hook"))
                #'eglot-ensure)
              eval-forms))
      (use-package-concat
       (use-package-process-keywords name rest state)
       `((eval-after-load 'eglot ',(macroexp-progn (nreverse eval-forms)))))))

  (defun use-package-normalize/:lsp-server (_name _keyword args)
    "Normalizer for `:lsp-server' in `use-package' forms.
The parameter ARGS is explained in the `use-package' documentation."
    (cond
     ;; (mode . ("prog")) and ((mode1 mode2) . ("prog"))
     ((or (symbolp (caar args))
          (and (listp (caar args))
               (cl-every #'symbolp (caar args))))
      args)
     ;; everything else
     ((listp (caar args))
      (nth 0 args))
     (t (error "Invalid :lsp-server value: %S" args))))
  (defun use-package-handler/:lsp-server (name _keyword args rest state)
    "Handler for `:lsp-server' in `use-package' forms.
  The parameters NAME, ARGS, REST, and STATE are explained in the
  `use-package' documentation."
    (let (eval-forms)
      (when args
        (dolist (entry args)
          (push `(add-to-list 'eglot-server-programs ',entry) eval-forms)))
      (use-package-concat
       (use-package-process-keywords name rest state)
       `((eval-after-load 'eglot ',(macroexp-progn (nreverse eval-forms)))))))

  (defun use-package-normalize/:lsp-config (_name _keyword args)
    "Normalizer for `:lsp-config' in `use-package' forms.
The parameter ARGS is explained in the `use-package' documentation."
    (let ((configs (cdr (car args))))
      (cond
       ((keywordp (caar configs))
        configs)
       ((listp (caar configs))
        (car configs))
       (t
        (error "Invalid :lsp-config value: %S" args)))))
  (defun use-package-handler/:lsp-config (name _keyword args rest state)
    "Handler for `:lsp-config' in `use-package' forms.
The parameters NAME, ARGS, REST, and STATE are explained in the
`use-package' documentation."
    (let (eval-forms)
      (when args
        (dolist (entry args)
          (push `(setq-default eglot-workspace-configuration
                  (append eglot-workspace-configuration
                   '(,entry)))
                eval-forms)))
      (use-package-concat
       (use-package-process-keywords name rest state)
       `((eval-after-load 'eglot ',(macroexp-progn (nreverse eval-forms))))))))

;; TODO document eglot-tempel
(use-package eglot-tempel
  :init
  (eglot-tempel-mode t))

;; (use-package eglot-booster
;;   :disabled t
;;   :config
;;   (eglot-booster-mode))

(use-package consult-eglot
  :config
  ;; Eglot exposes the LSP `textDocument/documentSymbol' request through Imenu
  ;; and `workspace/symbol' through Xref by `xref-find-apropos' (when
  ;; `eglot-extend-to-xref' is non-nil). `consult-eglot' runs the interactive
  ;; selection of the targets from `workspace/symbols' through Consult instead
  ;; of completing-read, which enhances it with some useful Consult features
  ;; such as narrowing.
  ;;
  ;; The `workspace/symbols' call presents symbols from any file known to the
  ;; language server that belong to the same project.

  ;; Start `consult-eglot-symbols' search with active region, if available.
  (defalias '+consult-eglot-symbols-dwim 'consult-eglot-symbols)
  (consult-customize
   +consult-eglot-symbols-dwim
   :initial (when (use-region-p)
              (deactivate-mark)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))))

  (bind-keys :map eglot-mode-map
             ("C-M-." . +consult-eglot-symbols-dwim)))

;;;;;;;;;;;;;
;;;; dap ;;;;

;; TODO document dape
(use-package dape
  :config
  (bind-key "y" dape-global-map +prefix-map)
  (setopt dape-key-prefix "")

  (setopt dape-buffer-window-arrangement 'right
          dape-inlay-hints nil
          dape-info-hide-mode-line t)

  ;; Add CGO_ENABLED=0 to dlv config otherwise it doesn't work on NixOS.
  (setf (alist-get 'dlv dape-configs)
        '(modes (go-mode go-ts-mode)
          ensure dape-ensure-command
          command "dlv"
          command-args ("dap" "--listen" "127.0.0.1::autoport")
          command-cwd dape-command-cwd
          command-insert-stderr t
          port :autoport
          :request "launch"
          :type "go"
          :cwd "."
          :program "."
          :env (:CGO_ENABLED "0")))

  ;; Add CGO_ENABLED=0 to dlv-gotest config otherwise it doesn't work on NixOS.
  (add-to-list 'dape-configs
               '(dlv-gotest
                 modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd dape-command-cwd
                 command-insert-stderr t
                 port :autoport
                 :request "launch"
                 :type "debug"
                 :mode "test"
                 :cwd dape-cwd
                 :program
                 (lambda ()
                   (concat "./" (file-relative-name
                                 default-directory
                                 (funcall dape-cwd-fn))))
                 :args
                 (lambda ()
                   (when-let* ((test-name (go-test--get-current-test)
                                ;; or (gotest-ts-get-subtest-ts)
                                ))
                    (if test-name `["-test.run" ,test-name]
                     (error "No test selected"))))
                 :env (:CGO_ENABLED "0")))

  ;; Dape's default configuration assumes `codelldb' is manually installed in
  ;; `user-emacs-directory', which doesn't work with my NixOS setup. Here I
  ;; configure Dape to use the appropriate path for codelldb.
  (dolist (config '(codelldb-cc codelldb-rust))
    (setf (alist-get config dape-configs)
          (plist-put (alist-get config dape-configs)
                     'command "codelldb")))

  (add-to-list 'dape-configs
               '(debugpy-pytest
                 modes (python-mode python-ts-mode)
                 ensure
                 (lambda (config)
                   (dape-ensure-command config)
                   (let ((python (dape-config-get config 'command)))
                    (unless
                        (zerop
                         (process-file-shell-command
                          (format "%s -c \"import debugpy.adapter\"" python)))
                      (user-error "%s module debugpy is not installed" python))))
                 command "python"
                 command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport)
                 port :autoport
                 :request "launch"
                 :type "python"
                 :cwd dape-cwd
                 :module "pytest"
                 :args [dape-buffer-default]
                 :justMyCode nil
                 :console "integratedTerminal"
                 :showReturnValue t
                 :stopOnEntry nil))

  ;; Pulse source line.
  (with-eval-after-load 'pulsar
    (dolist (hook '(dape-display-source-hook))
      (add-hook hook #'pulsar-pulse-line)))

  ;; Dape automatically adds `global-mode-string' to `mode-line-misc-info'. I
  ;; don't like that, so let's remove it.
  (defun +dape-remove-mode-line-misc-info ()
    (setq-local mode-line-misc-info
                (assoc-delete-all 'global-mode-string mode-line-misc-info)))
  (add-hook 'dape-active-mode-hook #'+dape-remove-mode-line-misc-info)

  ;; Turn on global bindings for setting breakpoints with mouse.
  (dape-breakpoint-global-mode))

;; TODO Use apheleia-use-package and eglot-use-package as inspiration for
;; dape-use-package, add to local lisp directory
;; Adds :dap keyword to use-package forms
;; (use-package dape-use-package)

;;;;;;;;;;;;;;;;;;;;;;;
;;;; documentation ;;;;

(use-package eldoc
  :config
  ;; The built-in `eldoc' feature is especially useful in programming
  ;; modes. While we are in a function call, it produces an indicator in the
  ;; echo area (where the minibuffer appears upon invocation) that shows the
  ;; name of the function, the arguments it takes, if any, and highlights the
  ;; current argument we are positioned at. This way, we do not have to go back
  ;; to review the signature of the function just to rememeber its arity. Same
  ;; principle for variables, where `eldoc-mode' puts the first line of the
  ;; documentation string in the echo area.
  ;;
  ;; The `eldoc-documentation-compose' and `eldoc-documentation-compose-eagerly'
  ;; documentation strategies help compose information from multiple ElDoc
  ;; sources at the same time. The eager option displays results as they come
  ;; in; the other collates all the answers and displays them when they're all
  ;; ready. I like the eager option.
  (setopt eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)

  ;; ElDoc resizes the echo area which is intrusive. Let's not do that.
  (setopt eldoc-echo-area-use-multiline-p nil)

  ;; Skip showing documentation in the echo area and use an `eldoc-doc-buffer'
  ;; window if it is already displayed.
  (setopt eldoc-echo-area-prefer-doc-buffer t)

  ;; Don't use modeline for `eval-expression', etc.
  (setopt eldoc-message-function #'message)

  ;; ElDoc will query functions in `eldoc-documentation-functions' in the order
  ;; they're in and source documentation from them. Flymake diagnostics are more
  ;; urgent, so I want to make sure they're first. By default Flymake adds
  ;; itself to the end.
  (defun +eldoc-setup-elisp ()
    "Setup `eldoc-documentation-functions' for `emacs-lisp-mode' buffers."
    (setq-local eldoc-documentation-functions '(flymake-eldoc-function
                                                elisp-eldoc-var-docstring
                                                elisp-eldoc-funcall)))
  (defun +eldoc-setup-eglot ()
    "Setup `eldoc-documentation-functions' for `eglot-managed-mode' buffers."
    (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq-local eldoc-documentation-functions '(flymake-eldoc-function
                                                ;; markdown-eldoc-function
                                                ;; eglot-signature-eldoc-function
                                                eglot-hover-eldoc-function))
    (eldoc-mode +1))
  (add-hook 'emacs-lisp-mode-hook #'+eldoc-setup-elisp)
  (add-hook 'eglot-managed-mode-hook #'+eldoc-setup-eglot)

  ;; ElDoc detects movement and uses `eldoc-idle-delay' to determine when to ask
  ;; its backend documentation functions for information. To improve
  ;; performance, it doesn't trigger on every command; instead, it maintains a
  ;; list of common interactive commands. If you use things like Paredit or
  ;; Combobulate then it won't display if interact with one of those
  ;; commands. Luckily there's the `eldoc-add-command-completion' command.
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "combobulate-")

  (bind-keys
   :map eglot-mode-map
   ;; TODO make C-c C-d toggle eldoc buffer
   ("C-c C-d" . eldoc)
   ("C-h C-." . eldoc)))

;; TODO document help
(use-package help
  :no-require
  :config
  ;; Display `shortdoc' examples in *Help* buffers.
  (add-hook 'help-fns-describe-function-functions
            #'shortdoc-help-fns-examples-function 100)

  (setopt help-window-select t
          help-window-keep-selected t)

  (bind-keys :map help-map
             ("a" . describe-face) ; orig. `apropos-command'
             ("C-a" . find-face-definition) ; orig. `about-emacs'
             ("C" . describe-char) ; orig. `describe-coding-system'
             ("F" . apropos-function) ; parallel to f: `describe-function'
             ("C-f" . find-function) ; orig. `view-emacs-FAQ'
             ("C-h" . embark-prefix-help-command)
             ("K" . describe-keymap)
             ("C-k" . find-function-on-key) ; orig. `Info-goto-emacs-key-command-node'
             ("L" . apropos-library) ; orig. `describe-language-environment'
             ("C-l" . find-library) ; orig. `view-lossage'
             ("U" . apropos-user-option)
             ("V" . apropos-variable) ; parallel to v: `describe-variable'
             ("C-v" . find-variable)
             ("X" . apropos-command)
             ("." . display-local-help)))

;; TODO document helpful and find a better section for it
(use-package helpful
  :config

  ;; Can't be 1 if I want +helpful-next/previous to work
  (setopt helpful-max-buffers 5)

  (defvar +helpful--buffer-ring-size 5
    "How many buffers are stored for use with `*helpful-next'.")

  (defvar +helpful--buffer-ring (make-ring +helpful--buffer-ring-size)
    "Ring that stores the current Helpful buffer history.")

  (defun +helpful--buffer-index (&optional buffer)
    "If BUFFER is a Helpful buffer, return it’s index in the buffer ring."
    (let ((buf (or buffer (current-buffer))))
      (and (eq (buffer-local-value 'major-mode buf) 'helpful-mode)
           (seq-position (ring-elements +helpful--buffer-ring) buf #'eq))))

  (defun +record-new-buffers-a (buf)
    "Update the buffer ring according to the current buffer and HELP-BUF."
    (let ((buf-ring +helpful--buffer-ring))
      (let ((newer-buffers (or (+helpful--buffer-index) 0)))
        (dotimes (_ newer-buffers) (ring-remove buf-ring 0)))
      (when (/= (ring-size buf-ring) +helpful--buffer-ring-size)
        (ring-resize buf-ring +helpful--buffer-ring-size))
      (ring-insert buf-ring buf)))

  (defun +helpful--next (&optional buffer)
    "Return the next live Helpful buffer relative to BUFFER."
    (let ((buf-ring +helpful--buffer-ring)
          (index (or (+helpful--buffer-index buffer) -1)))
      (cl-block nil
        (while (> index 0)
          (cl-decf index)
          (let ((buf (ring-ref buf-ring index)))
            (if (buffer-live-p buf) (cl-return buf)))
          (ring-remove buf-ring index)))))

  (defun +helpful--previous (&optional buffer)
    "Return the previous live Helpful buffer relative to BUFFER."
    (let ((buf-ring +helpful--buffer-ring)
          (index (1+ (or (+helpful--buffer-index buffer) -1))))
      (cl-block nil
        (while (< index (ring-length buf-ring))
          (let ((buf (ring-ref buf-ring index)))
            (if (buffer-live-p buf) (cl-return buf)))
          (ring-remove buf-ring index)))))

  (defun +helpful-next ()
    "Go to the next Helpful buffer."
    (interactive)
    (if-let* ((buf (+helpful--next)))
        (funcall helpful-switch-buffer-function buf)
      (user-error "No helpful buffer to switch to")))

  (defun +helpful-previous ()
    "Go to the previous Helpful buffer."
    (interactive)
    (if-let* ((buf (+helpful--previous)))
        (funcall helpful-switch-buffer-function buf)
      (user-error "No helpful buffer to switch to")))

                                        ; Keep a record of buffers so our next/previous commands work.
  (advice-add #'helpful--buffer :filter-return #'+record-new-buffers-a)

  (bind-keys
   :map global-map
   ("C-c C-d" . helpful-at-point)
   :map help-map
   ("." . helpful-at-point)
   ("f" . helpful-callable)
   ("F" . helpful-function)
   ("k" . helpful-key)
   ("o" . helpful-symbol)
   ("v" . helpful-variable)
   ("x" . helpful-command)
   :map helpful-mode-map
   ("l" . +helpful-previous)
   ("r" . +helpful-next)))

;; TODO: document democratize
;; <https://flandrew.srht.site/listful/sw-emacs-democratize.html>
;; (use-package democratize
;;   :config
;;   (democratize-examples-in-helpful)
;;   (democratize-examples-in-help))

;; TODO document man
(use-package man
  :config
  (defun +man-copy-name-as-kill ()
    "Copy name of current man page into the kill ring."
    (interactive nil Man-mode)
    (when-let ((str
                (save-excursion
                  (goto-char (point-min))
                  (and (looking-at (rx bol )))))))
    (setq str
          (if (string-match " " Man-arguments)
              (let ((args (string-split Man-arguments " ")))
                (apply #'format "%s(%s)" (reverse args)))
            Man-arguments))
    (kill-new str)
    (message str))

  ;; Provide a `+man-index' command to quickly navigate to keywords within man
  ;; pages. The index is automatically built when needed. A piece of text is
  ;; considered a keyword if all of the following are true:
  ;; 1. At the beginning of the line with 2-8 spaces in front
  ;; 2. Uses the Man-overstrike face
  (defvar-local +man-index-index nil)
  ;; (defvar +man-index-kw-regexp "^ \\{2,8\\}")
  (defvar +man-index-kw-regexp
    "\\(?:^ \\{2,8\\}\\|, \\)\\(\\(?:\\S-\\|[[:punct:]]\\)+\\)")
  (defvar +man-index-kw-face 'Man-overstrike)
  (defun +man-index-build ()
    "Builds the +man-index-index hashtable."
    (unless (eq major-mode 'Man-mode)
      (error "Unsupported mode for +man-index-build"))
    (if (get-buffer-process (current-buffer))
        (error "man page still rendering, try again when done"))
    (setq-local +man-index-index (make-hash-table :test 'equal))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-end (line-end-position)))
          ;; Only consider lines starting with 2-8 spaces
          (when (looking-at "^ \\{2,8\\}")
            (let ((pos (point)))
              ;; Scan the whole line for Man-overstrike faces
              (while (< pos line-end)
                (let ((face (get-text-property pos 'face)))
                  (when (eq face +man-index-kw-face)
                    (let* ((start pos)
                           (end (next-single-property-change start 'face nil line-end))
                           (kw (string-trim
                                (buffer-substring-no-properties start end))))
                      (unless (gethash kw +man-index-index)
                        (puthash kw start +man-index-index)))
                    (setq pos (next-single-property-change pos 'face nil line-end)))
                  (setq pos (1+ pos))))))
          (forward-line 1)))))
  (defun +man-index-topics ()
    "Return a list of index topics."
    (unless +man-index-index
      (+man-index-build))
    (hash-table-keys +man-index-index))
  (defun +man-index (topic)
    "Go to TOPIC in the current man page."
    (interactive (list (completing-read "Topic: " (+man-index-topics) nil t)))
    (goto-char (gethash topic +man-index-index )))

  (bind-keys :map help-map
             ("M" . man)
             :map Man-mode-map
             ("i" . +man-index)
             ("q" . Man-kill)
             ("w" . +man-copy-name-as-kill)))

(use-package info
  :config
  (defun +Info-copy-current-node-name (arg)
    "Put the name of the current Info node into the kill ring.
The name of the Info file is prepended to the node name in parentheses.
With a prefix argument, put the name inside a function call to `info'."
    (interactive "P")
    (if arg
        (Info-copy-current-node-name 0)
      (Info-copy-current-node-name)))

  (bind-keys :map Info-mode-map
             ("w" . +Info-copy-current-node-name)))

;; TODO: document iman <https://github.com/emacsattic/iman/tree/master>
;; `iman' merges `man' and `info', and also stores an index.  this is a great
;; concept. merging in devdocs would be cool, but probably not worth the effort
;; (use-package iman
;;   :config
;;   (bind-keys :map help-map
;;              ("i" . iman)))

(use-package devdocs
  :config
  (defun +devdocs-lookup (&optional ask-docs initial-input)
    "Look up a DevDocs documentation entry.

Display entries in the documents `devdocs-current-docs' for
selection. With a prefix argument (or, from Lisp, if ASK-DOCS is
non-nil), first read the name of one or more installed documents and set
`devdocs-current-docs' for this buffer.

If INITIAL-INPUT is not nil, insert it into the minibuffer history. By
default, it is the symbol at point."
    (interactive "P")
    (let* ((initial-input (or initial-input (thing-at-point 'symbol t)))
           (docs (devdocs--relevant-docs ask-docs))
           (prompt "Go to documentation: "))
      ;; Clean up empty strings
      (when (and (stringp initial-input)
                 (string-match-p "\\`[[:space:]\n\r]*\\'" initial-input))
        (setq initial-input nil))
      ;; If we have a default, modify the prompt to show "(default: ...): "
      (when initial-input
        (setq prompt
              (replace-regexp-in-string
               (rx ": " eos)
               (format " (default: %s): " initial-input)
               prompt)))
      ;; Temporarily add the default to minibuffer-history so M-n can pick it.
      (let ((minibuffer-history (if initial-input
                                    (cons initial-input minibuffer-history)
                                  minibuffer-history)))
        ;; Use devdocs--read-entry directly (same as devdocs-lookup does).
        (let* ((entry (devdocs--read-entry prompt docs nil))
               (buffer (devdocs--render entry))
               (window (display-buffer buffer)))
          (when window
            (with-selected-window window
              (devdocs-goto-target)
              (recenter 0))
            (when devdocs-window-select
              (select-window window)))))))

  (defun +devdocs-visit ()
    "Visit the current devdocs page with a web browser."
    (interactive)
    (devdocs-copy-url)
    (browse-url-generic (current-kill 0)))

  (defmacro +devdocs-setup (feature hooks docs)
    "Define a devdocs setup function for FEATURE, adding DOCS in HOOKS."
    `(with-eval-after-load ',feature
      (defun ,(intern (format "+devdocs-%s-setup" feature)) ()
       (setq-local devdocs-current-docs ',docs))
      ,@(mapcar (lambda (hook)
                  `(add-hook ',hook #',(intern (format "+devdocs-%s-setup" feature))))
         hooks)))

  (+devdocs-setup python (python-mode-hook python-ts-mode-hook)
                  ("python~3.14" "numpy~2.2" "pandas~2" "statsmodels" "scikit_learn"
                   "scikit_image" "pytorch~2.7" "tensorflow" "matplotlib"))
  (+devdocs-setup js (js-mode-hook js-ts-mode-hook)
                  ("javascript" "dom" "node" "express" "npm" "http"))
  (+devdocs-setup json-ts-mode (json-ts-mode-hook js-json-mode-hook) ("npm" "jq"))
  (+devdocs-setup sgml (html-mode-hook) ("html" "htmx" "http"))
  (+devdocs-setup css-mode (css-base-mode-hook) ("css"))
  (+devdocs-setup cc-mode (c-mode-hook) ("c"))
  (+devdocs-setup go-ts-mode (go-ts-mode-hook) ("go" "http"))
  (+devdocs-setup nix-ts-mode (nix-ts-mode-hook) ("nix"))
  (+devdocs-setup sh-script (sh-base-mode-hook) ("bash"))
  (+devdocs-setup make-mode (makefile-gmake-mode-hook) ("gnu_make"))
  (+devdocs-setup magit (magit-mode-hook) ("git"))

  (bind-keys :map help-map
             ("D" . +devdocs-lookup)
             :map devdocs-mode-map
             ("v" . +devdocs-visit)))

;; (use-package rfc-mode)

;;;;;;;;;;;;
;;;; vc ;;;;

(use-package vc
  ;; The concept of "version control" pertains to a system of versioning files,
  ;; to track and visualise changes from record to record. These
  ;; version-controlled files may be part of a project.

  ;; There are many programs that fall in the category of Version Control
  ;; Software (VCS). I only use `git', simply because it is ubiquitous though
  ;; there are others which have technical merits as well.

  ;; VCSs have some common features, such as how they record a unit of history,
  ;; and how they handle the synchronisation of their state across
  ;; computers. Because of these commonalities, Emacs is able to provide a layer
  ;; of abstraction, known as "Version Control", else `vc.el' and its
  ;; accoutrements.

  ;; [Technically, the `vc.el' file is not the only one defining relevant
  ;; functionality. There are VCS-specific variants, such as `vc-git.el', as
  ;; well as complementary features like `vc-annotate.el'. All these hereinafter
  ;; referred to as `vc'.]

  ;; With `vc', we can carry out all the common actions related to version
  ;; control, such as to commit (to make a record of) changes and pull/push them
  ;; from/to the remote (i.e. the server with which we sync our
  ;; project). Whatever VCS we use, the workflow is the same:
  ;;
  ;; - Make changes to a file
  ;; - Type `C-x v v' (`vc-next-action')
  ;;   - If the file is already under version control, `vc' will produce a "log
  ;;     edit" buffer to let you commit the changes.
  ;;   - If the file is not under version control, `vc' will use a minibuffer
  ;;     prompt to ask which VCS to use. These are also known as backends and
  ;;     are stored in the user option `vc-handled-backends'.
  ;;   - If the file is not under version control but is in a directory which
  ;;     itself is version controlled, then the file will be added to the list
  ;;     of tracked files.
  ;; - Type `C-x v v' again and `vc' will proceed to the next action, which is to
  ;;   commit the changes to history. This is done in the next `log-edit'
  ;;   buffer.
  ;; - By convention, the message of each commit is separated into a summary and
  ;;   the body of the message. An empty line divides them. The summary is the
  ;;   first line of the message and should, as a matter of best practices, be
  ;;   brief yet sufficiently descriptive. The rest is free form text. In the
  ;;   `log-edit' buffer, the empty separator line between the summary and the
  ;;   body is shown as a border, so there is no need to add another line there.
  ;; - Once the message is ready, type `C-c C-c' (`log-edit-done') to confirm it
  ;;   or `C-c C-k' (`log-edit-kill-buffer') to cancel the operation.
  ;; - From the `log-edit' buffer, it is possible to see the underlying changes
  ;;   in a diff buffer. Do it with `C-c C-d' (`log-edit-show-diff').
  ;; - The record of commits to the history of the entire project is accessed
  ;;   with the command `vc-print-root-log', while that of individual files is
  ;;   handled by the command `vc-print-log'.
  ;; - To pull from a remote, do `vc-update'. To push, invoke `vc-push'.
  ;; - A Dired-like buffer is also available to perform these actions across
  ;;   many edited files. Check the commands `vc-dir', `vc-root-dir', or even
  ;;   `project-vc-dir'.
  ;; - Merge conflicts are handled in the affected files with the help of the
  ;;   built-in `smerge-mode'.

  ;; There is more functionality, though this should already give you an
  ;; overview of what is on offer. The gist is that `vc' provides a fast and
  ;; minimalist way to accomplish the basic tasks related to version
  ;; control. For more demanding operations, there is either the command-line or
  ;; the wonderful `magit' Emacs package.
  :config
  (require 'vc-dir)
  (require 'vc-git)
  (require 'log-view)
  (require 'log-edit)

  ;; I can see the files with "C-c C-f"
  (remove-hook 'log-edit-hook #'log-edit-show-files)

  ;; Remember more commit messages
  (setopt log-edit-maximum-comment-ring-size 1000)

  ;; `consult-history' support for `log-edit-mode'
  (with-eval-after-load 'consult
    (add-to-list 'consult-mode-histories
                 '(log-edit-mode
                   log-edit-comment-ring
                   log-edit-comment-ring-index
                   log-edit-beginning-of-line)))

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'log-edit-comment-ring))

  ;; The default `vc-git-expanded-log-entry' makes it hard to visually see where
  ;; the commit message starts and ends. I redefine it here to add a newline at
  ;; the top and indent it by 2 spaces.
  (defun vc-git-expanded-log-entry (revision)
    (with-temp-buffer
      (apply #'vc-git-command t nil nil
             `("log"
               ,revision
               "-1" "--no-color" ,@(ensure-list vc-git-log-switches)
               "--"))
      (goto-char (point-min))
      (insert "\n")
      (unless (eobp)
        (while (re-search-forward "^  " nil t)
          (replace-match "")
          (forward-line))
        ;; Indent the expanded log entry.
        (indent-rigidly (point-min) (point-max) 2)
        (buffer-string))))

  (setopt vc-follow-symlinks t
          vc-find-revision-no-save t
          vc-git-log-edit-summary-target-len 50
          vc-git-log-edit-summary-max-len 70
          vc-git-diff-switches '("--patch-with-stat" "--histogram")
          vc-git-print-log-follow t
          vc-git-log-switches "--stat"
          vc-git-root-log-format
          `("%d%h %ad %an: %s"
            ;; The first shy group matches the characters drawn by --graph.
            ;; We use numbered groups because `log-view-message-re' wants the
            ;; revision number to be group 1.
            "^\\(?:[*/\\| ]+ \\)?\
\\(?2: ([^)]+)\\)?\\(?1:[0-9a-z]+\\) \
\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \
\\(?3:.*?\\):"
            ((1 'log-view-message)
             (2 'change-log-list nil lax)
             (3 'change-log-name)
             (4 'change-log-date)))
          vc-git-revision-complete-only-branches t)

  (bind-keys :map +prefix-map
             ("v" . vc-prefix-map)
             :map vc-prefix-map
             ("c" . vc-prepare-patch)
             ("e" . vc-ediff)
             ("F" . vc-update) ; symmetric with P: `vc-push'
             ("k" . vc-delete-file) ; 'k' for kill==>delete is more common
             ("x" . nil) ; unmap `vc-delete-file'
             :map vc-dir-mode-map
             ("c" . vc-prepare-patch)
             ("d" . vc-diff) ; orig `vc-dir-clean-files', parallel to D: `vc-root-diff'
             ("F" . vc-update) ; symmetric with P: `vc-push'
             ("k" . vc-dir-delete-file) ; 'k' for kill==>delete is more common
             :map vc-git-stash-shared-map
             ("k" . vc-git-stash-delete-at-point) ; symmetry with `vc-dir-delete-file'
             :map log-edit-mode-map
             ("M-r" . consult-history) ; orig. `log-edit-comment-search-backward'
             ("M-s" . nil)  ; unmap `log-edit-comment-search-forward'
             :map log-view-mode-map
             ("RET" . log-view-find-revision) ; orig. `log-edit-toggle-entry-display'
             ("<tab>" . log-view-toggle-entry-display) ; orig. `log-view-msg-next'
             ("<backtab>" . nil) ; unmap `log-view-msg-prev'
             ("c" . vc-prepare-patch)
             ("f" . nil) ; unmap `log-view-find-revision'
             ("F" . vc-update)
             ("P" . vc-push)
             ("s" . vc-log-search)
             :map diff-mode-map
             ("L" . vc-print-root-log)
             ("u" . vc-revert)
             ;; Emacs 29 can use "C-x v v" in diff buffers, which is great, but
             ;; now I need quick access to it.
             ("v" . vc-next-action)))

(use-package magit
  ;; The `magit' package, maintained by Jonas Bernoulli, is the best front-end
  ;; to `git' I have ever used. Not only is it excellent at getting the job
  ;; done, it also helps you learn more about what `git' has to offer.

  ;; At the core of its interface is `transient'. This is a library that was
  ;; originally developed as Magit-specific code that was then abstracted away
  ;; and ultimately incorporated into Emacs version 29. With `transient', we get
  ;; a window pop up with keys and commands corresponding to them. The window is
  ;; interactive, as the user can set a value or toggle an option and have it
  ;; take effect when the relevant command is eventually invoked. For `git', in
  ;; particular, this interface is a genious way to surface the plethora of
  ;; options.

  ;; To start, call the command `magit-status'. It brings up a buffer that shows
  ;; information about the state of the repository. Sections include an overview
  ;; of the current `HEAD', untracked files, unstaged changes, staged changes,
  ;; and recent commits. Each section's visibility state can be cycled by
  ;; pressing `TAB' (variations of this are available--remember to do `C-h m'
  ;; (`describe-mode') in an unfamiliar major mode to get information about its
  ;; key bindings).

  ;; From the status buffer, we can perform all the usual version control
  ;; operations. By typing `?' (`magit-dispatch'), we bring up the main
  ;; `transient' menu, with keys that then bring up their own submenus, such as
  ;; for viewing commit logs, setting the remotes, switching branches, etc.

  ;; Before I used `magit', I only knew the basics of adding files for a commit,
  ;; writing a commit message inline with the `-m' flag on the command line, and
  ;; pushing to the remote. Magit shows the staging area in the status buffer
  ;; and makes "staging" a key part of the process of committing changes to
  ;; history. To stage something, is to make it a candidate for the next commit
  ;; action: only the staged parts are commited.

  ;; Magit has a refined understanding of context. We can target an individual
  ;; line, a single diff hunk, a single file, or a range of files for staging or
  ;; unstaging (among others). If the region is active, then only the selection
  ;; is affected. If the cursor is on or somewhere inside a diff hunk, then that
  ;; is targeted. If the cursor is over a file, then the file is the
  ;; target. Same idea for the section heading, which extends to everything
  ;; under it.

  ;; This contextuality extends to every `git' command that accepts a commit
  ;; hash as an argument. For example, if we are in a Magit commit log view and
  ;; want to do a hard reset on the commit at point, Magit knows what commit
  ;; hash to use (and presents it as an option when we choose where to reset
  ;; to). Same principle for rebasing, cherry picking, and more.

  ;; Magit is good for newer users but also for those who have experience with
  ;; `git' and the command-line in general. With it, I can easily maintain a
  ;; project that needs to track separate remotes and push/pull between them in
  ;; a fairly complicated manner. Partly supported by `transient' and partly by
  ;; the Emacs completion user interface, we have all we need to select targets
  ;; with ease.

  ;; The only downside of this wonderful package is that it is slow on Windows
  ;; (based on what others have told me and showed me)... I don't normally use
  ;; Windows but in the case I forced to a combination of `vc' and the
  ;; command-line will suffice.
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (setq git-commit-summary-max-length 50
        ;; I used to also include `overlong-summary-line' in this list, but I
        ;; realised I do not need it. My summaries are always in check. When I
        ;; exceed the limit, it is for a good reason.
        git-commit-style-convention-checks '(non-empty-second-line))

  (setopt magit-diff-refine-hunk t
          magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
          magit-commit-diff-inhibit-same-window nil)

  ;; When 'C-c C-c' or 'C-c C-k' are pressed in the Magit commit message buffer,
  ;; delete the magit-diff buffer related to the current repo.
  (defun +magit-kill-diff-buffer-in-current-repo (&rest _)
    "Kill the `magit-diff-mode' buffers related to the current repository."
    (let ((magit-diff-buffer-in-current-repo (magit-get-mode-buffer 'magit-diff-mode)))
      (kill-buffer magit-diff-buffer-in-current-repo)))
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (add-hook 'with-editor-post-finish-hook
                        #'+magit-kill-diff-buffer-in-current-repo nil t)
              (add-hook 'with-editor-post-cancel-hook
                        #'+magit-kill-diff-buffer-in-current-repo nil t)))

  ;; Magit has a penchant for open buffers. Jonas Bernoulli already explained
  ;; the reasoning behind this behavior.
  ;; <https://github.com/magit/magit/issues/2124#issuecomment-125987469>.
  ;;
  ;; But he also suggested a solution to clean up Magit-related buffers when the
  ;; work is done. This function collects the available Magit buffers, then
  ;; restores the window configuration as it was before calling `magit-status',
  ;; and finally applies `kill-buffer' on the buffers previously collected.
  (defun +magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  (bind-keys :map vc-prefix-map
             ("V" . magit-status)
             :map +project-prefix-map
             ("V" . magit-project-status)
             :map magit-status-mode-map
             ;; Apply our +magit-kill-buffers command only in magit-status
             ("q" . +magit-kill-buffers)))

(use-package diff-mode
  :config
  ;; This built-in mode is an easy and effective way to interact with diffs. A
  ;; "diff" is a Unix tradition of showing line-wise differences in a file. If
  ;; you, say, edit a line to replace "this" with "that", the diff output will
  ;; show the original line prefixed with a minus sign and the new line prefixed
  ;; with a plus sign.

  ;; With `diff-mode', we can write a patch file with `C-x C-w' (`write-file').
  ;; We can also apply the "diff hunk" at point, if we have the corresponding
  ;; files. The diff hunk is the section of the diff that pertains to a given
  ;; region in the file and is delimited by a heading that enumerates the
  ;; affected range, like `@@ -6125,7 +6125,9 @@'. Type `C-c C-a'
  ;; (`diff-apply-hunk').

  ;; The `diff-mode' buffers specify the `outline-regexp', meaning that they can
  ;; be used with the buit-in `outline-minor-mode' to, for example, fold the
  ;; individual diff hunks and move between them.

  ;; Outside of Emacs, I have settings for `git' with produce more informative
  ;; diff hunk headings in Elisp and Org buffers. See what Prot wrote about it
  ;; here: https://protesilaos.com/codelog/2021-01-26-git-diff-hunk-elisp-org/.

  (setopt diff-default-read-only t
          diff-font-lock-prettify t)

  (add-hook 'diff-mode-hook #'outline-minor-mode)

  (bind-keys :map diff-mode-map
             ("TAB" . outline-cycle)
             ("M-n" . diff-file-next)
             ("M-o" . nil) ; unmap `diff-goto-source'
             ("M-p" . diff-file-prev)))

(use-package ediff
  ;; The built-in `ediff' provides several commands that let us compare files or
  ;; buffers side-by-side. The defaults of `ediff' are bad, in my opinion: it
  ;; puts buffers one on top of the other and places the "control panel" in a
  ;; speparate Emacs frame. The first time I tried to use it, I thought I broke
  ;; my setup because it is unlike anything we normally interact with. As such,
  ;; the settings I have for `ediff-split-window-function' and
  ;; `ediff-window-setup-function' are what I would expect Emacs maintainers to
  ;; adopt as the new default. I strongly encourage everyone to start with them.

  ;; In my workflow, the points of entry to the `ediff' feature are the commands
  ;; `ediff-files', `ediff-buffers'. Sometimes I use the 3-way variants with
  ;; `ediff-files3' and `ediff-buffers3', though this is rare.
  :init
  (defun +ediff-store-layout ()
    "Store current frame window configuration as a frame parameter.
Add this function to the `ediff-before-setup-hook'.

Also see `+ediff-restore-layout'."
    (let ((frame (selected-frame)))
      (set-frame-parameter
       frame
       '+ediff-last-layout
       (current-window-configuration frame))))

  (defun +ediff-restore-layout ()
    "Restore the frame's window configuration.
Add this function to the `ediff-quit-hook'.

Also see `+ediff-store-layout'."
    (if-let* ((layout (frame-parameter (selected-frame) '+ediff-last-layout)))
        (set-window-configuration layout)
      ;; We do not signal a `user-error' here because that would prevent
      ;; `ediff-quit' from closing the Ediff session.
      (message "No Ediff window configuration for the current frame")))

  (add-hook 'ediff-before-setup-hook #'+ediff-store-layout)
  (add-hook 'ediff-quit-hook #'+ediff-restore-layout)

  (setopt ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-keep-variants nil
          ediff-make-buffers-readonly-at-startup nil
          ediff-show-clashes-only t))

(use-package diff-hl
  ;; `diff-hl' use the margins or fringes to highlight changes in the current
  ;; buffer. The indicators are colour-coded to denote whether a change is an
  ;; addition, removal, or change that includes a bit of both.
  ;;
  ;; This package offers some more features, such as the ability to move between
  ;; diff hunks while editing the buffers.
  ;;
  ;; I like to lean into native/built-in Emacs functionality where it's equal or
  ;; better than the third-party alternatives. `diff-hl' relies on the built-in
  ;; `vc.el' library instead of talking to git directly (thus expanding support
  ;; to whatever VCs vc.el supports, and not git alone), which also means it can
  ;; take advantage of its caching and other user configuration for vc.el.
  ;; Overall, it should be faster and lighter than something like `git-gutter'

  :init
  ;; I hate it when packages hard-code global keybindings. Do not touch my
  ;; global bindings unless I explicitly say so. And now trying to undo what the
  ;; package author thought was helpful causes unnecessary errors. I cannot set
  ;; `diff-hl-command-prefix' to nil or "" because then everything blows up.
  (setq-default diff-hl-command-prefix (kbd "C-x v ,"))

  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  :config
  ;; Redefine fringe bitmaps to be sleeker by making them solid bars (with no
  ;; border) that only take up up half the horizontal space in the fringe. This
  ;; approach lets us avoid robbing fringe space from other packages/modes that
  ;; may need to benefit from it (like magit, flymake, or flyspell).
  (if (fboundp 'fringe-mode) (fringe-mode '8))
  (setq-default fringes-outside-margins t)

  (defun +diff-hl-define-thin-bitmaps-a (&rest _)
    (let* ((scale (if (and (boundp 'text-scale-mode-amount)
                           (numberp text-scale-mode-amount))
                      (expt text-scale-mode-step text-scale-mode-amount)
                    1))
           (spacing (or (and (display-graphic-p) (default-value 'line-spacing)) 0))
           (h (+ (ceiling (* (frame-char-height) scale))
                 (if (floatp spacing)
                     (truncate (* (frame-char-height) spacing))
                   spacing)))
           (w (min (frame-parameter nil (intern (format "%s-fringe" diff-hl-side)))
                   diff-hl-bmp-max-width))
           (_ (if (zerop w) (setq w diff-hl-bmp-max-width))))
      (define-fringe-bitmap 'diff-hl-bmp-middle
        (make-vector
         h (string-to-number (let ((half-w (1- (/ w 2))))
                               (concat (make-string half-w ?1)
                                       (make-string (- w half-w) ?0)))
                             2))
        nil nil 'center)))

  (advice-add 'diff-hl-define-bitmaps :after #'+diff-hl-define-thin-bitmaps-a)

  (defun +diff-hl-type-at-pos-fn (type _pos)
    (if (eq type 'delete)
        'diff-hl-bmp-delete
      'diff-hl-bmp-middle))
  (setq diff-hl-fringe-bmp-function #'+diff-hl-type-at-pos-fn)
  (setq diff-hl-dired-fringe-bmp-function #'+diff-hl-type-at-pos-fn)
  (setq diff-hl-draw-borders nil)

  (defun +diff-hl-make-diff-hl-faces-transparent-h ()
    (mapc (lambda (face)
            (set-face-background face nil))
          '(diff-hl-insert
            diff-hl-delete
            diff-hl-change)))

  (add-hook 'diff-hl-mode-hook #'+diff-hl-make-diff-hl-faces-transparent-h)

  ;; FIX: To minimize overlap between flymake indicators and diff-hl indicators
  ;;   in the left fringe.
  (with-eval-after-load 'flymake
    ;; Let diff-hl have left fringe, flymake can have right fringe
    (setq flymake-fringe-indicator-position 'right-fringe)
    ;; A non-descript, left-pointing arrow
    (define-fringe-bitmap 'flymake-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)

    (setq flymake-error-bitmap '(flymake-fringe-bitmap-double-arrow
                                 compilation-error)
          flymake-warning-bitmap '(flymake-fringe-bitmap-double-arrow
                                   compilation-warning)
          flymake-note-bitmap '(flymake-fringe-bitmap-double-arrow
                                compilation-info)))

  ;; `diff-hl' should just work with Tramp. But slow or high latency connections
  ;; can cause performance problems. The following configuration inhibits it in
  ;; remote buffers.
  (setopt diff-hl-disable-on-remote t)
  (defun +diff-hl-enable-maybe-h ()
    "Conditionally enable `diff-hl-dired-mode' in Dired buffers.
Respects `diff-hl-disable-on-remote'."
    ;; Neither `diff-hl-dired-mode' or `diff-hl-dired-mode-unless-remote'
    ;; respect `diff-hl-disable-on-remote', so...
    (unless (and (bound-and-true-p diff-hl-disable-on-remote)
                 (file-remote-p default-directory))
      (diff-hl-dired-mode +1)))
  (add-hook 'dired-mode-hook #'+diff-hl-enable-maybe-h)

  ;; Update `diff-hl' when `magit' alters git state.
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  ;; Don't delete the current hunk's indicators while we're editing.
  (defvar +diff-hl--pending-update nil)

  ;; BUG: flydiff errors out when no there are no commits
  (defun +diff-hl--debounced-update ()
    "Schedule a diff-hl update when Emacs becomes idle for a short time."
    (unless +diff-hl--pending-update
      (setq +diff-hl--pending-update
            (run-with-idle-timer
             2.0 nil
             (lambda ()
               (setq +diff-hl--pending-update nil)
               (when diff-hl-flydiff-mode
                 (diff-hl-flydiff-update)))))))

  (add-hook 'after-change-functions
            (defun +diff-hl--after-change (_beg _end _len)
              (+diff-hl--debounced-update)))

  (setopt diff-hl-global-modes '(not image-mode pdf-view-mode))
  ;; Don't block Emacs when updating `diff-hl'.
  ;; (setopt diff-hl-update-async t)
  ;; Get realtime feedback in diffs after staging/unstaging hunks.
  (setopt diff-hl-show-staged-changes nil)

  ;; Recenter to location of diff.
  (advice-add 'diff-hl-next-hunk
              :after (defun +diff-hl-recenter (&optional _) (recenter)))

  (defvar-keymap diff-hl-repeat-map
    :repeat (:hints ((diff-hl-next-hunk . "next")
                     (diff-hl-previous-hunk . "prev")
                     (diff-hl-show-hunk-next . "show next")
                     (diff-hl-show-hunk-previous . "show prev")
                     (diff-hl-show-hunk . "show")
                     (diff-hl-revert-hunk . "revert")
                     (diff-hl-stage-dwim . "Stage"))))

  (bind-keys :map +toggle-prefix-map
             ("d" . diff-hl-mode)
             :map vc-prefix-map
             ("n" . diff-hl-next-hunk)
             ("p" . diff-hl-previous-hunk)
             ("S" . diff-hl-stage-dwim)
             ("U" . diff-hl-revert-hunk)
             ("*" . diff-hl-show-hunk)
             ("SPC" . diff-hl-mark-hunk)
             :map diff-hl-mode-map
             ([remap vc-diff] . nil) ; undo the default remapping of `vc-diff'
             :map diff-hl-command-map
             ("n" . diff-hl-next-hunk)
             ("p" . diff-hl-previous-hunk)
             ("]" . nil) ; orig. diff-hl-next-hunk
             ("[" . nil) ; orig. diff-hl-previous-hunk
             ("}" . diff-hl-show-hunk-next)
             ("{" . diff-hl-show-hunk-previous)
             ("U" . diff-hl-revert-hunk)
             ("SPC" . diff-hl-mark-hunk)
             :map diff-hl-repeat-map
             ("n" . diff-hl-next-hunk)
             ("p" . diff-hl-previous-hunk)
             ("}" . diff-hl-show-hunk-next)
             ("{" . diff-hl-show-hunk-previous)
             ("*" . diff-hl-show-hunk)
             ("u" . nil) ; orig. diff-hl-revert-hunk
             ("U" . diff-hl-revert-hunk)
             ("S" . diff-hl-stage-dwim))

  (with-eval-after-load 'diff-hl-show-hunk
    (bind-keys :map diff-hl-show-hunk-map
               ;; Unfortunately there's no easier way to change the footer in the
               ;; `diff-hl-show-hunk-inline-popup' other then redefining the whole
               ;; function, so it will be misleading with these bindings.
               ("[" . nil) ; orig. diff-hl-show-hunk-previous
               ("]" . nil) ; orig. diff-hl-show-hunk-next
               ("S" . diff-hl-show-hunk-stage-hunk)
               ("c" . diff-hl-show-hunk-copy-original-text)
               ("n" . diff-hl-show-hunk-next)
               ("p" . diff-hl-show-hunk-previous)
               ("U" . diff-hl-show-hunk-revert-hunk)
               ("{" . diff-hl-show-hunk-previous)
               ("}" . diff-hl-show-hunk-next))))

;;;;;;;;;;;;;;;;
;;;; comint ;;;;

(use-package comint
  ;; The built-in `comint' library defines the infrastructure necessary to run a
  ;; command line shell or other Read Evaluate Print Loop (REPL) interfaces. It
  ;; underpins the standart `shell' as well as the built-in Emacs Lisp REPL of
  ;; `ielm'.
  ;;
  ;; What I define here are some basic tweaks to control the behaviour of Comint
  ;; buffers. The `ansi-color-for-comint-mode' in particular takes care to
  ;; interpret the ANSI escape sequences such that they produce the same result
  ;; they would have had they been executed in a terminal emulator. For example:
  ;;
  ;;     echo -e "\e[31mThis text is red\e[0m"
  ;;
  ;; I never want to see the escape sequences. They look busy and will distort
  ;; the output when there are lots of them. In the section about `compile', I
  ;; have a similar setting for the compilation buffers.
  :config

  ;; Support for OS-specific escape sequences such as what `ls --hyperlink'
  ;; uses. I normally don't use those, but I am checking this to see if there
  ;; are any obvious advantages/disadvantages.
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output)

  ;; Mirror `display-buffer-base-action' into `display-comint-buffer-action'.
  (setopt display-comint-buffer-action display-buffer-base-action)

  (setq-default comint-scroll-to-bottom-on-input t
                comint-scroll-to-bottom-on-output nil
                comint-input-autoexpand 'input)

  (setopt comint-prompt-read-only t
          comint-buffer-maximum-size 9999
          comint-completion-autolist t
          comint-input-ignoredups t))

;;;;;;;;;;;;;;;;;;;;;
;;;; compilation ;;;;

(use-package compile
  ;; Similar to the `comint' library, Emacs comes with a built-in interface for
  ;; running compilation-related commands. In principle, any shell command will
  ;; do. The output is collected in a buffer which (i) keeps track of errors and
  ;; warnings, and (ii) adds direct links to the relevant sources.
  ;;
  ;; The way to modify hot `compile' adds those links are described in great
  ;; detail in the doc string of the variable
  ;; `compilation-error-regexp-alist'. Here is a sample:
  ;;
  ;;     (add-to-list 'compilation-error-regexp-alist-alist
  ;;                   '(+compile-sample
  ;;                     "^[\s\t]*\\(?:.*(\\)\\(?1:.*\\):\\(?2:[0-9]+\\)?:\\(?3:[0-9]+\\)?"
  ;;                     1 2 3))
  ;;
  ;;     (add-to-list 'compilation-error-regexp-alist '+compile-sample)
  ;;
  ;; The difference between `compile' and `comint' is that the latter is
  ;; interactive and does not add links to errors/warnings. Use whichever one is
  ;; relevant to the task at hand.
  :config

  ;; Set `compile' up to handle ANSI escape sequences, like I do with `comint'.
  ;; Note that `compilation-filter-hook' is a set of filters to be applied to
  ;; the output of our compiler.
  (setopt ansi-color-for-compilation-mode t)
  (setopt ansi-osc-for-compilation-buffer t)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook 'ansi-osc-compilation-filter)

  (setopt compilation-scroll-output t ; Automatically scroll build output
          compilation-always-kill t ; Kill compilation process before starting another.
          compilation-message-face 'default ; Don't underline.
          compilation-ask-about-save nil ; I'm not scared of saving everything
          )

  (defun +compile (&optional arg)
    "Runs `project-compile'.
With prefix argument ARG (\\[universal-argument]), prompt for a
directory to run `compile'."
    (interactive "P")
    (if arg
        (let* ((dir (read-directory-name "Compile in directory: " default-directory nil t))
               (default-directory dir)
               (current-prefix-arg nil)  ; prevent propagation to `compile'
               (compilation-read-command t)
               (orig (symbol-function 'read-from-minibuffer)))
          (fset 'read-from-minibuffer
                (lambda (_prompt initial &rest args)
                  (apply orig (format "Compile command in %s: " dir) initial args)))
          (unwind-protect
              (call-interactively 'compile)
            (fset 'read-from-minibuffer orig)))
      ;; This ensures the ensuing interactive call to `compile' uses the buffer
      ;; local value of `compile-command' of the buffer we called `+compile'
      ;; from. Calling `project-compile' interactively doesn't seem to do that.
      ;; (call-interactively 'project-compile)
      (let ((default-directory (project-root (project-current t)))
            (compilation-buffer-name-function
             (or project-compilation-buffer-name-function
                 compilation-buffer-name-function)))
        (call-interactively 'compile))))

  (defun +compile-send-input (input &optional nl)
    "Send INPUT to the current process.
Interactively also sends a terminating newline."
    (interactive "MInput: \nd")
    (let ((string (concat input (if nl "\n"))))
      ;; This is just for visual feedback
      (let ((inhibit-read-only t))
        (insert-before-markers string))
      ;; This is the important stuff
      (process-send-string
       (get-buffer-process (current-buffer))
       string)))
  (defun +comint-send-self ()
    "Send the pressed key to the current process."
    (interactive)
    (+compile-send-input
     (apply #'string
            (append (this-command-keys-vector) nil))))

  (defun +compile-toggle-comint ()
    "Restart compilation with (or without) `comint-mode'."
    (interactive)
    (cl-callf (lambda (mode) (if (eq mode t) nil t))
        (elt compilation-arguments 1))
    (recompile))

  (defun +compile-input-from-history ()
    "Insert command from compile input history."
    (interactive)
    (let* ((history compile-history)
           (default (car history))
           (selected (completing-read
                      (format-prompt "Insert input from history" default)
                      history nil nil nil 'compile-history default)))
      (delete-minibuffer-contents)
      (insert selected)))

  (defvar +compile-commands
    '(;; (c++-mode-hook
      ;;       . (concat "g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion \
      ;; -O2 -I/Users/admin/problems/include " buffer-file-name " && ./a.out"))
      ;; (c++-ts-mode-hook
      ;;       . (concat "g++ -std=c++17 -Wall -Wextra -Wshadow -Wno-sign-conversion \
      ;; -O2 -I/Users/admin/problems/include " buffer-file-name " && ./a.out"))
      (go-ts-mode-hook . (concat "go run " buffer-file-name " "))
      ;; (go-ts-mode-hook . "go build -o a.out ")
      (rust-ts-mode-hook . (concat "cargo run "))
      ;; (rust-ts-mode-hook . "rustc -o a.out ")
      (sh-mode-hook . (concat buffer-file-name " "))
      (python-ts-mode-hook . (concat "python " buffer-file-name " "))))

  (dolist (pair +compile-commands)
    (let ((mode (car pair))
          (command (cdr pair)))
      (add-hook mode
                (lambda ()
                  (setq-local compile-command command)))))

  (add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

  (bind-keys
   :map +prefix-map
   ("C-," . +compile)
   ("C-." . recompile)
   :map compilation-mode-map
   ("C-x C-q" . +compile-toggle-comint)
   ("C-d" . +comint-send-self)
   ("C-j" . +comint-send-self)
   ("y" . +comint-send-self)
   ("n" . +comint-send-self)
   :map compilation-minor-mode-map
   ("C-x C-q" . +compile-toggle-comint)
   :map compilation-shell-minor-mode-map
   ("C-x C-q" . +compile-toggle-comint)
   :map minibuffer-local-shell-command-map
   ("M-r" . +compile-input-from-history)
   :map +goto-prefix-map
   ("n" . next-error)
   ("M-n" . next-error)
   ("p" . previous-error)
   ("M-p" . previous-error)))

;; TODO create compile-use-package that adds to compile-commands. or use
;; compile-multi-use-package
;; Adds :compile keyword to use-package forms
;; (use-package compile-use-package)

(use-package compile-multi
  :config
  ;; `compile-multi' is a multi-target interface to `compile'. It lets you
  ;; configure and interactively select compilation commands based on arbitrary
  ;; project types, build frameworks, or test tools.
  ;;
  ;; Conceptually, it defines a framework of triggers and actions. A trigger is
  ;; any predicate that applies to the current file, project, or directory. An
  ;; action is something that runs in response, such as a shell command or an
  ;; interactive function. For example, we might define a trigger that detects a
  ;; Makefile and then automatically parses its targets into a list of
  ;; selectable actions.
  ;;
  ;; The result is a flexible interface for building context-aware compilation
  ;; menus, which allows us to invoke the right command without needing to
  ;; hardcode it in advance. Currently I have automatic target generation
  ;; functions for Makefiles and rudimentary support for Nix flakes.

  ;; I do not rely on the `prefix-argument' to run `compile' commands in
  ;; `compilation-shell-minor-mode', because I have "C-x C-q" bound to
  ;; `+compile-toggle-comint' in the relevant modes. For that reason, I redefine
  ;; `compile-multi' to ignore the prefix argument behavior. The trade-off is
  ;; that I cannot preemptively run a compile command in
  ;; compilation-shell-minor-mode, but I like using "C-u" to be able to edit
  ;; compile-multi targets before running them.
  (defun compile-multi (&optional query command)
    "Multi-target interface to compile.
With optional argument QUERY allow user to modify compilation command before
running. COMMAND when set will be used instead of prompting the user for a
compile-multi command."
    (interactive "P")
    (let* ((default-directory (or (and compile-multi-default-directory
                                       (funcall compile-multi-default-directory))
                                  default-directory))
           (compile-cmd
            (or command
                (plist-get (cdr (or (compile-multi--get-task)
                                    (user-error "compile-multi: Invalid task read from user")))
                           :command))))
      (cond
       ((stringp compile-cmd)
        (when query
          (setq compile-cmd
                (compilation-read-command compile-cmd)))
        (compile compile-cmd))
       ((and (functionp compile-cmd)

             (if query
                 (eval-expression
                  (read--expression "Eval: " (format "(%s)" `(call-interactively ,compile-cmd))))
               (call-interactively compile-cmd)))
        ((functionp compile-cmd)
         (if query
             (eval-expression
              (read--expression "Eval: " (format "(%s)" compile-cmd)))
           (funcall compile-cmd)))
        (t (error "Don't know how to run the command %s" compile-cmd))))))

  ;; Integration with Makefile projects.
  ;;
  ;; It detects existing Makefiles, parses their build targets, and generates
  ;; entries in `compile-multi' for them. It also provides configurable parallel
  ;; build support.
  (defconst +compile-multi-makefile-names '("Makefile" "makefile" "GNUmakefile")
    "List of Makefile names recognized by various make implementations.")

  (defun +compile-multi-make--targets-from-file (makefile)
    "Read makefile targets from MAKEFILE."
    (let (targets)
      (if (not makefile)
          (let ((proj (project-current))
                (root (if proj
                          (project-root proj)
                        default-directory)))
            (error "No build file found for project %s" root)))
      (with-temp-buffer
        (insert-file-contents makefile)
        (goto-char (point-min))
        (while (re-search-forward "^\\([^: \n]+\\) *:\\(?: \\|$\\)" nil t)
          (let ((str (match-string 1)))
            (unless (string-match "^\\." str)
              (push str targets)))))
      (nreverse targets)))

  (defun +compile-multi--join-shell-command (argv)
    "Join quoted arguments from ARGV into a shell command."
    (string-join (mapcar #'shell-quote-argument argv) " "))

  (defconst +compile-multi-build-jobs--type
    '(optional
      (choice
       (const ninja :tag
        "Match ninja https://github.com/ninja-build/ninja/blob/fd7067652cae480190bf13b2ee5475efdf09ac7d/src/ninja.cc#L239")
       (const -1 :tag "Use `num-processors'.")
       (const -2 :tag "Use half of `num-processors'.")
       (integer :tag "Use this value as the number of jobs."))))

  (defcustom +compile-multi-build-jobs 'ninja
    "Number of jobs to use for building a project (when applicable)."
    :type +compile-multi-build-jobs--type
    :group 'compile-multi)

  (defun +compile-multi--guess-parallelism (jobs)
    "Query the available CPU cores respecting JOBS.
See `+compile-multi-build-jobs' for supported values for JOBS."
    (when jobs
      (pcase jobs
        ('ninja
         (pcase (num-processors)
           ((or 0 1) 1)
           (2 3)
           (_ (+ (num-processors) 2))))
        (-1 (num-processors))
        (-2 (/ (num-processors) 2))
        (0 nil)
        ((cl-type integer) jobs)
        (_ (projection--log :warning "Unsupported `jobs' argument: %S." jobs)
           nil))))

  (defun +compile-multi-make-run-build (&optional target)
    "Build command generator for Make projects.
Set TARGET as the TARGET to build when set."
    (+compile-multi--join-shell-command
     `("make"
       ,@(when-let* ((jobs (+compile-multi--guess-parallelism +compile-multi-build-jobs)))
          (list "-j" (number-to-string jobs)))
       ,@(when target (list target)))))

  (defun +compile-multi-make-targets (&optional project-type makefile)
    "`compile-multi' target generator function for Makefile projects."
    (setq project-type (or project-type "make"))
    (setq makefile
          (or makefile
              (cl-find-if #'file-exists-p +compile-multi-makefile-names)))
    (when makefile
      (cl-loop
       for target in (+compile-multi-make--targets-from-file makefile)
       collect (cons (concat project-type ":" target)
                     (+compile-multi-make-run-build target)))))

  (push `((file-exists-p "Makefile")
          ,#'+compile-multi-make-targets)
        compile-multi-config)

  ;; TODO +compile-multi-nix-flake-targets
  ;; nix flake show --json
  (push `((file-exists-p "flake.nix")
          (,(concat "nix" ":" "build")
           :command "nix build ."
           :annotation "nix build .")
          (,(concat "nix" ":" "check")
           :command "nix flake check"
           :annotation "nix flake check")
          (,(concat "nix" ":" "run")
           :command "nix run ."
           :annotation "nix run .")
          (,(concat "nix" ":" "format")
           :command "nix fmt"
           :annotation "nix fmt"))
        compile-multi-config)

  ;; Set the default directory resolver to the current project root.
  (setopt compile-multi-default-directory
          (defun +project-current-root ()
            (if-let ((proj (project-current)))
                (project-root proj)
              default-directory)))

  ;; Parallel build support for `make' in the default `compile-command'.
  (setopt compile-command
          (concat "make "
                  (when-let* ((jobs (+compile-multi--guess-parallelism
                                     +compile-multi-build-jobs)))
                    (concat "-j " (number-to-string jobs) " "))
                  "-k "))

  (bind-keys :map global-map
             ("C-c C-," . compile-multi)))

;; TODO create compile-multi-use-package that adds to compile-multi-config
;; Adds :compile keyword to use-package forms
;; (use-package compile-multi-use-package)

(use-package consult-compile-multi
  :config
  ;; `consult-compile-multi' is an extension for `compile-multi' that runs the
  ;; interactive selection of targets through `consult' instead of
  ;; `completing-read', which enhances it with some useful consult features such
  ;; as narrowing.

  (setopt consult-compile-multi-narrow
          '((?m "Make" make)
            (?n "Nix Flake" nix)))

  (consult-compile-multi-mode))

;;;;;;;;;;;;;;;
;;;; shell ;;;;

(use-package shell
  :config
  ;; A terminal ("terminal emulator") is an application that provides a
  ;; text-centric interface and handles all the technicalities of presenting
  ;; text accordingly. Whereas the shell (or "command-line shell") is the
  ;; program that runs inside the terminal whose job is to interpret the user's
  ;; input and communicate with the computer. Something like xterm or
  ;; gnome-terminal is a terminal. While the likes of bash, zsh, and fish are
  ;; shells.

  ;; In Emacs we can have both. Emacs can run a process that constitutes a fully
  ;; fledged terminal emulator, such as the `vterm' package, or it can provide
  ;; the interface necessary for a mere shell to handle the command-line
  ;; interactivity.

  ;; A terminal emulator is only needed if we use server that require Terminal
  ;; User Interface (TUI) capabilities, such as htop. I do not run many of
  ;; those, in large part because Emacs has better or equally capable
  ;; alternatives like `proced' to do what htop does in the terminal.

  ;; With the TUI out of the way, we can have `shell' run a native Unix shell
  ;; for us. Mine is bash because I am a simpleton, but I also provide a few
  ;; niceties that improve the user experience.

  ;; I run a shell to do things like interface with my system's package manager
  ;; or run a program with some flags. `shell' is more than enough for this
  ;; purpose. To make it a bit easier to work with multiple shells that need to
  ;; be named after the directory they are in, I use the command `+shell': it
  ;; not only uses a unique and informative buffer name, but it also keeps track
  ;; of `cd' commands to update the buffer name accordingly.

  ;; Note that these also exists a shell implemented in Emacs Lisp. It is called
  ;; `eshell'. Unlike `shell', it does not read the `~/.bashrc' and is its own
  ;; little Emacs-only thing with its own command-line syntax. In short, it is
  ;; "okay" in a vacuum but I have no other use for it beside tinkering with
  ;; Elisp, while I prefer to have a reliable `~/.bashrc' at all times.

  ;; My `+shell-mode' defines a few extra key bindings (per the
  ;; `+shell-mode-map') and also implements a bookmark handler for shell
  ;; buffers. The bookmarking functionality is a wonderful extra, as is
  ;; leverages Emacs' TRAMP infrastructure to re-establish the connection to the
  ;; given host. For example, if I do `find-file' and then input
  ;; `/sudo::/usr/share/' to go to `/usr/share' with sudo privileges, then I can
  ;; open a shell there and bookmark it. When I jump back to the bookmark, Emacs
  ;; will automatically handle the sudo part while taking me to that shell in
  ;; its directory.

  ;;;; Helper functions

  (defun +shell--beginning-of-prompt-p ()
    "Return non-nil if point is at the beginning of a shell prompt."
    (if comint-use-prompt-regexp
        (looking-back comint-prompt-regexp (line-beginning-position))
      (eq (point) (comint-line-beginning-position))))

  (defun +shell--insert-and-send (&rest args)
    "Insert and execute ARGS in the last shell prompt.
ARGS is a list of strings."
    (if (+shell--beginning-of-prompt-p)
        (progn
          (insert (mapconcat #'identity args " "))
          (comint-send-input))
      (user-error "Not at the beginning of prompt; won't insert: %s" args)))

  (defun +shell--last-input ()
    "Return last input as a string."
    (buffer-substring-no-properties
     comint-last-input-start
     comint-last-input-end))

  ;;;; Input from shell command history using completion

  (with-eval-after-load 'consult
    (add-to-list
     'consult-mode-histories
     '(shell-mode comint-input-ring comint-input-ring-index comint-bol)))

  (defun +consult-history-comint-send ()
    (declare (interactive-only t))
    (interactive)
    (consult-history)
    (comint-send-input))

  ;;;; Outline support for shell prompts

  (add-hook 'shell-mode-hook
            (lambda ()
              (setq outline-regexp shell-prompt-pattern)))

  ;;;; Directory tracking

  (defun +shell-update-name-on-cd (&rest _)
    "Update the shell buffer name after a cd for use in `+shell'."
    (when-let* ((input (+shell--last-input))
                ((string-match-p "cd " input)))
      (rename-buffer (format "*shell in %s*" default-directory) :make-unique)))

  ;;;; Bookmark support
  ;;;;; NOTE 2025-06-26 Emacs 31 supports bookmarking shell buffers
  ;;;;; natively. Remove this code when it's time to upgrade

  ;; Adapted from esh-mode.el
  (declare-function bookmark-prop-get "bookmark" (bookmark prop))

  (defun +shell-bookmark-name ()
    "Return name of bookmark based on currect directory."
    (format "shell-%s"
            (file-name-nondirectory
             (directory-file-name
              (file-name-directory default-directory)))))

  (defvar sh-shell-file)

  (defun +shell-bookmark-make-record ()
    "Create a bookmark for the current Shell buffer."
    `(,(+shell-bookmark-name)
      (location . ,default-directory)
      (shell-file-name . ,sh-shell-file)
      (handler . +shell-bookmark-jump)))

  (defun +shell-bookmark-jump (bookmark)
    "Default BOOKMARK handler for Shell buffers."
    (let ((default-directory (bookmark-prop-get bookmark 'location))
          (explicit-shell-file-name (bookmark-prop-get bookmark 'shell-file-name)))
      (shell (get-buffer-create (car bookmark)))))

  (put '+shell-bookmark-jump 'bookmark-handler-type "Shell")

  ;;;; General commands

  (defvar-local +shell--shell nil)
  (defvar-local +shell--last-buffer nil)

  (defcustom +shell-kill-buffer-on-exit t
    "Kill a +shell process buffer after the process terminates."
    :type 'boolean)

  (defun +shell--maybe-remember-buffer (buffer)
    (when (and buffer
               (eq major-mode 'shell-mode))
      (setq +shell--last-buffer buffer)))

  (defsubst +shell--set-this-buffer-shell (s &optional this)
    (with-current-buffer (or this (current-buffer)) (setq +shell--shell s)))

  (defun +shell--switch-to-buffer (buffer)
    (unless (eq buffer (current-buffer))
      (switch-to-buffer-other-window buffer)))

  (defun +shell (&optional prompt)
    "Start an inferior shell in the specified directory.

If PROMPT is nil, don't prompt for a directory and use
`default-directory'."
    (interactive (list t))
    (let* ((origin (current-buffer))
           (dir (if (eq prompt nil)
                    default-directory
                  (read-directory-name "Directory: " default-directory)))
           (default-directory dir)
           (shell (get-buffer-create (format "*shell in %s*" default-directory))))
      (switch-to-buffer-other-window shell)
      (shell shell)
      (setq +shell--last-buffer origin)
      (with-current-buffer shell
        (when +shell-kill-buffer-on-exit
          (let* ((buffer (current-buffer))
                 (process (get-buffer-process buffer))
                 (sentinel (process-sentinel process)))
            (set-process-sentinel
             process
             (lambda (proc event)
               (when sentinel
                 (funcall sentinel proc event))
               (unless (buffer-live-p proc)
                 (if (not (one-window-p))
                     (kill-buffer-and-window))
                 (kill-buffer buffer))))))
        (+shell--set-this-buffer-shell (current-buffer) origin)
        (+shell--set-this-buffer-shell (current-buffer)))))

  (defun +project-shell ()
    "Start an inferior shell in the current project's root directory."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (+shell nil)))

  (defun +shell-home ()
    "Start an inferior shell in user's home directory."
    (interactive)
    (let ((default-directory (or (getenv "HOME") (expand-file-name "~"))))
      (+shell nil)))

  (defun +shell-pop-to-buffer (&optional arg impl buffer)
    "Switch to running a shell in the current project's root directory.

If shell is the current buffer, switch to the previously used
buffer.

If no shell is running, execute `+shell' to start a fresh one.

With \\[universal-argument] prefix argument, the user can specify a
directory."
    (interactive "P")
    (let* ((in-shell (eq major-mode 'shell-mode))
           (in-live-shell (and in-shell (get-buffer-process (current-buffer))))
           (shell (and (buffer-live-p +shell--shell) +shell--shell)))
      (cond (arg (call-interactively '+shell)) ;
            (in-live-shell
             (when (and (not (eq shell buffer))
                        (buffer-live-p +shell--last-buffer))
               (+shell--switch-to-buffer +shell--last-buffer)))
            (shell (+shell--set-this-buffer-shell shell)
                   (+shell--switch-to-buffer shell))
            (t (call-interactively '+project-shell)))
      (+shell--maybe-remember-buffer buffer)))

  (defun +shell-command-at-line (&optional prefix)
    "Run contents of line around point as a shell command and
replace the line with output. With a prefix argument, append the
output instead."
    (interactive "P")
    (let ((command
           (if (use-region-p)
               (buffer-substring-no-properties
                (region-beginning)
                (region-end))
             (thing-at-point 'line))))
      (cond ((use-region-p)
             (call-interactively #'delete-region))
            ((null prefix)
             (kill-whole-line)
             (indent-according-to-mode))
            (t (newline-and-indent)))
      (insert (string-trim
               (ansi-color-apply
                (shell-command-to-string command))))
      (exchange-point-and-mark)))

  ;;;; Minor mode setup

  (define-minor-mode +shell-mode
    "Provide extra functionality for the Emacs `shell'.
Add a bookmark handler for shell buffer and activate the
`+shell-mode-map':
\\{+shell-mode-map}"
    :init-value nil
    :global nil
    (if +shell-mode
        (progn
          (add-hook 'comint-output-filter-functions
                    #'+shell-update-name-on-cd nil :local)
          (setq-local bookmark-make-record-function
                      #'+shell-bookmark-make-record))
      (remove-hook 'comint-output-filter-functions
                   #'+shell-update-name-on-cd :local)
      (setq-local bookmark-make-record-function nil)))

  (add-hook 'shell-mode-hook #'+shell-mode)

  ;; (setopt tramp-default-remote-shell "/bin/bash")

  (setopt shell-command-prompt-show-cwd t ; Emacs 27.1
          shell-input-autoexpand 'input
          shell-highlight-undef-enable t ; Emacs 29.1
          ;; shell-kill-buffer-on-exit t ; Emacs 29.1
          +shell-kill-buffer-on-exit t ; also kills window
          shell-completion-fignore '("~" "#" "%"))

  (setopt shell-font-lock-keywords
          '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-builtin-face)
            ("^[^ \t\n]+:.*" . font-lock-string-face)
            ("^\\[[1-9][0-9]*\\]" . font-lock-constant-face)))

  (bind-keys :map global-map
             ("C-!" . +shell-command-at-line)
             :map +prefix-map
             ("C-z" . +shell-pop-to-buffer)
             ("RET" . +shell-home)
             :map +project-prefix-map
             ("z" . +project-shell)
             :map shell-mode-map
             ("M-r" . +consult-history-comint-send)
             ("C-c C-k" . comint-clear-buffer)
             ("C-c C-w" . comint-write-output)
             ("C-x C-z" . +shell-pop-to-buffer)))

(use-package native-complete
  :disabled t ; NOTE disabled 2025-06-26
  :config
  (with-eval-after-load 'shell
    (native-complete-setup-bash))

  (defun +capf-setup-shell ()
    (add-hook 'completion-at-point-functions #'native-complete-at-point nil t))
  (add-hook 'shell-mode-hook #'+capf-setup-shell))

;; (use-package dwim-shell-command)

;;;;;;;;;;;;;;;;;;
;;;; terminal ;;;;

(use-package vterm
  ;; The built-in terminal emulators are not up to par with the likes of Xterm
  ;; and its peers. Perhaps they were a good compromise in yesteryears, but we
  ;; have come to expect better from our system. Thankfully these is `vterm',
  ;; which is an implementation of the external libvterm library.

  ;; `vterm' is a fully fledged terminal emulator inside of Emacs. Its main
  ;; differences with `shell' can be summarised this:

  ;; - `vterm' can handle graphics and ANSI escape sequences. `shell' cannot.
  ;; - `shell' behaves more like an ordinary Emacs buffer. `vterm' is like an
  ;;    external application that has been embedded in the Emacs frame.
  ;; - `vterm' does tab-completion like a standard terminal. `shell' can use the
  ;;    Emacs completion framework.

  ;; My workflow is to keep `shell' as my main conduit to the command line, such
  ;; as for when I need to call one of my scripts, and only use `vterm' when I
  ;; really need a CLI tool that is likely to produce graphical artefacts.
  :disabled t
  :config
  (defun +project-vterm ()
    "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (vterm-buffer-name (format "*vterm in %s" default-directory))
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
        (vterm vterm-buffer-name))))

  ;; TODO make vterm keep track of directory and update buffer name, just like
  ;; my +shell command

  (setopt vterm-kill-buffer-on-exit nil
          vterm-max-scrollback 9999))

;;;;;;;;;;;;;;
;;;; prog ;;;;

(use-package elisp
  :no-require
  :init
  ;; The `emacs-lisp-mode' is the major mode for programming in Emacs Lisp and
  ;; `lisp-interaction-mode' is its derivative for use in the *scratch* buffer.
  (defgroup +elisp nil
    "Better Emacs Lisp code viewing.")

  (defcustom +elisp-better-lisp-indent t
    "Override `calculate-lisp-indent' for better indentation of quoted lists."
    :group '+elisp
    :type 'boolean)

  (defvar +elisp-face nil)
  (defvar +elisp-calculate-lisp-indent-check-for-keyword nil)

  ;; Extracted from:
  ;; github.com/doomemacs/doomemacs/blob/master/modules/lang/emacs-lisp/autoload.el
  (defun +elisp-highlight-vars-and-faces (end)
    "Match defined variables and functions.
Functions are differentiated into \"special forms\", \"built-in functions\" and
\"library/userland functions\"."
    (catch 'matcher
      (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
        (let ((ppss (save-excursion (syntax-ppss))))
          (cond ((nth 3 ppss)  ; strings
                 (search-forward "\"" end t))
                ((nth 4 ppss)  ; comments
                 (forward-line +1))
                ((let ((symbol (intern-soft (match-string-no-properties 0))))
                   (and (cond ((null symbol) nil)
                              ((eq symbol t) nil)
                              ((keywordp symbol) nil)
                              ((special-variable-p symbol)
                               (setq +elisp-face 'font-lock-variable-name-face))
                              ((and (fboundp symbol)
                                    (eq (char-before (match-beginning 0)) ?\()
                                    (not (memq (char-before (1- (match-beginning 0)))
                                               (list ?\' ?\`))))
                               (let ((unaliased (indirect-function symbol)))
                                 (unless (or (macrop unaliased)
                                             (special-form-p unaliased))
                                   (let (unadvised)
                                     (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                     (setq unaliased (indirect-function unadvised)))))
                                     unaliased)
                                   (setq +elisp-face
                                         (if (subrp unaliased)
                                             'font-lock-constant-face
                                           'font-lock-function-name-face))))))
                        (throw 'matcher t)))))))
      nil))

  ;; Taken from:
  ;; reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists
  (defun +elisp-calculate-lisp-indent (&optional parse-start)
    "Add better indentation for quoted and backquoted lists."
    ;; The `calculate-lisp-indent-last-sexp' is defined with `defvar' with it's
    ;; value omitted, marking it special and only defining it locally. So if you
    ;; don't have this, you'll get a void variable error.
    (defvar calculate-lisp-indent-last-sexp)
    (save-excursion
      (beginning-of-line)
      (let ((indent-point (point))
            ;; Setting this to a number inhibits calling hook
            (desired-indent nil)
            (retry t)
            state calculate-lisp-indent-last-sexp containing-sexp)
        (cond ((or (markerp parse-start) (integerp parse-start))
               (goto-char parse-start))
              ((null parse-start) (beginning-of-defun))
              (t (setq state parse-start)))
        (unless state
          ;; Find outermost containing sexp
          (while (< (point) indent-point)
            (setq state (parse-partial-sexp (point) indent-point 0))))
        ;; Find innermost containing sexp
        (while (and retry state (> (elt state 0) 0))
          (setq retry nil
                containing-sexp (elt state 1)
                calculate-lisp-indent-last-sexp (elt state 2))
          ;; Position following last unclosed open.
          (goto-char (1+ containing-sexp))
          ;; Is there a complete sexp since then?
          (if (and calculate-lisp-indent-last-sexp (> calculate-lisp-indent-last-sexp (point)))
              ;; Yes, but is there a containing sexp after that?
              (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp indent-point 0)))
                (if (setq retry (car (cdr peek))) (setq state peek)))))
        (unless retry
          ;; Innermost containing sexp found
          (goto-char (1+ containing-sexp))
          (if (not calculate-lisp-indent-last-sexp)
              ;; `indent-point' immediately follows open paren. Don't call hook.
              (setq desired-indent (current-column))
            ;; Find the start of first element of containing sexp.
            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
            (cond ((looking-at "\\s("))
                  ;; First element of containing sexp is a list. Indent under that
                  ;; list.
                  ((> (save-excursion (forward-line 1) (point)) calculate-lisp-indent-last-sexp)
                   ;; This is the first line to start within the containing sexp.
                   ;; It's almost certainly a function call.
                   (if (or
                        ;; Containing sexp has nothing before this line except the
                        ;; first element. Indent under that element.
                        (= (point) calculate-lisp-indent-last-sexp)

                        ;; First sexp after `containing-sexp' is a keyword. This
                        ;; condition is more debatable. It's so that I can have
                        ;; unquoted plists in macros. It assumes that you won't
                        ;; make a function whose name is a keyword.
                        (and +elisp-calculate-lisp-indent-check-for-keyword
                             (when-let (char-after (char-after (1+ containing-sexp)))
                               (char-equal char-after ?:)))

                        ;; Check for quotes or backquotes around.
                        (let* ((positions (elt state 9))
                               (last (car (last positions)))
                               (rest (reverse (butlast positions)))
                               (any-quoted-p nil)
                               (point nil))
                          (or
                           (when-let (char (char-before last))
                             (or (char-equal char ?')
                                 (char-equal char ?`)))
                           (progn
                             (while (and rest (not any-quoted-p))
                               (setq point (pop rest)
                                     any-quoted-p
                                     (or
                                      (when-let (char (char-before point))
                                        (or (char-equal char ?') (char-equal char ?`)))
                                      (save-excursion
                                        (goto-char (1+ point))
                                        (looking-at-p "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                             any-quoted-p))))
                       ;; Containing sexp has nothing before this line except the
                       ;; first element. Indent under that element.
                       nil
                     ;; Skip the first element, find start of second (the first
                     ;; argument of the function call) and indent under.
                     (progn (forward-sexp 1)
                            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)))
                   (backward-prefix-chars))
                  (t
                   ;; Indent beneath first sexp on same line as
                   ;; `calculate-lisp-indent-last-sexp'. Again, it's almost
                   ;; certainly a function call.
                   (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                   (backward-prefix-chars)))))
        ;; Point is at the point to indent under unless we are inside a string.
        ;; Call indentation hook except when overridden by `lisp-indent-offset' or
        ;; if the desired indentation has already been computed.
        (let ((normal-indent (current-column)))
          (cond ((elt state 3)
                 ;; Inside a string, don't change indentation.
                 nil)
                ((and (integerp lisp-indent-offset) containing-sexp)
                 ;; Indent by constant offset
                 (goto-char containing-sexp)
                 (+ (current-column) lisp-indent-offset))
                ;; in this case `calculate-lisp-indent-last-sexp' is not `nil'
                (calculate-lisp-indent-last-sexp
                 (or
                  ;; try to align the parameters of a known function
                  (and lisp-indent-function
                       (not retry)
                       (funcall lisp-indent-function indent-point state))
                  ;; If the function has no special alignment or it does not apply
                  ;; to this argument, try to align a constant-symbol under the
                  ;; last preceding constant symbol, if there is such one of the
                  ;; last 2 preceding symbols, in the previous uncommented line.
                  (and (save-excursion
                         (goto-char indent-point)
                         (skip-chars-forward " \t")
                         (looking-at ":"))
                       ;; The last sexp may not be at the indentation where it
                       ;; begins, so find that one, instead.
                       (save-excursion
                         (goto-char calculate-lisp-indent-last-sexp)
                         ;; Handle prefix characters and whitespace following an
                         ;; open paren. (Bug#1012)
                         (backward-prefix-chars)
                         (while (not (or (looking-back "^[ \t]*\\|([ \t]+" (line-beginning-position))
                                         (and containing-sexp (>= (1+ containing-sexp) (point)))))
                           (forward-sexp -1)
                           (backward-prefix-chars))
                         (setq calculate-lisp-indent-last-sexp (point)))
                       (> calculate-lisp-indent-last-sexp
                          (save-excursion
                            (goto-char (1+ containing-sexp))
                            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                            (point)))
                       (let ((parse-sexp-ignore-comments t)
                             indent)
                         (goto-char calculate-lisp-indent-last-sexp)
                         (or (and (looking-at ":")
                                  (setq indent (current-column)))
                             (and (< (line-beginning-position) (prog2 (backward-sexp) (point)))
                                  (looking-at ":")
                                  (setq indent (current-column))))
                         indent))
                  ;; another symbols or constants not preceded by a constant as
                  ;; defined above.
                  normal-indent))
                ;; in this case `calculate-lisp-indent-last-sexp' is `nil'
                (desired-indent)
                (t
                 normal-indent))))))

  (define-minor-mode +elisp-mode
    "A global minor mode for better Emacs Lisp code."
    :global t
    (if +elisp-mode
        (progn
          (font-lock-add-keywords 'emacs-lisp-mode '((+elisp-highlight-vars-and-faces . +elisp-face)))
          (when +elisp-better-lisp-indent
            (advice-add 'calculate-lisp-indent :override #'+elisp-calculate-lisp-indent)))
      (advice-remove 'calculate-lisp-indent #'+elisp-calculate-lisp-indent)
      (font-lock-remove-keywords 'emacs-lisp-mode '((+elisp-highlight-vars-and-faces . +elisp-face)))))

  (+elisp-mode 1)

  :config
  ;; Then I define a few commands that streamline certain tasks I do
  ;; frequently. These are variations of buit-in commands with tweaks that meet
  ;; my expectations.

  ;; Concretely, the generic `eval-print-last-sexp' evaluates and prints the
  ;; return value of a form without also commenting it out. This can be useful,
  ;; though I generally find that I keep prepending a comment to prevent
  ;; `eval-buffer' from evaluating code I had not intended to evaluate further.
  (defun +elisp-eval-and-print-last-sexp ()
    "Evaluate and print expression before point like `eval-print-last-sexp'.
Prepend a comment to the return value. Also copy the return value to the
`kill-ring' and set the mark to where point was before inserting the
return value."
    (declare (interactive-only t))
    (interactive)
    (if-let* ((string (thing-at-point 'sexp :no-properties))
              (_ (not (string-prefix-p ";" string)))
              (expression (read string)))
        (let ((start (point))
              (return-value (eval expression)))
          (kill-new (format "%S" return-value))
          (message "Copied: `%S'" return-value)
          (push-mark (point))
          (insert (format "\n%S\n" return-value))
          (string-insert-rectangle (+ (mark) 1) (- (point) 1) ";; => ")
          (indent-region start (point)))
      (user-error "No expression at point")))

  (defvar-keymap +elisp-macroexpand-mode-map
    :doc "Key map for `+elisp-macroexpand-mode'."
    :parent special-mode-map)

  ;; The `+elisp-pp-macroexpand-last-sexp' is a variant of
  ;; `pp-macroexpand-last-sexp', whose primary goal is to conform with the
  ;; `display-buffer-alist'. I can thus macroexpand with the confidence that the
  ;; resulting buffer will not mess up with my work.
  (define-derived-mode +elisp-macroexpand-mode emacs-lisp-mode "MacroExpand"
    "Like `emacs-lisp-mode' but for macroexpanded forms."
    :interactive nil
    (read-only-mode 1)
    (display-line-numbers-mode 1)
    ;; NOTE: the following features are for Emacs 31
    ;; (setq-local elisp-fontify-semantically t
    ;;             elisp-add-help-echo t)
    (cursor-sensor-mode))

  (add-to-list 'display-buffer-alist
               '("*elisp-macroexpand*"
                 (display-buffer-below-selected)
                 (window-height . 0.3)
                 (dedicated . t)
                 (preserve-size . (t . t))
                 (body-function . select-window)))

  (defun +elisp-pp-macroexpand-last-sexp ()
    "Like `pp-macroexpand-last-sexp', but with a generic `display-buffer'.
Now use `display-buffer-alist' like the Lisp gods intended."
    (declare (interactive-only t))
    (interactive)
    (if-let* ((thing (thing-at-point 'sexp :no-properties))
              (expression (read thing))
              (buffer (get-buffer-create "*elisp-macroexpand*"))
              (inhibit-read-only t))
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert (format "%S" (macroexpand-1 expression)))
            (+elisp-macroexpand-mode)
            (pp-buffer))
          (display-buffer buffer))
      (user-error "No expression to macroexpand")))

  ;; Advise eval commands to use region if active
  (defun +eval-region-if-active (arg)
    "Advice for evaluation commands to have them call `eval-region' when the
region is active."
    (when (use-region-p)
      (eval-region (region-beginning) (region-end) arg)
      (deactivate-mark)
      t))
  (dolist (fn '(eval-print-last-sexp eval-last-sexp eval-defun))
    (advice-add fn :before-until #'+eval-region-if-active))

  (bind-keys :map +prefix-map
             ("C-e" . eval-last-sexp)
             :map lisp-interaction-mode-map
             ("C-c C-b" . nil) ; unmap `elisp-byte-compile-buffer'
             ("C-c C-j" . +elisp-eval-and-print-last-sexp)
             ("C-c C-f" . nil) ; unmap `elisp-byte-compile-file'
             ("C-M-q" . nil) ; unmap `indent-pp-sexp'
             :map emacs-lisp-mode-map
             ("C-M-x" . eval-defun)
             ("C-c C-b" . nil) ; unmap `elisp-byte-compile-buffer'
             ("C-c C-c" . eval-defun)
             ("C-c C-e" . eval-last-sexp)
             ("C-x C-e" . eval-last-sexp)
             ("C-c C-f" . nil) ; unmap `elisp-byte-compile-file'
             ("C-c C-j" . +elisp-eval-and-print-last-sexp)
             ("C-c C-k" . eval-buffer)
             ("C-c C-l" . load-file)
             ("C-c C-m" . emacs-lisp-macroexpand)
             ("C-c M-m" . +elisp-pp-macroexpand-last-sexp)
             ("C-M-q" . nil) ; unmap `indent-pp-sexp'
             ("C-c C-r" . eval-region)))

(use-package ielm
  :config
  ;; Toggle between Ielm and Elisp buffers.
  (defvar-local +ielm--last-buffer nil)
  (defvar-local +ielm--working-buffer nil)

  (defcustom +ielm-kill-buffer-on-exit t
    "Kill an Ielm buffer after the process terminates."
    :type 'boolean)

  (defun +ielm (&optional buf-name)
    "Interactively evaluate Emacs Lisp expressions.
Creates a buffer named BUF-NAME if provided (`*ielm*' by default),
See `inferior-emacs-lisp-mode' for details."
    (interactive)
    (let* (old-point
           (buf-name (or buf-name "*ielm*"))
           (buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        (unless (zerop (buffer-size)) (setq old-point (point)))
        (inferior-emacs-lisp-mode)
        (setq-local trusted-content :all)

        (when +ielm-kill-buffer-on-exit
          (let* ((buffer (current-buffer))
                 (process (get-buffer-process buffer))
                 (sentinel (process-sentinel process)))
            (set-process-sentinel
             process
             (lambda (proc event)
               (when sentinel
                 (funcall sentinel proc event))
               (unless (buffer-live-p proc)
                 (if (not (one-window-p))
                     (kill-buffer-and-window))
                 (kill-buffer buffer)))))))
      (switch-to-buffer-other-window buf)
      (when old-point (push-mark old-point))
      buf))

  (defun +ielm-pop-to-buffer (&optional arg)
    "Switch to an Ielm process buffer.

If no Ielm process for current buffer exists, `+ielm' is called
interactively."
    (interactive "P")
    (let* ((in-repl (eq major-mode 'inferior-emacs-lisp-mode))
           (repl (and (buffer-live-p +ielm--working-buffer)
                      +ielm--working-buffer))
           (origin (current-buffer)))
      (cond (in-repl
             (switch-to-buffer-other-window +ielm--last-buffer))
            (repl
             (switch-to-buffer-other-window repl))
            (t
             (setq repl (call-interactively '+ielm))
             (setq-local +ielm--working-buffer repl)
             (with-current-buffer repl
               (setq-local +ielm--last-buffer origin))))))

  (bind-keys :map emacs-lisp-mode-map
             ("C-c C-z" . +ielm-pop-to-buffer)
             :map ielm-map
             ("C-c C-z" . +ielm-pop-to-buffer)))

;; (use-package beardbolt
;;   :config
;;   (bind-keys :map global-map
;;              ("C-c M-d" . beardbolt-starter)))

(use-package python
  :lsp-ensure (python-mode python-ts-mode)
  :format ruff python-mode python-ts-mode
  :lint ((python-mode python-ts-mode)
         flymake-collection-ruff
         (flymake-collection-pycodestyle :disabled t))
  :config
  ;; Make sure packages that try to use python-mode are redirected to
  ;; python-ts-mode
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

  (defvar-local +python-shell--last-buffer nil)
  (defcustom +python-shell-kill-buffer-on-exit t
    "Kill an inferior Python process buffer after the process terminates."
    :type 'boolean)
  (defun +python-shell-pop-to-buffer (&optional arg)
    "Switch to inferior Python process buffer.

If no inferior Python process for current buffer exists, `run-python' is
called an interactively."
    (interactive "P")
    (let* ((in-repl (eq major-mode 'inferior-python-mode))
           (repl (python-shell-get-buffer))
           (origin (current-buffer)))
      (cond (in-repl
             (switch-to-buffer-other-window +python-shell--last-buffer))
            (repl
             (switch-to-buffer-other-window repl))
            (t
             (call-interactively 'run-python)
             (python-shell-with-shell-buffer
               (setq-local +python-shell--last-buffer origin)
               ;; Force envrc to update in the inferior Python process buffer,
               ;; otherwise envrc's environment variables don't seem to apply.
               (envrc--update)
               ;; TODO: should this be an :after advice on `run-python'?
               (when +python-shell-kill-buffer-on-exit
                 (let* ((buffer (python-shell-get-buffer))
                        (process (python-shell-get-process))
                        (sentinel (process-sentinel process)))
                   (set-process-sentinel
                    process
                    (lambda (proc event)
                      (when sentinel
                        (funcall sentinel proc event))
                      (unless (buffer-live-p proc)
                        (if (not (one-window-p))
                            (kill-buffer-and-window))
                        (kill-buffer buffer)))))))))))

  (defun +python-shell-send-dwim (&optional arg msg)
    "Send the block or statement at point to inferior Python process.
The block is delimited by `python-nav-beginning-of-block' and
`python-nav-end-of-block'. If not in a block, evaluates the statement
delimited by `python-nav-beginning-of-statement' and
`python-nav-end-of-statement'."
    (interactive)
    (let ((beg (save-excursion
                 (or (python-nav-beginning-of-block)
                     (python-nav-beginning-of-statement))
                 (point-marker)))
          (end (save-excursion
                 (or (python-nav-end-of-block)
                     (python-nav-end-of-statement))
                 (point-marker)))
          (python-indent-guess-indent-offset-verbose nil))
      (if (and beg end)
          (python-shell-send-region beg end)
        (user-error "Can't get code block from current position"))))

  (setopt python-shell-interpreter "python"
          python-shell-interpreter-args "-i"
          python-shell-dedicated 'project
          ;; python-shell-completion-native-enable nil
          python-shell-completion-native-disabled-interpreters '("ipython"
                                                                 "jupyter")
          python-indent-guess-indent-offset t
          python-indent-guess-indent-offset-verbose nil
          python-indent-offset 4)

  (bind-keys :map python-ts-mode-map
             ;; ("C-c C-a" . nil)
             ;; ("C-c C-b" . nil)
             ("C-c C-c". +python-shell-send-dwim)
             ("C-c C-d" . nil) ; unmap `python-describe-at-point'
             ;; ("C-c C-e" . eval-last-sexp)
             ("C-c C-f" . nil) ; unmap `python-eldoc-at-point'
             ;; ("C-c C-i" . nil)
             ("C-c C-j" . nil) ; unmap `imenu' ;; TODO: ("C-c C-j" . +python-shell-insert-commands-map)
             ("C-c C-k" . python-shell-send-buffer)
             ("C-c C-l" . python-shell-send-file)
             ;; ("C-c C-m" . macroexpand)
             ;; TODO: ("C-c C-n" . +python-shell-eval-dwim-and-next)
             ;; TODO: ("C-c C-o" . +python-shell-find-and-clear-output) ; if prefix, clear entire buffer
             ;; ("C-c C-p" . +python-shell-pprint-eval-last-sexp)
             ("C-c C-q" . +kill-this-buffer) ;; ("C-c C-q" . +python-shell-quit)
             ;; TODO: ("C-c C-r" . +python-refactor-map)
             ("C-c M-r" . python-shell-restart)
             ;; ("C-c C-s" . nil)
             ;; TODO: ("C-c C-t" . +python-test-commands-map)
             ;; ("C-c C-u" . dape)
             ("C-c C-v" . nil) ; unmap `python-check'
             ;; ("C-c C-w" . nil)
             ;; TODO: ("C-c C-x" . run-python) ; +python-shell-start-map?
             ;; ("C-c C-y" . nil)
             ("C-c C-z" . +python-shell-pop-to-buffer)
             ;; ("C-c ," . +python-test-commands-map)
             :map inferior-python-mode-map
             ;; TODO: ("M-j" . +comint-newline-and-indent) cider-repl-newline-and-indent
             ;; ("C-<return>" . cider-repl-closing-return?)
             ;; ("C-c C-a" . comint-bol-or-process-mark)
             ;; ("C-c C-b" . nil)
             ;; ("C-c C-c" . comint-interrupt-subjob)
             ;; ("C-c C-d" . +pydoc)
             ;; ("C-c C-e" . nil)
             ;; ("C-c C-f" . apheleia-format-buffer)
             ;; ("C-c C-i" . nil)
             ;; ("C-c C-j" . +python-shell-insert-commands-map)
             ;; ("C-c C-k" . nil)
             ;; ("C-c C-l" . python-shell-send-file)
             ;; ("C-c C-m" . macroexpand)
             ;; ("C-c C-n" . comint-next-prompt)
             ;; ("C-c C-o" . comint-delete-prompt)
             ;; ("C-c C-p" . comint-previous-prompt)
             ;; ("C-c C-q" . comint-send-eof)
             ;; ("C-c C-r" . +python-refactor-map)
             ;; ("C-c C-s" . nil)
             ;; ("C-c C-t" . +python-test-commands-map)
             ;; ("C-c C-u" . comint-kill-input)
             ;; ("C-c C-v" . nil)
             ;; ("C-c C-w" . comint-write-output)
             ;; ("C-c C-x" . +python-shell-start-map)
             ;; ("C-c C-y" . nil)
             ("C-c C-z" . +python-shell-pop-to-buffer)))

(use-package ess)

(use-package pydoc
  :config
  (with-eval-after-load 'inheritenv
    (inheritenv-add-advice 'shell-command-to-string))

  (defvar +pydoc--history nil
    "Minibuffer history for `+pydoc'.")

  (defun +pydoc (arg)
    "Display pydoc information for object at point.

When \\[prefix-argument] prefix ARG or no object is found at point,
prompt for an object name.

When double \\[prefix-argument] \\[prefix-argument] prefix ARG, reload
the `pydoc' module list before prompting."
    (interactive "P")
    (let* ((symbol (thing-at-point 'symbol t))
           (reload (equal arg '(16))))
      (if (or arg (not symbol))
          ;; NOTE `pydoc' reproduced here to change the completing-read prompt
          ;; (call-interactively #'pydoc)
          (let ((name (completing-read
                       (format-prompt "Pydoc for function or module" symbol)
                       (pydoc-all-modules reload)
                       nil nil nil '+pydoc--history symbol)))
            (pydoc-setup-xref (list #'pydoc name)
                              (called-interactively-p 'interactive))
            (pydoc-with-help-window (pydoc-buffer)
              (call-process-shell-command (concat pydoc-command " " name)
                                          nil standard-output)))
        (pydoc-at-point))))

  (defun +pydoc-eglot-override ()
    (when (derived-mode-p 'python-base-mode)
      (set (make-local-variable 'minor-mode-overriding-map-alist)
           `((eglot--managed-mode
              . ,(let ((map (copy-keymap eglot-mode-map)))
                  (define-key map (kbd "C-c C-d") #'+pydoc)
                  map))))))
  (add-hook 'python-mode-hook #'+pydoc-eglot-override)
  (add-hook 'python-ts-mode-hook #'+pydoc-eglot-override)

  (bind-keys :map python-mode-map
             ("C-c C-d" . +pydoc)
             :map python-ts-mode-map
             ("C-c C-d" . +pydoc)
             :map inferior-python-mode-map
             ("C-c C-d" . +pydoc)))

(use-package nix-mode
  :interpreter (("nix-shell" . nix-shebang-mode)
                ("nix" . nix-shebang-mode)))

(use-package nix-update)

(use-package nix-ts-mode
  ;; `nix-ts-mode' is better for syntax highlighting of files, but `nix-mode'
  ;; provides a bunch of other niceties like shebang detection, nix-build and
  ;; nix-repl comint support, store path and log introspection, and interactive
  ;; flake commands.
  ;;
  ;; Luckily for us, `nix-ts-mode' adds `nix-mode' as its parent, so we get the
  ;; best of both worlds when both are loaded.
  :mode "\\.nix\\'"
  :lsp-ensure nix-ts-mode
  :config
  ;; Make sure packages that try to use nix-mode are redirected to nix-ts-mode
  (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))

  (bind-keys :map nix-ts-mode-map
             ("C-c u" . nix-update-fetch)
             ("C-c C-z" . nix-repl)))

;; TODO document go-ts-mode
;; TODO add C-c C-a `go-import-add' from `go-mode'
(use-package go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode)
         ("/go\\.mod\\'" . go-mod-ts-mode))
  :lsp-ensure (go-ts-mode go-mod-ts-mode)
  :lsp-config '((:gopls . (:ui.completion.usePlaceholders t
                           :hints (:assignVariableTypes t
                                   :constantValues t
                                   :parameterNames t
                                   :rangeVariableTypes t)
                           :hoverKind "FullDocumentation"))
                (:plysp . (:hoverKind "None")))
  :config
  ;; Make sure packages that try to use go-mode are redirected to go-ts-mode
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  (setopt go-ts-mode-indent-offset tab-width))

;; (use-package gotest
;;   :config
;;   (bind-keys :map go-ts-mode-map
;;              :prefix "C-c C-t"
;;              :prefix-map go-test-commands-map
;;              ;; https://github.com/travisjeffery/dotfiles/blob/7d11d67e1d2109b49e1c399c67872d676d80826c/.emacs.d/init.el#L1478
;;              ("C-t" . go-test-current-test)
;;              ("C-f" . go-test-current-file)))

;; (use-package go-tag)

(use-package templ-ts-mode)

;; TODO https://sr.ht/~p00f/hare-ts-mode/
;; (use-package hare-ts-mode)

(use-package c-ts-mode
  :lsp-ensure (c-ts-mode c++-ts-mode)
  :config
  ;; Make sure packages that try to use c-mode or c++-mode are redirected to
  ;; c-ts-mode or c++-ts-mode
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

  (setopt c-ts-mode-indent-style 'k&r
          c-ts-mode-indent-offset tab-width))

;; (use-package geiser)

;; 1. look at emmet-expand-yas and eglot-tempel to make emmet-mode integrate with tempel
;; 2. C-j to expand emmet snippets?
;;    or overload with M-/ (emmet-tempel-expand, otherwise dabbrev-expand)
;;    or find a way to make this into a capf so i can use with tab instead and leave C-j + M-/ alone
;; 3. if emmet-mode is good enough i probably don't need emmet2 since i don't know how it differs
;; emmet-mode https://github.com/smihica/emmet-mode
;; emmet2-mode https://github.com/P233/emmet2-mode
;; 4. is lsp-tailwindcss actually needed? can't i use devdocs or something to browse tailwindcss docs?
;;    is it worth the added complexity of switching over to lsp-mode from eglot?
;; (use-package web)

(use-package sgml-mode
  :config
  (add-hook 'sgml-mode-hook #'sgml-electric-tag-pair-mode))

(use-package json-ts-mode
  :config
  ;; Make sure packages that try to use js-json-mode are redirected to
  ;; json-ts-mode
  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))

  (setopt json-ts-mode-indent-offset tab-width))

(use-package toml-ts-mode
  :lsp-ensure (toml-ts-mode conf-toml-mode)
  :lsp-server ((toml-ts-mode conf-toml-mode) . ("taplo" "lsp" "stdio"))
  :format
  (taplo-fmt . ("taplo" "format" "-"))
  toml-ts-mode conf-toml-mode
  :config
  ;; Make sure packages that try to use conf-toml-mode are redirected to
  ;; toml-ts-mode
  (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))

  (setopt toml-ts-mode-indent-offset tab-width))

;; TODO transer my overleaf resumes to local
;; (use-package latex)

;; TODO document sh-script
(use-package sh-script
  :config
  (defun +ensure-executable (&rest _)
    (unless (file-exists-p buffer-file-name)
      (basic-save-buffer))
    (executable-make-buffer-file-executable-if-script-p))
  (with-eval-after-load 'executable
    (advice-add 'executable-interpret :before #'+ensure-executable))

  (bind-keys
   :map sh-mode-map
   ("C-c C-c" . executable-interpret)))

;; (use-package conf-mode)

;; (use-package sxhdrc)

(use-package dts-mode
  ;; setup for zmk keymaps
  :mode (("\\.keymap\\'" . dts-mode)
         ("\\.overlay\\'" . dts-mode)))

(use-package markdown-mode)

(use-package text-mode
  :config
  ;; These are some basic settings for plain text files but also for any major
  ;; mode that inherits from `text-mode' (like Org and Markdown).
  ;;
  ;; Notice how I am adding an association to the `auto-mode-alist'. The file
  ;; names specified in that regular expression will be using `text-mode' when I
  ;; visit them.
  (add-to-list 'auto-mode-alist
               '("\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'"
                 . text-mode)))

;; TODO get gitcommit tree-sitter grammar to work on my nix machine
;; (use-package git-commit-ts-mode
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . git-commit-ts-mode))
;;   :config
;;   (with-eval-after-load 'magit
;;     (setopt git-commit-major-mode 'git-commit-ts-mode)))

;; (use-package git-modes
;;   :config
;;   (add-to-list 'auto-mode-alist
;;                (cons "/.dockerignore\\'" 'gitignore-mode)))

;; (use-package csv)

;;;;;;;;;;;;;;;;;;
;;;; spelling ;;;;

(use-package jinx
  ;; For spell checking on demand, I rely on the `jinx' package by Daniel
  ;; Mendler.
  ;;
  ;; In terms of workflow, I do not like to see any spell checking while I
  ;; type. I prefer to write out the entire draft and then do a spell check at
  ;; the end. Whatever typos there are do not bother me. (this is the "alla
  ;; prima" method of creativity that Protesilaos Stavrou talks about in
  ;; https://protesilaos.com/books/2024-10-15-alla-prima-method-productivity/).
  :config
  ;; Sometimes I want to stick to lower-case for informal documents, but I still
  ;; want to have spell-checking. Invoke `+jinx-lower-case-only' in the buffer
  ;; of such an informal document.
  (defun +jinx--lower-case-word-valid-p (start)
    "Return non-nil if word, that is assumed to be in lower case, at
START is valid, or would be valid if capitalized or upcased."
    (let ((word (buffer-substring-no-properties start (point))))
      (or (member word jinx--session-words)
          (cl-loop for dict in jinx--dicts thereis
                   (or
                    (jinx--mod-check dict (upcase word))
                    (jinx--mod-check dict (capitalize word))
                    (jinx--mod-check dict word))))))
  (defun +jinx-lower-case-only ()
    "Make `jinx-mode' assume that everything in the current buffer is
written in lower case and ignore casing while spell-checking."
    (interactive)
    (jinx-mode 0)
    (set (make-local-variable 'jinx--predicates)
         (cl-substitute
          #'+jinx--lower-case-word-valid-p
          #'jinx--word-valid-p
          jinx--predicates))
    (jinx-mode 1))

  ;; There is a specific category of errors for with `jinx' cannot help:
  ;; duplicate words. When quickly rephrasing a sentence or adjusting a
  ;; paragraph it could happen that I do not pay enough attention to the words I
  ;; remove and so I keep typing with to care whatsoever.
  ;; the the
  ;; I want a command that scans the buffer, finds consecutive occurrences of
  ;; the same word, and removes all but one of them.
  (defun +delete-duplicate-words ()
    "Delete duplicate words via `query-replace-regexp'."
    (interactive nil text-mode)
    (save-excursion
      (if (region-active-p)
          (query-replace-regexp "\\(\\b\\w+\\b\\)\\W+\\1\\b" "\\1" nil
                                (region-beginning) (region-end))
        (query-replace-regexp "\\(\\b\\w+\\b\\)\\W+\\1\\b" "\\1" nil
                              (point-min) (point-max)))))

  (bind-keys
   :map global-map
   ("C-$" . +delete-duplicate-words)
   ("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages)
   :map +toggle-prefix-map
   ("s" . jinx-mode)
   ("S" . +jinx-lower-case-only)))

;;;;;;;;;;;;;;;;;
;;;; reading ;;;;

(use-package pdf-tools
  ;; The `pdf-tools' package builds on top of the external libraries `poppler'
  ;; and `imagemagick' (if Emacs is compiled with support for it) to deliver a
  ;; series of minor modes for reading and interacting with PDF files from
  ;; inside of Emacs. As it depends on those external files, it requires extra
  ;; steps to make is work properly and varies depending on your operating
  ;; system. The value proposition of `pdf-tools' is that renders PDFs much
  ;; better than the built-in DocView.
  ;;
  ;; All you need to start reading PDFs is to activate `pdf-view-mode' when you
  ;; open an appropriate PDF file. Once inside the resulting buffer, do C-h m
  ;; (`describe-mode') to learn about the key bindings and the commands they
  ;; call.
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; There are a number of extra minor modes that users may find helpful:
  ;; `pdf-annot-minor-mode' which provides annotation capabilities,
  ;; `pdf-sync-minor-mode' which syncs the PDF with its corresponding TeX file
  ;; when you are running some setup that compiles the latter to the former,
  ;; `pdf-isearch-minor-mode' which allows you to easily search through the file
  ;; with isearch, and `pdf-occur-global-minor-mode' which allows you to produce
  ;; a buffer of locations with matching queries using occur.
  ;;
  ;; Another helpful integration is with Emacs' outline-mode and imenu by means
  ;; of `pdf-outline-minor-mode'. Simply hit "o" while viewing a PDF to produce
  ;; an outline of the document and then, optionally, `imenu' to navigate it
  ;; using minibuffer completion.
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)

  ;; Most PDF files use a white background for their page, making it impossible
  ;; to discern the file's boundaries in the buffer while using the
  ;; `modus-operandi' theme. To introduce a distinction between the buffer's
  ;; backdrop and the PDF page's background, the former must be rendered as some
  ;; shade of gray. Ideally, `pdf-tools' would provide a face that the themes
  ;; could support directly, though this does not seem to be the case for the
  ;; time being. We must thus employ the face remapping technique to change the
  ;; buffer-local value of the "default" face.
  (defun +pdf-tools-backdrop (&rest _)
    (cond ((and (boundp 'modus-themes-collection)
                (+in-list-p (car custom-enabled-themes) modus-themes-collection))
           (modus-themes-with-colors
             (face-remap-add-relative
              'default
              `(:background ,bg-dim))))
          ((and (boundp 'ef-themes-collection)
                (+in-list-p (car custom-enabled-themes) ef-themes-collection))
           (ef-themes-with-colors
            (face-remap-add-relative
             'default
             `(:background ,bg-dim))))
          ;; FIX `doric-themes-with-colors' doesn't exist in doric-themes v0.1
          ;; ((+in-list-p (car custom-enabled-themes) doric-themes-collection)
          ;;  (doric-themes-with-colors
          ;;   (face-remap-add-relative
          ;;    'default
          ;;    `(:background ,bg-dim))))
          ))

  ;; The idea is to assign that function to a hook that gets called when
  ;; `pdf-tools' renders the document: `pdf-tools-enabled-hook'. This is enough
  ;; when you only use one theme. However, it has the downside of setting the
  ;; background color value only at render time. In other words, the face
  ;; remapping function does not get evaluated anew whenever the theme changes,
  ;; such as invoking M-x modus-themes-toggle.
  ;;
  ;; To have our face remapping adapt gracefully while switching between the
  ;; Modus themes, we need to also account for the current theme and control the
  ;; activation of `pdf-view-themed-minor-mode'. To which end we arrive at
  ;; something like the following:
  (defun +in-list-p (val list)
    "Return t if VAL is in LIST, otherwise nil."
    (when (member val list) t))

  (defun +pdf-tools-themed-mode-toggle (&rest _)
    (when (derived-mode-p 'pdf-view-mode)
      (let* ((collections (append
                           (when (bound-and-true-p ef-themes-collection)
                             ef-themes-collection)
                           (when (bound-and-true-p modus-themes-collection)
                             modus-themes-collection)
                           (when (bound-and-true-p doric-themes-collection)
                             doric-themes-collection))))
        (if (+in-list-p (car custom-enabled-themes) collections)
            (pdf-view-themed-minor-mode 1)
          (pdf-view-themed-minor-mode -1)))
      (+pdf-tools-backdrop)))

  (defun +pdf-tools-themes-toggle (&rest _)
    (mapc
     (lambda (buf)
       (with-current-buffer buf
         (+pdf-tools-themed-mode-toggle)))
     (buffer-list)))

  (add-hook 'pdf-tools-enabled-hook #'+pdf-tools-themed-mode-toggle)
  (add-hook 'enable-theme-functions #'+pdf-tools-themes-toggle)

  ;; With those in place, PDFs have a distinct backdrop for their page, while
  ;; buffers with major-mode as `pdf-view-mode' automatically switches to dark
  ;; mode when `modus-themes-toggle' is called.

  ;; TODO: pdf-avy-highlight needs pdf-util-convert-program
  ;; I can use `avy' to create highlights in `pdf-view-mode' buffers. The
  ;; workflow is straightforward: call `+pdf-avy-highlight', type an initial
  ;; pattern to select where the highlight should start, then type the label to
  ;; confirm. Repeat the process to mark the end of the highlighted region. This
  ;; approach works fine, but using the mouse can sometimes be the simpler
  ;; choice.
  ;; <https://github.com/dalanicolai/dala-emacs-lisp/blob/master/pdf-avy-highlight.el>

  (setopt pdf-view-display-size 'fit-height
          pdf-view-use-dedicated-register nil
          pdf-outline-imenu-use-flat-menus t
          large-file-warning-threshold nil)

  (bind-keys
   :map pdf-view-mode-map
   ("a" . pdf-annot-add-text-annotation)
   ("d" . pdf-annot-delete)
   ("D" . pdf-view-themed-minor-mode)
   ("M-g g" . pdf-view-goto-page)
   ("M-g M-g" . pdf-view-goto-page)
   ("h" . pdf-annot-add-highlight-markup-annotation)
   ("i" . consult-imenu)
   ("s" . pdf-annot-add-strikeout-markup-annotation)
   ("M" . pdf-annot-add-markup-annotation)
   ("u" . pdf-annot-add-underline-markup-annotation)
   ("C-w" . pdf-view-kill-ring-save)
   ("M-w" . pdf-view-kill-ring-save)
   ("~" . pdf-annot-add-squiggly-markup-annotation)))

;; This package extends the built-in `save-place-mode' by adding support for PDF
;; buffers under PDFView or DocView mode. Revisiting PDF files will restore the
;; saved place (i.e. the current page and zoom.)
(use-package saveplace-pdf-view)

;; TODO document pdf-meta-edit
;; <https://github.com/krisbalintona/pdf-meta-edit>
;; (use-package pdf-meta-edit)

;; NOTE document nov
(use-package nov
  ;; Activate nov-mode for epub files
  :mode ("\\.epub\\'" . nov-mode)
  ;; (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :init
  (setq-default nov-text-width 80)
  :config
  ;; Using a variable pitch font in nov-mode often causes the bottom most line
  ;; to clip. Adding extra line spacing ameliorates this.
  (add-hook 'nov-mode-hook (lambda ()
                             (setq-local line-spacing 1))))

;;;;;;;;;;;;;
;;;; org ;;;;

(use-package org
  ;; Org, also known as "Org mode", is one of the potentially most useful
  ;; feature sets available to every Emacs user. At its core, Org is a
  ;; lightweight markup language: you can have headings and paragraphs, mark a
  ;; portion of text  with emphasis, produce bullet lists, include code blocks,
  ;; and the like. Though what really sets Org apart from other markup languages
  ;; is the rich corpus of Emacs Lisp written around it to do all sorts of tasks
  ;; with this otherwise plain text format.
  ;;
  ;; With Org you can write technical documents, maintain a simple of highly
  ;; sophisticated system for task management, organize your life using the
  ;; agenda, write tables that can evaluate formulas to have spreadsheet
  ;; functionality, have embedded LaTeX, evaluate code blocks in a wide range of
  ;; programming languages and reuse their results for literate programming,
  ;; include the contents of other files into a singular file, use one file to
  ;; generate other files/directories with all their contents, and export the
  ;; Org document to a variety of formats like .pdf and .odt. Furthermore, Org
  ;; can be used as a lightweight, plain text database, as each heading can have
  ;; its own metadata. This has practical applications in most of the
  ;; aforementioned.
  ;;
  ;; In short, if something can be done with plain text, Org probably does it
  ;; already or has all the elements for piecing it together.
  :init
  (setopt org-directory (expand-file-name "~/OneDrive/zettelkasten/"))
  ;; Must be evaluated before Org is loaded, otherwise we have to use the Custom
  ;; UI. No thanks!
  (setopt org-export-backends '(html texinfo md))
  :config
  ;; This being Emacs, everything is customizable and Org is a good example of
  ;; this. There are a lot of user options for us to tweak things to our liking.
  (setopt org-ellipsis "⤵"
          org-M-RET-may-split-line '((default . nil))
          org-cycle-separator-lines 0
          org-loop-over-headlines-in-active-region 'start-level
          org-insert-heading-respect-content t
          org-fontify-quote-and-verse-blocks t
          org-bookmark-names-plist nil)

  ;; Perhaps the most obvious feature of the Org syntax is the heading
  ;; levels. Users can fold them to conceal or reveal only the section of the
  ;; document they are interested in. For some documents, where there are a lot
  ;; of deeply nested headings with plenty of text below them, I find that
  ;; `org-indent-mode' makes it easier for me to make sense of the contents.

  ;; As the name suggests, this minor mode will indent headings and their text
  ;; according to their level of depth. This makes it easier to tell that
  ;; something belongs to a heading of depth 3 (three asterisks) instead of two.

  ;; The indentation produced by this mode is purely visual though: it does not
  ;; actually append tabs or spaces to your file. That is what the user option
  ;; `org-adapt-indentation' does. For me, I do not want anything to perform
  ;; sweeping modifications to my files, as it can easily lead to mistakes or
  ;; make things more inconvenient.

  ;; Whereas the purely visual `org-indent-mode' is a feature we can turn on and
  ;; off at will without worrying that something will change in our file.

  ;; We can set up this minor mode via a hook, so it takes effect in all Org
  ;; buffers. An alterative is to enable on a per-file basis, by adding the
  ;; "indent" keyword to the "#+startup" keyword somewhere close to the top of
  ;; the file. For example:
  ;;
  ;; #+startup: content indent
  ;;
  ;; What this `#+startup' directive does is to (i) show all the headings while
  ;; folding their contents and (ii) activate `org-indent-mode'. If you add the
  ;; `#+startup' to an already open file, then you need to do `org-mode-restart'
  ;; for changes to take effect, or move the cursor to that line and type "C-c
  ;; C-c". Below I configure `org-startup-folded' which is the underlying
  ;; variable for the "content" part shown in this example.

  ;; By default, `org-indent-mode' will hide the leading asterisks, leaving only
  ;; one at a time. This looks cleaner overall, though I find it harder to add a
  ;; new heading as I am not quite sure how many asterisks I need to add when I
  ;; perform a manual insertion. Plus, I like seeing all the leading starts,
  ;; anyway, in the same way I prefer to have all the emphasis markers on
  ;; display. As such, I set the user option
  ;; `org-indent-mode-turns-on-hiding-stars' to `nil'.

  ;; The other small tweak I make is to make the indentation a bit more
  ;; pronouced, by setting `org-indent-indentation-per-level' to `4' instead of
  ;; `2'.
  (add-hook 'org-mode-hook #'org-indent-mode)
  (setopt org-adapt-indentation nil ; No, non, nein, όχι to literal indentation!
          org-indent-mode-turns-on-hiding-stars nil
          org-indent-indentation-per-level 4
          org-startup-folded 'content)

  ;; One of the many use-cases for Org is to maintain a plain text to-do list. A
  ;; heading that starts with a to-do keyword, such as "TODO", is treated as a
  ;; task and its state is considered not completed.

  ;; We can switch between the task states with shift and the left or right
  ;; arrow keys. Or we can select a keyword directly with 'C-c C-t', which calls
  ;; `org-todo' by default. I personally prefer the latter approach, as it is
  ;; more precise

  ;; By default, the `org-todo-keywords' are 'TODO' and 'DONE'. We can write
  ;; more keywords if we wish to implement a descriptive workflow. For example,
  ;; we can have a 'HOLD' keyword for something that is to be done but is not
  ;; actionable yet. We can have a 'NEXT' keyword for something that is to be
  ;; completed right after we finish the currently active task, and so on. While
  ;; the number of keywords is not limited, the binary model is the same: we
  ;; have words that represent the incomplete state and those that count as the
  ;; completion of the task. For instance, both 'CANCEL' and 'DONE' mean that a
  ;; task is not actionable anymore and we move on to other things. As such, the
  ;; extra keywords are a way for the user to make tasks more descriptive and
  ;; easy to find. In the value of `org-todo-keywords', we use the bar character
  ;; to separate the incomplete state to the left from the completed one to the
  ;; right. Learn about the !, @, and more by reading the relevant section of
  ;; the Org manual. Evaluate: (info "(org) Tracking TODO state changes")
  ;;
  ;; One of the agenda's headline features is the ability to produce a view that
  ;; lists headings with the given keyword. So having the right terms can make
  ;; search and retrieval of data more easy. On the flip-side, too many keywords
  ;; add cognitive load and require more explicit search terms to yield the
  ;; desired results. I used to work with a more descriptive set of keywords,
  ;; but ultimately decided to keep things simple.
  (setopt org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "CANCEL(c@)"
                               "DONE(d!)"))
          org-use-fast-todo-selection 'expert
          org-fontify-done-headline nil
          org-fontify-todo-headline nil
          org-fontify-whole-heading-line t ; fontify `org-tidy' inline symbol
          org-enforce-todo-dependencies t
          org-enforce-todo-checkbox-dependencies t)

  ;; Org can keep a record of state changes, such as when we set an entry marked
  ;; with the 'TODO' keyword as 'DONE' or when we reschedule an appointment.
  ;; This data is stored in 'LOGBOOK' drawer right below the heading. I choose
  ;; to opt into this feature beacuse it is sometimes useful to capture mistakes
  ;; or figure out intent in the absence of further clarification (though I do
  ;; tend to write why something happened).
  (setopt org-log-done 'time
          org-log-into-drawer t
          org-log-note-clock-out nil
          org-log-redeadline 'time
          org-log-reschedule 'time)

  ;; The refile mechanism is how we can reparent a heading, by moving it from
  ;; one place to another. We do this with the command `org-refile', bound to
  ;; 'C-c C-w' by default. A common workflow where refiling is essential is to
  ;; have an "inbox" file or heading, where unprocessed information is stored
  ;; at, and periodically process its contents to move the data where it
  ;; belongs. Though it can also work file without any such inbox, in those
  ;; cases where a heading should be stored someplace else. The
  ;; `org-refile-targets' specifies the files that are available when we try to
  ;; refile the current heading. With how I set it up, all the agenda files'
  ;; headings up to level 2 plus the "Notes" and "Tasks" headings in a separate
  ;; "projects.org" file are included as possible entries.
  (setopt org-refile-targets '(("20250112T073531--projects.org" :regexp
                                . "\\(?:\\(?:Note\\|Task\\)s\\)")
                               ("20250120T090205--archive.org" :level . 0)
                               (org-agenda-files :maxlevel . 2))

          ;; Show full path of file when refiling.
          org-refile-use-outline-path 'file
          ;; Refile in a single step, but the list becomes more cluttered.
          org-outline-path-complete-in-steps nil
          ;; Allow creation of new nodes on refile by adding "/new node name"
          org-refile-allow-creating-parent-nodes 'confirm
          org-refile-use-cache nil)

  ;; After refiling, you will have to manually save your opened Org files. This
  ;; is inconvenient. Fortunately we can create a function to do that for us and
  ;; add it after the `org-refile' action.
  (defun +org-save-org-agenda-files ()
    "Save `org-agenda-files' without user confirmation.
See also `org-save-all-org-buffers'."
    (interactive)
    (let* ((agenda-files (append (mapcar 'file-truename
                                         (file-expand-wildcards
                                          (concat org-directory "*.org")))
                                 '("20250120T090205--archive.org"))))
      (message "Saving org-agenda-files buffers...")
      (save-some-buffers t (lambda ()
                             (when (member (buffer-file-name) agenda-files)
                               t)))
      (message "Saving org-agenda-files buffers... done")))

  (advice-add 'org-refile :after
              (lambda (&rest _)
                (+org-save-org-agenda-files)))

  ;; Each Org heading can have one or more tags associated with it, while all
  ;; headings inherit any potential #+filetags. We can add tags to a heading
  ;; when the cursor is over it by typing the ever flexible 'C-c C-c'. Though
  ;; the more specific `org-set-tags-command' also gets the job done, plus it
  ;; does not require that the cursor is positioned on the heading text.
  ;;
  ;; Tagging is useful for searching and retrieving the data we store. The Org
  ;; agenda, in particural, provides commands to filter tasks by tag.
  ;;
  ;; The user option `org-tag-alist' lets us specify tags we always want to use,
  ;; though we can write tags per file as well by using the #+tags keyword. I do
  ;; the latter as a global list of tags is not useful in my case.
  ;;
  ;; Note that in the settings below I disable the auto-alignment that Org does
  ;; where it shifts tags to the right of the heading. I do not like it.
  (setopt org-tag-alist nil
          org-auto-align-tags nil
          org-tags-column 0)

  ;; One of the nice things about Org is its flexible linking mechanism. It can
  ;; produce links to a variety of file types or buffers and even navigate to a
  ;; section therein.
  ;;
  ;; At its simplest form, we have the "file" link type, which points to a file
  ;; system path, with an optional extension for a match inside the file, as
  ;; documented in the manual. (info "(org) Search Options")
  ;;
  ;; Links to buffers are also common and valuable. For example, we can have a
  ;; link to a page produced by the `man' command, which gives us quick access
  ;; to the documentation of some program. When Org follows that link, it opens
  ;; the buffer in the appropriate major mode. For me, the most common scenario
  ;; is a link to an email, which I typically associate with a task that shows
  ;; up in my agenda.
  ;;
  ;; Org supports lots of link types out-of-the-box, though more can be added by
  ;; packages. Denote does this: it defines a "denote" link type which behaves
  ;; the same way as the "file" type except that it uses the identifier of the
  ;; file instead of its full path (so even if the file is renamed, the link
  ;; will work for as long as the identifier remains the same).
  ;;
  ;; Links can be generated automatically as part of as `org-capture'
  ;; template. The command `org-store-link' produces one manually, storing it to
  ;; a special data structure from which it can be retrieved later for insertion
  ;; with the command `org-insert-link'. The latter command can also create new
  ;; links, simply by receiving data that is different from what was already
  ;; stored.
  (setopt org-return-follows-link t)

  ;; Org can combine prose with code, by placing the latter inside a block that
  ;; is delimited by '#+BEGIN_SRC' and '#+END_SRC' lines.
  ;;
  ;; Code blocks can use the syntax highlighting ("fontification" in Emacs
  ;; parlance) of a given major mode. They can also have optional parameters
  ;; passed to their header, which expand the capabilities of the block.
  ;;
  ;; More generally, Org is capable of evaluating code blocks and passing their
  ;; return value to other code blocks. It is thus possible to write a fully
  ;; fledged program as an Org document. This paradigm is known as "literate
  ;; programming".
  ;;
  ;; Org can evaluate code blocks in many languages. This is known as "Org
  ;; Babel" and the files which implement support for a given language are
  ;; typically named `ob-LANG.el' where 'LANG' is the name of the language.
  ;;
  ;; I seldom work with Org Babel (or literate programming for that matter), so
  ;; I do not load the requisite code for any particular language
  ;; automatically. Note that Emacs Lisp is loaded by default.

  ;; To evaluate a code block, we type Org's omnipotetnt 'C-c C-c'. The results
  ;; will be producet below the code block. There is an optional parameter that
  ;; controls how - or even if - the results are displayed.

  ;; There are many other types of blocks apart from 'SRC'. Those do different
  ;; things, such as:
  ;;
  ;; #+BEGIN_QUOTE
  ;; Treat the contents as a block quote or equivalent.
  ;;
  ;; #+BEGIN_VERSE
  ;; Do not reflow any line breaks (for poetry and such).
  ;;
  ;; #+BEGIN_EXPORT
  ;; Evaluate the code for the given export target (like html or latex),
  ;; optionally replacing it with its results or keeping both of them.
  (setopt org-confirm-babel-evaluate nil
          org-src-window-setup 'current-window
          org-src-preserve-indentation t
          org-edit-src-content-indentation 0)

  ;; Org is a capable authoring tool in no small part because it can be
  ;; converted to other file formats. A typical example is to write a technical
  ;; document in Org and then export it to a PDF. Another use-case is to export
  ;; an Info manual (texinfo format) and an HTML web page.
  ;;
  ;; The default set of export targets is specified in the value of the user
  ;; option `org-export-backends'. It is one of those rare cases where it has to
  ;; be evaluated before the package is loaded (which is why I configure it in
  ;; the :init section of this use-package block). Other than that, we can load
  ;; an export backend by loading the corresponding `ox-FORMAT.el' file.
  ;; (setopt org-export-headline-levels 8
  ;;         org-html-htmlize-output-type nil
  ;;         org-html-head-include-default-style nil)

  ;; It's well known that the `TAB' key is heavily overloaded in
  ;; Emacs. Depending on the context and configuration, it can perform one of
  ;; four types of actions: line indentation, candidate completion (during
  ;; editing), or field navigation and visibility cycling (during
  ;; reading).
  ;;
  ;; Personally, I want to move in the opposite direction: removing visibility
  ;; cycling from the list of `TAB'-triggered actions. Three types of behaviors
  ;; are already plenty. I'd rather assign visibility control to a more complex
  ;; keybinding and prioritize field navigation instead. I also value
  ;; consistency in keybindings, so unifying `TAB' behavior across modes is
  ;; important to me (granted, if you don't use Info or navigate Help buffers
  ;; with `TAB', you might not miss that behavior in Org mode).
  ;;
  ;; What exactly is considered a "field" is largely up to the user. In general,
  ;; it should be a structural element in a file where a non-trivial action can
  ;; be performed, making it useful to have an easy way to jump between
  ;; them. For my setup, I chose to treat only links and headlines as fields,
  ;; similar to how Info handles navigation. Of course, others might include
  ;; property drawers, code blocks, custom buttons, or other interactive
  ;; elements. I wouldn't overdo it though--too many fields and `TAB' navigation
  ;; loses its utility.

  (defun +org-next-visible-heading-or-link (&optional arg)
    "Move to the next visible heading or link, whichever comes first.
With prefix ARG and the point on a heading(link): jump over subsequent
headings(links) to the next link(heading), respectively.  This is useful
to skip over a long series of consecutive headings(links)."
    (interactive "P")
    (let ((next-heading (save-excursion
                          (org-next-visible-heading 1)
                          (when (org-at-heading-p) (point))))
          (next-link (save-excursion
                       (when (+org-next-visible-link) (point)))))
      (when arg
        (if (and (org-at-heading-p) next-link)
            (setq next-heading nil)
          (if (and (looking-at org-link-any-re) next-heading)
              (setq next-link nil))))
      (cond
       ((and next-heading next-link) (goto-char (min next-heading next-link)))
       (next-heading (goto-char next-heading))
       (next-link (goto-char next-link)))))

  (defun +org-previous-visible-heading-or-link (&optional arg)
    "Move to the previous visible heading or link, whichever comes first.
With prefix ARG and the point on a heading(link): jump over subsequent
headings(links) to the previous link(heading), respectively.  This is useful
to skip over a long series of consecutive headings(links)."
    (interactive "P")
    (let ((prev-heading (save-excursion
                          (org-previous-visible-heading 1)
                          (when (org-at-heading-p) (point))))
          (prev-link (save-excursion
                       (when (+org-next-visible-link t) (point)))))
      (when arg
        (if (and (org-at-heading-p) prev-link)
            (setq prev-heading nil)
          (if (and (looking-at org-link-any-re) prev-heading)
              (setq prev-link nil))))
      (cond
       ((and prev-heading prev-link) (goto-char (max prev-heading prev-link)))
       (prev-heading (goto-char prev-heading))
       (prev-link (goto-char prev-link)))))

  ;; Adapted from org-next-link to only consider visible links
  (defun +org-next-visible-link (&optional search-backward)
    "Move forward to the next visible link.
When SEARCH-BACKWARD is non-nil, move backward."
    (interactive)
    (let ((pos (point))
          (search-fun (if search-backward #'re-search-backward
                        #'re-search-forward)))
      ;; Tweak initial position: make sure we do not match current link.
      (cond
       ((and (not search-backward) (looking-at org-link-any-re))
        (goto-char (match-end 0)))
       (search-backward
        (pcase (org-in-regexp org-link-any-re nil t)
          (`(,beg . ,_) (goto-char beg)))))
      (catch :found
        (while (funcall search-fun org-link-any-re nil t)
          (let* ((begin (match-beginning 0)))
            (unless (org-fold-core-folded-p begin)
              (let* ((context (save-excursion
                                (goto-char begin)
                                (org-element-context)))
                     (etype (org-element-type context)))
                (pcase etype
                  (`node-property
                   (goto-char begin)
                   (throw :found t))
                  (`link
                   (goto-char (org-element-property :begin context))
                   (throw :found t)))
                ))))
        (goto-char pos)
        ;; No further link found
        nil)))

  ;; In +org-tab and +org-shifttab, I preserved the default behavior of
  ;; `org-cycle' within a table: it navigates between table fields.

  (defun +org-shifttab (&optional arg)
    "Move to the previous visible heading or link.
If already at a heading, move first to its beginning.  When inside a table,
move to the previous field."
    (interactive "P")
    (cond
     ((org-at-table-p) (call-interactively #'org-table-previous-field))
     ((and (not (bolp)) (org-at-heading-p)) (beginning-of-line))
     (t (call-interactively #'+org-previous-visible-heading-or-link))))

  (defun +org-tab (&optional arg)
    "Move to the next visible heading or link.
When inside a table, re-align the table and move to the next field."
    (interactive "P")
    (cond
     ((org-at-drawer-p) (call-interactively #'org-cycle))
     ((org-at-table-p) (org-table-justify-field-maybe)
      (call-interactively #'org-table-next-field))
     (t (call-interactively #'+org-next-visible-heading-or-link))))

  ;; For visibility cycling, I now rely on Org Speed Keys (a built-in feature of
  ;; Org mode).
  ;;
  ;; Speed Keys let you trigger commands with a single keystroke when the point
  ;; is at the beginning of a headline. A number of keys are predefined out of
  ;; the box; for example, `c' is already mapped to `org-cycle', which is what
  ;; `TAB' normally does in Org mode.
  ;;
  ;; I've customized `org-speed-commands' to only bind editing actions to keys
  ;; that require the Shift modifier. I like keeping lowercase keys reserved for
  ;; non-destructive commands. As a next step, I may remap Space and
  ;; Shift-Space/Backspace to scroll the buffer. That would bring me even closer
  ;; to a more consistent reading experience.

  (setopt org-use-speed-commands t
          org-speed-commands
          '(("Outline Navigation and Visibility")
            ("n" . (org-speed-move-safe 'org-next-visible-heading))
            ("p" . (org-speed-move-safe 'org-previous-visible-heading))
            ("f" . (org-speed-move-safe 'org-forward-heading-same-level))
            ("b" . (org-speed-move-safe 'org-backward-heading-same-level))
            ("u" . (org-speed-move-safe 'outline-up-heading))
            ("j" . org-goto)
            ("c" . org-cycle)
            ("C" . org-shifttab)
            (" " . org-display-outline-path)
            ("s" . org-toggle-narrow-to-subtree)
            ("Editing")
            ("I" . (progn (forward-char 1) (call-interactively
                                            'org-insert-heading-respect-content)))
            ("^" . org-sort)
            ("W" . org-refile)
            ("@" . org-mark-subtree)
            ("," . org-mark-subtree)
            ("T" . org-todo)
            (":" . org-set-tags-command)
            ("Misc")
            ("?" . org-speed-command-help)))

  ;; By default, `org-cycle' moves through three visibility states when toggling
  ;; a headline: folded -> children -> subtree -> folded. Personally, I rarely
  ;; want to expand an entire subtree. My preference is to only toggle between a
  ;; folded headline and its immediate children. If I ever want to see deeper
  ;; levels, I'll expand those manually. The hook below makes it so org-cycle
  ;; skips the subtree step entirely, preventing that brief but distracting
  ;; "flash" of the full subtree before folding again.
  (defun +org-cycle-skip-subtree (state)
    "Skip subtree after a visibility state change."
    (when (eq state 'children)
      (setq org-cycle-subtree-status 'subtree)))
  (add-hook 'org-cycle-hook #'+org-cycle-skip-subtree)

  ;; Open Org links in current window. Default is `'find-file-other-window'
  ;;
  ;; HACK: Can I replace this hack with some `display-buffer-alist'
  ;; configuration?
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Recenter and pulse the current line, and display the hidden contents of Org
  ;; and Outline headings.
  (with-eval-after-load 'pulsar
    (dolist (hook '(org-follow-link-hook org-agenda-after-show-hook))
      (add-hook hook #'pulsar-recenter-middle)
      (add-hook hook #'pulsar-reveal-entry)))

  (bind-keys
   ;; I don't like that Org binds one zillion keys, so if I want one for
   ;; something more important, I disable it from here.
   :map org-mode-map
   ("<tab>" . +org-tab)
   ("<backtab>" . +org-shifttab)
   ("C-'" . nil)
   ("C-," . nil)
   ("C-<return>" . nil)
   ("C-M-<return>" . nil)
   ("C-j" . nil)
   ("C-c l" . org-store-link)
   ("C-c M-l" . org-insert-last-stored-link)
   ("C-c C-M-l" . org-toggle-link-display)
   ("M-." . org-edit-special) ; mnemonic is global M-. that goes to source (alias for C-c ')
   ("M-g o" . consult-org-heading) ; alt. `consult-outline'
   ("M-g M-o" . consult-org-heading) ; alt. `consult-outline'
   :map org-src-mode-map
   ("M-," . org-edit-src-exit) ; see M-. above
   ))

(use-package org-capture
  :config
  ;; The `org-capture' command allows us to quickly store data in some
  ;; structured way. This is done with the help of a templating system where we
  ;; can, for example, record the date the entry was recorded, prompt for user
  ;; input, automatically use the email's subject as the title of the task, and
  ;; the like. The documentation string of `org-capture-templates' covers the
  ;; technicalities.

  ;; As for my workflow, here is an overview:
  ;;
  ;; When I want to quickly capture any data or idea, I add it to the
  ;; 'zettelkasten/inbox.org' file. My goal is to have a non-disruptive
  ;; process. That is, type a key sequence to enter "capture mode", type some
  ;; text, and then just forget about it. I do not want to have to think where I
  ;; should store this text nor about any related information such as tags or
  ;; dates, at least not yet. Not everything goes into the inbox. This is just a
  ;; fallback for those cases where I need more information to decide on the
  ;; appropriate action.

  ;; I periodically review those headings to decide if I want to do something
  ;; with them or not. If I do not want them, I delete them. Otherwise, I file
  ;; them under another heading in the 'zettelkasten/projects.org' using the
  ;; `org-refile' command.

  ;; Tasks that have an inherent time component such as appointmets are given a
  ;; 'SCHEDULED' or 'DEADLINE' timestamp (set those on demand with the commands
  ;; `org-schedule' and `org-deadline', respectively). These are the only tasks
  ;; I want to see on my daily agenda. I often know in advance what this item is
  ;; about and when they will occur, so I can directly store them in a dedicated
  ;; 'zettelkasten/agenda.org' file for all my scheduled events and
  ;; meetings. The difference between 'SCHEDULED' and 'DEADLINE' is that the
  ;; former has no strict start or end time and so is flexible, while the latter
  ;; is more rigit. For example, "visit the vet today" does not have a strict
  ;; time associated with it because the doctor often deals with emergency
  ;; situations and thus their agenda is fluid. While a meeting like "work on
  ;; Emacs with PERSON" has to start at the agreed upon time.

  ;; I do not arbitrarily assign timestamps to tasks. If something does not have
  ;; a scheduled date or a deadline, then it does not belong in the agenda.
  ;; Otherwise, those arbitrarily defined "events" accumulate in the agenda and
  ;; crowd out the actual time-sensitive tasks. As a result, the cognitive load
  ;; is heavier and things will not be done. So when I want to do something at
  ;; some point, but have no specific plan for it, I add it to the
  ;; 'zettelkasten/projects.org' "Wishlist" heading. When I have free time, I
  ;; review my wishlist and pick something to work on from there depending on my
  ;; available time and moode. This keeps my workflow both focused and
  ;; stress-free.
  (setopt org-capture-templates
          `(("i" "Inbox" entry (file "20250110T181524--inbox.org")
             ,(concat "* TODO %?\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:\n\n"
               "%i"))
            ;; See the `ol-notmuch' section for available template extensions.
            ;; ("@" "Inbox [e-mail]" entry (file "20250110T181524--inbox.org")
            ;;  ,(concat "* TODO Process %:subject :@mail:\n"
            ;;           ":PROPERTIES:\n"
            ;;           ":CAPTURED: %U\n"
            ;;           ":END:\n\n"
            ;;           "%a\n%i%?")
            ;;  :empty-lines-after 1)
            ("m" "Meeting" entry (file+headline "20250111T062159--agenda.org"
                                  "Future")
             ,(concat "* %? :meeting:\n"
               "DEADLINE: %t\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:\n\n"
               "%i"))
            ("n" "Meeting note" entry (file "20250110T181524--inbox.org")
             ,(concat "* Note (%a)\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:\n\n"
               "%i%?"))
            ))

  ;; (setopt org-capture-templates-contexts
  ;;         '(("@" ((in-mode . "notmuch-search-mode")
  ;;                 (in-mode . "notmuch-show-mode")
  ;;                 (in-mode . "notmuch-tree-mode")))))

  (defun +org-capture-inbox ()
    "Capture something to the inbox and store a link to the current location
if possible."
    (interactive)
    (ignore-errors (call-interactively #'org-store-link))
    (org-capture nil "i"))

  ;; I want to directly capture Notmuch links - for example, to add e-mail
  ;; messages to your to-do list. For that, the function
  ;; `+org-notmuch-store-and-capture' captures the message-at-point (or query),
  ;; then calls org-mode's capture functionality.
  ;; (defun +org-notmuch-store-and-capture ()
  ;;   "Store a link to the current message or query and capture it with Org."
  ;;   (interactive)
  ;;   (call-interactively 'org-store-link)
  ;;   (org-capture nil "@"))

  ;;;;; Custom function to select a project to add to
  (defun +org--get-outline (&optional file)
    "Return `outline-regexp' headings and line numbers of current file or FILE."
    (with-current-buffer (find-file-noselect file)
      (let ((outline-regexp (format "^\\(?:%s\\)" (or (bound-and-true-p outline-regexp) "[*\^L]+")))
            candidates)
        (save-excursion
          (goto-char (point-min))
          (while (if (bound-and-true-p outline-search-function)
                     (funcall outline-search-function)
                   (re-search-forward outline-regexp nil t))
            (push
             ;; NOTE 2024-11-24: The -5 (minimum width) is a sufficiently high number to keep the
             ;; alignment consistent in most cases.  Larger files will simply shift the heading text
             ;; in minibuffer, but this is not an issue anymore.
             (format "%-5s\t%s"
                     (line-number-at-pos (point))
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             candidates)
            (goto-char (1+ (line-end-position)))))
        (if candidates
            (nreverse candidates)
          (user-error "No outline")))))

  (defvar +org-outline-history nil
    "Minibuffer history for `+org-outline-prompt'.")

  (defun +org-outline-prompt (&optional file)
    "Prompt for outline among headings retrieved by `+org--get-outline'.
With optional FILE use the outline of it, otherwise use that of
the current file."
    (let ((current-file (or file buffer-file-name))
          (default (car +org-outline-history)))
      (completing-read
       (format-prompt
        (format "Select heading inside `%s': "
                (propertize (file-name-nondirectory current-file) 'face 'error))
        default)
       (+common-completion-table-no-sort 'imenu (+org--get-outline current-file))
       nil :require-match nil '+org-outline-history default)))

  (defvar +org-file-history nil
    "Minibuffer history of `+org-file-prompt'.")

  (defun +org--not-useful-p (file)
    "Return non-nil if FILE is not a useful Org file for `org-capture'."
    (or (string-match-p "\\.org_archive\\'" file)
        (backup-file-name-p file)
        (not (string-match-p "\\.org\\'" file))))

  (defun +org-file-prompt ()
    "Select a file in the `org-directory'."
    (if-let* ((dir org-directory)
              (files (directory-files-recursively org-directory ".*" nil))
              (files (seq-remove #'+org--not-useful-p files)))
        (let ((default (car +org-file-history)))
          (completing-read
           (format-prompt "Select file" default)
           (+completion-table 'file files)
           nil :require-match nil '+org-file-history default))
      (user-error "There are no files in the `org-directory'")))

  (defun +org-select-heading-in-file ()
    "Move to heading in Org file stored in `org-directory'.
Prompt for file, then for heading inside of that file.

This function can also be used as part of an `org-capture' template to
navigate to a file+heading and then capture something which is inserted
there."
    (declare (interactive-only t))
    (interactive)
    (pcase-let* ((file (+org-file-prompt))
                 (line-with-heading (+org-outline-prompt file))
                 (`(,line ,text) (split-string line-with-heading "\t"))
                 (line (string-to-number line)))
      ;; NOTE 2024-11-24: `with-current-buffer' does not work with `org-capture'.
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line))))

  (defalias '+org-goto-heading-in-file '+org-select-heading-in-file
    "Alias for `+org-select-heading-in-file'.")

  (bind-keys
   :map +prefix-map
   ("c" . org-capture)
   ("i" . +org-capture-inbox)
   ;; :map notmuch-search-mode-map
   ;; ("@" . +org-notmuch-store-and-capture)
   ;; :map notmuch-show-mode-map
   ;; ("@" . +org-notmuch-store-and-capture)
   ;; :map notmuch-tree-mode-map
   ;; ("@" . +org-notmuch-store-and-capture)
   ))

(use-package org-agenda
  :init
  (add-to-list 'org-modules 'org-habit t)
  :config
  ;; With the Org agenda, we can visualize the tasks we have collected in our
  ;; Org files or, more specifically, in the list of files specified in the user
  ;; option `org-agenda-files'. In my workflow, only the files in the
  ;; `org-directory' can feed into the agenda. Though Org provides to add/remove
  ;; the current file on demand: `org-remove-file', and
  ;; `org-agenda-file-to-front'. If I ever need to write a task that is specific
  ;; to a certain file or buffer, then I use Org's linking mechanism to point to
  ;; the relevant context, but otherwise store my task in the usual place.

  ;; By default, Org provides many so called "views" for the agenda. One of them
  ;; is the daily/weekly agenda. Others show only the headings with "TODO"
  ;; keywords, or some other kind of search criteria. I personally never use
  ;; those views. I have my own custom agenda view, which consolidates in a
  ;; single buffer the following blocks of data, in this order:

  ;; Important tasks without a date
  ;; When I add a top priority to something, but there is no inherent deadline
  ;; to it.
  ;;
  ;; Pending scheduled tasks
  ;; Tasks with a 'SCHEDULED' date may sometimes not be done when they ought
  ;; to. So they need to be closer to the top for me to do them as soon as I
  ;; can.
  ;;
  ;; Today's agenda
  ;; What I am actually working on. Because I only assign a timestamp to tasks
  ;; that are indeed time-sensitive, this always reflects the commitments I have
  ;; for the day.
  ;;
  ;; Next three days
  ;; Like the above, but for the near future.
  ;;
  ;; Upcoming deadlines (+14d)
  ;; These are the deadlines I need to be aware of for the next 14 days after
  ;; the next three days above.
  ;;
  ;; Inbox
  ;; All items in my inbox so I'm reminded to process any remaining items at the
  ;; end of the day.
  ;;
  ;; Completed today
  ;; Tasks I've finished today. Useful for reflecting on my accomplishments at
  ;; the end of the day or for archiving.

  (setopt org-agenda-files '("20250110T181524--inbox.org"
                             "20250111T062159--agenda.org"
                             "20250112T073531--projects.org")
          ;; Basic agenda setup
          org-agenda-show-outline-path nil
          org-agenda-window-setup 'current-window
          ;; General agenda view options
          org-agenda-hide-tags-regexp "."
          org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                     (todo   . " %i %-12:c")
                                     (tags   . " %i %-12:c")
                                     (search . " %i %-12:c"))
          org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                        (todo priority-down category-keep)
                                        (tags priority-down category-keep)
                                        (search category-keep))
          org-agenda-remove-times-when-in-prefix nil
          ;; Agenda marks
          org-agenda-bulk-mark-char "#"
          ;; Agenda follow mode
          org-agenda-follow-indirect t
          ;; Agenda items with deadline and scheduled timestamps
          org-deadline-warning-days 0
          org-agenda-skip-scheduled-if-deadline-is-shown t
          org-agenda-skip-timestamp-if-deadline-is-shown t
          org-agenda-skip-deadline-prewarning-if-scheduled 1
          org-agenda-search-headline-for-time nil
          org-scheduled-past-days 365
          org-deadline-past-days 365
          ;; Time grid
          org-agenda-time-leading-zero t
          org-agenda-current-time-string (concat "Now " (make-string 70 ?.))
          org-agenda-time-grid '((daily today require-timed)
                                 ( 0500 0600 0700 0800 0900 1000
                                   1100 1200 1300 1400 1500 1600
                                   1700 1800 1900 2000 2100 2200)
                                 "" "")
          ;; Agenda global to-do list
          ;; Agenda tagged items
          ;; Agenda entry
          ;; Agenda logging and clocking
          ;; Agenda column view
          )

  ;; Agenda habits
  (require 'org-habit)
  (setopt org-habit-graph-column 50
          org-habit-preceding-days 9
          ;; Set to t if I always want to show the habit graph, even if there
          ;; are no habit for today.
          org-habit-show-all-today nil)

  (defun +org-agenda-include-priority-no-timestamp ()
    "Return nil if heading has a priority but no timestamp.
Otherwise, return the buffer position from where the search should
continue, per `org-agenda-skip-function'."
    (let ((point (point)))
      (if (and (eq (nth 3 (org-heading-components)) ?A)
               (not (org-get-deadline-time point))
               (not (org-get-scheduled-time point)))
          nil
        (line-beginning-position 2))))

  (defun +org--get-entry-end (&optional subtree)
    "Get the position of the end of entry at point, or SUBTREE, if not nil."
    (if subtree (save-excursion (org-end-of-subtree t) (point))
      (org-entry-end-position)))

  (defun +org-agenda-skip-if-habit (&optional subtree)
    "Skip an agenda entry (or SUBTREE, if not nil) if it is a habit."
    (let ((end (+org--get-entry-end subtree)))
      (if (org-is-habit-p)
          end
        nil)))
  (defun +org-agenda-skip-if-not-habit (&optional subtree)
    "Skip an agenda entry (or SUBTREE, if not nil) if it is not a habit."
    (let ((end (+org--get-entry-end subtree)))
      (if (not (org-is-habit-p))
          end
        nil)))

  (setopt org-agenda-custom-commands
          '(("A" "Daily agenda and top priority tasks"
             ((tags-todo "*"
               ((org-agenda-overriding-header "Important tasks without a date\n")
                (org-agenda-skip-function #'+org-agenda-include-priority-no-timestamp)
                (org-agenda-block-separator nil)))
              (agenda "" ((org-agenda-overriding-header "\nPending scheduled tasks")
                          (org-agenda-time-grid nil)
                          (org-agenda-start-on-weekday nil)
                          (org-agenda-span 1)
                          (org-agenda-show-all-dates nil)
                          (org-scheduled-past-days 365)
                          ;; Excludes today's scheduled items
                          (org-scheduled-delay-days 1)
                          (org-agenda-block-separator nil)
                          (org-agenda-entry-types '(:scheduled))
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                          (org-agenda-skip-function '+org-agenda-skip-if-habit)
                          (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                          (org-agenda-format-date "")))
              (agenda "" ((org-agenda-overriding-header "\nToday's agenda\n")
                          (org-agenda-span 1)
                          (org-deadline-warning-days 0)
                          (org-agenda-block-separator nil)
                          (org-scheduled-past-days 0)
                          (org-agenda-skip-function '+org-agenda-skip-if-habit)
                          ;; We don't need the `org-agenda-date-today'
                          ;; highlight because that only has a practical
                          ;; utility in multi-day views.
                          (org-agenda-day-face-function (lambda (date)
                                                          'org-agenda-date))
                          (org-agenda-format-date "%A %-e %B %Y")))
              (agenda "" ((org-agenda-overriding-header "\nNext three days\n")
                          (org-agenda-start-on-weekday nil)
                          (org-agenda-start-day nil)
                          (org-agenda-start-day "+1d")
                          (org-agenda-span 3)
                          (org-deadline-warning-days 0)
                          (org-agenda-block-separator nil)
                          (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                      'todo 'done))))
              (agenda "" ((org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")
                          (org-agenda-time-grid nil)
                          (org-agenda-start-on-weekday nil)
                          ;; We don't want to replicate the previous section's
                          ;; three days, so we start counting from the day
                          ;; after.
                          (org-agenda-start-day "+4d")
                          (org-agenda-span 14)
                          (org-agenda-show-all-dates nil)
                          (org-deadline-warning-days 0)
                          (org-agenda-block-separator nil)
                          (org-agenda-entry-types '(:deadline))
                          (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                      'todo 'done))))
              (tags-todo "inbox"
               ((org-agenda-overriding-header "\nInbox\n")
                (org-agenda-prefix-format "  %?-12t% s")
                (org-agenda-block-separator nil)))
              (agenda "" ((org-agenda-overriding-header "\nHabits")
                          (org-agenda-time-grid nil)
                          (org-agenda-start-on-weekday nil)
                          (org-agenda-span 1)
                          (org-agenda-show-all-dates nil)
                          (org-scheduled-past-days 365)
                          ;; Excludes today's scheduled items
                          ;; (org-scheduled-delay-days 1)
                          (org-agenda-block-separator nil)
                          (org-agenda-entry-types '(:scheduled))
                          (org-agenda-skip-function '+org-agenda-skip-if-not-habit)
                          (org-agenda-day-face-function (lambda (date)
                                                          'org-agenda-date))
                          (org-agenda-format-date "")))
              (tags "CLOSED>=\"<today>\""
               ((org-agenda-overriding-header "\nCompleted today\n")
                (org-agenda-block-separator nil))))
             ((org-agenda-fontify-priorities nil)
              (org-agenda-prefix-format "  %t %s")
              (org-agenda-dim-blocked-tasks nil)))))

  ;; TODO (setopt org-agenda-format-date #'+org-agenda-format-date-aligned) (from prot)

  (defun +org-agenda-set-outline ()
    "Set `outline-regexp' for my Org agenda buffers."
    (when (derived-mode-p 'org-agenda-mode)
      (setq-local outline-regexp "\\(^[ \t]+\\([A-Z]+ \\|[0-9]+:[0-9]+ \\)\\)\\|\\(^[A-Z][^ \t].*\\)")))
  (add-hook 'org-agenda-mode-hook #'+org-agenda-set-outline)

  (defun +org-agenda-custom ()
    "Call Org agenda with my custom daily agenda configuration."
    (interactive)
    (org-agenda nil "A"))

  (bind-keys
   :map +prefix-map
   ;; NOTE replaced abbrev maps, find somewhere to relocate them later
   ("a" . +org-agenda-custom)
   ("C-a" . org-agenda)
   :map org-agenda-mode-map
   ("n" . org-agenda-next-item)
   ("p" . org-agenda-previous-item)))

;; TODO make my org-mode look like this (mainly the utf-8? headline stars and src block):
;; https://raw.githubusercontent.com/jxq0/org-tidy/main/screenshot.png
;; (use-package org-modern
;;   ;; This is yet another high-quality package by Daniel Mendler which handles the
;;   ;; prettification of Org buffers in a performant way. Rather than write about
;;   ;; what the package does, it is better you check the GIF in the project's
;;   ;; README: https://github.com/minad/org-modern.
;;   :config
;;   (setopt org-modern-label-border 1
;;           org-modern-variable-pitch nil
;;           org-modern-timestamp t
;;           org-modern-table t
;;           org-modern-table-vertical 1
;;           org-modern-table-horizontal 0
;;           org-modern-list '((?+ . "")
;;                             (?- . "")
;;                             (?* . ""))
;;           org-modern-internal-target nil
;;           org-modern-radio-target nil)
;;   (add-hook 'org-mode-hook #'org-modern-mode)
;;   (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(use-package outline
  ;; The built-in `outline-minor-mode' defines folding and navigation commands
  ;; for the file's outline. The "outline" is the document's discernible
  ;; structure, defined by the local value of the variable `outline-regexp'.
  ;; Simply put, it is about the headings in the document.
  ;;
  ;; Any file can have its own outline. For example, in Emacs Lisp any comment
  ;; with three or more delimiters that starts at the beginning of the line
  ;; counts as a heading. Three delimiters make it a level 1 heading, four
  ;; delimiters for level 2, and so on. You will notice that I already use
  ;; outlines in all my files. Sometimes I enable the `outline-minor-mode',
  ;; though I do not really need the folding capabilities. Plus, I can navigate
  ;; the file using minibuffer completion among outline entries without enabling
  ;; `outline-minor-mode', courtesy of the command `consult-outline'.
  :config
  (add-hook 'prog-mode-hook #'outline-minor-mode)

  ;; Set our own ellipsis icon.
  (defvar +outline-display-table (make-display-table))
  (set-display-table-slot +outline-display-table 'selective-display
                          (vector (make-glyph-code ?⤵ 'escape-glyph)))
  (defun +outline-set-display-table ()
    (setf buffer-display-table +outline-display-table))
  (add-hook 'outline-minor-mode-hook '+outline-set-display-table)

  (setopt outline-minor-mode-highlight nil
          outline-minor-mode-cycle nil
          outline-minor-mode-use-buttons nil)

  (bind-keys :map outline-minor-mode-map
             ("M-RET" . outline-insert-heading)
             ("C-c C-n" . outline-next-visible-heading)
             ("C-c C-p" . outline-previous-visible-heading)
             :map +toggle-prefix-map
             ("o" . outline-minor-mode)))

;;;;;;;;;;;;;;;
;;;; notes ;;;;

(use-package denote
  :config
  ;; Denote is a simple note-taking tool for Emacs. It is based on the idea that
  ;; notes should follow a predictable and descriptive file-naming scheme. The
  ;; file name must offer a clear indication of what the note is about, without
  ;; reference to any other metadata. Denote basically streamlines the creation
  ;; of such files while providing facilities to link between them.
  ;;
  ;; Denote's file-naming scheme is not limited to "notes". It can be used for
  ;; all types of files, including those that are not editable in Emacs, such as
  ;; videos. Naming files in a consistent way makes their filtering and
  ;; retrieval easier. Denote provides facilities to rename files, regardless of
  ;; file type.

  (setopt denote-directory (expand-file-name "~/OneDrive/zettelkasten/")
          ;; If you want to have a "controlled vocabulary" of keywords, meaning
          ;; that you only use a predefined set of them, then you want
          ;; `denote-infer-keywords' set to nil, and `denote-known-keywords' to
          ;; have the keywords you need.
          denote-infer-keywords nil
          denote-sort-keywords t
          denote-known-keywords '("reference" "atlas" "literature" "evergreen")
          ;; Prompt for title, keywords, and signature in Denote commands that
          ;; prompt for user input to construct a Denote file name.
          denote-prompts '(title keywords signature))

  ;; Highlight Denote file names in Dired buffers.
  ;;
  ;; If you only want the `denote-dired-mode' in select directories, then modify
  ;; the variable `denote-dired-directories' and use
  ;; `denote-dired-mode-in-directories'.
  ;;
  ;; If you want the generic approach, which is great if you rename files
  ;; Denote-style in lots of different places, use `denote-dired-mode'.
  (setopt denote-dired-directories `(,(expand-file-name "~/OneDrive/zettelkasten"))
          denote-dired-directories-include-subdirectories t)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; Automatically rename Denote buffers when opening them so that instead of
  ;; their long file name they have the file's title and then the backlinks
  ;; indicator. Read the doc string of `denote-rename-buffer-format' for how to
  ;; modify this.
  (setopt denote-rename-buffer-format "%t%b")
  ;; Customize what the backlink indicator looks like.
  (setopt denote-buffer-has-backlinks-string " (<-->)")
  ;; `denote-rename-buffer-mode' provides the means to automatically rename the
  ;; buffer of a Denote file upon visiting the file.
  (denote-rename-buffer-mode 1)

  (bind-keys :map global-map
             ("C-c n" . +notes-prefix-map)
             :map +notes-prefix-map
             ("n" . denote)
             ("N" . denote-type)
             ("o" . denote-sort-dired) ; "order" mnemonic
             ;; Note that `denote-rename-file' can work from any context, not
             ;; just Dired buffers. That is why we bind it globally.
             ("r" . denote-rename-file)
             :map text-mode-map
             ("C-c n b" . denote-backlinks)
             ("C-c n i" . denote-link) ; "insert" mnemonic
             ("C-c n I" . denote-add-links)
             ("C-c n r" . denote-rename-file)
             ("C-c n R" . denote-rename-file-using-front-matter)
             :map org-mode-map
             ("C-c n d b" . denote-org-extras-dblock-insert-backlinks)
             ("C-c n d l" . denote-org-extras-dblock-insert-links)
             ("C-c n b" . denote-backlinks)
             ("C-c n i" . denote-link) ; "insert" mnemonic
             ("C-c n I" . denote-add-links)
             ("C-c n r" . denote-rename-file)
             ("C-c n R" . denote-rename-file-using-front-matter)
             :map dired-mode-map
             ("C-c n i" . denote-dired-link-marked-notes)
             ("C-c n r" . denote-dired-rename-marked-files)
             ("C-c n R" . denote-dired-rename-marked-files-using-front-matter)
             ("C-c n t" . denote-dired-rename-marked-files-with-keywords)))

;; With `denote-org', users have Org-specific extensions such as dynamic blocks,
;; links to headings, and splitting an Org subtree into its own standalone file.
;; (use-package denote-org)

(use-package consult-denote
  :config
  ;; This package is glue code to integrate `denote' with Daniel Mendler's
  ;; `consult' package. The idea is to enhance minibuffer interactions, such as
  ;; by providing a preview of the file-to-be-linked/opened and by adding more
  ;; sources to the `consult-buffer' command.
  (consult-denote-mode 1)

  ;; Disable auto preview of `consult-denote-buffer-source', shorten the name
  ;; to just "Denote", and use lowercase d as a prefix narrowing key.
  (consult-customize
   consult-denote-buffer-source
   :narrow ?d
   :preview-key '("M-." "C-M-n" "C-M-p")
   :hidden t
   :name "Denote")

  ;; By default `consult-denote-buffer-source' only shows buffers if their file
  ;; is an actual Denote note (see `denote-file-types'). I want it to show every
  ;; file that has a Denoted name, regardless of whether it is a note or not.
  ;; NOTE 2025-06-11 I might want to filter only actual Denote notes in the
  ;; future. In that case I can make a new source with the ?D key.
  (defun +consult-denote--buffers ()
    "Return file names of Denote buffers."
    (delq nil
          (mapcar
           (lambda (buffer)
             (when-let* ((file (buffer-file-name buffer))
                         ((buffer-live-p buffer))
                         ((denote-file-has-denoted-filename-p file)))
               (buffer-name buffer)))
           (buffer-list))))
  (consult-customize
   consult-denote-buffer-source
   :items #'+consult-denote--buffers)

  ;; Disable the subdirectory and silo sources.
  (consult-customize
   consult-denote-subdirectory-source
   :enabled (lambda () nil))
  ;; (with-eval-after-load 'denote-silo
  ;;   (consult-customize
  ;;    consult-denote-silo-source
  ;;    :enabled (lambda () nil)))

  ;; `consult-denote-find' only starts showing matches once
  ;; `consult-async-min-input' characters have been inserted. I want to see the
  ;; file names without having to type anything.
  (defun +denote-find-file ()
    (interactive)
    (let ((default-directory denote-directory))
      (call-interactively #'find-file)))

  (bind-keys :map +notes-prefix-map
             ("f" . +denote-find-file)
             ("g" . consult-denote-grep)
             :map +search-prefix-map
             ("M-n" . +denote-find-file)
             ("n" . consult-denote-grep)))

(use-package org-remark
  :disabled t
  ;; Simply saving, excerpting, or copying materials is not enough; information
  ;; needs to be processed to be transformed into useful knowledge. The reason is
  ;; that merely transporting material only increases the amount of information
  ;; without reprocessing it.

  ;; The Zettelkasten method emphasizes summarizing/reviewing in your own words
  ;; and establishing connections, providing multiple opportunities for
  ;; information processing. However, many introductions to the Zettelkasten
  ;; method often get caught up in the craze of double-linking, falling into the
  ;; trap of merely saving data-- essentially ignoring the method Niklas Luhmann
  ;; used to handle a large amount of literature notes.
  ;;
  ;; Luhmann had a habit of taking literature notes while reading. Each literature
  ;; was essentially an index of the material. He only excerpted the original text
  ;; from the book when absolutely necessary. Literature notes are an efficient
  ;; and in-depth method that records key points and inspirations, facilitating
  ;; quick review and deep reading, while also helping distinguish between
  ;; existing and new information.
  :config
  ;; `org-remark' allows us to highlight and annotate any text file. It can
  ;; automatically create a literature note for a given text file. We can select
  ;; any text and highlight it, which applies an overlay with a a user-defined
  ;; text face through its custom highlighter pens facility. The highlight and
  ;; any associated notes are kept in an Org file functioning as a plain text
  ;; database. This lets us easily manage our marginal notes and use the
  ;; built-in Org facilities on them. The entries in this file simply save the
  ;; locations of our highlighted text. We can automatically load the highlights
  ;; from previous sessions, and we can display the marginal notes for the
  ;; highlight at point.
  ;;
  ;; These marginal notes are external to the source document, leveraging all
  ;; the power of Org while acting like notes that are made inside of the
  ;; document. They are an incredibly efficient way of taking literature notes
  ;; while reading any text document.

  ;; These minor modes lets us highlight and annotate Info documentation,
  ;; websites, and EPUB books just like text files.
  (use-package org-remark-info :after info :config (org-remark-info-mode +1))
  (use-package org-remark-eww :after eww :config (org-remark-eww-mode +1))
  (use-package org-remark-nov :after nov :config (org-remark-nov-mode +1))

  ;; Automatically turn on highlights after re-starting Emacs. Without this
  ;; global minor mode we would need to remember to activate `org-remark-mode'
  ;; for each file where we add highlights and annotations, which is often
  ;; impractical.
  (org-remark-global-tracking-mode)

  ;; Don't tell me that a buffer has no highlights. Too noisy.
  (setopt org-remark-report-no-highlights nil)

  ;; Heading titles can be longer than the default of 40.
  (setopt org-remark-line-heading-title-max-length 70)

  ;; Create a Denote-compatible marginal note
  (defun +org-remark-denote--note (filename)
    "Find the Denote filename similar to FILENAME but with the 'literature' keyword."
    (let* ((is-file (and filename (file-exists-p filename)))
           (source-title (if is-file
                             (denote-retrieve-filename-title filename)
                           filename))
           (source-signature (when is-file
                               (denote-retrieve-filename-signature filename)))
           (source-keywords (when is-file
                              (let ((kw (denote-retrieve-filename-keywords filename)))
                                (when kw (split-string kw "_")))))
           (buffer-files (cl-remove nil (mapcar #'buffer-file-name (buffer-list))))
           (files (cl-union (denote-directory-files) buffer-files)))
      (cl-find-if
       (lambda (file)
         (let* ((file-title (denote-retrieve-filename-title file))
                (file-signature (denote-retrieve-filename-signature file))
                (file-keywords
                 (let ((kw (denote-retrieve-filename-keywords file)))
                   (when kw (split-string kw "_")))))
           (and (string= file-title source-title)
                (member "literature" file-keywords)
                (if source-signature
                    (string= file-signature source-signature)
                  t)
                (if (and source-keywords file-keywords)
                    (seq-set-equal-p
                     (seq-remove (lambda (elt) (member elt '("literature" "reference")))
                                 source-keywords)
                     (seq-remove (lambda (elt) (member elt '("literature" "reference")))
                                 file-keywords))
                  t))))
       files)))

  (defun +org-remark-denote-file-name-function ()
    "Return a Denote-compatible file name for the current buffer.

When the current buffer is visiting a file, the name of the
marginal notes file will be \"DATE==SIGNATURE--TITLE__literature.org\"
in your `denote-directory'."
    ;; TODO: find a cleaner way to override the
    ;; `org-remark-source-find-file-name-functions' that get added by the
    ;; different `org-remark-*' minor modes instead of having a cond clause for
    ;; source-filename
    (let* ((source-filename (cond ((eq major-mode 'nov-mode)
                                   nov-file-name)
                                  ((eq major-mode 'pdf-view-mode)
                                   buffer-file-name)
                                  ((eq major-mode 'Info-mode)
                                   (concat
                                    "info-"
                                    (denote-sluggify
                                     'title
                                     (file-name-nondirectory
                                      Info-current-file))))
                                  ((and (eq major-mode 'eww-mode)
                                        (stringp (plist-get eww-data :url)))
                                   (let* ((url (plist-get eww-data :url))
                                          (is-url-denote (denote-file-has-denoted-filename-p url))
                                          (filename (or (denote-retrieve-filename-title url)
                                                        url)))
                                     (+eww-denote-slug-hyphenate filename)))
                                  (t
                                   (org-remark-source-find-file-name))))
           (is-source-denote (if source-filename
                                 (denote-file-has-denoted-filename-p source-filename)
                               nil)))
      (let ((literature-note (+org-remark-denote--note source-filename)))
        (cond ((not source-filename) "marginalia.org")
              (literature-note literature-note)
              (t
               (denote-format-file-name
                (denote-directory)
                (denote-generate-identifier-as-date nil (current-time))
                (if-let* (is-source-denote
                          (denote-keywords (denote-retrieve-filename-keywords source-filename)))
                    (remove
                     "reference"
                     (append (split-string denote-keywords "_") '("literature")))
                  '("literature"))
                (if-let* (is-source-denote
                          (denote-title (denote-retrieve-filename-title source-filename)))
                    denote-title
                  (file-name-sans-extension (file-name-nondirectory source-filename)))
                (or denote-file-type ".org")
                (if-let* (is-source-denote
                          (denote-signature (denote-retrieve-filename-signature source-filename)))
                    denote-signature
                  "")))))))

  (setopt org-remark-notes-file-name #'+org-remark-denote-file-name-function)

  (defun +org-remark-denote-prepend-front-matter ()
    "Insert Denote front matter into empty org-remark marginalia files."
    (interactive)
    (when-let ((file-name buffer-file-name))
      (when (and (denote-file-has-denoted-filename-p file-name)
                 (member "literature"
                         (denote-extract-keywords-from-path file-name))
                 (= (point-min) (point-max)))
        (with-current-buffer (current-buffer)
          (denote-rename-file
           file-name
           (denote-retrieve-filename-title file-name)
           (denote-extract-keywords-from-path file-name)
           (denote-retrieve-filename-signature file-name)
           (denote-retrieve-filename-identifier file-name))
          (save-excursion
            (goto-char (point-max))
            (while (and (not (bobp))
                        (looking-back "\n" 1))
              (delete-char -1)))
          (insert
           (format
            "\n#+reference: %s\n\n"
            (replace-regexp-in-string
             "=" "_"
             (denote-retrieve-filename-signature file-name))))))))

  (bind-keys
   :map org-remark-mode-map
   ("M-n" . org-remark-view-next)
   ("M-p" . org-remark-view-prev)
   ("C-c m d" . org-remark-delete)
   ("C-c m l" . org-remark-mark-line)
   ("C-c m m" . org-remark-mark)
   ("C-c m o" . org-remark-open)
   ("C-c m r" . org-remark-remove)
   ("C-c m v" . org-remark-view)))

;; TODO i really wish i could fully replace org-noter with org-remark because of
;; all the annoying bugs
(use-package org-noter
  :init
  ;; i don't have djvu-read-mode but org-noter keeps trying to load in org-noter-djvu.
  ;; another one in the long list of annoying bugs with this package...
  (require 'org-noter-core)
  (setopt org-noter-supported-modes
          (remove 'djvu-read-mode org-noter-supported-modes))
  :config
  ;; default value always gives an error and i can't be bothered to fix it
  (setopt org-noter--show-arrow-hook '())

  ;; heavy-handed way of preventing org-noter from modifying my mode-line
  ;; without my permission.
  (defun org-noter--mode-line-text () "")

  (setopt org-noter-always-create-frame nil
          org-noter-kill-frame-at-session-end nil
          org-noter-highlight-selected-text t)

  (bind-keys :map org-noter-doc-mode-map
             ("i" . org-noter-insert-precise-note)
             ("M-i" . org-noter-insert-note)
             ("M-n" . org-noter-sync-next-note)
             ("M-p" . org-noter-sync-prev-note)
             ("M-." . org-noter-sync-current-note)
             ("C-M-n" . org-noter-sync-next-page-or-chapter)
             ("C-M-p" . org-noter-sync-prev-page-or-chapter)
             ("C-M-." . org-noter-sync-current-page-or-chapter)))

;; NOTE Multi-Line Type Answer Box plugin?
;; NOTE Straight Reward plugin
(use-package anki-editor
  ;; Flashcards can be a very effective and useful tool. They trigger active
  ;; retrieval of knowledge, make spaced repetition convenient, and facilitate
  ;; micro-learning. They are only useful for lower-order learning like simple
  ;; memorisation, not higher-order learning.

  ;; `anki-editor' enables us to use all the Org constructs for writing Anki
  ;; notes, allowing us to maintain flashcards as part of our Org notes. Doing
  ;; so helps mitigate the risk of fragmented learning by making memorization
  ;; remain contextual rather than isolated. To accomplish this we need (i) an
  ;; `Anki' client, (ii) the `AnkiConnect' plugin which allows us to communicate
  ;; with Anki over a simple HTTP API, and (iii) `curl'.

  ;; An Anki note is represented as an Org entry with property
  ;; `:ANKI_NOTE_TYPE:'. Each subheading of a note entry corresponds to a field
  ;; in Anki. If one field is missing, for the given note type, `anki-editor'
  ;; will use the Org entry as that missing field. If no fields are given, it
  ;; will use the Org entry as the first field and the body below the heading as
  ;; the second field.

  ;; The destination Anki deck is provided by the `ANKI_DECK' property either at
  ;; the top of an Org file `#+ANKI_DECK: my-target-deck' or within the
  ;; properties drawer of an individual entry `:ANKI_DECK: my-target-deck'.

  ;; The `ANKI_NOTE_TYPE' property is used to specify the Anki note type and is
  ;; required.

  ;; Tags can be provided with a `ANKI_TAGS' property, where multiple tags are
  ;; separeted by spaces. Or with Org tags, which can be turned off if you would
  ;; like to keep Org tags separated from Anki.

  ;; The `ANKI_NOTE_ID' property is used to synchronize an Org entry and its
  ;; corresponding Anki note.

  ;; A note entry might look like this:
  ;;
  ;; * Example flashcard
  ;;   :PROPERTIES:
  ;;   :ANKI_DECK: English
  ;;   :ANKI_NOTE_TYPE: Basic (and reversed card)
  ;;   :END:
  ;;   This is the content of the flashcard.
  ;;
  ;; Or this:
  ;;
  ;; * Raining                                                      :vocab:idioms:
  ;;   :PROPERTIES:
  ;;   :ANKI_DECK: English
  ;;   :ANKI_NOTE_TYPE: Basic (and reversed card)
  ;;   :ANKI_TAGS: vocab idioms
  ;;   :END:
  ;; ** Front
  ;;    (it's) raining cats and dogs
  ;; ** Back
  ;;    it's raining very hard

  ;; Upon pushing to Anki with `anki-editor-push-notes' or
  ;; `+anki-editor-push-tree', `anki-editor' will export the Org tree as
  ;; Anki-compatible HTML. However, without a proper CSS file it will still look
  ;; plain. So we replace it with https://github.com/gongzhitaao/orgcss, a
  ;; simple sytlesheet for Org-exported HTML, to obtain a proper styling for
  ;; source code blocks, lists, tables, etc.

  :config
  ;; By default, `anki-editor-cloze-*' always asks for hints and requires card
  ;; number input. I don't use hints much, and usually want the card number to
  ;; increase so I provide two helper functions:
  ;; `+anki-editor-cloze-region-auto-incr' and
  ;; `+anki-editor-cloze-region-dont-incr'.
  (defun +anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hints and increase the card number."
    (interactive)
    (anki-editor-cloze-region +anki-editor--cloze-number "")
    (setq +anki-editor--cloze-number (1+ +anki-editor--cloze-number))
    (forward-sexp))

  (defun +anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hints using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- +anki-editor--cloze-number) "")
    (forward-sexp))

  ;; We need to initialize `+anki-editor--cloze-number'.
  (defun +anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1."
    (interactive)
    (setq +anki-editor--cloze-number (or arg 1)))

  ;; Reset the cloze number after each capture.
  (add-hook 'org-capture-after-finalize-hook #'+anki-editor-reset-cloze-number)

  ;; By default `anki-editor-push-notes' will push the whole file. This is slow
  ;; when the file contains many old entries that didn't really need to
  ;; change. `+anki-editor-push-tree' is provided for this purpose.
  (defun +anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes 'tree)
    (+anki-editor-reset-cloze-number))

  (bind-keys
   :map org-mode-map
   ("C-c a a" . anki-editor-insert-note)
   ("C-c a c" . +anki-editor-cloze-region-auto-incr)
   ("C-c a C" . +anki-editor-cloze-region-dont-incr)
   ("C-c a d" . anki-editor-delete-notes)
   ("C-c a p" . +anki-editor-push-tree)
   ("C-c a r" . +anki-editor-reset-cloze-number)))

;;;;;;;;;;;;;;;;;;;;;;
;;;; bibliography ;;;;

;; TODO move files in zettelkasten/reference/ back to zettelkasten
;; TODO better biblio keymap
;;   w for yanking (copy bibtex)
;;   SPC and S-SPC for scrolling
;;   y for insert bibtex
(use-package biblio
  :config
  ;; A bibliography provides pointers to stuff in the world outside of our
  ;; writings like books, articles, web pages, videos, etc. When we want to
  ;; reference these outside things, we attach an indicator which is used to look
  ;; up that reference in our bibliography. Think of the bibliography as a
  ;; database of links and information about those links. Now when we want to
  ;; associate a piece of writing to some external thing, we don't have to rely on
  ;; the physical form of that thing.
  ;;
  ;; Biblio provides facilities to browse and gather bibliographic references from
  ;; various well-curated sources, and formats them as BibTeX entries, the file
  ;; format of my bibliography. This is better than typing all entries manually
  ;; with the built-in BibTeX mode, which is inefficient and could easily lead to
  ;; errors. Simply run `biblio-lookup', select the source, and enter a search
  ;; query. Once the search has completed, a new buffer opens with the
  ;; results. You can then select your target entry and insert it into the buffer
  ;; you called biblio-lookup from, copy it and paste it later, or a perform a
  ;; number of other possible commands.
  (defvar +bibliography-files
    (list (concat (denote-directory) "reference/" "bibliography.bib"))
    "List of bibliography files.")

  (defvar +bibliography-directory
    (concat (denote-directory) "reference")
    ;; (concat
    ;;  (file-name-as-directory (getenv "HOME"))
    ;;  "OneDrive/zettelkasten/reference")
    "Location of BibTeX files and attachments.")

  (defun +biblio--combined-lookup ()
    "Combines `biblio-lookup' and `biblio-doi-insert-bibtex'."
    (let* ((dbs (biblio--named-backends))
           (db-list (append dbs '(("DOI" . biblio-doi-backend))))
           (db-selected (biblio-completing-read-alist
                         "Backend: "
                         db-list)))
      (if (eq db-selected 'biblio-doi-backend)
          (let ((doi (read-string "DOI: ")))
            (biblio-doi-insert-bibtex doi))
        (biblio-lookup db-selected))))
  (defun +biblio-lookup ()
    "Insert Biblio search results into the current buffer or selected
BibTeX file."
    (interactive)
    (if-let ((current-mode major-mode)
             +bibliography-files
             (bibfiles (length +bibliography-files))
             (bibfile (cond ((eq bibfiles 1)
                             (car +bibliography-files))
                            ((equal major-mode 'bibtex-mode)
                             (buffer-file-name))
                            (t
                             (completing-read
                              "Select BibTeX file: " +bibliography-files)))))
        (progn
          (find-file bibfile)
          (goto-char (point-max))
          (+biblio--combined-lookup)
          (save-buffer))
      (message "No BibTeX file(s) defined.")))

  (defun +bibtex-extract-attachments ()
    "Extract attachment file names from BibTeX files in `+bibliography-directory'."
    (let ((attachments '()))
      (dolist (bibtex-file +bibliography-files)
        (with-temp-buffer
          (insert-file-contents bibtex-file)
          (goto-char (point-min))
          (while (re-search-forward "^[[:space:]]*file[[:space:]]*=[[:space:]\n\r]*[{\"]\\([^}\"]+\\)[}\"]" nil t)
            (let ((file-paths (split-string (match-string 1)
                                            "[[:space:]]*;[[:space:]]*")))
              (dolist (file-path file-paths)
                (message "Attachment: %s" (expand-file-name (string-trim file-path
                                                                         +bibliography-directory)))
                (push (expand-file-name (string-trim file-path)
                                        +bibliography-directory)
                      attachments))))))
      attachments))

  (defun +bibtex-extract-files ()
    "List files recursively in `+bibliography-directory', excluding `.bib' and
`.csl'."
    (seq-remove
     (lambda (file)
       (or (string-suffix-p ".bib" file)
           (string-suffix-p ".csl" file)))
     (mapcar 'expand-file-name
             (directory-files-recursively +bibliography-directory ""))))

  (defun +bibtex-missing-files ()
    "List BibTeX attachments not listed in a BibTeX file entry."
    (interactive)
    (let* ((files (+bibtex-extract-files))
           (attachments (+bibtex-extract-attachments))
           (missing (cl-remove-if
                     (lambda (f) (member f attachments)) files)))
      (message "%s files not registered in bibliography" (length missing))
      (dolist (file missing)
        (message file))))

  (defun +bibtex-missing-attachments ()
    "List BibTeX file entries with missing attachment(s)."
    (interactive)
    (let* ((files (+bibtex-extract-files))
           (attachments (+bibtex-extract-attachments))
           (missing (cl-remove-if
                     (lambda (f) (member f files)) attachments)))
      (message "%s BibTeX files without matching attachment." (length missing))
      (dolist (file missing)
        (message file))))

  (bind-keys
   :map +bib-prefix-map
   ;; One minor inconvenience is that you must jump to the relevant bibliography
   ;; file before inserting a new entry, and it provides two seperate search
   ;; functions (`biblio-lookup' and `biblio-doi-insert-bibtex'). We write a
   ;; function that prompts for a BibTeX file to insert into, and combines the
   ;; two search functions.
   ("l" . +biblio-lookup)))

;; `biblio-openlibrary' provides a backend for `biblio' that supports queries
;; based on ISBN using OpenLibrary's Read API. The API does allow queries based
;; on a multitude of identifiers, but this package only allows and expects
;; queries based on an ISBN idetifier because biblio already provides
;; functionality for the other more common use cases.
(use-package biblio-openlibrary :disabled t)

;; `biblio-gbooks' provides a backend for `biblio' that supports queries using
;; Google's Books API. While the existing biblio backends provide good coverage
;; of peer-reviewed scientific articles, they don't have good coverage of
;; fiction and non-fiction books.
(use-package biblio-gbooks :disabled t)

;; mostly for ebib-add-file-entry to scaffold bibtex-entries with the file key already set up
;; (use-package ebib)

(use-package citar
  :config
  ;; Citar provides a highly configurable `completing-read' front-end to browse
  ;; and act on bibliographic data. It is a reference manager of sorts because
  ;; it is the tool I use to access and manage my bibliography. It has support
  ;; for cross-referenced entries, completion-at-point, bibliographic notes,
  ;; attachments, navigating to the source bibliography file, and contextual
  ;; Embark actions. It also integrates with `org-cite', Org mode's citation
  ;; module.

  (with-eval-after-load 'embark
    ;; `citar-embark-mode' adds contextual Embark actions in the minibuffer and
    ;; with org-at-point. The actions are generic and work the same across Org,
    ;; Markdown, and LaTeX modes.
    (citar-embark-mode 1)
    ;; I prefer to have the Embark menu open with `org-open-at-point'.
    (setopt citar-at-point-function 'embark-act))

  (add-hook 'org-mode-hook #'citar-capf-setup)

  ;; Configure the formatting for the sections in the completing-read UI.
  (setopt citar-templates
          '((main . "${author editor:30%sn}    ${date year issued:4}    ${title:80}")
            (suffix . "${=key= id:15}    ${=type=:15}    ${tags keywords keywords:*}")
            (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
            (note . "#+title: Notes on ${author editor}, ${title}")))

  (setopt citar-select-multiple nil
          org-cite-global-bibliography +bibliography-files
          org-cite-insert-processor 'citar
          org-cite-follow-processor 'citar
          org-cite-activate-processor 'citar
          citar-bibliography org-cite-global-bibliography
          citar-library-paths (list (concat (denote-directory) "reference/"))
          citar-notes-paths (list (denote-directory)))

  (bind-keys :map +bib-prefix-map
             ("f" . citar-open) ; "find" mnemonic
             :map org-mode-map
             ("C-c i" . org-cite-insert)))

(use-package citar-denote
  :config
  ;; `citar-denote' makes it possible to write notes on BibTeX entries with the
  ;; help of the `citar' package. These notes have the citation's unique key
  ;; associated with them in the file's front matter. They also get a
  ;; configurable keyword in their file name (`citar-denote-keyword'), making it
  ;; easy to find them in Dired and/or retrieve them with the various Denote
  ;; methods.
  (citar-denote-mode)

  (setopt
   ;; Allow multiple notes per bibliographic entry.
   citar-open-always-create-notes nil
   ;; Change the default keyword for bibliographic notes. I'm using these like
   ;; the literature notes in my zettelkasten.
   citar-denote-keyword "literature"
   ;; Use citation key as signature when creating new notes.
   citar-denote-signature 'citekey)

  (bind-keys
   :map +bib-prefix-map
   ;; Adds citation keys or converts existing Denote file to a bibliographic
   ;; note. When converting a regular Denote file, adds the
   ;; `citar-denote-keyword' to the front matter and renames the file
   ;; accordingly.
   ("t" . citar-denote-add-citekey)
   ;; Remove citation keys. When no more reference items are left, the
   ;; `citar-denote-keyword' is removed and the file is renamed.
   ("T" . citar-denote-remove-citekey)))

;;;;;;;;;;;;;;;
;;;; timer ;;;;

(use-package tmr
  :config
  ;; With `tmr' we can set timers using a convenient notation. The point of
  ;; entry is the `tmr' command (or `tmr-with-details' if you want to describe
  ;; what the timer is about).
  ;;
  ;; Set a timer by specifying one of these:
  ;;
  ;; Input  Meaning
  ;; 5      5 minutes from now
  ;; 5m     Same as above
  ;; 1h     1 hour from now
  ;; 06:35  From now until 06:35
  ;;
  ;; To view the running timers in a tabulated list, invoke the command
  ;; `tmr-tabulated-view'. From there, type `C-h m' (or M-x `describe-mode') to
  ;; learn about all the available commands and their respective key bindings.
  (bind-keys :map global-map
             ("C-c t" . tmr-prefix-map)))

;;;;;;;;;;;;;;;
;;;; email ;;;;

;; TODO consult-notmuch <https://codeberg.org/jao/consult-notmuch>
;; (use-package consult-notmuch)

(use-package ol-notmuch
  :disabled t
  ;; It can be useful to include links to e-mail messages or search queries in
  ;; your Org files. `ol-notmuch' supports this. In simple terms, this package
  ;; provides glue code between Notmuch and Org capture that allows me to create
  ;; links that point to e-mails. When I follow the link, it opens in a fully
  ;; fledged Notmuch buffer. This is how I build up my agenda of appointments.
  ;; It highlights the power of Emacs' interconnectedness, as I go from my
  ;; e-mail to the agenda, to editing, file management, and related.

  ;; You can use the normal Org mechanisms to store links: `org-store-link'
  ;; stores a link to a particular message when you are in `notmuch-show-mode'
  ;; or `notmuch-tree-mode'. When you are in `notmuch-search-mode', it stores a
  ;; link to the query. Note that queries are performed at the time the link is
  ;; opened, so the result may be different from when the link was stored.

  ;; You can insert this link later with `org-insert-link'. From org-mode, you
  ;; can go to the query or message the link points to with either
  ;; `org-agenda-open-link' in agenda buffers, or `org-open-at-point' elsewhere
  ;; - both typically bound to 'C-c C-o'.

  ;; You can add some specific capture-template for this. In your capture
  ;; templates, the following notmuch-specific template expansion values are
  ;; available:
  ;;
  ;; %:date, %:date-timestamp (TODO), %:date-timestamp-inactive (TODO)
  ;; %:from, %:fromname (TODO), %:fromaddress (TODO)
  ;; %:to, %:toname (TODO), %:toaddress (TODO)
  ;; %:maildir (TODO)
  ;; %:message-id
  ;; %:path (TODO)
  ;; %:subject

  ;; Remember, if you define your own link types, any property you store with
  ;; `org-link-store-props' can be accessed in capture templates in a similar
  ;; way.
  :after notmuch)

;;;;;;;;;;;;;;;
;;;; media ;;;;

;;;;;;;;;;;;;;;;;
;;;; browser ;;;;

(use-package browse-url
  ;; The built-in `browse-url' package makes it possible to open a web page from
  ;; inside Emacs. Where that web page is opened depends either on the specific
  ;; command used to pick a web browser or on the value of the user option
  ;; `browse-url-browser-function'. In the latter case, Emacs will use whichever
  ;; browser the user specifies (well, technically, it will call the given
  ;; function which is responsible for setting up the browser).
  ;;
  ;; I generally prefer to open links inside of Emacs using the EWW browser. It
  ;; can render HTML well, although it does not handle CSS and JavaScript. If
  ;; the page cannot be displayed properly because it depends on some JavaScript
  ;; functionality, then I use the command `eww-browse-with-external-browser',
  ;; which is bound to `&' by default, to open the page in whichever browser I
  ;; have specified in the `browse-url-secondary-browser-function'.
  :config
  (defun +browse-url-purified-handler (fn)
    "Clear the redundant stuff from urls.
Some links are prefixed with google redirection url, this
removes (and other similar stuff) so that the url handler works
properly."
    (lambda (url &rest _)
      (funcall
       fn
       (s-chop-prefix "https://www.google.com/url?q=" url))))

  (setopt browse-url-browser-function 'eww-browse-url
          browse-url-secondary-browser-function 'browse-url-default-browser
          browse-url-handlers `((;; (".*\\.mp4"
                                 ;;  . (lambda (link &rest _) (empv-play-or-enqueue link)))
                                 ".*\\.mp4"
                                 . ,(+browse-url-purified-handler #'mpv-play-url))
                                (".*\\(youtube.com/watch.*\\|youtu.be/.*\\)"
                                 . ,(+browse-url-purified-handler #'mpv-play-url))
                                ("."
                                 . eww-browse-url))))

(use-package goto-addr
  ;; The built-in `goto-addr' is used to turn any plain text web URL into a
  ;; clickable button. All we need is to enable the `goto-addr-mode' (or its
  ;; programming variant `goto-address-prog-mode'). I personally do not use this
  ;; directly, though there are other packages that may request the
  ;; functionality. As such, I define the configurations I prefer to have, which
  ;; are strictly stylistic.
  :config
  (add-hook 'text-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (setopt ;; goto-address-mail-face nil
   goto-address-mail-mouse-face 'highlight))

(use-package shr
  ;; Emacs can render HTML and is thus capable of displaying web pages and
  ;; richly formatted emails. The core functionality is handled by the built-in
  ;; Simple HTML Renderer (`shr'). Other tools are available to navigate web
  ;; pages:
  ;;
  ;; - `browse-url' to follow links
  ;; - `goto-addr' to buttonize links
  ;; - `eww' for a web browser
  ;;
  ;; What Emacs cannot do as of this writing (2025-02-03) is parse CSS and run
  ;; JavaScript. This means that modern web pages will generally not work as
  ;; expected or not load any contents at all. This should not be a problem for
  ;; HTML email, but it makes much of the World Wide Web unusable.
  ;;
  ;; For me, this limitation is not a real problem as the stuff I read is fine
  ;; without all the bells and whistles of CSS and JavaScript. What I have here
  ;; are the settings to make HTML look the way I prefer. The two most important
  ;; variables in this regard are `shr-use-colors' and `shr-use-fonts' because
  ;; (i) I want web pages to always use my theme colours instead of hardcoding
  ;; their own and (ii) I do not want SHR to render text in `variable-pitch'
  ;; because for me the right way to opt in to this feature is by enabling
  ;; `variable-pitch-mode' in the given buffer (as I do, for example, via the
  ;; `text-mode-hook').

  :config
  ;; Imenu support for shr modes
  ;; From https://github.com/oantolin/emacs-config/blob/master/my-lisp/shr-heading.el
  (defun +shr-heading-next (&optional arg)
    "Move forward by ARG headings (any h1-h5).
If ARG is negative move backwards, ARG defaults to 1."
    (interactive "p")
    (unless arg (setq arg 1))
    (catch 'return
      (dotimes (_ (abs arg))
        (when (> arg 0) (end-of-line))
        (if-let ((match
                  (funcall (if (> arg 0)
                               #'text-property-search-forward
                             #'text-property-search-backward)
                           'face '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5)
                           (lambda (tags face)
                             (cl-loop for x in (if (consp face) face (list face))
                                      thereis (memq x tags))))))
            (goto-char
             (if (> arg 0) (prop-match-beginning match) (prop-match-end match)))
          (throw 'return nil))
        (when (< arg 0) (beginning-of-line)))
      (beginning-of-line)
      (point)))

  (defun +shr-heading-previous (&optional arg)
    "Move backward by ARG headings (any h1-h5).
If ARG is negative move forwards instead, ARG defaults to 1."
    (interactive "p")
    (+shr-heading-next (- (or arg 1))))
  (defun +shr-heading--line-at-point ()
    "Return the current line."
    (concat
     (and-let* ((faces (ensure-list (get-char-property (point) 'face)))
                (level (cl-find-if (lambda (f) (string-match "shr-h.$" (symbol-name f)))
                                   faces))
                (indent (- (aref (symbol-name level) 5) 49)))
       (make-string indent ? ))
     (buffer-substring (line-beginning-position) (line-end-position))))

  (defun +shr-heading-setup-imenu ()
    "Setup Imenu for h1-h5 headings in eww buffer.
Add this function to appropriate major mode hooks such as
`eww-mode-hook' or `elfeed-show-mode-hook'."
    (setq-local
     imenu-prev-index-position-function #'+shr-heading-previous
     imenu-extract-index-name-function  #'+shr-heading--line-at-point))

  (setopt shr-use-colors nil ; t is bad for accessibility
          shr-use-fonts nil ; t is superfluous, given `variable-pitch-mode'
          shr-max-image-proportion 0.6
          shr-image-animate nil ; No GIFs, thank you!
          shr-width fill-column
          shr-max-width fill-column
          shr-discard-aria-hidden t
          ;; shr-fill-text nil ; TODO should i use `visual-line-mode' for shr?
          shr-cookie-policy nil))

;; TODO document shr-tag-pre-highlight
(use-package shr-tag-pre-highlight
  :config
  ;; Add indentation, background color, and #+BEGIN_SRC lang blocks.
  (defun +shr-tag-pre-highlight (pre)
    "Highlighting code in PRE."
    (let* ((shr-folding-mode 'none)
           (shr-current-font 'default)
           (light (eq (frame-parameter nil 'background-mode) 'light))
           (code (with-temp-buffer
                   (shr-generic pre)
                   ;; (indent-rigidly (point-min) (point-max) 2)
                   (buffer-string)))
           (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang
                      (shr-tag-pre-highlight--get-lang-mode lang))))
      (shr-ensure-newline)
      (shr-ensure-newline)
      (setq start (point))
      (insert
       (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shr-tag-pre-highlight-fontify code mode)))
           code)
       (propertize "#+END_SRC" 'face 'org-block-end-line ))
      (shr-ensure-newline)
      (setq end (point))
      (if light
          (add-face-text-property start end '(:background "#D8DEE9" :extend t))
        (add-face-text-property start end '(:background "#292b2e" :extend t)))
      (shr-ensure-newline)
      (insert "\n")))

  (add-to-list 'shr-external-rendering-functions '(pre . +shr-tag-pre-highlight)))

(use-package url-cookie
  ;; The built-in `url-cookie' provides functionality to handle cookies in web
  ;; pages. For example, we can use the command `url-cookie-list' to produce a
  ;; listing of all the cookies that are set by the current web
  ;; page. Personally, I prefer to reject all cookies when I browse the web
  ;; through Emacs, so I set the variable `url-cookie-untrusted-urls' to not
  ;; trust any page. Whatever we set here will affect the behaviour of `eww',
  ;; though not of external browsers.
  :config
  (setopt url-cookie-untrusted-urls '(".*")))

;; TODO merge eww and elpher history into one list so back and other history
;; commands work accross both browsers
(use-package eww
  ;; The built-in `eww', i.e. Emacs Web Browser, is a text-centric web browser
  ;; that renders an HTML file in an Emacs buffer. It essentially is a set of
  ;; helpful commands to navigate the web while loading each page with `shr'.
  ;;
  ;; Because `shr' only parses HTML code without CSS or JavaScript, `eww' is
  ;; thus limited to what effectively is a simple, textual representation of the
  ;; web page. This is not good enough for websites that provide rich
  ;; interactivity, though it works reasonably well for blogs or pages with
  ;; text-heavy content.
  ;;
  ;; I use `eww' inside Emacs to quickly visit links to pages that are likely
  ;; okay in an HTML-only presentation. If the page does not behave properly,
  ;; then I type `&' (`eww-browse-with-external-browser') to use my "secondary"
  ;; browser, which should be a fully fledged application like Firefox.
  :config

  (defun +eww-buffer-name ()
    (when-let ((string (or (plist-get eww-data :title)
                           (plist-get eww-data :url))))
      (when (not (string-blank-p string))
        (format "%s" (truncate-string-to-width
                      string eww-buffer-name-length)))))

  ;; The `+shell-command-with-exit-code-and-output' function is
  ;; courtesy of Harold Carr, who also sent a patch that improved
  ;; `+eww-download-html'
  ;;
  ;; More about Harold: <http://haroldcarr.com/about/>.
  (defun +shell-command-with-exit-code-and-output (command &rest args)
    "Run COMMAND with ARGS.
Return the exit code and output in a list."
    (with-temp-buffer
      (list (apply 'call-process command nil (current-buffer) nil args)
            (buffer-string))))

  (defun +eww-denote-slug-hyphenate (str)
    "Replace spaces, underscores, slashes, dots, and colons with hyphens in
STR.
Also replace multiple hyphens with a single one and remove any
leading and trailing hyphen."
    (replace-regexp-in-string
     "^-\\|-$" ""
     (replace-regexp-in-string
      "-\\{2,\\}" "-"
      (replace-regexp-in-string "[/_:.?#=&%[:space:]]+" "-" str))))

  (defun +web-archive-download-html (url)
    "Download URL as a web page to `eww-download-directory'."
    (interactive (list (read-string "URL: "
                                    (or (get-text-property (point) 'shr-url)
                                        (eww-current-url)
                                        ""))))
    (let* ((dir (if (stringp eww-download-directory)
                    eww-download-directory
                  (funcall eww-download-directory)))
           (_ (access-file dir "Download failed"))
           (name (+eww-denote-slug-hyphenate url))
           (filepath (expand-file-name
                      (denote-format-file-name
                       dir (denote-get-identifier (current-time))
                       '("reference") name ".html" "")))
           ;; TODO extract download command (defcustom +eww-download-command)
           ;; (out (+shell-command-with-exit-code-and-output
           ;;       "wget" "-q" (format "%s" (plist-get eww-data :url))
           ;;       "-O" (format "%s" (shell-quote-argument path))))
           ;; (out (+shell-command-with-exit-code-and-output
           ;;       "monolith" url "-o" path))
           (out (+shell-command-with-exit-code-and-output
                 "single-file" "--browser-executable-path" "chromium"
                 url filepath)))
      (if (= (car out) 0)
          (progn
            (message "Downloaded page at: %s" filepath)
            filepath)
        (error "Download failed: %s" (cdr out)))))

  (defun +web-archive-bookmark-handler (bookmark)
    "Open the archived page saved in BOOKMARK."
    (eww-open-file (bookmark-prop-get bookmark 'filename)))

  (defun +web-archive-url (&optional url bookmark-name)
    "Archive URL in `eww-download-directory' and save it as a bookmark.
Use link at point if there is one, otherwise the current page's URL."
    (interactive)
    (let* ((url (or url (read-string "URL: "
                                     (or (get-text-property (point) 'shr-url)
                                         (eww-current-url)
                                         ""))))
           (title (cond ((derived-mode-p 'eww-mode)
                         (plist-get eww-data :title))
                        (t url)))
           (bookmark-name (read-string "Bookmark name: " title)))
      (bookmark-store bookmark-name
                      `((filename . ,(+web-archive-download-html url))
                        (handler . ,#'+web-archive-bookmark-handler)
                        (location . ,url)
                        (last-modified . ,(current-time))
                        (type . web-archive))
                      nil)))

  (with-eval-after-load 'consult
    (add-to-list 'consult-bookmark-narrow
                 '(?a "Web archive" +web-archive-bookmark-handler
                   ;; any other handlers under web archive
                   )
                 t))

  (defvar +eww--occur-feed-regexp
    (concat "\\(rss\\|atom\\)\\+xml.\\(.\\|\n\\)"
            ".*href=[\"']\\(.*?\\)[\"']")
    "Regular expression to match web feeds in HTML source.")

  (defun +eww-find-feed ()
    "Produce bespoke buffer with RSS/Atom links from XML source."
    (interactive)
    (let* ((url (or (plist-get eww-data :start)
                    (plist-get eww-data :contents)
                    (plist-get eww-data :home)
                    (plist-get eww-data :url)))
           (title (or (plist-get eww-data :title) url))
           (source (plist-get eww-data :source))
           (buf-name (format "feeds: %s" title)))
      (with-temp-buffer
        (insert source)
        (occur-1 +eww--occur-feed-regexp "\\3" (list (current-buffer)) buf-name))
      ;; Handle relative URLs, so that we get an absolute URL out of them.
      ;; Findings like "rss.xml" are not particularly helpful.
      ;;
      ;; NOTE 2021-03-31: the base-url heuristic may not always be
      ;; correct, though it has worked in all cases I have tested it on.
      (when (get-buffer buf-name)
        (with-current-buffer (get-buffer buf-name)
          (let ((inhibit-read-only t)
                (base-url (replace-regexp-in-string "\\(.*/\\)[^/]+\\'" "\\1" url)))
            (goto-char (point-min))
            (unless (re-search-forward +common-url-regexp nil t)
              (re-search-forward ".*")
              (replace-match (concat base-url "\\&"))))))))

  ;; Consult support
  (with-eval-after-load 'consult
    (defvar +consult--source-eww-buffer
      `( :name "EWW"
         :narrow   ?w
         :category buffer
         :state    ,#'consult--buffer-state
         :hidden t
         :items    ,(lambda ()
                      (mapcar #'buffer-name
                       (seq-filter (lambda (buf)
                                     (with-current-buffer buf
                                      (derived-mode-p 'eww-mode)))
                        (buffer-list))))))
    (add-to-list 'consult-buffer-sources '+consult--source-eww-buffer :append))

  (defun +consult-history-eww ()
    "Fetch URL from `eww-history' and render the page."
    (interactive)
    (let* ((history (mapcar (lambda (entry)
                              (let* ((title (or (plist-get entry :title) ""))
                                     (url (plist-get entry :url))
                                     (display (format "%s # %s" title url)))
                                (cons display url)))
                            eww-history))
           (url (cdr (assoc (consult--read (mapcar #'car history)
                                           :prompt "History: "
                                           :require-match t)
                            history))))
      (eww url)))

  (defun +consult-bookmark-eww ()
    "Call `consult-bookmark' pre-narrowed to EWW bookmarks."
    (interactive)
    (let ((unread-command-events (list ?w ?\s)))
      (call-interactively #'consult-bookmark)))

  ;; Use minibuffer completion to select a URL among those linked from the
  ;; current web page. Then visit that URL using `eww'. With an optional
  ;; universal prefix argument (`C-u' with default key bindings), visit the
  ;; selected URL in a new buffer.

  (defun +eww--get-urls ()
    "Get all links in the current buffer."
    (let ((links nil))
      (save-excursion
        (goto-char (point-min))
        (while (text-property-search-forward 'face 'shr-link)
          (when-let* ((position (point))
                      (button (button-at position)))
            (push
             (list position
                   (button-label button)
                   (shr-url-at-point nil))
             links))))
      (nreverse links)))

  ;; BUG Not even sure this annotation function is working as intended.
  (defun +eww--annotate-with-url (candidate)
    "Annotate CANDIDATE with its URL for display in completion."
    (when-let* ((link-data (+eww--get-urls))
                (entry (seq-find
                        (pcase-lambda (`(,pos ,name ,url))
                          (string= candidate (format "%s	%s" name url)))
                        link-data))
                (url (nth 2 entry))
                (url (nth 2 entry)))
      (concat "  # " url)))

  (defun +eww-buffer-url-prompt ()
    "Prompt for a URL in the current buffer."
    (when-let* ((completion-extra-properties `(:annotation-function
                                               ,#'+eww--annotate-with-url))
                (link-data (+eww--get-urls))
                (candidates (mapcar
                             (pcase-lambda (`(,position ,name ,url ,_))
                               (cons (format "%s	%s" name url) position))
                             link-data))
                (table (+common-completion-table-no-sort nil (mapcar #'car candidates)))
                (selection
                 (completing-read
                  (format-prompt "Select link in the current page" nil)
                  table))
                (index (cdr (assoc selection candidates))))
      (assoc index link-data)))

  (defun +eww-follow-link-on-page (&optional new-buffer)
    "Follow URL among those in the current buffer using completion.
With optional NEW-BUFFER as a prefix argument, visit the URL in a new buffer
instead of the current one."
    (interactive "P" eww-mode)
    (unless (derived-mode-p 'eww-mode)
      (user-error "This command only works in an EWW buffer"))
    (if-let* ((data (+eww-buffer-url-prompt))
              (url (nth 2 data)))
        (eww url new-buffer)
      (error "Cannot find URL in data `%s'" data)))

  (defun +eww-jump-to-link-on-page ()
    "Go to the position of a URL among those in the current buffer."
    (interactive nil eww-mode)
    (unless (derived-mode-p 'eww-mode)
      (user-error "This command only works in an EWW buffer"))
    (if-let* ((data (+eww-buffer-url-prompt))
              (position (car data)))
        (goto-char position)
      (error "Cannot position in data `%s'" data)))

  ;; Imenu support for `eww'.
  (add-hook 'eww-mode-hook '+shr-heading-setup-imenu)

  ;;; Automatically activate `eww-readable' on certain URLs
  (defvar +eww-auto-readable-urls
    (regexp-opt '("guardian.co.uk" "theguardian.com" "github.com"
                  "eldiario.es")))
  (defun +eww-autoread ()
    (when (string-match-p +eww-auto-readable-urls (or (eww-current-url)))
      (eww-readable)))
  (add-hook 'eww-after-render-hook #'+eww-autoread)

  (setopt eww-auto-rename-buffer '+eww-buffer-name
          eww-header-line-format ""
          eww-history-limit 150
          eww-download-directory (concat denote-directory "reference/"))

  (bind-keys
   :map +search-prefix-map
   ("M-w" . eww-search-words)
   :map eww-mode-map
   ("<" . beginning-of-buffer)
   (">" . end-of-buffer)
   ("a" . bookmark-set) ; TODO link at point or current page
   ("b" . +buffers-major-mode) ; overrides `eww-add-bookmark'
   ("B" . +consult-bookmark-eww) ; overrides `eww-list-bookmarks'
   ("d" . +web-archive-url)
   ("F" . +eww-follow-link-on-page)
   ("g" . eww-reload)
   ("C-c C-r" . +consult-history-eww)
   ("H" . +consult-history-eww) ; unmap `eww-list-histories'
   ("l" . eww-back-url)
   ("M" . +eww-jump-to-link-on-page)
   ("o" . eww)
   ;; Copy text from HTML page for pasting into org-mode. HTML hyperlinks get
   ;; converted to [[link][desc]]
   ;; ("O" . org-eww-copy-for-org-mode)
   ("r" . eww-forward-url)
   ("t" . eww-top-url)
   ("u" . eww-up-url)
   ("w" . eww-copy-page-url)
   ("W" . +eww-find-feed)
   ("s" . eww-search-words) ; overrides `eww-switch-to-buffer'
   ("S" . nil) ; unmap `eww-list-buffers'
   ("V" . +eww-view-archive)
   ("S-RET" . eww-open-in-new-buffer)
   :map eww-link-keymap
   ("v" . nil) ; stop overriding `eww-view-source'
   :map html-mode-map
   ("C-c C-e" . browse-url-of-file)))

;; TODO document elpher
(use-package elpher
  :config
  ;; Imenu support for `elpher'.
  (add-hook 'eww-mode-hook '+shr-heading-setup-imenu)

  (setopt elpher-default-url-type "gemini")

  (bind-keys
   :map elpher-mode-map
   ("<" . beginning-of-buffer)
   (">" . end-of-buffer)
   ;; ("a" . +elpher-bookmark-set) ; overrides `elpher-bookmark-link' link at point or current page
   ("b" . +buffers-major-mode)
   ;; ("B" . +consult-bookmark-elpher) ; overrides `elpher-open-bookmarks' (narrow elpher G)
   ;; ("d" . +elpher-download) ; link at point or current page
   ;; ("f" . link-hint-open-link) ; or `elpher-jump' if no link-hint
   ("g" . elpher-reload) ; overrides `elpher-go'
   ("l" . elpher-back)
   ;; ("L" . +elpher-list-histories)
   ("m" . elpher-jump)
   ("n" . elpher-next-link)
   ("o" . elpher-go-current)
   ("p" . elpher-prev-link)
   ;; ("r" . +elpher-forward)
   ("R" . nil) ; unmap `elpher-reload'
   ("s" . nil) ; unmap `elpher-show-history'
   ("S" . nil) ; unmap `elpher-show-visited-pages'
   ("t" . elpher-root-dir)
   ("v" . elpher-view-raw)
   ;; ("w" . +elpher-copy-page-url) ; link at point or current page
   ("<tab>" . elpher-next-link)
   ("<backtab>" . elpher-prev-link)))

;;;;;;;;;;;;
;;;; ai ;;;;

;; TODO document gptel
(use-package gptel
  :config

  (setopt gptel-directives
          `((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications.  Speak in specific,
   topic relevant terminology.  Do NOT hedge or qualify.  Speak directly and be willing to make creative guesses.

  Explain your reasoning.  if you don’t know, say you don’t know.  Be willing to reference less reputable sources for
   ideas.

  Do NOT summarize your answers.

  If you use LaTex notation, enclose math in \\( and \\), or \\[ and \\] delimiters.

   Never apologize.  Ask questions when unsure.")
            (tutor . "You are a tutor and domain expert in the domain of my questions.  You will lead me to discover the answer myself by providing hints.  Your instructions are as follows:
  - If the question or notation is not clear to you, ask for clarifying details.
  - At first your hints should be general and vague.
  - If I fail to make progress, provide more explicit hints.
  - Never provide the answer itself unless I explicitly ask you to.  If my answer is wrong, again provide only hints to correct it.
  - If you use LaTeX notation, enclose math in \\( and \\) or \\[ and \\] delimiters.")
            (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.  Do NOT use markdown backticks (```) to format your response.")
            (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, without any markdown code fences.")
            (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
            (explain . "Explain what this code does to a novice programmer.")))

  ;; TODO add karthink's gptel-rewrite-commit-message rewrite directive
  ;; <https://github.com/karthink/.emacs.d/blob/25a0aec771c38e340789d7c304f3e39ff23aee3e/init.el#L4570>

  (defvar gptel--ollama
    (gptel-make-ollama "Ollama"
      :host "localhost:11434"
      :stream t
      :models '(deepseek-r1:1.5b deepscaler qwen3:1.7b
                gemma3:1b llama3.2:1b
                (qwen2.5vl:3b :description "Qwen 2.5vl: Vision capable model"
                 :capabilities (media)
                 :mime-types (image/jpeg image/png)))))

  ;; TODO replace visual-line-mode in gptel-mode with visual-fill-column-mode
  (add-hook 'gptel-mode-hook #'visual-line-mode)

  (setq gptel--system-message (alist-get 'default gptel-directives))
  (setopt gptel-default-mode 'org-mode
          gptel-include-reasoning nil
          gptel-model 'qwen3:1.7b
          gptel-backend gptel--ollama
          gptel-display-buffer-action '(pop-to-buffer-same-window))

  (bind-keys :map global-map
             ("C-c <return>" . gptel-send)
             ("C-c C-<return>" . gptel-menu)
             ("C-c C-g" . gptel-abort)
             :map embark-region-map
             ("+" . gptel-add)
             :map gptel-mode-map
             ("M-a" . gptel-beginning-of-response)
             ("M-e" . gptel-end-of-response)))

(use-package gptel-quick
  :config
  (setopt gptel-quick-backend gptel-backend
          ;; Reasoning models don't work well with `gptel-quick'
          gptel-quick-model 'gemma3:1b)
  (bind-keys :map embark-general-map
             ("?" . gptel-quick)))

;;;;;;;;;;;;;
;;;; irc ;;;;

(use-package rcirc
  ;; IRC is a communication protocol for live text messaging. The typical
  ;; use-case involves multiple users writing to the same "channel". Messages
  ;; appear in chronological order, with reference to the username who wrote
  ;; them. Users can direct a message to one another by means of writing `@'
  ;; followed by the username. Users can join multiple channels at
  ;; once. Channels have a `#' prefix to their name. There can also be
  ;; one-to-one exchanges, which behave like every messaging app. It is all
  ;; plain text and it just works!

  ;; IRC is not like a modern chat application though because it does not
  ;; persist messages by default. Users must make a special arrangement for
  ;; that, which I never bothered to learn. My usage is super simple: log in
  ;; during some special events, such as EmacsConf, and only follow what is
  ;; happening live. Otherwise, I do not bother with IRC.

  ;; Emacs comes with two built-in IRC clients: `erc' and `rcirc'. `erc' has
  ;; lots of features and I would consider it better for power users. While
  ;; `rcirc' is more minimal and thus easier for casual IRC users such as
  ;; myself.

  ;; In the following code block, the essential data to streamline the log in
  ;; process is `rcirc-server-alist'. What I have there uses the function
  ;; `+auth-get-field' to extract a password from my `.authinfo.gpg' file. It
  ;; then uses the `rcirc-default-nick' to establish a connection. The only
  ;; channel I join automatically whenever I do `M-x rcirc' is that of
  ;; `#emacs'. Again, I am not into IRC.
  :config
  (setopt rcirc-server-alist `(("irc.libera.chat"
                                :channels ("#emacs")
                                :port 6697
                                :encryption tls))
          rcirc-prompt "%t> "
          rcirc-default-nick "svitax"
          rcirc-default-user-name rcirc-default-nick)

  (rcirc-track-minor-mode 1))

;; TODO document erc
(use-package erc
  :disabled t
  :config
  (with-eval-after-load 'consult
    (defvar +erc-buffer-source
      `( :name "ERC"
         :hidden t
         :narrow ?I
         :category buffer
         :action ,#'switch-to-buffer
         :state ,'consult--buffer-state
         :items ,(lambda () (mapcar 'buffer-name (erc-buffer-list))))
      "Source for `consult-buffer' to list ERC buffers.")

    (add-to-list 'consult-buffer-sources '+erc-buffer-source :append))

  (defun +erc-close-buffers ()
    "Close all ERC buffers."
    (interactive)
    (mapc 'kill-buffer (erc-buffer-list nil erc-server-process)))

  (setopt erc-autojoin-channels-alist '()
          erc-join-buffer 'buffer
          erc-header-line-format " %n on %t (%m,%l)"
          erc-hide-list '("NICK" "JOIN" "PART" "QUIT" "MODE" "AWAY")
          erc-track-exclude-types '("324" "329" "332" "333" "353" "477" "JOIN" "MODE" "NICK" "PART" "QUIT")
          erc-hide-prompt t
          erc-hide-timestamps t
          erc-echo-timestamps nil
          erc-kill-buffer-on-part t
          erc-kill-server-buffer-on-quit t
          erc-kill-queries-on-quit t
          erc-timestamp-format "%H:%M"
          erc-log-insert-log-on-open t
          erc-log-channels-directory (expand-file-name "emacs/erc-logs" (xdg-cache-home))
          erc-fill-function 'erc-fill-static
          erc-fill-static-center 14
          erc-fill-column 82)

  (bind-keys
   :map erc-mode-map
   ("C-c C-q" . +erc-close-buffers)))

(use-package erc-hl-nicks :disabled t)

;;;;;;;;;;;;;;
;;;; guix ;;;;

;;;;;;;;;;;;;;;
;;;; tools ;;;;

;; (use-package password-store)

;; (use-package tmr)

;; TODO document leetcode
;;
;; Remember you need to login to LeetCode in your browser first before the
;; my_cookies utility works.
(use-package leetcode
  :disabled t
  :config
  ;; I am unable to (nor do I necessarily want to) install a Python package
  ;; globally beacuse I use GNU Guix. Ideally I want to package the my_cookies
  ;; Python dependency for Guix, but until then the Python community has
  ;; developed a way of isolating installation requirements through a
  ;; virtual-environment.  Let's allow my_cookies to be installed in an isolated
  ;; Python virtual-environment.
  (defun leetcode--install-my-cookie ()
    "Install leetcode dependencies."
    (let ((async-shell-command-display-buffer t))
      (async-shell-command
       (format "python3 -m venv --clear %s && %s/bin/pip3 install my_cookies" leetcode-python-environment leetcode-python-environment)
       (get-buffer-create "*leetcode-install*"))))

  (defun leetcode--my-cookies-path ()
    "Find the path to the my_cookies executable."
    (or (executable-find (format "%s/bin/my_cookies" leetcode-python-environment))
        (executable-find "my_cookies")))

  (defun leetcode--check-deps ()
    "Check if all dependencies installed."
    (if (leetcode--my-cookies-path)
        t
      (leetcode--install-my-cookie)
      nil))

  (defcustom leetcode-python-environment (file-name-concat user-emacs-directory "leetcode-env")
    "The path to the isolated python virtual-environment to use."
    :group 'leetcode
    :type 'directory)

  (defun leetcode--cookie-get-all ()
    "Get leetcode session with `my_cookies'. You can install it with pip."
    (let* ((my-cookies (leetcode--my-cookies-path))
           (my-cookies-output (shell-command-to-string (leetcode--my-cookies-path)))
           (cookies-list (seq-filter (lambda (s) (not (string-empty-p s)))
                                     (s-split "\n" my-cookies-output 'OMIT-NULLS)))
           (cookies-pairs (seq-map (lambda (s) (s-split-up-to " " s 1 'OMIT-NULLS)) cookies-list)))
      cookies-pairs))

  ;; Add the abillity to specify the LeetCode session cookie manually instead of
  ;; needing the my_cookies Python utility.
  (defun leetcode--check-deps ()
    "Check if all dependencies installed."
    (if (or (leetcode--my-cookies-path)
            leetcode-session-cookie)
        t
      (leetcode--install-my-cookie)
      nil))

  (defcustom leetcode-session-cookie nil
    "LeetCode session cookie."
    :group 'leetcode
    :type 'string)

  (defun leetcode--local-cookie-get ()
    "Gets locally set session cookie."
    (when-let ((my-cookie leetcode-session-cookie))
      `((,leetcode--cookie-session ,my-cookie))))

  (aio-defun leetcode--login ()
             "We are not login actually, we are retrieving LeetCode login session
from local browser. It also cleans LeetCode cookies in `url-cookie-file'."
             (ignore-errors (url-cookie-delete-cookies leetcode--domain))
             (let* ((leetcode-cookie (or (leetcode--local-cookie-get)
                                         (leetcode--cookie-get-all))))
               (cl-loop for (key value) in leetcode-cookie
                        do (url-cookie-store key value nil leetcode--domain "/" t)))
             ;; After login, we should have our user data already.
             (message "LeetCode fetching user data...")
             (aio-await (leetcode--fetch-user-status)))

  ;; (load-library (expand-file-name "secrets.el.gpg" user-emacs-directory)
  ;; (setopt leetcode-session-cookie my/leetcode-session-cookie)

  (setopt leetcode-prefer-language "golang")

  (bind-keys
   :map leetcode-solution-mode-map
   ("C-c C-c" . leetcode-submit)
   ("C-c C-k" . leetcode-quit)
   ("C-c C-r" . leetcode-restore-layout)
   ("C-c C-t" . leetcode-try)))

;;;;;;;;;;;;;;;
;;;; icons ;;;;

;; I define a user option to conditionally load icons in various parts of the
;; Emacs interface. These are purely cosmetic. I prefer less noise in my buffers,
;; but they help communicate more information which is sometimes helpful.

;; TODO a minor mode that activates and deactivates icons

(defcustom +emacs-load-icons t
  "When non-nil, enable iconography in various contexts.
This installs and uses the `nerd-icons' package and its variants.
NOTE that you still need to invoke `nerd-icons-install-fonts'
manually to first get the icon files."
  :group '+emacs
  :type 'boolean)

(use-package nerd-icons
  :if +emacs-load-icons
  :config
  ;; Show icons for files in the Magit status and other buffers
  (with-eval-after-load 'magit
    (setopt magit-format-file-function #'magit-format-file-nerd-icons))

  (+alist-set "lock" '(nerd-icons-faicon "nf-fa-lock" :face
                       nerd-icons-orange)
              'nerd-icons-extension-icon-alist)

  ;; This fixes the missing Nix icon when using `consult-buffer'
  (with-eval-after-load 'nix-ts-mode
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(nix-mode nerd-icons-devicon "nf-dev-nixos" :face
                   nerd-icons-blue))
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(nix-ts-mode nerd-icons-devicon "nf-dev-nixos" :face
                   nerd-icons-blue)))

  ;; This fixes the missing JSON icon when using `consult-buffer'
  (with-eval-after-load 'json-ts-mode
    (dolist (ext '("json" "jsonl"))
      (+alist-set ext '(nerd-icons-codicon "nf-cod-json" :face
                        nerd-icons-yellow)
                  'nerd-icons-extension-icon-alist))

    (dolist (mode '(js-json-mode json-ts-mode))
      (add-to-list 'nerd-icons-mode-icon-alist
                   `(,mode nerd-icons-codicon "nf-cod-json" :face
                     nerd-icons-yellow))))

  ;; This fixes the missing EPUB icon when using `consult-buffer'
  (with-eval-after-load 'nov
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(nov-mode nerd-icons-mdicon "nf-md-book_open" :face
                   nerd-icons-green)))

  ;; This fixes the missing .bib icon when using `consult-buffer'
  (with-eval-after-load 'bibtex
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(bibtex-mode nerd-icons-mdicon "nf-md-book"
                   :face nerd-icons-lblue)))

  (with-eval-after-load 'mistty
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(mistty-mode nerd-icons-devicon "nf-dev-terminal"
                   :face nerd-icons-green)))

  (with-eval-after-load 'ess
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(ess-r-mode nerd-icons-devicon "nf-dev-r"
                   :face nerd-icons-blue))
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(inferior-ess-mode nerd-icons-devicon "nf-dev-terminal"
                   :face nerd-icons-blue)))

  (with-eval-after-load 'citar
    (defvar citar-indicator-files-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon "nf-fa-file_text_o" :face 'nerd-icons-dorange)
       :function #'citar-has-files :padding "  " :tag "has:files"))

    (defvar citar-indicator-links-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon "nf-fa-link" :face 'nerd-icons-dred)
       :function #'citar-has-links :padding "  " :tag "has:links"))

    (defvar citar-indicator-notes-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon "nf-fa-pencil_square_o" :face 'nerd-icons-dgreen)
       :function #'citar-has-notes :padding "  " :tag "has:notes"))

    (defvar citar-indicator-cited-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon "nf-fa-comment_dots" :face 'nerd-icons-dyellow)
       :function #'citar-is-cited :padding "  " :tag "is:cited"))

    (setq citar-indicators
          (list citar-indicator-files-icons ; citar-indicator-links-icons
                citar-indicator-notes-icons ; citar-indicator-cited-icons
                ))))

(use-package nerd-icons-completion
  :if +emacs-load-icons
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

  ;; By default, icons are shown in all sorts of completion prompts. When those
  ;; have different kinds of candidates, like files in `find-file' and buffers
  ;; in `consult-buffer', the icons are super helpful. If all the candidates
  ;; have the same icon though, it becomes too noisy so I prefer not to see any
  ;; icon.
  (setopt nerd-icons-completion-category-icons nil))

(use-package nerd-icons-corfu
  :if +emacs-load-icons
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :if +emacs-load-icons
  :config
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :if +emacs-load-icons
  :config
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode))

(use-package nerd-icons-grep
  :if +emacs-load-icons
  :config
  (when grep-use-headings
    (nerd-icons-grep-mode)))

(use-package compile-multi-nerd-icons
  :config
  (setq compile-multi-nerd-icons-alist
        '((make nerd-icons-devicon "nf-dev-gnu" :face nerd-icons-red-alt)
          (nix nerd-icons-devicon "nf-dev-nixos" :face nerd-icons-blue-alt))))
