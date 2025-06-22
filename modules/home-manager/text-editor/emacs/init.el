;;; init.el --- This is my init. -*- lexical-binding: t; -*-

;;; Commentary:

;; init is where my Emacs config starts.

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
    (setq native-compile-prune-cache t))

  ;; I like starting with a scratch buffer. I know that a lot of users specify a
  ;; dashboard or an Org agenda view, but I prefer to keep things generic in
  ;; this regard.
  (setopt initial-buffer-choice t
          initial-major-mode 'lisp-interaction-mode
          initial-scratch-message
          (format ";; This is `%s'. Use `%s' to evaluate and print results.\n\n"
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
             ("l" . +bib-prefix-map) ; "lib" mnemonic ; ("C-l" . ) ; downcase-region
             ("m" . +mail-prefix-map) ; ("C-m" . ) ; mule-keymap
             ("n" . +notes-prefix-map) ; ("C-n" . next-buffer) ; set-goal-column
             ;; ("o" . ) ; other-window ("C-o" . guix) ; delete-blank-lines ; "os" mnemonic
             ("p" . +project-prefix-map) ; ("C-p" . previous-buffer) ; mark-page
             ("q" . kbd-macro-query) ("C-q" . read-only-mode)
             ("r" . +registers-prefix-map) ("C-r" . find-file-read-only)
             ("s" . save-some-buffers) ("C-s" . save-buffer)
             ("t" . +tab-prefix-map) ("C-t" . transpose-lines)
             ;; ("u" . ) ; undo ("C-u" . vundo) ; upcase-region ; "undo" mnemonic?
             ;; ("v" . magit-status) ; vc-prefix-map ("C-v" . find-alternate-file)
             ("w" . +window-prefix-map) ("C-w" . write-file)
             ("x" . +toggle-prefix-map) ("C-x" . exchange-point-and-mark)
             ("y" . +dap-prefix-map) ; ("C-y" . ) ; "why" mnemonic
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

;; I'm a longtime Vim user. I just prefer modal editing. I almost got the
;; default Emacs bindings to be as comfortable as Vim-style editing with
;; home-row mods on my keyboard, but nothing beats plain old Vim bindings.
;; Yes, Evil is heavy and does not integrate nicely with Emacs. Yes, it needs a
;; lot of extra work to make them work everywhere. But I'm ok with that.
;;
;; NOTE I'm currently trying out home-row mods again, without the super key so I
;; can hold "n" and "i". Maybe this time it will stick.
;;
;; TODO maybe use phisearch for / and ?
(use-package evil
  :disabled t
  :config
  ;; The "basic" state
  (defvar +evil-basic-tag " <BA> "
    "Mode line tag for the +evil-basic state.")
  (defvar +evil-basic-message "-- BASIC --"
    "Echo area message when entering the basic state.")
  (evil-define-state basic
                     "Basic Vim keys to work in most (?) read-only major modes."
                     :tag '+evil-basic-tag
                     :message '+evil-basic-message)
  (evil-define-key 'basic global-map
                   "gy" 'evil-beginning-of-line
                   "ge" 'evil-end-of-line
                   "y" 'evil-backward-char
                   "h" 'evil-next-line
                   "a" 'evil-previous-line
                   "e" 'evil-forward-char
                   "i" 'evil-insert
                   "v" 'evil-visual-char
                   "V" 'evil-visual-line)
  (setq +evil-basic-state-modes
        '(completion-list-mode
          compilation-mode
          Buffer-menu-mode
          Custom-mode
          edebug-mode
          Info-mode
          help-mode
          diff-mode
          ediff-mode
          log-view-mode
          org-agenda-mode
          dired-mode
          magit-status-mode
          magit-diff-mode
          magit-log-mode
          notmuch-hello-mode
          notmuch-search-mode
          notmuch-show-mode
          notmuch-tree-mode
          special-mode
          tabulated-list-mode
          world-clock-mode))
  (defun +evil-need-basic-p ()
    "Return non-nil if the basic state should be used."
    (or buffer-read-only
        (memq major-mode +evil-basic-state-modes)))
  (defun +evil-normal-or-basic-state ()
    "Return to normal or basic state per `+evil-need-basic-p'."
    (interactive)
    (if (+evil-need-basic-p)
        (evil-basic-state)
      (evil-force-normal-state)))

  (dolist (mode +evil-basic-state-modes)
    (evil-set-initial-state mode 'basic))

  ;; Make sure some modes start out in the Emacs state.
  (setq +evil-emacs-state-modes
        '(comint-mode
          rcirc-mode
          eshell-mode
          inferior-emacs-lisp-mode
          reb-mode
          shell-mode
          term-mode
          wdired-mode
          log-edit-mode
          git-commit-mode))

  (dolist (mode +evil-emacs-state-modes)
    (evil-set-initial-state mode 'emacs))

  ;; Shift commands
  (defun +evil-shift-left (&optional beg end)
    "Left shift the region and keep it highlighted.
The region is between BEG and END, if it is active. Otherwise, it is retrieved
interactively with regular Evil motions."
    (interactive "r")
    (if (region-active-p)
        (progn
          (evil-shift-left beg end)
          (evil-active-region 1))
      (call-interactively #'evil-shift-left)))
  (defun +evil-shift-right (&optional beg end)
    "Right shift the region and keep it highlighted.
The region is between BEG and END, if it is active. Otherwise, it is retrieved
interactively with regular Evil motions."
    (interactive "r")
    (if (region-active-p)
        (progn
          (evil-shift-right beg end)
          (evil-active-region 1))
      (call-interactively #'evil-shift-right)))

  ;; TODO find a good key to put +evil-erase operator
  ;; "erase" operator
  (evil-define-operator +evil-erase (beg end type &rest _)
                        "Erase text from BEG to END with TYPE.
Unlike the delete operator, do not store the erased text anywhere."
                        (interactive "<R><x><y>")
                        (when (and (memq type '(inclusive exclusive))
                                   (not (evil-visual-state-p))
                                   (eq '+evil-erase evil-this-operator)
                                   (save-excursion (goto-char beg) (bolp))
                                   (save-excursion (goto-char end) (eolp))
                                   (<= 1 (evil-count-lines beg end)))
                          ;; Imitate Vi strangeness: if motion meets above criteria, delete
                          ;; linewise. Not for change operator or visual state.
                          (let ((new-range (evil-line-expand beg end)))
                            (setq beg (car new-range)
                                  end (cadr new-range)
                                  type 'line)))
                        (cond
                         ((eq type 'block)
                          (evil-apply-on-block #'delete-region beg end nil))
                         ((and (eq type 'line)
                               (= end (point-max))
                               (or (= beg end)
                                   (/= (char-before end) ?\n))
                               (/= beg (point-min))
                               (= (char-before beg) ?\n))
                          (delete-region (1- beg) end))
                         (t (delete-region beg end)))
                        (when (and (eq type 'line)
                                   (called-interactively-p 'any))
                          (evil-first-non-blank)
                          (when (and (not evil-start-of-line)
                                     evil-operator-start-col
                                     ;; Special exceptions to ever saving column:
                                     (not (memq evil-this-motion '(evil-forward-word-begin
                                                                   evil-forward-WORD-begin))))
                            (move-to-column evil-operator-start-col))))

  ;; Minibuffer
  (defconst +evil-minibuffer-maps '(minibuffer-local-map
                                    minibuffer-local-ns-map
                                    minibuffer-local-completion-map
                                    minibuffer-local-must-match-map
                                    minibuffer-local-isearch-map
                                    evil-ex-completion-map))
  (defun +evil-minibuffer-insert ()
    "Switch to insert state.

This function is meant to be hooked in the minibuffer:

    (add-hook \='minibuffer-setup-hook \='+evil-minibuffer-insert)

`evil-set-initial-state' can not be used for the minibuffer since it does not
have a mode."
    (set (make-local-variable 'evil-echo-state) nil)
    ;; (evil-set-initial-state 'mode 'insert) is the evil-proper way to do this,
    ;; but the minibuffer doesn't have a mode.
    ;; The alternative is to create a minibuffer mode (here), but then it may
    ;; conflict with other packages' if they do the same.
    (evil-insert 1))
  (evil-define-operator +evil-change-in-minibuffer
                        (beg end type register yank-handler delete-func)
                        "A version of `evil-change' that won't insert a new line on buffers without one."
                        (interactive "<R><x><y>")
                        ;; If there was no new line before the change, there should be none after.
                        ;; Delete any new line that might have been inserted and ignore an error if
                        ;; one wasn't.
                        (let ((new-inserted (and (eq type 'line) (/= ?\n (char-before end)))))
                          (evil-change beg end type register yank-handler delete-func)
                          (when new-inserted (ignore-errors (delete-char 1)))))
  (defun +evil-minibuffer-setup ()
    "Initialize minibuffer for `evil'."
    (dolist (map +evil-minibuffer-maps)
      (evil-define-key 'normal map "c" '+evil-change-in-minibuffer)
      (evil-define-key 'normal map (kbd "<escape>") 'abort-recursive-edit)
      (evil-define-key 'normal map (kbd "RET") 'exit-minibuffer))

    (add-hook 'minibuffer-setup-hook '+evil-minibuffer-insert)
    ;; Because of the above minibuffer-setup-hook, some evil-ex bindings need to
    ;; be reset.
    (evil-define-key 'normal 'evil-ex-completion-map
                     (kbd "<escape>") 'abort-recursive-edit)
    (evil-define-key 'insert 'evil-ex-completion-map
                     (kbd "C-p") 'previous-complete-history-element)
    (evil-define-key 'insert 'evil-ex-completion-map
                     (kbd "C-n") 'next-complete-history-element)
    (evil-define-key 'normal 'evil-ex-completion-map
                     (kbd "C-p") 'previous-history-element)
    (evil-define-key 'normal 'evil-ex-completion-map
                     (kbd "C-n") 'next-history-element))
  (add-hook 'minibuffer-setup-hook #'+evil-minibuffer-setup)

  ;; TODO +evil-visual-paste-no-kill doesn't work how i'd like it to
  ;; Do not pollute the kill-ring in visual state
  (defun +evil-visual-paste-no-kill (&rest args)
    "Do not add visual selection to the `kill-ring' while pasting.
Add as :around advice to `evil-paste-after' and `evil-paste-before', applying
its ARGS."
    (if (evil-visual-state-p)
        (cl-letf (((symbol-function 'evil-yank) #'ignore))
          (apply args)
          (setq evil-last-paste nil))
      (apply args)))
  ;; (advice-add #'evil-paste-after :around #'+evil-visual-paste-no-kill)
  ;; (advice-add #'evil-paste-before :around #'+evil-visual-paste-no-kill)

  ;; Make Emacs the insert state
  (defalias 'evil-insert-state 'evil-emacs-state)
  (evil-define-key 'emacs 'global
                   (kbd "<escape>") #'+evil-normal-or-basic-state)
  (setq evil-emacs-state-cursor evil-insert-state-cursor)

  ;; Setup my prefix keymap on SPC and C-x
  (defun +evil-prefix-or-self-insert ()
    "Self-insert key or return `+prefix-map'.
For use as a leader key in the Emacs/Insert evil state when the buffer is not
writeable."
    (interactive)
    (if (+evil-need-basic-p)
        (set-transient-map +prefix-map)
      (self-insert-command 1)))
  (evil-define-key '(emacs insert) global-map
                   (kbd "SPC") #'+evil-prefix-or-self-insert)
  (evil-define-key '(normal visual motion basic) global-map
                   (kbd "SPC") #'+prefix-map)

  (setopt evil-want-C-u-scroll t
          evil-want-Y-yank-to-eol t ; consistent with D
          evil-echo-state nil
          evil-want-fine-undo t
          evil-undo-system 'undo-redo ; Emacs 28
          ;; Evil search
          ;; I want the same Isearch experience as core Emacs, but with Vim
          ;; keys. This doesn't give exactly that, but it helps
          evil-symbol-word-search t
          evil-flash-delay 0.5
          evil-ex-hl-update-delay lazy-highlight-initial-delay
          evil-regexp-search nil
          evil-shift-width tab-width)

  (evil-define-key '(motion normal visual) 'global
                   "y" 'evil-backward-char
                   "Y" 'evil-window-top
                   "gy" 'evil-beginning-of-line
                   "h" 'evil-next-line
                   "gh" 'evil-next-visual-line
                   "a" 'evil-previous-line
                   "A" 'evil-lookup
                   "ga" 'evil-previous-visual-line
                   "e" 'evil-forward-char
                   "E" 'evil-window-bottom
                   "ge" 'evil-end-of-line
                   "U" 'evil-redo
                   (kbd "C-s") 'save-buffer
                   (kbd "C-r") 'isearch-backward
                   "gc" '+comment-dwim
                   "gd" 'xref-find-definitions
                   ">" '+evil-shift-right
                   "<" '+evil-shift-left
                   (kbd "<escape>") '+keyboard-quit-dwim
                   ":" 'execute-extended-command)
  (evil-define-key '(normal visual) 'global
                   "H" 'evil-join
                   "j" 'evil-append
                   "J" 'evil-append-line
                   "k" 'evil-yank
                   "K" 'evil-yank-line)
  (evil-define-key '(visual operator) 'global
                   "j" evil-outer-text-objects-map)
  (evil-define-key 'operator 'global
                   "a" 'evil-previous-line)

  (with-eval-after-load 'avy
    (evil-define-key '(normal motion visual) global-map
                     "s" 'avy-goto-char-timer))

  (with-eval-after-load 'golden-ratio-scroll
    (evil-define-key '(normal motion visual) global-map
                     (kbd "C-d") #'+golden-ratio-scroll-screen-down
                     (kbd "C-u") #'+golden-ratio-scroll-screen-up))

  (with-eval-after-load 'magit
    (evil-define-key '(visual basic) magit-status-mode-map
                     (kbd "K") #'magit-discard)
    (evil-define-key 'basic magit-status-mode-map
                     (kbd "l") #'magit-log)
    (evil-define-key 'visual magit-status-mode-map
                     (kbd "s") #'magit-stage
                     (kbd "u") #'magit-unstage))

  (with-eval-after-load 'org
    (evil-define-key '(normal visual motion) org-mode-map
                     (kbd "<tab>") #'org-cycle
                     (kbd "<return>") #'org-ctrl-c-ctrl-c))

  (with-eval-after-load 'nov
    (evil-define-key 'basic nov-mode-map
                     (kbd "C-f") #'nov-scroll-up
                     (kbd "C-b") #'nov-scroll-down
                     (kbd "C-d") #'nov-scroll-up
                     (kbd "C-u") #'nov-scroll-down
                     "d" #'nov-scroll-up
                     "u" #'nov-scroll-down
                     "f" #'+ace-link-nov
                     "gy" 'evil-beginning-of-line
                     "ge" 'evil-end-of-line
                     "y" 'evil-backward-char
                     "h" 'evil-next-line
                     "a" 'evil-previous-line
                     "e" 'evil-forward-char
                     "v" 'evil-visual-char
                     "V" 'evil-visual-line))

  (with-eval-after-load 'help
    (evil-define-key 'basic help-mode-map
                     (kbd "C-i") 'help-go-forward
                     (kbd "C-o") 'help-go-back))

  (with-eval-after-load 'info
    (evil-define-key 'basic Info-mode-map
                     (kbd "C-f") #'Info-scroll-up
                     (kbd "C-b") #'Info-scroll-down
                     (kbd "C-d") #'Info-scroll-up
                     (kbd "C-u") #'Info-scroll-down))

  (with-eval-after-load 'compile
    (evil-define-key 'basic compilation-mode-map
                     "gr" 'recompile))

  ;; (evil-mode 1)
  )

(use-package meow
  :disabled t
  :config
  (defmacro +meow--call-negative (form)
    `(let ((current-prefix-arg -1))
      (call-interactively ,form)))

  (defun +meow-negative-find ()
    (interactive)
    (+meow--call-negative 'meow-find))

  (defun +meow-negative-till ()
    (interactive)
    (+meow--call-negative 'meow-till))


  (defun +meow-insert-at-cursor ()
    (interactive)
    (if meow--temp-normal
        (progn
          (message "Quit temporary normal mode")
          (meow--switch-state 'motion))
      (meow--cancel-selection)
      (meow--switch-state 'insert)))

  (defun +meow-inner-str () (interactive) (meow-inner-of-thing ?g))
  (defun +meow-bounds-str () (interactive) (meow-bounds-of-thing ?g))
  (defun +meow-paragraph () (interactive) (meow-inner-of-thing ?p))

  (meow-motion-overwrite-define-key
   '("h" . meow-next)
   '("a" . meow-prev)
   '("<escape>" . ignore))

  ;; (meow-normal-define-key
  ;;  ;; Mnemonic
  ;;  ;;; bldwz
  ;;  '("b" . meow-back-word) '("B" . meow-back-symbol)
  ;;  '("l" . ) '("L" . )
  ;;  '("d" . meow-kill) '("D" . )
  ;;  '("w" . meow-next-word) '("W" . meow-next-symbol)
  ;;  '("z" . view_mode) '("Z" . sticky_view_mode)

  ;;  '("gb" . goto_window_bottom) '("zb" . align_view_bottom)
  ;;  '("gl" . goto_last_line)
  ;;  '("gd" . meow-find-ref) '("zd" . delete_fold) '("md" . surround_delete)
  ;;  ;;; nrtsg
  ;;  '("n" . meow-search) '("N" . meow-pop-search)
  ;;  '("r" . meow-replace-char) '("R" . meow-replace)
  ;;  '("t" . meow-till) '("T" . +meow-till-prev) '("C-t" . meow-pop-marker)
  ;;  '("s" . select_regex) '("S" . split_selection) '("C-s" . save_selection)
  ;;  '("g" . goto_mode) '("G" . goto_line)

  ;;  '("gn" . goto_next_buffer)
  ;;  '("gr" . goto_reference) '("mr" . surround_replace)
  ;;  '("gt" . goto_window_top) '("zt" . align_view_top)
  ;;  '("gs" . meow-back-to-indentation) '("ms" . surround_add)
  ;;  '("gg" . goto_file_start)
  ;;  ;;; qxmcv
  ;;  '("q" . replay_macro) '("Q" . record_macro)
  ;;  '("x" . meow-line) '("X" . +meow-rline)
  ;;  '("m" . match_mode) '("M" . mark)
  ;;  '("c" . meow-change) '("C" . ) '("C-c" . toggle_comments)
  ;;  '("v" . select_mode) '("V" . )

  ;;  '("gx" . goto_type_definition)
  ;;  '("gm" . goto_window_center) '("zm" . align_view_center) '("mm" . match_brackets)
  ;;  '("gc" . comment) '("zc" . close_fold)
  ;;  ;;; 'fouj
  ;;  '("'" . select_mark) '("\"" . select_register)
  ;;  '("f" . meow-find) '("F" . +meow-find-prev)
  ;;  '("o" . meow-open-below) '("O" . meow-open-above) '("C-o" . meow-pop-to-mark)
  ;;  '("u" . meow-undo) '("U" . redo)
  ;;  '("j" . meow-append) '("J" . append_at_line_end)

  ;;  '("gf" . goto_file)
  ;;  '("go" . ) '("zo" . open_fold)
  ;;  ;;; yhaei
  ;;  '("y" . meow-left) '("Y" . )
  ;;  '("h" . meow-next) '("H" . join_selections) '("C-h" . harpoon_1)
  ;;  '("a" . meow-prev) '("A" . lookup) '("C-a" . harpoon_2)
  ;;  '("e" . meow-right) '("E" . ) '("C-e" . harpoon_3)
  ;;  '("i" . meow-insert) '("I" . insert_at_line_start) '("C-i" . meow-unpop-to-mark)

  ;;  '("gy" . beginning-of-line)
  ;;  '("ga" . avy) '("za" . toggle_fold) '("ma" . select_textobject_around)
  ;;  '("ge" . end-of-line)
  ;;  '("gi" . goto_implementation) '("zi" . ) '("mi" . select_textobject_inner)
  ;;  ;;; kp,.?
  ;;  '("k" . meow-save) '("K" . )
  ;;  '("p" . meow-yank) '("P" . meow-yank-pop)
  ;;  ;; NOTE: `,' cancels multicursor
  ;;  ;; i want to use , for reverse/flip selection and ; for repeat motion
  ;;  '("," . keep_primary_selection) '(";" . collapse_selection) '("C-," . )
  ;;  '("." . repeat_last_insert) '(":" . command_mode) '("C-." . )
  ;;  '("?" . ) '("!" . ) '("C-?" . )

  ;;  '("gp" . goto_previous_buffer)
  ;;  '("g." . goto_last_modification)
  ;;  ;;; others
  ;;  '("-" . negative-argument) '("+" . meow-universal-argument)
  ;;  '("/" . meow-visit) '("?" . +meow-rvisit)

  ;;  '("<esc>" . meow-cancel-selection)

  ;;  '("<prev>" . meow-page-up) '("<next>" . meow-page-down)

  ;;  '(")" . meow-forward-slurp) '("(" . meow-forward-barf)
  ;;  '("{" . meow-backward-slurp) '("}" . meow-backward-barf)

  ;;  '("SPC" . keypad)
  ;;  '("%" . select_all)
  ;;  '("*" . search_selection)
  ;;  '("<" . indent) '(">" . unindent)

  ;; '("=" . format_selections)
  ;; '("M-." . repeat_last_motion)
  ;; '("`" . switch_to_lowercase) '("~" . switch_case) '("M-`" . switch_to_uppercase)
  ;; '("a" . append_mode) '("A" . insert_at_line_end) '("C-a" . increment)
  ;; '("C" . copy_selection_on_next_line) '("M-c" . change_selection_noyank) '("M-C" . copy_selection_on_prev_line)
  ;; '("M-d" . delete_selection_noyank)
  ;; '("S" . split_selection) '("M-s" . split_selection_on_newline)
  ;; '("C-x" . decrement)
  ;; '("y" . yank)
  ;; '("M-," . remove_primary_selection)
  ;; '("M--" . merge_selections)
  ;; '("_" . trim_selections) '("M-_" . merge_consecutive_selections)
  ;; '("&" . align_selections)
  ;; '(";" . collapse_selection) '("M-;" . flip_selections)
  ;; '("M-:" . ensure_selections_forward)
  ;; '("|". shell_pipe) '("M-|" . shell_pipe_to)
  ;; '("!" . shell_insert_output) '("M-!" . shell_append_output)
  ;; '("$" . shell_keep_pipe)
  ;; '("(" . rotate_selections_backward) '(")" . rotate_selections_forward)
  )

(use-package helix
  :disabled t
  :config
  (setopt helix-normal-state-cursor '(box "#fec43f")
          helix-insert-state-cursor '(bar "#fec43f"))

  (add-hook 'shell-mode-hook (lambda () (helix-insert-state)))

  (bind-keys :map helix-normal-state-map
             ("y" . helix-backward-char) ("gy" . helix-first-non-blank)
             ("h" . helix-next-line) ("gh" . helix)
             ("a" . helix-previous-line)
             ("e" . helix-forward-char) ("ge" . helix-end-of-line)

             ("j" . helix-append)

             ("k" . helix-yank)
             ("H" . helix-join-line)

             ("M-s" . nil))

  (helix-mode))

;;;;;;;;;;;;;;;;
;;;; themes ;;;;

(use-package modus-themes
  :config
  (setopt modus-themes-common-palette-overrides
          `(;; With `modus-themes-preset-overrides-faint' the grays are toned
            ;; down, gray backgrounds are removed from some contexts, and almost
            ;; all accent colors are desaturated. It makes the themes less
            ;; attention-grabbing.
            ,@modus-themes-preset-overrides-faint))

  (setopt modus-operandi-palette-overrides
          `(;; (bg-mode-line-active bg-blue-intense)
            ;; (fg-mode-line-active fg-main)
            (bg-region bg-sage)))

  (setopt modus-vivendi-palette-overrides
          `(;; (fg-main "#ebebeb")
            ;; (bg-main "#1e1e1e")
            ;; (bg-mode-line-active bg-lavender)
            ;; (fg-mode-line-active fg-main)
            (cursor yellow-warmer)
            (bg-region bg-lavender)))

  (set-fringe-bitmap-face 'right-arrow 'shadow)

  ;; We use the `enable-theme-functions' hook to ensure that these values are
  ;; updated after we switch themes. This special hook available in Emacs 29+
  ;; works with anything that uses the basic `enable-theme' function.
  (defun +customize-theme-faces (&rest _)
    (modus-themes-with-colors
      (custom-set-faces
       ;; By default, the background of the `region' face extends from the end
       ;; of the line to the edge of the window. To limit it to the end of the
       ;; line, we need to override the face's `:extend' attribute.
       '(region ((t :extend nil)))
       ;; The `git-gutter' and `git-gutter-fr' packages default to drawing
       ;; bitmaps for the indicators they display (e.g. bitmap of a plus sign
       ;; for added lines). I replace these bitmaps with contiguous lines which
       ;; look nicer, but require a change to the foreground of the relevant
       ;; faces to yield the desired color combinations.
       `(git-gutter-fr:added ((,c :foreground ,bg-added-fringe :background ,fringe)))
       `(git-gutter-fr:deleted ((,c :foreground ,bg-removed-fringe :background ,fringe)))
       `(git-gutter-fr:modified ((,c :foreground ,bg-changed-fringe :background ,fringe)))
       `(keycast-key ((,c :inherit bold :foreground ,fg-mode-line-active :background ,bg-mode-line-active)))
       `(keycast-command ((,c :inherit mode-line :foreground ,fg-mode-line-active :background ,bg-mode-line-active))))))
  (add-hook 'enable-theme-functions #'+customize-theme-faces)

  (modus-themes-select 'modus-vivendi))

(use-package ef-themes)

(use-package doric-themes)

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
                              :default-height 110)
                             (presentation
                              :default-height 260)
                             (t
                              :default-family "Aporetic Sans Mono"
                              ;; font height is 1/10pt.
                              :default-height 150
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
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
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
          pulsar-region-face 'pulsar-cyan
          pulsar-highlight-face 'pulsar-magenta)

  ;; Pulse after `pulsar-pulse-region-functions'.
  (setopt pulsar-pulse-region-functions pulsar-pulse-region-common-functions)

  (dolist (func '(beginning-of-buffer
                  end-of-buffer
                  beginning-of-defun
                  end-of-defun
                  backward-sexp
                  forward-sexp
                  forward-list
                  backward-list
                  backward-up-list
                  down-list))
    (add-to-list 'pulsar-pulse-functions func))
  ;; There are convenience functions/commands which pulse the line using a
  ;; specific color: `pulsar-pulse-line-green' is one of them.
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-blue)

  (add-hook 'next-error-hook #'pulsar-pulse-line-red)
  (add-hook 'next-error-hook #'pulsar-reveal-entry)

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
  :config
  (druid-modeline-text-mode 1)
  (add-hook 'prog-mode-hook #'druid-modeline-prog-mode)
  (add-hook 'org-mode-hook #'druid-modeline-org-mode)
  (add-hook 'org-capture-mode-hook #'druid-modeline-org-capture-mode)
  (add-hook 'org-agenda-mode-hook #'druid-modeline-org-agenda-mode)
  (add-hook 'gptel-mode-hook #'druid-modeline-gptel-mode)
  (add-hook 'shell-mode-hook #'druid-modeline-shell-mode)
  (add-hook 'term-mode-hook #'druid-modeline-shell-mode)
  (add-hook 'vterm-mode-hook #'druid-modeline-shell-mode)
  (add-hook 'mistty-mode-hook #'druid-modeline-shell-mode)
  (add-hook 'pdf-view-mode-hook #'druid-modeline-pdf-mode)
  (add-hook 'nov-mode-hook #'druid-modeline-nov-mode)
  (add-hook 'eww-mode-hook #'druid-modeline-eww-mode)
  (add-hook 'Info-mode-hook #'druid-modeline-info-mode)
  (add-hook 'elpher-mode-hook #'druid-modeline-elpher-mode))

(use-package keycast
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

;; TODO use visual-fill-column. wraps long lines at fill-column instead of at
;; the window edge. put after fill block.
(use-package visual-line
  :no-require
  :config
  ;; I normally do not use `visual-line-mode'. What it does is to break long
  ;; lines to span multiple lines without actually affecting the underlying
  ;; text. In other words, we still have one long line only its visualisation is
  ;; as a paragraph.

  ;; For the cases where I am fine with `visual-line-mode', I enable the mode by
  ;; adding it to these mode hooks.
  (dolist (mode '(help-mode-hook
                  special-mode-hook
                  Custom-mode-hook
                  epa-info-mode-hook))
    (add-hook mode #'visual-line-mode)))

(use-package truncate-lines
  :no-require
  :config
  ;; Truncate lines by default in a number of places and do not produce a
  ;; message about the fact. Note that the function used to achieve this,
  ;; i.e. `+truncate-lines-silently', my also be set up elsewhere and described
  ;; in that context. Here I only cover the basic parent modes.
  (defun +truncate-lines-silently ()
    "Toggle line truncation without printing messages."
    (let ((inhibit-message t))
      (toggle-truncate-lines t)))

  (dolist (mode '(text-mode-hook
                  prog-mode-hook
                  dired-mode-hook
                  +fundamental-mode-hook
                  hexl-mode-hook
                  comint-mode-hook))
    (add-hook mode #'+truncate-lines-silently)))

(use-package fill
  :no-require
  :config
  ;; `auto-fill-mode' automatically breaks long lines so that they wrap at the
  ;; `fill-column' length. This way, a paragraph is not a single long line, but
  ;; several shorter lines with newline characters between them. Often times
  ;; this is more pleasant to work with instead of having to rely on
  ;; `visual-line-mode' to visually wrap long lines. Relevant programs strip
  ;; away the newlines inside a paragraph, but there are some that do not. For
  ;; those I might rely upon `virtual-auto-fill-mode'.
  (add-hook 'text-mode-hook #'auto-fill-mode)
  (setopt fill-column 80)

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

  ;; TODO fill column only if a line is currently passing it
  ;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

  (bind-keys :map global-map
             ("M-Q" . +unfill-dwim)
             :map +prefix-map
             ("f" . set-fill-column)
             :map +toggle-prefix-map
             ("q" . auto-fill-mode)))

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
  :config
  ;; Buffer-local "direnv" integration for Emacs
  (setopt envrc-show-summary-in-minibuffer nil)
  (envrc-global-mode))

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

(use-package add-node-modules-path
  :disabled t
  :config
  ;; Adds the "node_modules/.bin" directory to the buffer "exec_path"
  (add-hook 'js-base-mode #'add-node-modules-path)
  (when (executable-find "pnpm")
    (setopt add-node-modules-path-command '("pnpm bin" "pnpm bin -w"))))

;;;;;;;;;;;;;;;
;;;; files ;;;;

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
     (if-let* ((marks (dired-get-marked-files 'no-dir))
               (_ (> (length marks) 1)))
         (list
          marks
          (+dired-grep-marked-files-prompt))
       (user-error "Mark multiple files"))
     dired-mode)
    (let ((buffer-name (format "*+dired-grep-marked for `%s'*" regexp)))
      (compilation-start
       (concat
        "find . -not " (shell-quote-argument "(")
        " -wholename " (shell-quote-argument "*/.git*")
        " -prune " (shell-quote-argument ")")
        " -type f"
        " -exec grep -nHER --color=auto " regexp " "
        (shell-quote-argument "{}")
        " " (shell-quote-argument ";") " ")
       'grep-mode
       (lambda (_mode) buffer-name)
       t)))

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

  (bind-keys
   :map +prefix-map
   ("d" . dired)
   ("C-j" . dired-jump)
   :map dired-mode-map
   ("e" . wdired-change-to-wdired-mode)
   ("i" . +dired-insert-subdir) ; override `dired-maybe-insert-subdir'
   ("l" . dired-up-directory)
   ("/" . +dired-limit-regexp)
   ("M-e" . +dired-subdirectory-next)
   ("M-a" . +dired-subdirectory-previous)
   ("C-c C-n" . +dired-subdirectory-next)
   ("C-c C-p" . +dired-subdirectory-previous)
   ("C-c C-l" . +dired-limit-regexp)
   ("C-c C-s" . +dired-search-flat-list)
   ("M-s g" . +dired-grep-marked-files) ; M-s M-g is `consult-grep'
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
             ("C-+" . dired-create-empty-file) ; mirror `dired-create-directory'
             ("C-<return>" . dired-do-open) ; Emacs 30
             ("M-s f" . nil)))

(use-package dired-preview
  :config
  ;; The `dired-preview' package previews the file at point in an Emacs
  ;; window.
  (setopt dired-preview-delay 0.5)

  (bind-keys :map dired-mode-map
             ("V" . dired-preview-mode)))

;; TODO document files
(use-package files
  :config
  (setopt y-or-n-p-use-read-key t
          use-short-answers t
          confirm-kill-processes nil
          confirm-kill-emacs 'yes-or-no-p
          large-file-warning-threshold nil)

  (bind-keys :map +prefix-map
             ("C-f" . find-file)
             ;; TODO add mark to xref before navigating to library
             ;; ("b" . find-library)
             ;; ("m" . man)
             ))

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
  :config
  (recentf-mode))

;; TODO document saveplace
(use-package saveplace
  ;; Tell Emacs to record where we were in the file, so we resume there on next
  ;; visit. Turn on place saving globally.
  :config
  (save-place-mode))

;;;;;;;;;;;;;;;;;;;
;;;; bookmarks ;;;;

(use-package bookmark
  ;; Bookmarks are compartments that store arbitrary information about a file or
  ;; buffer. The records are used to recreate that file/buffer inside of
  ;; Emacs. Put differently, we can easily jump back to a file or directory (or
  ;; anything that has a bookmark recorder+handler, really). Use the
  ;; `bookmark-set' command (`C-x r m' by default) to record a bookmark and then
  ;; visit one of your bookmarks with `bookmark-jump' (`C-x r b' by default).
  :config
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
  :config
  (setopt register-preview-delay 0.8)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'register-alist)))

;;;;;;;;;;;;;;;;;;;;
;;;; completion ;;;;

(use-package minibuffer
  :config
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
  (setopt marginalia-max-relative-age 0 ; absolute time
          marginalia-align 'right)
  (bind-keys :map minibuffer-local-map
             ("M-]" . marginalia-cycle))
  (marginalia-mode))

(use-package orderless
  :init
  ;; NOTE these dispatchers aren't particularly useful but I leave this here as
  ;; an example of how to write and use them.
  ;; (setf (alist-get ?` orderless-affix-dispatch-alist) #'orderless-flex)
  ;; (setf (alist-get ?~ orderless-affix-dispatch-alist) #'+orderless-beg-or-end)
  ;; (setf (alist-get ?. orderless-affix-dispatch-alist) #'+orderless-file-ext)
  (defun +orderless-beg-or-end (component)
    "Match COMPONENT as a prefix or suffix string."
    (orderless-regexp (format "\\(^%s\\|%s$\\)" component component)))
  (defun +orderless-file-ext (component)
    "Match COMPONENT to a file suffix when completing file names."
    (when minibuffer-completing-file-name
      (orderless-regexp (format "\\.%s\\'" component))))
  :config
  ;; `basic' only matches candidates that have the same beginning as the text in
  ;; the minibuffer. It is required for /ssh: completion to work for TRAMP.
  ;;
  ;; `partial-completion' divides the minibuffer text into words separated by
  ;; hyphens or spaces, and completes each word separately. This is wonderful
  ;; for files because it can expand ~/.l/s/fo to ~/.local/share/fonts. It also
  ;; expands em-l-m to emacs-lisp-mode. Bear in mind we do not need to have
  ;; partial-completion first as basic will never match something like this.
  ;;
  ;; `orderless', which is the most powerful/flexible is placed last. I do this
  ;; because Emacs tries the styles in the given order from left to right,
  ;; moving until it finds a match. As such, I usually want to start with tight
  ;; matches (e.g. li-fa-di for list-faces-display) and only widen the scope of
  ;; the search as I need to. This is easy to do because none of the built-in
  ;; completion styles parses the empty space (the default
  ;; orderless-component-separator), so as soon as I type a space after some
  ;; characters I am using orderless.
  (setopt completion-styles '(basic partial-completion orderless))
  ;; While we can override only the categories we care about, the presence of
  ;; those `completion-category-defaults' will surprise us in some cases because
  ;; we will not be using what was specified in the `completion-styles'. As
  ;; such, I set `completion-category-defaults' to nil, to always fall back to
  ;; my preferred `completion-styles' and then I further configure overrides
  ;; where those make sense to me.
  (setopt completion-category-defaults nil)
  ;; We can opt for per-category styles by configuring the user option
  ;; `completion-category-overrides'.
  (setq completion-category-overrides
        ;; In order to narrow our Citar searches not only using citation keys
        ;; (i.e. using authors, titles, etc.), we need a completion style that
        ;; is order independent.
        '((citar-candidate (styles . (orderless basic))))))

(use-package vertico
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
          vertico-multiform-commands `(("consult-\\(.*\\)?\\(find\\|fd\\|grep\\|ripgrep\\)"
                                        ,@+vertico-multiform-maximal)
                                       ("citar-\\(.*\\)"
                                        ,@+vertico-multiform-maximal))
          vertico-cycle t
          vertico-count 5)

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
             ("M-r" . vertico-multiform-buffer)
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
  ;; always scrolls over the list of candidates while doing preview. The
  ;; `consult-customize' macro allows us to configure the the preview on a
  ;; per-command basis.
  :config
  (bind-keys :map vertico-map
             ("C-M-n" . vertico-next)
             ("C-M-p" . vertico-previous))

  (setopt consult-preview-key '("M-." "C-M-n" "C-M-p"))
  (consult-customize
   consult-theme :preview-key (list :debounce 0.3 "M-." "C-M-n" "C-M-p")
   consult-mark :preview-key 'any)

  ;; When I call `consult-buffer', I usually instantly narrow to the buffer
  ;; subset, although I sometimes want to select a recent file or a shell
  ;; buffer. Typing "f SPC" or "s SPC" on those occasions is easier than almost
  ;; always typing "b SPC". I hide all sources, except normal buffers by
  ;; default.
  (dolist (src consult-buffer-sources)
    (unless (or (null (symbol-value src))
                (eq src 'consult--source-buffer)
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
  ;; TODO `consult-grep-dwim' that uses `(locate-dominating-file ".git")' to
  ;; determine whether to use consult-grep or consult-git-grep.
  ;; (setopt consult-grep-args '("grep" (consult--grep-exclude-args)
  ;;                             "--null" "--line-buffered" "--color=never"
  ;;                             "--ignore-case" "--with-filename" "--line-number"
  ;;                             "-I" "-R"))

  ;; NOTE document pulsar and consult integration
  (with-eval-after-load 'pulsar
    (setq consult-after-jump-hook nil)
    (dolist (fn '(pulsar-recenter-center pulsar-reveal-entry))
      (add-hook 'consult-after-jump-hook fn)))

  (defun +consult-tab (tab)
    "Switch to TAB by name."
    (interactive
     (if-let* ((tabs (mapcar (lambda (tab)
                               (cdr (assq 'name tab)))
                             (tab-bar-tabs))))
         (list (consult--read tabs :prompt "Tabs: " :category 'tab))
       (user-error "No tabs found.")))
    (tab-bar-select-tab-by-name tab))

  ;; Add a `consult' command to visualize `xref' history, adaptation of
  ;; `consult-mark'
  (defun +consult-xref-history ()
    "Jump to Xref history elements (using `xref--history')."
    (interactive)
    (consult-global-mark (flatten-list xref--history)))

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
             ("b" . consult-buffer)
             :map +goto-prefix-map
             ("M-g" . consult-goto-line)
             ("g" . consult-goto-line)
             :map +search-prefix-map
             ("M-b" . consult-bookmark)
             ("c" . count-matches)
             ("M-f" . consult-find) ; fd
             ("f" . consult-focus-lines) ; C-u to unfocus
             ("M-g" . +consult-grep-dwim) ; rg
             ("M-h" . consult-history)
             ("M-i" . consult-imenu)
             ("M-k" . consult-kmacro)
             ("k" . consult-keep-lines)
             ("M-l" . +consult-line-dwim)
             ("M-m" . consult-mark)
             ("M-r" . consult-register)
             ("M-s" . consult-outline)
             ("M-t" . +consult-tab)
             ("M-," . +consult-xref-history)
             :map +registers-prefix-map
             ("b" . consult-bookmark)
             :map +toggle-prefix-map
             ("t" . consult-theme)
             :map consult-narrow-map
             ;; Available filters are displayed with the `consult-narrow-help'
             ;; command at the prompt
             ("?" . consult-narrow-help)
             :map help-map
             ("C-i" . consult-info)
             :map isearch-mode-map
             ("M-s M-l" . consult-line) ; needed by consult-line to detect Isearch
             ("M-s M-h" . consult-isearch-history)))

;; (use-package consult-omni
;;   :config
;;   (bind-keys :map +search-prefix-map
;;              ("M-w" . consult-omni)))

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
     #'abbreviate-file-name
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

  ;; Minor mode to pass the selected directory directly to cd, meant to be used
  ;; in `shell-mode'.
  (define-minor-mode +consult-dir-shell-mode
    "Provide extra functionality to integrate the Emacs `shell' with
    `consult-dir'."
    :init-value nil
    :global nil
    (if +consult-dir-shell-mode
        (setq-local consult-dir-default-command
                    (lambda ()
                      (interactive)
                      (+shell--insert-and-send "cd" default-directory)))
      (setq-local consult-dir-default-command nil)))
  (add-hook 'shell-mode-hook #'+consult-dir-shell-mode)

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
             ("M-s M-f" . consult-dir-jump-file)
             :map vertico-map
             ("M-s M-f" . consult-dir-jump-file)))

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
  (add-hook 'embark-collect-mode-hook #'+truncate-lines-silently)

  (setopt embark-confirm-act-all nil
          embark-mixed-indicator-both nil
          embark-mixed-indicator-delay 1.0
          embark-indicators '(embark-minimal-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))

  (bind-keys :map global-map
             ("C-." . embark-act)
             :map minibuffer-local-map
             ("C-c C-a" . embark-act-all)
             ("C-c C-c" . embark-collect)
             ("C-c C-e" . embark-export)
             ("C-c C-i" . embark-select)))

;; Needed for correct exporting while using Embark with Consult commands.
(use-package embark-consult)

(use-package corfu
  :config
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

  (global-corfu-mode)
  (corfu-popupinfo-mode 1)

  (setopt corfu-cycle t
          corfu-preview-current nil
          corfu-min-width 20
          corfu-popupinfo-delay '(0.25 . 0.25))

  ;; TODO I'm not sure if corfu-history-mode is actually working
  ;; Sort by input history
  ;; (with-eval-after-load 'savehist
  ;;   (corfu-history-mode 1)
  ;;   (add-to-list 'savehist-additional-variables 'corfu-history))
  ;; Ensure `savehist-mode' is on and add `corfu-history' to the saved variables
  (unless (bound-and-true-p savehist-mode)
    (savehist-mode 1)
    (corfu-history-mode 1))
  (add-to-list 'savehist-additional-variables 'corfu-history)

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
  (add-hook 'completion-at-point-functions #'cape-file)

  ;; In order to merge capfs you can try the functions `cape-capf-super'. It is
  ;; only necessary if you want to combine multiple capfs, such that the
  ;; candidates from multiple sources appear together in the completion list at
  ;; the same time. `cape-capf-super' is not needed of multiple capfs should be
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

;; TODO https://protesilaos.com/emacs/dotemacs#h:567bb00f-1d82-4746-93e5-e0f60721728a
;; i want dabbrev to be separate from tab completion and snippets
;; (use-package dabbrev)

;;;;;;;;;;;;;;;;;;
;;;; snippets ;;;;

(use-package tempel
  ;; This is yet another nimble package from Daniel Mendler. It lets us define a
  ;; template which we can insert at any point and fill in the empty fields with
  ;; whatever values we want.
  :config
  ;; TODO i want tempel-complete to appear with eglot-completion-at-point
  ;; https://github.com/eryg-kai/emacs-config/blob/master/config/completion.el#L225C1-L265C20
  ;; (cape-super-capf #'eglot-completion-at-point #'tempel-complete)
  ;; (cape-super-capf #'elisp-completion-at-point #'tempel-complete)
  (bind-keys :map tempel-map
             ("C-g" . tempel-done)
             ("M-e" . tempel-next)
             ("M-a" . tempel-previous)))

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
                                    (project-eshell "Eshell" ?e)
                                    (project-find-file "File" ?f)
                                    (+project-consult-grep "Grep" ?g)
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
             ("b" . project-switch-to-buffer)
             ("d" . project-dired)
             ("e" . project-eshell)
             ("f" . project-find-file)
             ("g" . +project-consult-grep)
             ("k" . project-kill-buffers)
             ("p" . project-switch-project)
             ("r" . project-query-replace-regexp)
             ("," . project-compile)
             ("&" . project-async-shell-command)
             ("!" . project-shell-command)
             :map +search-prefix-map
             ("M-p" . project-switch-project)))

(use-package projection
  :disabled t
  :config
  ;; Projection adds project type support for Emacs' built-in `project.el' and
  ;; other valuable features. You can use the provided collection of project
  ;; types or write your own. Each project type can optionally expose different
  ;; command types such as build, configure, test, run, package, and
  ;; install. These commands can be called interactively and you can override
  ;; what command to run for these command types by passing a prefix argument
  ;; (`C-u' by default). The command you enter will be cached so subsequent
  ;; attempts to run the same command type will use the same command. Projection
  ;; supports both shell-commands, interactive functions, and helper functions
  ;; which can return either of these as valid targets for each of the command
  ;; types.
  (global-projection-hook-mode))

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
             :map +prefix-map
             ("k" . +kill-this-buffer)
             ("C-b" . ibuffer)
             ("C-n" . next-buffer)
             ("C-p" . previous-buffer)
             ;; :map +buffer-prefix-map
             ;; ("c" . clone-indirect-buffer-other-window)
             ;; ("g" . revert-buffer-quick)
             ;; ("k" . +kill-this-buffer)
             ;; ("m" . +buffers-major-mode) ; (prot) if i can filter in consult-buffer by major mode i don't need this
             ;; ("r" . +rename-file-and-buffer)
             ;; ("v" . +buffers-vc-root) ; (prot) if i can filter in consult-buffer by vc root i don't need this
             ))

;; (use-package uniquify)

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

  (bind-keys
   :map global-map
   ;; TODO put all s-* keybinds into sxhkd
   ("s-a" . windmove-up)
   ("s-A" . windmove-swap-states-up)
   ("s-e" . windmove-right)
   ("s-E" . windmove-swap-states-right)
   ("s-h" . windmove-down)
   ("s-H" . windmove-swap-states-down)
   ("s-o" . other-window)
   ("s-r" . +window-toggle-split)
   ;; ("s-r" . window-layout-transpose) ; Emacs 31 override `move-to-window-line-top-bottom'
   ;; ("s-R" . rotate-windows-back) ; Emacs 31
   ("s-y" . windmove-left)
   ("s-Y" . windmove-swap-states-left)
   :map +prefix-map
   ("0" . delete-window) ; `s-k' or `s-0'
   ("1" . delete-other-windows) ; `s-K' or `s-1'
   ("!" . delete-other-windows-vertically) ; `s-!'
   ("2" . split-window-below) ; `s-s' or `s-2'
   ("@" . split-root-window-below) ; `s-S' or `s-@'
   ;; ("M-2" . +split-window-below-and-focus) ; lambda emacs
   ("3" . split-window-right) ; `s-v' or `s-3'
   ("#" . split-root-window-right) ; `s-V' or `s-\#'
   ;; ("M-3" . +split-window-right-and-focus) ; lambda emacs
   ("4" . ctl-x-4-prefix)
   ("$" . window-toggle-side-windows)
   ("5" . ctl-x-5-prefix)
   ("%" . toggle-window-dedicated)
   ;; ("6" . )
   ;; ("^" . tear-off-window) ; ^ should be tear or detach
   ;; ("7" . )
   ("+" . balance-windows-area) ; `s-+'
   ;; ("8" . )
   ;; ("*" . )
   ;; ("9" . )
   ;; ("&" . )
   ))

(use-package too-wide-minibuffer-mode
  ;; Adjust the minibuffer size and position so it is displayed under the
  ;; selected window if the frame is too wide. Great for my ultrawide monitor.
  :config
  ;; This mode is not compatible with `minibuffer-follows-selected-frame' set to
  ;; `t' and there is more than one frame. Disable this setting to avoid a
  ;; warning message.
  (setopt minibuffer-follows-selected-frame nil)

  (too-wide-minibuffer-mode +1))

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

  (setopt display-buffer-base-action
          '((display-buffer-reuse-window
             display-buffer-same-window
             display-buffer-in-previous-window
             display-buffer-use-some-window)))

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
          split-width-threshold 135
          window-min-height 4
          window-min-width 10))

(use-package auto-side-windows
  :init (auto-side-windows-mode +1)
  :custom
  ;; Respects display actions when switching buffers
  (switch-to-buffer-obey-display-actions t)

  ;; Ensure Org src buffers are opend using display-buffer
  (org-src-window-setup 'plain)

  ;; Top side window configurations
  (auto-side-windows-top-buffer-names
   '(;; Messages buffers
     "^\\*Backtrace\\*$"
     "^\\*Async-native-compile-log\\*$"
     "^\\*Compile-Log\\*$"
     "^\\*Multiple Choice Help\\*$"
     "^\\*Quick Help\\*$"
     "^\\*TeX Help\\*$"
     "^\\*TeX errors\\*$"
     "^\\*Warnings\\*$"
     "^\\*Process List\\*$"
     ;; VC buffers
     "^COMMIT_EDITMSG$"
     ;; Org buffers
     ;; "^\\*Org Agenda\\*$"
     "^\\*Org Src.*\\$"
     "^\\*Org Select\\*$"
     "^\\*Org-Babel Error Output\\*"
     "\\(\\*Capture\\*\\|CAPTURE-.*\\)"))
  (auto-side-windows-top-buffer-modes
   '(messages-buffer-mode
     backtrace-mode))

  ;; Right side window configurations
  (auto-side-windows-right-buffer-names
   '(;; Doc buffers
     "^\\*eldoc.*\\*$"
     "^\\*info\\*$"
     "^\\*Metahelp\\*$"
     "^\\*Man.*\\*$"
     "^\\*woman.*\\*$"
     ;; REPL buffers
     "^\\*eshell.*\\*$"
     "^\\*shell.*\\*$"
     "^\\*term.*\\*$"
     "^\\*vterm.*\\*$"
     "^\\*mistty.*\\*$"
     "^\\*jupyter-repl.*\\*$"
     ;; VC buffers
     "^magit-diff:.*$"
     "^magit-process:.*$"
     ;; "^\\*difftastic.*\\*"
     ;; Others
     "^\\*Embark.*\\*$"
     "^\\*.*scratch\\*$"
     "^\\*marginal notes\\*$"))
  (auto-side-windows-right-buffer-modes
   '(;; Doc buffers
     Info-mode
     TeX-output-mode
     eldoc-mode
     help-mode
     helpful-mode
     shortdoc-mode
     ;; Messages buffers
     compilation-mode
     ;; REPL buffers
     eshell-mode
     shell-mode
     term-mode
     mistty-mode
     comint-mode
     debugger-mode
     ;; Grep buffers
     flymake-diagnostics-buffer-mode
     locate-mode
     occur-mode
     grep-mode
     xref--xref-buffer-mode
     pdf-occur-buffer-mode
     ;; VC buffers
     magit-status-mode
     magit-log-mode
     magit-diff-mode
     magit-process-mode
     difftastic-mode
     ))

  ;; Example: Custom parameters for top windows (e.g., fit height to buffer)
  ;; (auto-side-windows-top-alist '((window-height . fit-window-to-buffer)))
  ;; (auto-side-windows-top-window-parameters '((mode-line-format . ...))) ;; Adjust mode-line
  (auto-side-windows-common-window-parameters '())
  (auto-side-windows-right-alist '((window-width . 132)))
  (auto-side-windows-top-alist '((window-height . 10)))
  (auto-side-windows-common-alist '((body-function . +select-window)))

  ;; Maximum number of side windows on the left, top, right and bottom
  (window-sides-slots '(1 1 1 1)) ; Example: Allow one window per side

  ;; Force left and right side windows to occupy full frame height
  (window-sides-vertical t)

  ;; Make changes to tab-/header- and mode-line-format persistent when toggling windows visibility
  (window-persistent-parameters
   (append window-persistent-parameters
           '((tab-line-format . t)
             (header-line-format . t)
             (mode-line-format . t))))
  :config
  (defun +select-window (window)
    "Select WINDOW but prioritize top side windows."
    (let ((top-win
           (seq-find (lambda (w)
                       (eq (window-parameter w 'window-side) 'top))
                     (window-list))))
      (select-window (or top-win window))))

  (with-eval-after-load 'org-remark
    (setopt org-remark-notes-display-buffer-action '(auto-side-windows--display-buffer))))

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
  ;; for selection purposes, and (iii) for long lines to be trucated, meaning to
  ;; stretch beyond the visible portion of the window without wrapping below,
  ;; and for this to be done silently without messaging me about it. The latter
  ;; depends on my custom `+truncate-lines-silently'.
  (setopt list-matching-lines-jump-to-current-line nil)

  (add-hook 'occur-mode-hook #'+truncate-lines-silently)
  (add-hook 'occur-mode-hook #'hl-line-mode)

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
   :map minibuffer-local-isearch-map
   ("M-/" . isearch-complete-edit)
   :map occur-mode-map
   ("t" . toggle-truncate-lines)))

(use-package grep
  ;; `grep' is a wrapper for the U  nix program of the same name. Not much to add
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
          grep-use-headings t) ; Emacs 30

  (let ((executable (or (executable-find "rg") "grep"))
        (rgp (string-match-p "rg" grep-program)))
    (setopt grep-program executable
            grep-template (if rgp
                              "rg -nH --null -e <R> <F>"
                            "grep <X> <C> -nH --null -e <R> <F>")
            xref-search-program (if rgp 'ripgrep 'grep))))

;; TODO recursive project search https://www.youtube.com/watch?v=1jBbVUnNbDU

;; TODO recoll
;; can recoll search through my epubs?
;; if it can, set up the nov keymap to use it instead of consult-ripgrep or consult-lini
;; maybe define a function +consult-ripgrep-or-recoll that calls recoll if current mode is nov
;; (defvar +recoll-major-modes)

(use-package xref
  :config
  ;; `xref' provides infrastructure to jump to and from a definition. For
  ;; example, with point over a function, call `xref-find-definitions' will jump
  ;; to the file where the function is defined or provide an option to pick one
  ;; among multiple definitions, where applicable.
  ;;
  ;; Use Consult to select xref locations with preview.
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

  (with-eval-after-load 'pulsar
    (dolist (func '(xref-go-back
                    xref-go-forward
                    xref-find-definitions))
      (add-to-list 'pulsar-pulse-functions func))))

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
             ("M-j" . avy-goto-char-timer)
             :map isearch-mode-map
             ("M-j" . avy-isearch)))

;; TODO document link-hint
(use-package link-hint
  :config
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
             ("C-j" . +link-hint-jump-link))
  (with-eval-after-load 'eww
    (bind-keys :map eww-mode-map
               ("f" . link-hint-open-link)
               ("m" . +link-hint-jump-link)))
  (with-eval-after-load 'nov
    (bind-keys :map nov-mode-map
               ("f" . link-hint-open-link)
               ("m" . +link-hint-jump-link)))
  (with-eval-after-load 'elpher
    (bind-keys :map elpher-mode-map
               ("f" . link-hint-open-link)
               ("m" . +link-hint-jump-link))))

;; TODO document paragraphs
(use-package paragraphs
  :no-require
  :config
  (with-eval-after-load 'pulsar
    (dolist (func '(forward-paragraph
                    backward-paragraph
                    org-forward-paragraph
                    org-backward-paragraph))
      (add-to-list 'pulsar-pulse-functions func)))

  (bind-keys ("M-e" . forward-paragraph)
             ("M-a" . backward-paragraph)))

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
   :map global-map
   ("M-e" . logos-forward-page-dwim)
   ("M-a" . logos-backward-page-dwim)
   :map +narrow-prefix-map
   ("p" . logos-narrow-dwim)))

;; NOTE document golden-ratio-scroll
(use-package golden-ratio-scroll
  :no-require
  :config
  (bind-keys
   :map global-map
   ("C-v" . +golden-ratio-scroll-screen-down)
   ("M-v" . +golden-ratio-scroll-screen-up))

  (setq-default scroll-preserve-screen-position t
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
    (setq this-command 'scroll-down-command))
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
          ((region-active-p)
           (narrow-to-region (region-beginning) (region-end)))
          ((derived-mode-p 'org-mode)
           (or (ignore-errors (org-edit-src-code))
               (ignore-errors (org-narrow-to-block))
               (org-narrow-to-subtree)))
          (t (narrow-to-defun))))

  (defun +narrow-to-sexp ()
    "Narrow to sexp containing point."
    (interactive)
    (narrow-to-region
     (save-excursion (up-list -1 t t) (point))
     (save-excursion (up-list +1 t t) (point))))

  (put 'narrow-to-page 'disabled nil)

  (bind-keys
   :map global-map
   ("C-c n" . +narrow-prefix-map)
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
    (bind-keys :map org-mode-map
               ("C-c n" . +narrow-prefix-map)
               ("C-c C-n" . +narrow-or-widen-dwim)
               :map +narrow-prefix-map
               ("b" . org-narrow-to-block)
               ("e" . org-narrow-to-element)
               ("s" . org-narrow-to-subtree))))

(use-package goto-chg
  ;; The `goto-chg' package, authored by David Andersson and maintained by
  ;; Vasilij Scheidermann, moves the cursor to the point where the last change
  ;; happenend. Calling the command again cycles to the point before that and so
  ;; on. Simple and super effective.
  :config
  (bind-keys
   ("C-(" . goto-last-change)
   ("C-)" . goto-last-change-reverse)))

;; (use-package poi) ;; or better-jumper

;;;;;;;;;;;;;;;;;
;;;; editing ;;;;

(use-package editing
  :no-require
  :config
  (defun +open-line-below (&optional next)
    "Insert a new line below current line."
    (interactive "P")
    (if next
        (progn (end-of-line)
               (open-line 1)
               (next-line)
               (indent-according-to-mode))
      (save-excursion (end-of-line)
                      (open-line 1))))

  (defun +mark-line ()
    "Put point at beginning of this line, mark at end.

If region is active, extend selection downward by line. If
`visual-line-mode' is on, consider line as visual line."
    (interactive)
    (if (region-active-p)
        (forward-line 1)
      (progn
        (push-mark (line-beginning-position) t t)
        (end-of-line)
        (forward-line 1))))

  (defun +keyboard-quit-dwim ()
    "Do-What-I-Mean for a general `keyboard-quit'.
The generic `keyboard-quit' does not do the expected thing when the
minibuffer is open. Whereas we want it to close the minibuffer, even
without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular 'keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))

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
This command can then be followed by the standard `yank-pop' (default is bound to \\[yank-pop])."
    (interactive)
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (delete-region (line-beginning-position) (line-end-position)))
    (yank)
    (setq this-command 'yank))

  (defun +delete-indentation-below ()
    "Join the current line with the line beneath it."
    (interactive)
    (delete-indentation 1))

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

  (defun +kill-line-backward ()
    "Kill from point to the beginning of line."
    (interactive)
    (kill-line 0)
    (setq this-command 'kill-line))

  (defun +zap-to-char-backward (char &optional arg)
    "Backward `zap-to-char' for CHAR.
Optional ARG is a numeric prefix to match ARGth occurrence of CHAR."
    (interactive
     (list
      (read-char-from-minibuffer "Zap to char: " nil 'read-char-history)
      (prefix-numeric-value current-prefix-arg)))
    (zap-to-char (- arg) char t))

  (defun +zap-up-to-char-backward (char &optional arg)
    "Backward `zap-up-to-char' for CHAR.
Optional ARG is a numeric prefix to match ARGth occurrence of CHAR."
    (interactive
     (list
      (read-char-from-minibuffer "Zap up to char: " nil 'read-char-history)
      (prefix-numeric-value current-prefix-arg)))
    (zap-up-to-char (- arg) char t))

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

  ;; Make Emacs repeat the C-u C-SPC command (`pop-to-mark-command') by
  ;; following it up with another C-SPC. It is faster to type C-u
  ;; C-SPC, C-SPC, C-SPC, than C-u C-SPC, C-u C-SPC, C-u C-SPC...
  (setopt set-mark-command-repeat-pop t)

  ;; Do the reverse of C-u C-SPC (`pop-to-mark-command')
  (defun +unpop-to-mark-command ()
    "Unpop off mark ring. Does nothing if mark ring is empty."
    (interactive)
    (when mark-ring
      (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
      (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
      (when (null (mark t)) (ding))
      (setq mark-ring (nbutlast mark-ring))
      (goto-char (marker-position (car (last mark-ring))))))

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

  (setopt kill-whole-line t)

  (bind-keys
   :map global-map
   ("C-w" . +kill-region)
   ("M-w" . +kill-ring-save)
   ("M-k" . +kill-line-backward)
   ;; `+save-next-kill' causes the following command, if it kills, to save in
   ;; the kill ring instead. With prefix argument has same behavior as
   ;; `append-next-kill', which adds to previous kill.
   ;; ("C-M-w" . +save-next-kill)

   ;; `+keyboard-quit-dwim' closes an open but unfocused minibuffer.
   ("C-g" . +keyboard-quit-dwim)
   ("<escape>" . +keyboard-quit-dwim)

   ;; `+duplicate-dwim' will duplicate the region if active, otherwise the
   ;; current line.
   ("C-M-w" . +duplicate-dwim)
   ("C-M-y" . +yank-replace-dwim)

   ;; `+mark-line' will mark the current line, or if region is active it will
   ;; move forward a line.
   ("C-M-SPC" . +mark-line) ; overrides mark-sexp

   ;; The default `delete-char' doesn't respect the values of
   ;; `delete-active-region'. Make it so `C-d' deletes the region if active.
   ("C-d" . delete-forward-char)

   ;; Open new lines similar to Vim's o command.
   ("C-o" . +open-line-below)

   ;; Easier key for `delete-blank-lines'.
   ("M-o" . delete-blank-lines)

   ;; Join the current line with the line below it similar to Vim's J command.
   ("C-^" . +delete-indentation-below) ; Complements `M-^' for delete-indentation

   ;; Kills up to a char similar to Vim's dt command.
   ("C-z" . zap-up-to-char) ; Complements `M-z' for zap-to-char
   ("C-Z" . +zap-up-to-char-backward)
   ("M-Z" . +zap-to-char-backward)

   ;; TODO I would rather replace this with something like dogears or
   ;; better-jumper
   ;;
   ;; Pop and unpop to mark command.
   ("C-{" . pop-to-mark-command)
   ("C-}" . +unpop-to-mark-command)

   ;; Escape urls and insert dates
   ("C-<" . +escape-url-dwim)
   ("C-=" . +insert-date)

   ;; The `+comment-line-dwim' command is like the built-in `comment-dwim', but
   ;; toggles line wise commenting instead of appending them by default.
   ("M-;" . +comment-line-dwim)
   ("C-M-;" . +comment-sexp-dwim)
   ("s-;" . +comment-sexp-dwim)

   :map +prefix-map
   ("C-c" . +kill-terminal-or-restart)
   ("h" . mark-whole-buffer)))

(use-package substitute
  ;; I use `substitute' to efficiently replace targets in the buffer or
  ;; context. The `substitute' package provides a set of commands that perform
  ;; text replacement (i) throughout the buffer, (ii) limited to the current
  ;; definition (per `narrow-to-defun'), (iii) from point to the end of the
  ;; buffer, and (iv) from point to the beginning of the buffer

  ;; These substitutions are meant to be as quick as possible and to not move
  ;; the point. As such, they differ from the standard `query-replace' (which I
  ;; still use where relevant). The provided commands prompt for substitute text
  ;; and perform the substitution outright, without moving the point.
  :config
  ;; Produce a message after the substitution that reports on what happened. It
  ;; is a single line, like "Substituted `TARGET' with `SUBSTITUTE' N times
  ;; across the buffer."
  (add-hook 'substitute-post-replace-hook #'substitute-report-operation)

  ;; Set this to non-nil to highlight all occurrences of the current target.
  (setopt substitute-highlight t)

  ;; Set this to t if you want to always treat the letter casing
  ;; literally. Otherwise each commands accepts a `C-u' prefix argument to do
  ;; this on-demand.
  (setq substitute-fixed-letter-case nil)

  (defmacro +substitute-define-substitute-from-isearch-command (fn
                                                                doc
                                                                &optional
                                                                scope)
    "Produce substitute from Isearch command using FN, DOC, and SCOPE."
    `(defun ,fn (target sub &optional fixed-case)
      ,(format
        "Substitute TARGET with SUB %s from Isearch.

When called interactively, TARGET is the Isearch string and SUB is a
string that provided at the minibuffer prompt.

With optional FIXED-CASE as a prefix argument, do not try to
preserve the letter casing of the target text: the substitution
is literal.  Otherwise try to preserve the case (per
`replace-match').

Instead of the optional FIXED-CASE argument, the user can set the
option `substitute-fixed-letter-case' to non-nil.  That is the
same as always calling this command with FIXED-CASE." doc)
      (interactive
       (let ((target isearch-string))
        (list target
         (substitute--prompt target ,scope)
         current-prefix-arg)))
      (when (and isearch-other-end (< isearch-other-end (point)))
       (goto-char isearch-other-end))
      (isearch-done nil t)
      (isearch-clean-overlays)
      (substitute--operate target sub ,scope fixed-case)))

  (+substitute-define-substitute-from-isearch-command
   +substitute-target-in-buffer-from-isearch
   "throughout the buffer")

  (+substitute-define-substitute-from-isearch-command
   +substitute-target-in-defun-from-isearch
   "in the defun (per `narrow-to-defun')"
   'defun)

  (+substitute-define-substitute-from-isearch-command
   +substitute-target-below-point-from-isearch
   "to the end of the buffer"
   'below)

  (+substitute-define-substitute-from-isearch-command
   +substitute-target-above-point-from-isearch
   "to the beginning of the buffer"
   'above)

  (defvar +substitute-from-isearch-prefix-map (make-sparse-keymap)
    "Keymap with Substitute commands.
Meant to be assigned to a prefix key, like this:

    (define-key isearch-mode-map (kbd \"C-c s\")
    \=#'substitute-from-isearch-prefix-map)")

  (define-prefix-command '+substitute-from-isearch-prefix-map)

  (define-key substitute-prefix-map (kbd "b") #'+substitute-target-in-buffer-from-isearch)
  (define-key substitute-prefix-map (kbd "d") #'+substitute-target-in-defun-from-isearch)
  (define-key substitute-prefix-map (kbd "r") #'+substitute-target-above-point-from-isearch)
  (define-key substitute-prefix-map (kbd "s") #'+substitute-target-below-point-from-isearch)

  (bind-keys
   :map global-map
   ("C-c r" . substitute-prefix-map)
   :map isearch-mode-map
   ("C-c r" . +substitute-from-isearch-prefix-map)))

(use-package scratch
  :no-require
  :config
  ;; This custom library provides the means to create a scratch buffer for a
  ;; given major mode. It has the option to set a default major mode to use. It
  ;; can also copy the active region into the scratch buffer. Read the doc
  ;; string of the command `+scratch-buffer'.

  (defgroup +scratch ()
    "Scratch buffers for editable major mode of choice."
    :group 'editing)

  (defcustom +scratch-default-mode 'text-mode
    "Default major mode for `+scratch-buffer'."
    :type 'symbol
    :group '+scratch)

  (defun +scratch--list-modes ()
    "List known major modes."
    (let (symbols)
      (mapatoms
       (lambda (symbol)
         (when (and (functionp symbol)
                    (or (provided-mode-derived-p symbol 'text-mode)
                        (provided-mode-derived-p symbol 'prog-mode)))
           (push symbol symbols))))
      symbols))

  (defun +scratch--insert-comment ()
    "Insert comment for major mode, if appropriate.
Insert a comment if `comment-start' is non-nil and the buffer is
empty."
    (when (and (+common-empty-buffer-p) comment-start)
      (insert (format "Scratch buffer for: %s\n\n" major-mode))
      (goto-char (point-min))
      (comment-region (line-beginning-position) (line-end-position))))

  (defun +scratch--prepare-buffer (region &optional mode)
    "Add contents to scratch buffer and name it accordingly.

REGION is added to the contents to the new buffer.

Use the current buffer's major mode by default.  With optional
MODE use that major mode instead."
    (let ((major (or mode major-mode)))
      (with-current-buffer (pop-to-buffer (format "*%s scratch*" major))
        (funcall major)
        (+scratch--insert-comment)
        (goto-char (point-max))
        (unless (string-empty-p region)
          (when (+common-line-regexp-p 'non-empty)
            (insert "\n\n"))
          (insert region)))))

  (defvar +scratch--major-mode-history nil
    "Minibuffer history of `+scratch--major-mode-prompt'.")

  (defun +scratch--major-mode-prompt ()
    "Prompt for major mode and return the choice as a symbol."
    (intern
     (completing-read "Select major mode: "
                      (+scratch--list-modes)
                      nil
                      :require-match
                      nil
                      '+scratch--major-mode-history)))

  (defun +scratch--capture-region ()
    "Capture active region, else return empty string."
    (if (region-active-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      ""))

  (defun +scratch-buffer (&optional arg)
    "Produce a scratch buffer matching the current major mode.

With optional ARG as a prefix argument (\\[universal-argument]),
use `+scratch-default-mode'.

With ARG as a double prefix argument, prompt for a major mode
with completion.  Candidates are derivatives of `text-mode' or
`prog-mode'.

If region is active, copy its contents to the new scratch
buffer.

Buffers are named as *MAJOR-MODE scratch*.  If one already exists
for the given MAJOR-MODE, any text is appended to it."
    (interactive "P")
    (let ((region (+scratch--capture-region)))
      (pcase (prefix-numeric-value arg)
        (16 (+scratch--prepare-buffer region (+scratch--major-mode-prompt)))
        (4 (+scratch--prepare-buffer region +scratch-default-mode))
        (_ (+scratch--prepare-buffer region)))))

  (bind-keys
   :map global-map
   ("C-c s" . +scratch-buffer)))

;; TODO document move-text
(use-package move-text
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

  (bind-keys
   :map global-map
   ("M-P" . move-text-up)
   ("M-N" . move-text-down)
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
  (add-hook 'markdown-mode-hook #'+electric-pair-markdown-h)

  (defun +electric-pair-org-h ()
    (+electric-pair-add-fn '((?/ . ?/)
                             (?= . ?=)
                             (?~ . ?~)
                             (?_ . ?_))))
  (add-hook 'org-mode-hook #'+electric-pair-org-h))

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
   ("C-," . +er/expand-region-dwim) ; overrides mark-paragraph
   ("C-M-h" . er/contract-region) ; overrides mark-defun
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

  (defvar-keymap mc-mark-map
    :doc "multiple-cursors mark map."
    :prefix 'mc-mark-map)
  (defvar-keymap mc-mark-repeat-map
    :repeat (:hints ((mc/mark-next-like-this-symbol . "next")
                     (mc/mark-previous-like-this-symbol . "prev")
                     (mc/mark-next-lines . "next line")
                     (mc/mark-previous-lines . "prev line")
                     (mc/skip-to-next-like-this . "skip next")
                     (mc/skip-to-previous-like-this . "skip prev"))))
  (bind-keys
   :map global-map
   ("C-'" . mc-mark-map) ; C-c m
   :map mc-mark-map
   ("." . mc/mark-all-like-this-dwim)
   ("a" . mc/edit-beginnings-of-lines)
   ("d" . mc/mark-all-like-this-in-defun)
   ("e" . mc/edit-ends-of-lines)
   ("k" . mc-hide-unmatched-lines-mode) ; keep
   ("n" . mc/mark-next-like-this-symbol)
   ("p" . mc/mark-previous-like-this-symbol)
   ("C-n" . mc/mark-next-lines)
   ("C-p" . mc/mark-previous-lines)
   ("r" . mc/reverse-regions)
   ("s" . mc/sort-regions)
   (">" . mc/skip-to-next-like-this)
   ("<" . mc/skip-to-previous-like-this)
   :repeat-map mc-mark-repeat-map
   ("n" . mc/mark-next-like-this-symbol)
   ("p" . mc/mark-previous-like-this-symbol)
   ("C-n" . mc/mark-next-lines)
   ("C-p" . mc/mark-previous-lines)
   (">" . mc/skip-to-next-like-this)
   ("<" . mc/skip-to-previous-like-this)))

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

  (bind-keys
   :map flymake-mode-map
   ("M-g M-n" . flymake-goto-next-error)
   ("M-g M-p" . flymake-goto-prev-error)
   :map +toggle-prefix-map
   ("l" . flymake-mode)))

;; TODO document flymake-collection
(use-package flymake-collection)

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
             ("C-c f" . apheleia-format-buffer)))

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

;;;;;;;;;;;;;
;;;; lsp ;;;;

;; NOTE document eglot
(use-package eglot
  :init
  ;; Ask Eglot to stay away from completely taking over Flymake. Just add it as
  ;; another item.
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  ;; (add-hook 'eglot-managed-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)))

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
          (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] "))
                  mode-line-misc-info)))
  (add-hook 'eglot-managed-mode-hook #'+eglot-remove-mode-line-misc-info)

  (setopt eglot-extend-to-xref t)

  (bind-keys
   :map eglot-mode-map
   ("C-c C-i" . eglot-find-implementation)
   ("C-c C-t" . eglot-find-typeDefinition)
   ("C-c C-l" . eglot-find-declaration)))

(use-package eglot-use-package
  ;; Adds :lsp-hook, :lsp-server, and :lsp-config keywords for use-package forms
  ;;
  ;; Possible variations
  ;;
  ;; (use-package go-ts-mode
  ;;   :lsp-hook (go-ts-mode go-mod-ts-mode)
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

  (push :lsp-hook use-package-keywords)
  (push :lsp-server use-package-keywords)
  (push :lsp-config use-package-keywords)

  (defun use-package-normalize/:lsp-hook (_name _keyword args)
    "Normalizer for `:lsp-hook' in `use-package' forms.
The parameter ARGS is explained in the `use-package' documentation."
    (setq args (flatten-list args))
    (cond
     ((symbolp args) (list args))
     ((and (listp args) (cl-every #'symbolp args)) args)
     (t (error "Invalid :lsp-hook value: %S" args))))
  (defun use-package-handler/:lsp-hook (name _keyword args rest state)
    "Handler for `lsp-hook' in `use-package' forms.
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

;; (use-package consult-eglot
;;   :config
;;   ;; Eglot exposes the lsp `document/symbols' call through
;;   ;; Imenu. `consult-eglot' exposes the `workspace/symbols' call which can
;;   ;; present symbols from multiple open files or even files not directly loaded
;;   ;; by an open file but still used by your project.
;;   (bind-keys
;;    :map eglot-mode-map
;;    ("M-s M-i" . +consult-imenu) ;; C-u M-s M-i calls consult-eglot-symbols instead
;;    ("C-M-." . consult-eglot-symbols)))

;;;;;;;;;;;;;
;;;; dap ;;;;

;; (use-package dape)

;; TODO Use apheleia-use-package and eglot-use-package as inspiration for
;; dape-use-package, add to local lisp directory
;; Adds :dap keyword to use-package forms
;; (use-package dape-use-package)

;;;;;;;;;;;;;;;;;;;;;
;;;; compilation ;;;;

;; NOTE document compile
;; TODO running project-compile doesn't inherit environment defined by guix and envrc
(use-package compile
  :config
  ;; NOTE `compilation-filter-hook' is a set of filters to be applied to the
  ;; output of our compiler.

  ;; Automatically scroll build output
  (setopt compilation-scroll-output t)
  ;; Kill compilation process before starting another.
  (setopt compilation-always-kill t)
  ;; Don't underline.
  (setopt compilation-message-face 'default)
  ;; I'm not scared of saving everything
  (setopt compilation-ask-about-save nil)
  ;; Translate ANSI escape sequences into faces
  (defun +compile-ansi-color-apply ()
    "Translate control sequences into text properties in `compile' buffer."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))
  ;; (add-hook 'compilation-filter-hook '+compile-ansi-color-apply)

  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook 'ansi-osc-compilation-filter)

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
      (call-interactively 'project-compile)))

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
      (go-ts-mode-hook . (concat "go run " buffer-file-name))
      ;; (go-ts-mode-hook . "go build -o a.out ")
      (rust-ts-mode-hook . "cargo run ")
      ;; (rust-ts-mode-hook . "rustc -o a.out ")
      (sh-mode-hook . buffer-file-name)))

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
   ;; TODO is C-c C-q used anywhere at all? should i bind this globally?
   ("C-c C-q" . +kill-this-buffer)
   ("C-d" . +comint-send-self)
   ("C-j" . +comint-send-self)
   ("y" . +comint-send-self)
   ("n" . +comint-send-self)
   :map compilation-minor-mode-map
   ("C-x C-q" . +compile-toggle-comint)
   :map compilation-shell-minor-mode-map
   ("C-x C-q" . +compile-toggle-comint)
   :map minibuffer-local-shell-command-map
   ("M-s M-h" . +compile-input-from-history)
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
  :disabled t
  :config
  ;; `compile-multi' is a multi-target interface to `compile'. It allows you to
  ;; configure and interactively select compilation targets based on arbitrary
  ;; project types, build frameworks, or test tools.
  ;;
  ;; In simplified terms, `compile-multi' provides a framework for associating
  ;; actions with triggers. A trigger is any predicate that applies to the
  ;; current file, project, or directory. An action is a shell command or
  ;; interactive function or anything that can be invoked when the associated
  ;; trigger is set. For example, we can write a function that parses out all
  ;; the targets from a Makefile and generates actions for them. This allows us
  ;; to construct rich command interfaces.
  (bind-keys
   :map +prefix-map
   ("/" . compile-multi)))

;; TODO create compile-multi-use-package that adds to compile-multi-config
;; Adds :compile keyword to use-package forms
;; (use-package compile-multi-use-package)

(use-package consult-compile-multi
  :disabled t
  :config
  ;; `consult-compile-multi' is an extension for `compile-multi' that runs the
  ;; interactive selection of targets through `consult' instead of
  ;; `completing-read', which enhances it with some useful consult features such
  ;; as narrowing.
  (consult-compile-multi-mode))

(use-package projection-multi
  :disabled t
  :config
  ;; `projection' has an optional extension package called `projection-multi' to
  ;; integrate `compile-multi' into the current project type. It can extract
  ;; available compilation targets from Makefiles, CMake configuration, etc. and
  ;; lets you execute them easily. By default, `projection-multi-compile'
  ;; determines all project types matching the current project and then resolves
  ;; compilation targets based on them. For example, a project that would match
  ;; CMake and tox would let you select both tox and CMake build
  ;; targets.
  ;;
  ;; Currently automatic target generation functions are available for the
  ;; following project types: projection (simply presents available projection
  ;; commands for the matching project types), CMake, Make, Poetry Poe, and Tox.
  (bind-keys
   :map +prefix-map
   ("/" . projection-multi-compile)))

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
  (bind-keys
   :map help-map
   ("h" . describe-face)
   ("C-h" . embark-prefix-help-command)
   ("C-k" . describe-keymap)
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

(use-package info
  :config
  ;; `envrc' changes the INFOPATH, so every time I'm in an envrc managed
  ;; project, `info' can't find the Info manuals from my emacs-packages
  ;; derivations that were added to the INFOPATH.
  ;;
  ;; While I understand wanting to isolate the INFOPATH for example when you're
  ;; hacking on an Info manual for a package, I almost never need to do that.
  (defun +info ()
    (interactive)
    (let ((default-directory (getenv "HOME")))
      (call-interactively #'info)))

  (bind-keys :map help-map
             ("i" . +info)))

;; TODO document man
(use-package man
  :config
  (bind-keys :map help-map
             ("C-m" . man)))

(use-package devdocs
  :config
  (bind-keys :map help-map
             ("C-d" . devdocs-lookup)
             ("d" . devdocs-peruse)))

;; (use-package rfc-mode)

;;;;;;;;;;;;
;;;; vc ;;;;

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
  (setopt magit-define-global-key-bindings nil
          magit-section-visibility-indicator '("⤵"))
  :config
  (setq git-commit-summary-max-length 50
        ;; I used to also include `overlong-summary-line' in this list, but I
        ;; realised I do not need it. My summaries are always in check. When I
        ;; exceed the limit, it is for a good reason.
        git-commit-style-convention-checks '(non-empty-second-line))

  (setopt magit-diff-refine-hunk t
          magit-display-buffer-function #'display-buffer
          magit-commit-diff-inhibit-same-window t)

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

  (bind-keys
   :map +prefix-map
   ("v" . magit-status)
   :map +project-prefix-map
   ("v" . magit-project-status)
   :map magit-status-mode-map
   ;; Apply our +magit-kill-buffers command only in magit-status
   ("q" . +magit-kill-buffers)))

(use-package diff
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

  (setopt diff-default-read-only t))

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
  (setopt ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-keep-variants nil
          ediff-make-buffers-readonly-at-startup nil
          ediff-show-clashes-only t))

(use-package difftastic
  :config
  (setopt difftastic-display-buffer-function #'display-buffer)
  ;; (add-to-list 'display-buffer-alist '("\\*difftastic git show.*\\*"
  ;;                                      (display-buffer-reuse-window
  ;;                                       display-buffer-pop-up-window)
  ;;                                      (window-height . 0.5)
  ;;                                      (body-function . select-window)))
  )

;; TODO diff-hl
;; I would like to lean into native/built-in Emacs functionality where it's
;; equal or better than the third-party alternatives. `diff-hl' relies on the
;; built-in `vc.el' library instead of talking to git directly (thus expanding
;; support to whatever VCs vc.el supports, and not git alone), which also means
;; it can take advantage of its caching and other user configuration for
;; vc.el. Overall, it should be faster and lighter.
;;
;; However, everytime I have tried to use diff-hl, it has been buggy or slow to
;; refresh on changes. It still has issues with Magit altering the git state. It
;; is also easier to redefine fringe bitmaps for git-gutter than it is for
;; diff-hl.
;;
;; Doom Emacs has a lot of configuration code for diff-hl that I might look into
;; incorporating someday. In the meantime I'll keep using git-gutter.

(use-package git-gutter
  :config
  ;; `git-gutter' and `git-gutter-fringe' use the margins or fringes to
  ;; highlight changes in the current buffer. The indicators are colour-coded to
  ;; denote whether a change is an addition, removal, or change that includes a
  ;; bit of both.
  ;;
  ;; This package offers some more features, such as the ability to move between
  ;; diff hunks while editing the buffers. I still need to experiment with those
  ;; before customizing them to my liking.
  (add-hook 'prog-mode-hook #'git-gutter-mode)
  ;; The `git-gutter:update-interval' customizable variable was defined with
  ;; type 'integer, but I like it between 0.3 and 0.5 so I redefine it with type
  ;; 'number.
  (defcustom git-gutter:update-interval 0
    "Time interval in seconds for updating diff information."
    :type 'number
    :group 'git-gutter)
  (setopt git-gutter:update-interval 0.5))

(use-package git-gutter-fringe
  :config
  (setopt git-gutter-fr:side 'left-fringe)
  ;; Redefine fringe bitmaps to present the diff in the fringe as solid bars
  ;; (with no border) taking up less horizontal space in the fringe. However
  ;; this will look bad with themes that invert the foreground/background of
  ;; git-gutter-fr's faces (like `modus-themes' does.)
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [#b11111000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [#b11111000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [#b11111000] nil nil '(center repeated)))

;; (use-package eldoc-diffstat)

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

  ;; TODO: Do I need +shell-input-from-history when I have consult-history?
  (with-eval-after-load 'consult
    (add-to-list
     'consult-mode-histories
     '(shell-mode comint-input-ring comint-input-ring-index comint-bol)))

  (defun +consult-history-comint-send ()
    (declare (interactive-only t))
    (interactive)
    (consult-history)
    (comint-send-input))

  (defun +shell--build-input-history ()
    "Return `comint-input-ring' as a list."
    (when (and (ring-p comint-input-ring)
               (not (ring-empty-p comint-input-ring)))
      (let (history)
        ;; We have to build up a list ourselves from the ring vector.
        (dotimes (index (ring-length comint-input-ring))
          (push (ring-ref comint-input-ring index) history))
        (delete-dups history))))

  (defvar +shell--input-history-completion-history nil
    "Minibuffer history of `+shell--input-history-prompt'.
Not to be confused with the shell input history, which is stored
in the `comint-input-ring' (see `+shell--build-input-history').")

  (defun +shell--input-history-prompt ()
    "Prompt for completion against `+shell--build-input-history'."
    (let* ((history (+shell--build-input-history))
           (default (car history)))
      (completing-read
       (format-prompt "Insert input from history" default)
       history nil :require-match nil
       '+shell--input-history-completion-history
       default)))

  (defun +shell-input-from-history ()
    "Insert command from shell input history.
Only account for the history Emacs knows about, ignoring
`comint-input-ring-file-name' (e.g. ~/.bash_history)."
    (declare (interactive-only t))
    (interactive)
    (+shell--insert-and-send
     (+shell--input-history-prompt)))

  ;;;; Outline support for shell prompts

  (add-hook 'shell-mode-hook
            (lambda ()
              (setq outline-regexp shell-prompt-pattern)))

  ;;;; Directory navigation

  ;;;;; Directory tracking

  (defvar +shell-cd-directories nil
    "List of accumulated `shell-last-dir'.")

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables '+shell-cd-directories))

  (defun +shell-track-cd (&rest _)
    "Track shell input of cd commands.
Push `shell-last-dir' to `+shell-cd-directories'."
    (when-let* ((input (+shell--last-input))
                ((string-match-p "cd " input)))
      (push shell-last-dir +shell-cd-directories)))

  (defun +shell-update-name-on-cd (&rest _)
    "Update the shell buffer name after a cd for use in `+shell'."
    (when-let* ((input (+shell--last-input))
                ((string-match-p "cd " input)))
      (rename-buffer (format "*shell in %s*" default-directory) :make-unique)))

  (defvar +shell--cd-history nil
    "Minibuffer history for `+shell-cd'.")

  (defun +shell--cd-prompt ()
    "Prompt for a directory among `+shell-cd-directories'."
    (if-let* ((history +shell-cd-directories)
              (dirs (cons default-directory history))
              (def (if (listp dirs) (car dirs) shell-last-dir)))
        (completing-read
         (format-prompt "Select directory" def)
         dirs nil :require-match nil '+shell--cd-history def)
      (user-error "No directories have been tracked")))

  (defun +shell-cd ()
    "Switch to `+shell-cd-directories' using minibuffer completion."
    (declare (interactive-only t))
    (interactive)
    (+shell--insert-and-send
     "cd"
     (+shell--cd-prompt)))

  ;;;;; VC root directory

  (defun +shell--get-vc-root-dir ()
    "Return `vc-root-dir' or root of present Git repository."
    (or (vc-root-dir)
        (locate-dominating-file "." ".git")))

  (defun +shell-cd-vc-root-dir ()
    "Change into the `vc-root-dir'."
    (interactive)
    (if-let* ((root (+shell--get-vc-root-dir)))
        (+shell--insert-and-send "cd" root)
      (user-error "Cannot find the VC root of `%s'" default-directory)))

  ;;;; Bookmark support

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

  ;;;; Consult support

  (with-eval-after-load 'consult
    (defvar +consult--source-shell-buffer
      `( :name "Shell"
         :narrow   ?s
         :category buffer
         :state    ,#'consult--buffer-state
         :items    ,(lambda ()
                      (mapcar #'buffer-name
                       (seq-filter (lambda (buf)
                                     (with-current-buffer buf
                                      (derived-mode-p 'shell-mode)))
                        (buffer-list))))))
    (add-to-list 'consult-buffer-sources '+consult--source-shell-buffer :append))

  ;;;; General commands

  (defun +shell--history-or-motion (history-fn motion-fn arg)
    "Call HISTORY-FN or MOTION-FN with ARG depending on where point is.
If `+shell--beginning-of-prompt-p' returns non-nil call
HISTORY-FN, else MOTION-FN."
    (let ((fn (if (or (+shell--beginning-of-prompt-p)
                      (eq last-command 'comint-next-input)
                      (eq last-command 'comint-previous-input))
                  history-fn
                motion-fn)))
      (funcall-interactively fn arg)
      (setq this-command fn)))

  (defun +shell-up-dwim (arg)
    "Return previous ARGth history input or go ARGth lines up.
If point is at the beginning of a shell prompt, return previous
input, otherwise perform buffer motion."
    (interactive "^p")
    (+shell--history-or-motion 'comint-previous-input 'previous-line arg))

  (defun +shell-down-dwim (arg)
    "Return next ARGth history input or or go ARGth lines down.
If point is at the beginning of a shell prompt, return previous
input, otherwise perform buffer motion."
    (interactive "^p")
    (+shell--history-or-motion 'comint-next-input 'next-line arg))

  ;; (defun +shell ()
  ;;     "Like `shell' but always start a new shell.
  ;; Name the shell buffer after the `default-directory'.  If the name of
  ;; that buffer already exists, then reuse it."
  ;;     (interactive)
  ;;     (with-current-buffer (shell (format "*shell in %s*" default-directory))
  ;;       (add-hook 'comint-output-filter-functions #'+shell-update-name-on-cd nil
  ;;                 :local)))

  ;; (defun +project-shell ()
  ;;     "Start an inferior shell in the current project's root directory.
  ;; If a buffer already exists for running a shell in the project's root,
  ;; switch to it.  Otherwise, create a new shell buffer.
  ;; With \\[universal-argument] prefix arg, create a new inferior shell buffer even
  ;; if one already exists."
  ;;     (interactive)
  ;;     (let* ((default-directory (project-root (project-current t)))
  ;;            (shell-buffer (get-buffer (format "*shell in %s" default-directory))))
  ;;       (if (and shell-buffer (not current-prefix-arg))
  ;;           (if (comint-check-proc shell-buffer)
  ;;               (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
  ;;             (shell shell-buffer))
  ;;         (+shell))))

  (defvar +shell--shells nil)

  (defvar-local +shell--shell nil)
  (defvar-local +shell--last-buffer nil)

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
           (shell (shell (format "*shell in %s*" default-directory))))
      (setq +shell--last-buffer origin)
      (with-current-buffer shell
        (add-hook 'comint-output-filter-functions
                  #'+shell-update-name-on-cd nil :local)
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

  (defun +shell-switch (&optional arg impl buffer)
    "Switch to running a shell in the current project's root directory.

If shell is the current buffer, switch to the previously used
buffer.

With \\[universal-argument] prefix argument, the user can specify a
directory.

If no shell is running, execute `+shell' to start a fresh one."
    (interactive "P")
    (let* ((in-shell (eq major-mode 'shell-mode))
           (in-live-shell (and in-shell (get-buffer-process (current-buffer))))
           (shell (and (buffer-live-p +shell--shell) +shell--shell)))
      (cond (in-live-shell
             (when (and (not (eq shell buffer))
                        (buffer-live-p +shell--last-buffer))
               (+shell--switch-to-buffer +shell--last-buffer)))
            (shell (+shell--set-this-buffer-shell shell)
                   (+shell--switch-to-buffer shell))
            (t (if arg (call-interactively '+shell)
                 (call-interactively '+project-shell))))
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

  (defvar-keymap +shell-mode-map
    :doc "Key map for `+shell-mode'."
    "<up>" #'+shell-up-dwim
    "<down>" #'+shell-down-dwim
    "M-p" #'+shell-up-dwim
    "M-n" #'+shell-down-dwim
    "C-x C-z" #'+shell-switch
    ;; "C-c C-." #'+shell-cd-vc-root-dir
    ;; "C-c d" #'+shell-cd
    "C-c C-q" #'+kill-this-buffer)

  (define-minor-mode +shell-mode
    "Provide extra functionality for the Emacs `shell'.
Add a bookmark handler for shell buffer and activate the
`+shell-mode-map':
\\{+shell-mode-map}"
    :init-value nil
    :global nil
    (if +shell-mode
        (progn
          (add-hook 'comint-output-filter-functions #'+shell-track-cd nil :local)
          (setq-local bookmark-make-record-function #'+shell-bookmark-make-record))
      (remove-hook 'comint-output-filter-functions #'+shell-track-cd :local)
      (setq-local bookmark-make-record-function nil)))

  (add-hook 'shell-mode-hook #'+shell-mode)

  (setq-default comint-scroll-to-bottom-on-input t
                comint-scroll-to-bottom-on-output nil
                comint-input-autoexpand 'input)

  (setopt comint-prompt-read-only t
          comint-buffer-maximum-size 9999
          comint-completion-autolist t
          comint-input-ignoredups t)

  ;; (setopt tramp-default-remote-shell "/bin/bash")

  (setopt shell-command-prompt-show-cwd t ; Emacs 27.1
          shell-input-autoexpand 'input
          shell-highlight-undef-enable t ; Emacs 29.1
          shell-kill-buffer-on-exit t ; Emacs 29.1
          shell-completion-fignore '("~" "#" "%"))

  (setopt shell-font-lock-keywords
          '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-builtin-face)
            ("^[^ \t\n]+:.*" . font-lock-string-face)
            ("^\\[[1-9][0-9]*\\]" . font-lock-constant-face)))

  (bind-keys :map global-map
             ("C-!" . +shell-command-at-line)
             :map +prefix-map
             ("C-z" . +shell-switch)
             ("RET" . +shell-home)
             :map +project-prefix-map
             ("z" . +project-shell)
             :map shell-mode-map
             ("M-s M-h" . +consult-history-comint-send)
             ("C-c C-k" . comint-clear-buffer)
             ("C-c C-w" . comint-write-output)
             :map comint-mode-map
             ("C-c C-q" . +kill-this-buffer)))

(use-package native-complete
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

(use-package mistty
  :config

  (defun +mistty (&optional prompt)
    "Start an Mistty term in the specified directory.

If PROMPT is nil, don't prompt for a directory and use
`default-directory'."
    (interactive "P")
    (if prompt
        (let ((default-directory (read-directory-name "Directory: " default-directory)))
          (mistty))
      (mistty-in-project)))

  (bind-keys :map +prefix-map
             ("C-<return>" . +mistty)))

;;;;;;;;;;;;;;
;;;; prog ;;;;

;; NOTE should eval-prefix be in C-c instead so these can be mode-specific?
;; scheme, common lisp, jupyter?
(use-package elisp
  :no-require
  :init
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
             :map emacs-lisp-mode-map
             ("C-M-x" . eval-defun)
             ("C-c C-c" . eval-defun)
             ("C-c C-e" . eval-last-sexp)
             ("C-x C-e" . eval-last-sexp)
             ("C-c C-k" . eval-buffer)
             ("C-c C-m" . emacs-lisp-macroexpand)
             ("C-c M-m" . pp-macroexpand-last-sexp)
             ("C-c C-r" . eval-region)
             ("C-c C-l" . load-file)
             ("C-c C-z" . ielm)))

;; (use-package beardbolt
;;   :config
;;   (bind-keys :map global-map
;;              ("C-c M-d" . beardbolt-starter)))

(use-package ielm
  :config
  (bind-keys :map ielm-map
             ("C-c C-q" . +kill-this-buffer)))

(use-package python
  :lsp-hook (python-mode python-ts-mode)
  :config
  (with-eval-after-load 'inheritenv
    (inheritenv-add-advice 'run-python)
    (inheritenv-add-advice 'run-python-internal))

  (setopt python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt"
          python-shell-completion-native-enable nil
          python-indent-guess-indent-offset nil
          python-indent-offset 4))

(use-package jupyter
  :config
  (with-eval-after-load 'inheritenv
    (inheritenv-add-advice 'jupyter-run-repl)
    (inheritenv-add-advice 'jupyter-connect-repl))

  (defvar-local +jupyter--repl nil)
  (defvar-local +jupyter--last-buffer nil)

  (defun +jupyter--maybe-remember-buffer (buffer)
    (when (and buffer
               (eq major-mode 'jupyter-repl-mode))
      (setq +jupyter--last-buffer buffer)))

  (defsubst +jupyter--set-this-buffer-repl (s &optional this)
    (with-current-buffer (or this (current-buffer)) (setq +jupyter--repl s)))

  (defun +jupyter--switch-to-buffer (buffer)
    (unless (eq buffer (current-buffer))
      (switch-to-buffer-other-window buffer)))

  (defun +jupyter-run-repl ()
    (interactive)
    (let* ((origin (current-buffer))
           (_ (call-interactively 'jupyter-run-repl))
           (repl (jupyter-with-repl-buffer jupyter-current-client (current-buffer))))
      (with-current-buffer repl
        (+jupyter--set-this-buffer-repl repl origin)
        (+jupyter--set-this-buffer-repl repl))))

  (defun +jupyter-connect-repl ()
    (interactive)
    (let* ((origin (current-buffer))
           (_ (call-interactively 'jupyter-connect-repl))
           (repl (jupyter-with-repl-buffer jupyter-current-client (current-buffer))))
      (with-current-buffer repl
        (+jupyter--set-this-buffer-repl repl origin)
        (+jupyter--set-this-buffer-repl repl))))

  (defun +jupyter-repl-pop-to-buffer (&optional arg impl buffer)
    "Switch to a kernel's REPL buffer.

If REPL is the current buffer, switch to the previously used buffer.

With \\[universal-argument] prefix argument, the user can connect to an
existing kernel using the kernel's connection file.

If no REPL is running, execute `jupyter-run-repl' to start a fresh one."
    (interactive "P")
    (let* ((in-repl (eq major-mode 'jupyter-repl-mode))
           (repl (and (buffer-live-p +jupyter--repl) +jupyter--repl))
           (origin (current-buffer)))
      (cond (in-repl
             (when (and (not (eq repl buffer))
                        (buffer-live-p +jupyter--last-buffer))
               (+jupyter--switch-to-buffer +jupyter--last-buffer)))
            (repl (+jupyter--set-this-buffer-repl repl)
                  (+jupyter--switch-to-buffer repl))
            (t (if arg (call-interactively '+jupyter-connect-repl)
                 (call-interactively '+jupyter-run-repl))))
      (+jupyter--maybe-remember-buffer (or buffer origin))))

  (setopt jupyter-eval-use-overlays t)
  (bind-keys :map python-mode-map
             ("C-c C-b" . jupyter-repl-interrupt-kernel)
             ("C-c C-c" . jupyter-eval-defun)
             ("C-c C-d" . nil) ; unmap `python-describe-at-point'
             ("C-c C-k" . jupyter-eval-buffer)
             ("C-c C-l" . jupyter-load-file)
             ("C-c C-q" . jupyter-repl-shutdown-kernel)
             ("C-c C-r" . jupyter-eval-region)
             ("C-c M-r" . jupyter-repl-restart-kernel)
             ("C-c C-z" . +jupyter-repl-pop-to-buffer)
             :map jupyter-repl-mode-map
             ("C-c C-z" . +jupyter-repl-pop-to-buffer)
             :map jupyter-repl-interaction-mode-map
             ("C-c C-z" . +jupyter-repl-pop-to-buffer)))

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :lsp-hook nix-ts-mode
  :config
  (define-derived-mode nix-mode nix-ts-mode "Nix"))

;; TODO document go-ts-mode
;; TODO add C-c C-a `go-import-add' from `go-mode'
(use-package go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode)
         ("/go\\.mod\\'" . go-mod-ts-mode))
  :lsp-hook (go-ts-mode go-mod-ts-mode)
  :lsp-config '((:gopls . (:ui.completion.usePlaceholders t
                           :hints (:assignVariableTypes t
                                   :constantValues t
                                   :parameterNames t
                                   :rangeVariableTypes t)
                           :hoverKind "FullDocumentation"))
                (:plysp . (:hoverKind "None")))
  :config
  (define-derived-mode go-mode go-ts-mode "Go")
  (setopt go-ts-mode-indent-offset 4))

(use-package templ-ts-mode)

;; TODO https://sr.ht/~p00f/hare-ts-mode/
;; (use-package hare-ts-mode)

;; (use-package cc)

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
   ("C-c C-c" . executable-interpret)
   ("C-h C-." . man)))

;; (use-package conf-mode)

;; (use-package sxhdrc)

(use-package dts-mode
  ;; setup for zmk keymaps
  :mode (("\\.keymap\\'" . dts-mode)
         ("\\.overlay\\'" . dts-mode)))

(use-package markdown-mode)

;; TODO get gitcommit tree-sitter grammar to work on my nix machine
;; (use-package git-commit-ts-mode
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . git-commit-ts-mode))
;;   :config
;;   (with-eval-after-load 'magit
;;     (setopt git-commit-major-mode 'git-commit-ts-mode)))

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
    (cond ((+in-list-p (car custom-enabled-themes) modus-themes-collection)
           (modus-themes-with-colors
             (face-remap-add-relative
              'default
              `(:background ,bg-dim))))
          ((+in-list-p (car custom-enabled-themes) ef-themes-collection)
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
      (if (+in-list-p (car custom-enabled-themes)
                      (append ef-themes-collection
                              modus-themes-collection
                              doric-themes-collection))
          (pdf-view-themed-minor-mode 1)
        (pdf-view-themed-minor-mode -1))
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

  (setopt pdf-view-display-size 'fit-height
          pdf-view-use-dedicated-register nil
          pdf-outline-imenu-use-flat-menus t
          large-file-warning-threshold nil)

  (bind-keys
   :map pdf-view-mode-map
   ("i" . consult-imenu)
   ("d" . pdf-view-themed-minor-mode)))

;; This package extends the built-in `save-place-mode' by adding support for PDF
;; buffers under PDFView or DocView mode. Revisiting PDF files will restore the
;; saved place (i.e. the current page and zoom.)
(use-package saveplace-pdf-view)

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
          org-startup-folded 'content
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

  ;; I do not set up this minor mode via a hook, as I do not really need it for
  ;; small Org files. Instead, I set up the files I am interested in to have the
  ;; following somewhere close to the top:
  ;;
  ;; #+startup: content indent
  ;;
  ;; What this `#+startup' directive does is to (i) show all the headings while
  ;; folding their contents and (ii) activate `org-indent-mode'. If you add the
  ;; `#+startup' to an already open file, then you need to do `org-mode-restart'
  ;; for changes to take effect.

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
  (setopt org-adapt-indentation nil ; No, non, nein, όχι to literal indentation!
          org-indent-mode-turns-on-hiding-stars nil
          org-indent-indentation-per-level 4)

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
              (let ((context (save-excursion
                               (goto-char begin)
                               (org-element-context))))
                (pcase (org-element-lineage context '(link node-property) t)
                  (node-property
                   (goto-char begin)
                   (throw :found t))
                  (link
                   (goto-char (org-element-property :begin link))
                   (throw :found t)))))))
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
    (interactive)
    (cond
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
   ;; TODO how do i include org-shiftmeta* bindings with this npbf scheme?
   ("M-P" . org-metaup)
   ("M-N" . org-metadown)
   ("M-B" . org-metaleft)
   ("M-F" . org-metaright)
   ("C-'" . nil)
   ("C-," . nil)
   ("C-<return>" . nil)
   ("C-M-<return>" . nil)
   ("C-c l" . org-store-link)
   ("C-c M-l" . org-insert-last-stored-link)
   ("C-c C-M-l" . org-toggle-link-display)
   ("M-." . org-edit-special) ; mnemonic is global M-. that goes to source (alias for C-c ')
   ("M-s M-o" . consult-org-heading)
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

  ;; TODO org-store-link doesn't work with some modes like shell-mode
  (defun +org-capture-inbox ()
    (interactive)
    (call-interactively 'org-store-link)
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
;;   (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)


;;   )

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

  (bind-keys
   :map +notes-prefix-map
   ("n" . denote)
   ("N" . denote-type)
   ("o" . denote-sort-dired) ; "order" mnemonic
   ;; Note that `denote-rename-file' can work from any context, not just Dired
   ;; buffers. That is why we bind it globally.
   ("r" . denote-rename-file)
   :map text-mode-map
   ("C-x n b" . denote-backlinks)
   ("C-x n i" . denote-link) ; "insert" mnemonic
   ("C-x n I" . denote-add-links)
   ("C-x n r" . denote-rename-file)
   ("C-x n R" . denote-rename-file-using-front-matter)
   :map org-mode-map
   ("C-x n d b" . denote-org-extras-dblock-insert-backlinks)
   ("C-x n d l" . denote-org-extras-dblock-insert-links)
   ("C-x n b" . denote-backlinks)
   ("C-x n i" . denote-link) ; "insert" mnemonic
   ("C-x n I" . denote-add-links)
   ("C-x n r" . denote-rename-file)
   ("C-x n R" . denote-rename-file-using-front-matter)
   :map dired-mode-map
   ("C-x n i" . denote-dired-link-marked-notes)
   ("C-x n r" . denote-dired-rename-marked-files)
   ("C-x n R" . denote-dired-rename-marked-files-using-front-matter)
   ("C-x n t" . denote-dired-rename-marked-files-with-keywords)))

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

  (bind-keys
   :map +notes-prefix-map
   ("f" . +denote-find-file)
   ("g" . consult-denote-grep)
   :map +search-prefix-map
   ("M-n" . consult-denote-find)
   ("n" . consult-denote-grep)))

(use-package org-remark
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
  ;; and in-depth method that records key points and inspirations, faciliting
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

  ;; Create a Denote-compatible marginal note
  (defun +org-remark-denote--note (filename)
    "Find the Denote filename similar to FILENAME but with the 'literature' keyword."
    (if-let* ((source-title (denote-retrieve-filename-title filename))
              (source-signature (denote-retrieve-filename-signature filename))
              (source-keywords (denote-retrieve-filename-keywords filename))
              (source-keywords (if source-keywords
                                   (split-string source-keywords "_")
                                 nil))
              (buffer-files (mapcar
                             (lambda (buffer)
                               (buffer-file-name buffer))
                             (buffer-list)))
              (buffer-files (cl-remove nil buffer-files))
              (files (cl-union (denote-directory-files) buffer-files)))
        (cl-find-if (lambda (file)
                      (let* ((file-title (denote-retrieve-filename-title file))
                             (file-signature (denote-retrieve-filename-signature file))
                             (file-keywords (denote-retrieve-filename-keywords file))
                             (file-keywords
                              (if (and source-keywords file-keywords)
                                  (split-string file-keywords "_")
                                nil)))
                        (and (string= file-title source-title)
                             (string= file-signature source-signature)
                             (member "literature" file-keywords)
                             (seq-set-equal-p
                              (seq-remove (lambda (elt) (member elt '("literature" "reference")))
                                          source-keywords)
                              (seq-remove (lambda (elt) (member elt '("literature" "reference")))
                                          file-keywords)))))
                    files)
      nil))

  (defun +org-remark-denote-file-name-function ()
    "Return a Denote-compatible file name for the current buffer.

When the current buffer is visiting a file, the name of the
marginal notes file will be \"DATE==SIGNATURE--TITLE__literature.org\"
in your `denote-directory'."
    (if-let* ((source-filename (cond ((eq major-mode 'nov-mode)
                                      (file-name-nondirectory nov-file-name))
                                     ((eq major-mode 'pdf-view-mode)
                                      (file-name-nondirectory buffer-file-name))
                                     (t
                                      (org-remark-source-find-file-name))))
              (is-source-denote (if source-filename
                                    (denote-file-has-denoted-filename-p source-filename)
                                  nil))
              (literature-note (+org-remark-denote--note source-filename)))
        (cond (literature-note literature-note)
              (t
               (denote-format-file-name
                (denote-directory)
                (denote--find-first-unused-id (denote-get-identifier (current-time)))
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
                  ""))))
      "marginalia.org"))

  (setopt org-remark-notes-file-name #'+org-remark-denote-file-name-function)

  (bind-keys
   :map org-remark-mode-map
   ("C-c m d" . org-remark-delete)
   ("C-c m l" . org-remark-mark-line)
   ("C-c m m" . org-remark-mark)
   ("C-c m n" . org-remark-view-next)
   ("C-c m o" . org-remark-open)
   ("C-c m p" . org-remark-view-prev)
   ("C-c m r" . org-remark-remove)
   ("C-c m v" . org-remark-view)))

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

  (bind-keys
   :map +search-prefix-map
   ("l" . citar-open)
   :map +bib-prefix-map
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
   citar-denote-keyword "literature")

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

;;;;;;;;;;;;;;;;;
;;;; reading ;;;;

;;;;;;;;;;;;;;;
;;;; media ;;;;

(use-package mpv
  :config
  (setopt mpv-executable "umpv"))

(use-package empv
  :config
  ;; `empv-play-video' and `empv-play-audio' lets me select a local media file
  ;; under the following directories with a `completing-read' interface.  Also
  ;; `empv-play-file' uses a classic `read-file-name' interface.
  (setopt empv-video-dir (concat (denote-directory) "reference/")
          empv-audio-dir (concat (denote-directory) "reference/"))

  ;; If I start playing a YouTube video, it'll start playing itself in the
  ;; background. I'm often tempted to call `empv-toggle-video' to start watching
  ;; the video but it will not work. `mpv' tries to be smart when it's in the
  ;; background and only downloads the audio if possible. If I want to be able
  ;; to watch YouTube videos on demand, I need to add the following
  ;; configuration to change the args supplied to mpv to force it to download
  ;; videos.
  (add-to-list 'empv-mpv-args
               ;; It's bestvideo+bestaudio/best by default. We slightly change
               ;; it to override the default no-video behavior.
               "--ytdl-format=bestvideo+bestaudio/best[ext=mp4]/best"))

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
   ("M-s M-h" . +consult-history-eww)
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
   ("f" . elpher-jump)
   ("g" . elpher-reload) ; overrides `elpher-go'
   ("l" . elpher-back)
   ;; ("L" . +elpher-list-histories)
   ("m" . elpher-jump)
   ("o" . elpher-go-current)
   ;; ("r" . +elpher-forward)
   ("R" . nil) ; unmap `elpher-reload'
   ("s" . nil) ; unmap `elpher-show-history'
   ("S" . nil) ; unmap `elpher-show-visited-pages'
   ("t" . elpher-root-dir)
   ("v" . elpher-view-raw)
   ;; ("w" . +elpher-copy-page-url) ; link at point or current page
   ("<tab>" . elpher-next-link)
   ("<backtab>" . elpher-prev-link)))

;; NOTE I don't remember why this has to be outside a use-package block
(use-package qutebrowser
  :no-require
  :config
  ;; NOTE this is for when I want to use qutebrowser.el
  ;; (with-eval-after-load 'modus-themes
  ;;   (dolist (entry '((completion.match.fg . modus-themes-completion-match-0)
  ;;                    (completion.item.selected.match.fg . modus-themes-completion-match-0)
  ;;                    (completion.item.selected.bg . modus-themes-completion-selected)
  ;;                    (completion.item.selected.border.top . modus-themes-completion-selected)
  ;;                    (completion.item.selected.border.bottom . modus-themes-completion-selected)
  ;;                    (contextmenu.selected.bg . modus-themes-completion-selected)
  ;;                    (contextmenu.selected.fg . modus-themes-completion-selected)
  ;;                    (keyhint.bg . modus-themes-completion-selected)
  ;;                    (prompts.bg . modus-themes-completion-selected)))
  ;;     (let ((existing-entry (assoc (car entry) qutebrowser-theme-export-face-mappings)))
  ;;       (if existing-entry
  ;;           (setf (cdr existing-entry) (cdr entry))  ; Update the value if the key exists
  ;;         (push entry qutebrowser-theme-export-face-mappings)))))

  (defun +qutebrowser-choose-file ()
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-file qute-filename
        (insert (s-join "\n" files)))
      (unbind-key "C-c C-c" 'dired-mode-map)
      (kill-buffer)))

  (defun +qutebrowser-dired-hook (&optional _)
    (when (s-starts-with? "/tmp/qutebrowser-fileselect" buffer-file-name)
      (setq qute-filename buffer-file-name)
      (kill-buffer)
      (dired "~/")
      (bind-key "C-c C-c" #'+qutebrowser-choose-file 'dired-mode-map)))

  (add-hook 'server-visit-hook #'+qutebrowser-dired-hook))

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

  ;; This fixes the missing Nix icon when using `consult-buffer'
  (with-eval-after-load 'nix-ts-mode
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(nix-mode nerd-icons-devicon "nf-dev-nixos" :face
                   nerd-icons-blue))
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(nix-ts-mode nerd-icons-devicon "nf-dev-nixos" :face
                   nerd-icons-blue)))

  (with-eval-after-load 'nov
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(nov-mode nerd-icons-mdicon "nf-md-book_open" :face
                   nerd-icons-green)))

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
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :if +emacs-load-icons
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
  (nerd-icons-grep-mode))
