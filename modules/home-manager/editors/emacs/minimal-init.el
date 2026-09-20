;;; minimal-init.el --- This is my init. -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;
;;;; themes ;;;;

(use-package modus-themes
  :init
  (load-theme 'modus-vivendi))

;;;;;;;;;;;;
;;;; ui ;;;;

(use-package whitespace
  :config
  ;; Emacs has very comprehensive whitespace rendering capabilities. I do not
  ;; render newline and space characters (see my tab configuration) because they
  ;; are easy to infer in most cases, but also because `whitespace-mode'
  ;; highlights each whitespace with a face which can cripple performance in
  ;; larger files. Since I only render trailing whitespace, empty lines, and tab
  ;; characters to draw attention to fix these mistakes, this ends up not
  ;; mattering as much.
  (setopt whitespace-style '(face trailing tabs tab-mark lines-tail empty)
          whitespace-display-mappings
          '((space-mark   ?\     [?\u00B7]     [?.])
            (space-mark   ?\xA0  [?\u00A4]     [?_])
            (newline-mark ?\n    [?\u21A9 ?\n])
            (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])))

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

;;;;;;;;;;;;;;;
;;;; files ;;;;

(use-package files
  :config
  (setopt y-or-n-p-use-read-key t
          use-short-answers t))

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
  (setopt auto-revert-remote-files nil ; this slows down tramp
          auto-revert-verbose t ; show message when file changes
          auto-revert-avoid-polling t ; use save signal
          global-auto-revert-non-file-buffers t)

  (global-auto-revert-mode))

;;;;;;;;;;;;;;;;;;;;
;;;; completion ;;;;

(use-package minibuffer
  :config
  ;; The term "completion" describes a process where user input is
  ;; assited by pattern matching algorithms to type out incomplete
  ;; items. The most basic way of this model of interaction is what we
  ;; get in a command-line prompt, where we can hit `TAB' to expand
  ;; the word before point to something the program already knows
  ;; about (e.g. `ema' followed by `TAB' may complete to `emacs').
  ;;
  ;; In Emacs, completion encompasses user interfaces that show the
  ;; available candidates (the likely options) right away, as well as
  ;; provide more advanced capabilities for storing the history of
  ;; previous inputs, displaying helpful annotations next to each
  ;; candidate, and "completion styles" to control how user input is
  ;; matched to candidates. Because we use the minibuffer for most
  ;; common interactions, completion is an integral part of any setup.

  ;; The `completion-styles' are pattern matching algorithms. They
  ;; interpret input and match candidates accordingly.
  ;;
  ;; `emacs-22': prefix completion that only operates on the text
  ;; before point. If we are in "prefix|suffix", with "|" representing
  ;; the cursor, it will consider everything that expands "prefix" and
  ;; then add back to it the "suffix".
  ;;
  ;; `basic': prefix completion that also accounts for the text after
  ;; point. Using the above example, this one will consider patterns
  ;; that match all of `emacs22' as well as anything thet completes
  ;; "suffix".
  ;;
  ;; `partial-completion': this is used for file navigation. Instead
  ;; of typing out a full path like "~/.local/share/fonts", we do
  ;; "~/.l/s/f" or variants thereof to make the matches unique such as
  ;; ~/.l/sh/fon. It is a joy to navigate the file system in this way.
  ;;
  ;; `substring': matches the given sequence of characters literally
  ;; regardless of where it is in a word. So "pro" will match
  ;; "professional" as well as "reproduce".
  ;;
  ;; `flex': completion of an in-order subset of characters. It does
  ;; not matter where the characters are in the word, so long as they
  ;; are encountered in the given order. The input "lad" will thus
  ;; match "list-faces-display" as well as "pulsard-highlight-dwim".
  ;;
  ;; `initials': completion of acronyms and initialisms. Typing "lfd"
  ;; will thus match "list-faces-display". This completion style can
  ;; also be used for file system navigation, though I prefer to only
  ;; have `partial-completion' handle that task.

  ;; Now that you know about the completion styles I use or have
  ;; experimented with, take a look at the value of my
  ;; `completion-styles'. Emacs tries the styles in the given order
  ;; from left to right, moving to the next one until it finds a
  ;; match. As such, I usually want to start with tight matches
  ;; (e.g. "li-fa-di" for "list-faces-display") and only widen the
  ;; scope of the search as I need to.

  ;; That is not all though, as we still have to consider what happens
  ;; when the minibuffer prompt we are using defines a specific
  ;; completion category. We can arrange to have different
  ;; `completion-styles' per category and many more customisations
  ;; which, of course, I do take care of.
  (setopt completion-styles '(basic
                              substring initials
                              partial-completion
                              flex))

  (setopt completion-flex-nospace t)
  (setopt completion-pcm-leading-wildcard nil) ; Emacs 31
  (setopt completion-ignore-case t
          read-file-name-completion-ignore-case t
          read-buffer-completion-ignore-case t)

  ;; This builds on the code we have for the `completion-styles'. It
  ;; is also related to the generic minibuffer completion interface in
  ;; general.
  ;;
  ;; The `completion-styles' is the fallback option in case there is
  ;; no provision for the given completion category. The completion
  ;; category is a piece of metadata that is associated with the
  ;; completion table we are matching against while using the
  ;; minibuffer. For example, the `find-file' command has the `file'
  ;; category, while the `switch-to-buffer' command uses the `buffer'
  ;; category. The defaults for those are specified in the
  ;; `completion-category-defaults'. The overrides for them can be set
  ;; in the `completion-category-overrides'.
  ;;
  ;; While we can override only the categories we care about, the
  ;; presence of those `completion-category-defaults' will surprise us
  ;; in some cases because we will not be using what we specified in
  ;; the `completion-styles'. As such, I set
  ;; `completion-category-defaults' to nil, to always fall back to my
  ;; preferred `completion-styles' and then I further configure
  ;; overrides where those make sense to me.
  ;;
  ;; Overrides are not limited to completion styles. We can also set
  ;; our own metadata, as described in the documentation of the
  ;; function `completion-metadata'. Concretely, we can set our own
  ;; functions for annotating, sorting, and grouping candidates. It
  ;; also sets things up for certain prompts without a completion
  ;; category to get one and thus be subject to the customisations I
  ;; define herein.

  ;; Reset all the per-category defaults so that (i) we use the
  ;; standard `completion-styles' and (ii) can specify our own styles
  ;; in the `completion-category-overrides' without having to
  ;; explicitly override anything.
  ;; (setopt completion-category-defaults nil)

  ;; To set `eager-display' and `eager-update' in the overrides, we
  ;; need to have these variable configured accordingly.
  (setopt completion-eager-display 'auto) ; 'auto
  (setopt completion-eager-update t) ; 'auto

  (setopt completion-show-help nil)
  ;; Do not print messages in the echo area that pertain to completion
  ;; --- those are distracting.
  (setopt completion-show-inline-help nil)
  ;; Show useful annotations in various minibuffer prompts (though the
  ;; `marginalia' package greatly improves this).
  (setopt completions-detailed t)
  ;; Do not use rows and columns for completions: a single vertical
  ;; list is easier to follow.
  (setopt completions-format 'one-column)
  ;; Put an upper limit to the Completions window, so that it does not
  ;; disorient me.
  (setopt completions-max-height 12)
  ;; Rely on previous inputs to surface candidates towards the top of
  ;; the list (enable the built-in `savehist-mode' to persist
  ;; history).
  (setopt completions-sort 'historical)
  ;; Show the Completions buffer if I hit TAB but there is no unique
  ;; match yet.
  (setopt completion-auto-help t)
  ;; Never switch to the Completions buffer when I type TAB, because I
  ;; want to select candidates while the minibuffer is still in focus,
  ;; per `minibuffer-visible-completions'. This has the advantage of
  ;; auto-updating the completions as I type.
  (setopt completion-auto-select nil
          minibuffer-visible-completions t)

  (bind-keys :map minibuffer-local-completion-map
             ("C-n" . minibuffer-next-completion)
             ("C-p" . minibuffer-previous-completion)))

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

;;;;;;;;;;;;;;;;;
;;;; windows ;;;;

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
          split-window-preferred-direction 'horizontal
          window-min-height 4
          window-min-width 10)

  ;; Respects display actions when switching buffers
  (setopt switch-to-buffer-obey-display-actions t)
  ;; Ensure Org src buffers are opened using display-buffer
  (setopt org-src-window-setup 'plain))

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

  ;; Automatically place the cursor at the start of an Isearch match when
  ;; exiting.
  (defun +isearch-exit-at-start ()
    "Exit search at the beginning of current match."
    (unless (or isearch-mode-end-hook-quit
                (bound-and-true-p isearch-suspended)
                (not isearch-forward)
                (not isearch-other-end)
                (and (boundp 'avy-command)
                     (eq avy-command 'avy-isearch)))
      (goto-char isearch-other-end)))

  ;; Place the cursor on the opposite end of an Isearch when exitting. Do this
  ;; with `C-RET' while in Isearch.
  (defun +isearch-exit-at-end ()
    "Exit search at the end of the current match."
    (interactive)
    (let ((isearch-other-end (point)))
      (isearch-exit))
    (unless isearch-forward (goto-char isearch-other-end)))

  (add-hook 'isearch-mode-end-hook #'+isearch-exit-at-start)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'search-ring)
    (add-to-list 'savehist-additional-variables 'regexp-search-ring))

  (bind-keys :map global-map
             ("M-s c" . count-matches)
             ("M-s o" . occur)
             ("M-s M-o" . multi-occur)
             :map isearch-mode-map
             ;; The change to `C-g' is important for me as I want to
             ;; exit the search altogether, not resume the search of
             ;; the previous succesful match.
             ("C-g" . isearch-cancel) ; instead of `isearch-abort'
             ("<backspace>" . isearch-del-char)
             ("<C-return>" . +isearch-exit-at-end)
             ("M-/" . isearch-complete)
             :map minibuffer-local-isearch-map
             ("M-/" . isearch-complete-edit)
             :map occur-mode-map
             ("t" . toggle-truncate-lines)))

(use-package grep
  :config
  ;; `grep' is a wrapper for the Unix program of the same name. Not
  ;; much to add there.

  ;; Starting with Emacs 31, buffers using the `grep-mode' can now be
  ;; edited directly. For older versions of Emacs, we have the `wgrep'
  ;; package by Masahiro Hayashi. The idea is to collect the results
  ;; of a search in one place and quickly apply a change across all or
  ;; some of them. We have the same concept with `occur' as well as
  ;; with Dired buffers (see `wdired'). It uses key bindings like
  ;; those of the occur edit mode.

  ;; Type M-s g (`+grep') to perform a "local grep" across the current
  ;; directory. Do C-u M-s g to perform a "recursive grep" from the
  ;; current directory and into all subdirectories. This is basically
  ;; a streamlined version of M-x lgrep and M-x rgrep and is one of my
  ;; favorite commands.
  (defvar +grep--hist nil
    "Input history of grep searches.")

  (defun +grep-prompt (&optional recursive)
    "Prompt for grep pattern.
With optional RECURSIVE, indicate that the search will be called
recursively."
    (read-regexp
     (concat (if recursive
                 (propertize "Recursive" 'face 'warning)
               "Local")
             " grep for PATTERN: ")
     nil '+grep--hist))

  (defun +grep (regexp &optional recursive)
    "Run grep for REGEXP.
Search in the current directory using `lgrep'. With optional prefix
argument (\\[universal-argument]) for RECURSIVE, run a search
starting from the current directory with `rgrep'."
    (interactive
     (list
      (+grep-prompt current-prefix-arg)
      current-prefix-arg))
    (unless grep-command
      (grep-compute-defaults))
    (if recursive
        (rgrep regexp "*" default-directory)
      (lgrep regexp "*" default-directory)))

  (bind-keys :map global-map
             ("M-s g" . +grep)))

;;;;;;;;;;;;;;;;;
;;;; editing ;;;;

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

(use-package delsel
  :config
  ;; Every graphical application I have ever used will delete the selected text
  ;; upon the insertion of new text. Emacs does not do this by default. With
  ;; `delete-selection-mode' we get it.
  (delete-selection-mode +1))

;;;;;;;;;;;;;;;;;;
;;;; spelling ;;;;

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
                                        "DONE(d!)")))

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

  (bind-keys :map global-map
             ("C-c l" . org-store-link)
             :map org-mode-map
             ("C-c l" . org-store-link)
             ("M-." . org-edit-special) ; mnemonic is global M-. that goes to source (alias for C-c ')
             :map org-src-mode-map
             ("M-," . org-edit-src-exit) ; see M-. above
  ))

(use-package org-capture
  :config
  ;; The `org-capture' command allows us to quickly store data in some
  ;; structured way. This is done with the help of a templating system
  ;; where we can, for example, record the date the entry was
  ;; recorded, prompt for user input, automatically use the email's
  ;; subject as the title of the task, and the like. The documentation
  ;; string of `org-capture-templates' covers the technicalities.

  ;; As for my workflow, here is an overview:
  ;;
  ;; When I want to quickly capture any data or idea, I add it to the
  ;;`zettelkasten/inbox.org' file. My goal is to have a non-disrupte
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

  (defun +org-capture-inbox ()
    "Capture something to the inbox and store a link to the current location
if possible."
    (interactive)
    (ignore-errors (call-interactively #'org-store-link))
    (org-capture nil "i"))

  (bind-keys :map global-map
             ("C-x c" . org-capture)
             ("C-x i" . +org-capture-inbox)))

;; I want to directly capture Notmuch links - for example, to add e-mail
;; messages to your to-do list. For that, the function
;; `+org-notmuch-store-and-capture' captures the message-at-point (or query),
;; then calls org-mode's capture functionality.
;; (defun +org-notmuch-store-and-capture ()
;;   "Store a link to the current message or query and capture it with Org."
;;   (interactive)
;;   (call-interactively 'org-store-link)
;;   (org-capture nil "@"))

;; (setopt org-capture-templates-contexts
;;         '(("@" ((in-mode . "notmuch-search-mode")
;;                 (in-mode . "notmuch-show-mode")
;;                 (in-mode . "notmuch-tree-mode")))))

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

  (defun +org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading.

Slightly tweaked version of `org-agenda-format-date-aligned' that
produces dates with a fixed length."
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date t))
           (day (cadr date))
           (day-of-week (calendar-day-of-week date))
           (month (car date))
           (monthname (calendar-month-name month t))
           (year (nth 2 date))
           (iso-week (org-days-to-iso-week
                      (calendar-absolute-from-gregorian date)))
           ;; (weekyear (cond ((and (= month 1) (>= iso-week 52))
           ;;                  (1- year))
           ;;                 ((and (= month 12) (<= iso-week 1))
           ;;                  (1+ year))
           ;;                 (t year)))
           (weekstring (if (= day-of-week 1)
                           (format " (W%02d)" iso-week)
                         "")))
      (format "%s %2d %s %4d%s"
              dayname day monthname year weekstring)))

  (setopt org-agenda-format-date #'+org-agenda-format-date-aligned)

  (defun +org-agenda-set-outline ()
    "Set `outline-regexp' for my Org agenda buffers."
    (when (derived-mode-p 'org-agenda-mode)
      (setq-local outline-regexp "\\(^[ \t]+\\([A-Z]+ \\|[0-9]+:[0-9]+ \\)\\)\\|\\(^[A-Z][^ \t].*\\)")))
  (add-hook 'org-agenda-mode-hook #'+org-agenda-set-outline)

  (defun +org-agenda-custom ()
    "Call Org agenda with my custom daily agenda configuration."
    (interactive)
    (org-agenda nil "A"))

  (bind-keys :map global-map
             ;; TODO replaced abbrev maps, find somewhere to relocate them later
             ("C-x a" . +org-agenda-custom)
             ("C-x C-a" . org-agenda)
             :map org-agenda-mode-map
             ("n" . org-agenda-next-item)
             ("p" . org-agenda-previous-item)))

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
          denote-org-store-link-to-heading 'id)

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

  (defun +denote-find-file ()
    (interactive)
    (let ((default-directory denote-directory))
      (call-interactively #'find-file)))

  (bind-keys :map global-map
             ("C-c n f" . +denote-find-file)
             ("C-c n n" . denote)
             ("C-c n r" . denote-rename-file)))

;;;;;;;;;;;;;;;;;;;;;;
;;;; bibliography ;;;;

;;;;;;;;;;;;;;;
;;;; email ;;;;

;;;;;;;;;;;;
;;;; ai ;;;;
