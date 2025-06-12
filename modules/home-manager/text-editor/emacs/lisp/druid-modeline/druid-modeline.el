;;; druid-modeline.el --- Modeline -*- lexical-binding: t -*-

;; Copyright (C) 2024 Svitax Erevos

;; This package was highly based on the druid-modeline package by Nicolas
;; P. Rougier <Nicolas.Rougier@inria.fr>

;; Author: Svitax Erevos
;; Maintainer: Svitax Erevos <svitaxiom@protonmail.com>
;; URL: https://github.com/svitax/druid-modeline
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, mode-line, header-line

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Druid modeline is a an alternative to the GNU/Emacs modeline. There are
;; several modelines that can be installed on a per-mode basis or as the default
;; one.
;;
;; Usage example:
;;
;; Install prog mode modeline:
;; (add-hook 'prog-mode-hook #'druid-modeline-prog-mode)
;;
;; Make text mode modeline the default:
;; (druid-modeline-text-mode t)
;;
;; Install all available modes:
;; (add-hook 'prog-mode-hook            #'druid-modeline-prog-mode)
;; (add-hook 'text-mode-hook            #'druid-modeline-text-mode)
;; (add-hook 'org-mode-hook             #'druid-modeline-org-mode)
;; (add-hook 'pdf-view-mode-hook        #'druid-modeline-pdf-mode)
;; (add-hook 'mu4e-headers-mode-hook    #'druid-modeline-mu4e-headers-mode)
;; (add-hook 'mu4e-view-mode-hook       #'druid-modeline-mu4e-message-mode)
;; (add-hook 'mu4e-compose-mode-hook    #'druid-modeline-mu4e-compose-mode)
;; (add-hook 'elfeed-show-mode-hook     #'druid-modeline-elfeed-entry-mode)
;; (add-hook 'elfeed-search-mode-hook   #'druid-modeline-elfeed-search-mode)
;; (add-hook 'elpher-mode-hook          #'druid-modeline-elpher-mode)
;; (add-hook 'term-mode-hook            #'druid-modeline-term-mode)
;; (add-hook 'eat-mode-hook             #'druid-modeline-eat-mode)
;; (add-hook 'xwidget-webkit-mode-hook  #'druid-modeline-xwidget-mode)
;; (add-hook 'messages-buffer-mode-hook #'druid-modeline-message-mode)
;; (add-hook 'org-capture-mode-hook     #'druid-modeline-org-capture-mode)
;; (add-hook 'org-agenda-mode-hook      #'druid-modeline-org-agenda-mode

;;; Code:

(require 'cl-lib)

;;;; Group

(defgroup druid nil
  "Druid"
  :group 'convenience)

(defgroup druid-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'druid)

;;;; Custom

(defcustom druid-modeline-padding '(0.20 . 0.25)
  "Default vertical space adjustment (in fraction of character height)"
  :type '(cons
          (float :tag "Top spacing")
          (float :tag "Bottom spacing"))
  :group 'druid-modeline)

(defcustom druid-modeline-position #'druid-modeline-footer
  "Default position for the druid modeline"

  :type '(choice
          (const :tag "Top"    druid-modeline-header)
          (const :tag "Bottom" druid-modeline-footer))
  :group 'druid-modeline)

(defcustom druid-modeline-window-dedicated-symbol '("◦ " . "")
  "Pairs of strings showing a window is dedicated or not dedicated"
  :type '(cons
          (string :tag "Window is dedicated")
          (string :tag "Window is not dedicated"))
  :group 'druid-modeline)

;;;; Faces

(defface druid-modeline-popout-i
  '((t (:inherit (eldoc-highlight-function-argument bold))))
  "Face for when line is active"
  :group 'druid-modeline)

(defface druid-modeline-interactive-i
  '((t (:inherit (shr-selected-link bold))))
  "Face for interactive buffer"
  :group 'druid-modeline)

(defface druid-modeline-faded
  '((t (:inherit (font-lock-comment-face mode-line-active))))
  "Face for faded (active) elements"
  :group 'druid-modeline)

(defface druid-modeline-active
  '((t (:inherit (mode-line-active))))
  "Face for when line is active"
  :group 'druid-modeline)

(defface druid-modeline-inactive
  '((t (:inherit (font-lock-comment-face mode-line-inactive))))
  "Face for when line is inactive"
  :group 'druid-modeline)

(defface druid-modeline-status
  '((t (:inherit (tooltip bold))))
  "Face for line status"
  :group 'druid-modeline)

(defvar druid-modeline-base-face nil)

(defcustom druid-modeline-faces
  '((header-active      . (druid-modeline-active))
    (header-inactive    . (druid-modeline-inactive))
    (footer-active      . (druid-modeline-active))
    (footer-inactive    . (druid-modeline-inactive))
    (status-RW-active   . (druid-modeline-status))
    (status-RO-active   . (druid-modeline-status))
    (status-**-active   . (druid-modeline-status druid-modeline-popout-i))
    (status-IT-active   . (druid-modeline-status druid-modeline-interactive-i))
    (name-active        . (bold))
    (narrow-active      . ())
    (primary-active     . ())
    (secondary-active   . (druid-modeline-faded)))
  "Druid line faces.

Each face defined here is used by the modeline depending on the current
state (active / inactive). It is ok to define a face for a single
state. In such case, the alternative state will use defaults."
  :type '(alist
          :key-type (symbol :tag "Face")
          :value-type (repeat :tag "inherits" face)))

(defface druid-modeline--empty-face
  `((t (:inherit default)))
  "Empty face for setting mode-line / header-line."
  :group 'nil)

(defvar druid-modeline--selected-window nil
  "Selected window before mode-line was activated.")

(defun druid-modeline--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (unless (minibuffer-window-active-p (minibuffer-window))
    (setq druid-modeline--selected-window (selected-window))))

(defun druid-modeline--base-face (face-prefix)
  "Return the face for FACE-PREFIX according to current active state."

  (let* ((window (get-buffer-window (current-buffer)))
         (active (eq window druid-modeline--selected-window))
         (state (intern (concat (symbol-name face-prefix)
                                (if active "-active" "-inactive"))))
         (face (cadr (assoc state druid-modeline-faces))))
    face))

(defun druid-modeline-face (&optional face-prefix)
  "Return the face for FACE-PREFIX according to current active state and
make it inherit the base face."

  (let* ((window (get-buffer-window (current-buffer)))
         (active (eq window druid-modeline--selected-window))
         (state (intern (concat (symbol-name face-prefix)
                                (if active "-active" "-inactive"))))
         (face (cdr (assoc state druid-modeline-faces)))
         (face (if druid-modeline-base-face
                   (push druid-modeline-base-face face)
                 face))
         (face (reverse face)))
    `(:inherit ,face)))

;;;; Mode/header line creation

(defvar-local druid-modeline-left-fringe-width 0)
(defvar-local druid-modeline-right-fringe-width 0)

(defun druid-modeline--make (left right face-prefix)
  "Build a dynamic mode/header line made of LEFT and RIGHT part,
using the given FACE-PREFIX as the default."

  `(:eval
    (let* ((druid-modeline-base-face (druid-modeline--base-face ',face-prefix))
           (left (mapconcat
                  (lambda (element)
                    (if (stringp element)
                        (propertize element 'face druid-modeline-base-face)
                      (apply (car element) (cdr element))))
                  ',left))
           (right (mapconcat
                   (lambda (element)
                     (if (stringp element)
                         (propertize element 'face druid-modeline-base-face)
                       (apply (car element) (cdr element))))
                   ',right))
           (width (window-width))
           (outside fringes-outside-margins)
           (left-fringe (if outside -1.0 0.0))
           (left-margin (if outside 0.0 1.0))
           (right-fringe (if outside -1.0 0.0))
           (right-margin (if outside -1.0 0.0))
           (left-max-size (- width (length right) 2))
           (left (if (> (length left) left-max-size)
                     (concat (truncate-string-to-width left left-max-size)
                      (propertize "…" 'face `(:inherit  ,druid-modeline-base-face)))
                   left)))
     (concat
      (propertize " "
       'display `(space :align-to (+ left-margin
                                   (,left-fringe . left-fringe)
                                   (,left-margin . left-margin))))
      (propertize " " 'face 'fringe
       'display '(space :width (druid-modeline-left-fringe-width)))
      left
      (propertize " "
       'face `(:inherit ,druid-modeline-base-face)
       'display `(space :align-to
                  (- right-margin
                   (,right-fringe . right-fringe)
                   (,right-margin . right-margin)
                   (druid-modeline-right-fringe-width)
                   ,(length right))))
      right
      (propertize " " 'face 'fringe
       'display '(space :width (druid-modeline-right-fringe-width)))))))

(defun druid-modeline-header (left &optional right default)
  "Install a header line made of LEFT and RIGHT parts. Line can be
made DEFAULT."

  (if default
      (setq-default header-line-format (druid-modeline--make left right))
    (setq-local header-line-format (druid-modeline--make left right)))
  ;; (face-remap-set-base 'header-line 'druid-modeline--empty-face)
  (add-hook 'post-command-hook #'druid-modeline--update-selected-window))

(defun druid-modeline-footer (left &optional right default)
  "Install a footer line made of LEFT and RIGHT parts. Line can be
made DEFAULT."
  (face-remap-reset-base 'mode-line)
  (face-remap-reset-base 'mode-line-active)
  (face-remap-reset-base 'mode-line-inactive)
  (if default
      (setq-default mode-line-format (druid-modeline--make left right 'footer))
    (setq-local mode-line-format (druid-modeline--make left right 'footer)))
  ;; (face-remap-set-base 'mode-line 'druid-modeline--empty-face)
  (add-hook 'post-command-hook #'druid-modeline--update-selected-window))

;;;; Segments

;;;;; Status

(defconst druid-modeline-meow-state-tags
  '((normal :status-RW "NO" :status-** "N*" :status-RO "RO")
    (insert :status-RW "IN" :status-** "I*" :status-RO "RO")
    (beacon :status-RW "BE" :status-** "B*" :status-RO "RO")
    (motion :status-RW "MO" :status-** "M*" :status-RO "RO")
    (keypad :status-RW "KE" :status-** "K*" :status-RO "RO")
    (expand :status-RW "EX" :status-** "E*" :status-RO "RO")
    (structural :status-RW "ST" :status-** "S*" :status-RO "RO")
    (vterm-normal :status-RW "NO" :status-** "NO" :status-RO "RO")
    (vterm-insert :status-RW "IN" :status-** "IN" :status-RO "RO"))

  "Read-write, TODO **, read-only tags for Meow states.")

(defun druid-modeline--meow-get-tag (state variant)
  "Get Meow STATE tag of VARIANT.
VARIANT of the state tag is either :status-RW, :status-**, or :status-RO, as
defined in `druid-modeline-meow-state-tags.'"

  (let ((tags (alist-get state druid-modeline-meow-state-tags))
        (variant (intern (format ":%s" variant))))
    (plist-get tags variant)))

(defun druid-modeline-meow-state-tag (variant)
  "Return mode line tag VARIANT depending on the Meow state.
VARIANT of the state tag is either :status-RW, :status-**, or :status-RO, as
defined in `druid-modeline-meow-state-tags.'"

  (pcase (meow--current-state)
    ('normal (druid-modeline--meow-get-tag 'normal variant))
    ('insert (druid-modeline--meow-get-tag 'insert variant))
    ('beacon (druid-modeline--meow-get-tag 'beacon variant))
    ('expand (druid-modeline--meow-get-tag 'expand variant))
    ('motion (druid-modeline--meow-get-tag 'motion variant))
    ('keypad (druid-modeline--meow-get-tag 'keypad variant))
    ('structural (druid-modeline--meow-get-tag 'structural variant))
    ('vterm-normal (druid-modeline--meow-get-tag 'vterm-normal variant))
    ('vterm-insert (druid-modeline--meow-get-tag 'vterm-insert variant))))

(defcustom druid-modeline-symbol-list
  '((buffer-read-only  . "RO")
    (buffer-read-write . "RW")
    (buffer-modified   . "**")
    (buffer-terminal   . ">_")
    (buffer-clone      . "CC")
    (buffer-narrow     . "NRW")
    (window-dedicated . "◦")
    (vc-branch  . "")
    (vc-hash  . "#"))
  "Various symbols used in the modeline. It is possible to add padding to
left and right for symbols that do not align perfectly (NERD
fonts). Default symbols make use of NERD font and may appear as tofu if
the fontis not installed on your system. Either install NERD font or use
other characters."
  :type '(alist
          :key-type symbol
          :value-type (choice (string :tag "Label")
                       (cons  :tag "Label with padding" (string :tag "Label")
                        (cons :tag "Padding in pixels"
                         (integer :tag "Left")
                         (integer :tag "Right")))))
  :group 'druid-modeline)

(defun druid-modeline-symbol (name)
  "Retrieve SYMBOL from the nano-modeline-symbols list"
  (or (alist-get name druid-modeline-symbol-list) "??"))

(defun druid-modeline-buffer-status (&optional symbol face padding)
   "Return a prefix indicating if buffer is read-only, read-write or modified"
  (let* ((padding (or padding druid-modeline-padding))
         (top (propertize " " 'display `(raise ,(car padding))))
         (bot (propertize " " 'display `(raise ,(- (cdr padding)))))
         (face (or face (cond (buffer-read-only (druid-modeline-face 'status-RO))
                              ((buffer-modified-p) (druid-modeline-face 'status-**))
                              (t (druid-modeline-face 'status-RW)))))
         (symbol (or symbol (cond ((buffer-narrowed-p) (druid-modeline-symbol 'buffer-narrow))
                                  ((buffer-base-buffer) (druid-modeline-symbol 'buffer-clone))
                                  (buffer-read-only (druid-modeline-symbol 'buffer-read-only))
                                  ((buffer-modified-p) (druid-modeline-symbol 'buffer-modified))
                                  (t (druid-modeline-symbol 'buffer-read-write))))))
    (propertize (concat top symbol bot) 'face face)))

(defun druid-modeline-buffer-interactive (&optional symbol face padding)
  "Return a prefix indicating if buffer is interactive, read-write or modified"
  (let* ((padding (or padding druid-modeline-padding))
         (top (propertize " " 'display `(raise ,(car padding))))
         (bot (propertize " " 'display `(raise ,(- (cdr padding)))))
         (face (or face (cond (buffer-read-only (druid-modeline-face 'status-IT))
                              ((buffer-modified-p) (druid-modeline-face 'status-IT))
                              (t (druid-modeline-face 'status-IT)))))
         (symbol (or symbol (cond ((buffer-narrowed-p) (druid-modeline-symbol 'buffer-narrow))
                                  ((buffer-base-buffer) (druid-modeline-symbol 'buffer-clone))
                                  (buffer-read-only (druid-modeline-symbol 'buffer-read-only))
                                  ((buffer-modified-p) (druid-modeline-symbol 'buffer-modified))
                                  (t (druid-modeline-symbol 'buffer-read-write))))))
    (propertize (concat top symbol bot) 'face face)))

(defun druid-modeline-shell-status ()
  "Shell status"
  (druid-modeline-buffer-status ">_" (druid-modeline-face 'status-RO)))

;;;;; Name

;;;;;; Buffer Identification

(defun druid-modeline-buffer-identification (&optional name)
  "Buffer name"
  (propertize
   (cond (name name)
         ((derived-mode-p '(shell-mode)) "*shell*")
         ((derived-mode-p '(eshell-mode)) "*eshell*")
         ((derived-mode-p '(vterm-mode)) "*vterm*")
         (t (buffer-name)))
   'face (druid-modeline-face 'name)))

;;;;;; Remote

;; TODO +modeline-remote-status

;;;;; Narrow

(defun druid-modeline-narrow-status (&optional name)
  "Narrowed status"
  (propertize
   (cond (name name)
         ((buffer-narrowed-p) "[narrow]")
         (t ""))
   'face (druid-modeline-face 'narrow)))

;; TODO can i merge org-narrow-status into narrow-status by
;; using (and (eq major-mode 'org-mode))
(defun druid-modeline-org-narrow-status (&optional name)
  "Narrowed status for Org buffers"
  (propertize
   (cond (name name)
         ((buffer-narrowed-p)
          (format "[%s]"
                  (org-link-display-format
                   (substring-no-properties
                    (or (org-get-heading 'no-tags) "-"))))
          (t "")))
   'face (druid-modeline-face 'narrow)))

;;;;; Primary

;;;;;; VC Branch

(defun druid-modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    branch))

;; " "
(defun druid-modeline-vc-info (&optional symbol)
  "VC information as (branch, file status)"

  (when vc-mode
    (when-let* ((file (buffer-file-name))
                (backend (vc-backend file))
                (branch (druid-modeline--vc-branch-name file backend))
                (state (vc-state file)))
      (propertize (format "(%s%s, %s)" (or symbol "#") branch state)
                  'face (druid-modeline-face 'primary)))))

;;;;;; Shell Name

(defcustom druid-modeline-trim-shell-file-name t
  "If non-nil, trim the shell name in the modeline."
  :type 'boolean
  :group 'druid-modeline)

(defun druid-modeline-shell-name (&optional name)
  "Shell name"
  (let ((mode (cond ((derived-mode-p '(term-mode))
                     (cond ((term-in-char-mode) "char")
                           ((term-in-line-mode) "line")
                           (t "???")))
                    ((derived-mode-p '(eat-mode))
                     (cond (eat--semi-char-mode "semi-char")
                           (eat--char-mode "char")
                           (eat--line-mode "line")))
                    (t nil)))
        (shell (if druid-modeline-trim-shell-file-name
                   (file-name-base shell-file-name)
                 shell-file-name)))
    (propertize
     (cond (name name)
           (mode (format ("(%s, %s mode)" shell mode)))
           (t (format "(%s)" shell)))
     'face (druid-modeline-face 'primary))))

;;;;;; GPTel

(defun druid-modeline-gptel-backend ()
  "GPTel backend"
  (propertize
   (format "%s" (gptel-backend-name gptel-backend))
   'face (druid-modeline-face 'primary)))

(defun druid-modeline-gptel-query-status ()
  "gptel query status (ready, waiting,typing)"
  (propertize
   (format "(%s)" (if (stringp mode-line-process)
                      (downcase
                       (string-trim
                        (substring-no-properties mode-line-process)))
                    "ready"))))

(defun druid-modeline-button-gptel-context ()
  "Finalize the capture process."
  (let* ((context (or (car-safe (rassoc gptel--system-message gptel-directives))
                      (gptel--describe-directive gptel--system-message 15)))
         (prompt-re "I want you\\( to\\)* act as a\\(n\\)* \\([A-Za-z ]*\\).")
         (context (if (and (stringp gptel--system-message)
                           (string-match prompt-re gptel--system-message))
                      (match-string 3 gptel--system-message)
                    context)))
    ;; (nano-modeline-button (upcase (format "%s" context))
    ;;                       #'gptel-system-prompt 'active "Set context")
    (propertize (upcase (format "%s" context)))))

(defun druid-modeline-button-gptel-model ()
  "Finalize the capture process."
  (let ((model (gptel--model-name gptel-model)))
    ;; (nano-modeline-button (string-trim (upcase (format" %s" model)))
    ;;                       #'gptel-menu 'active "Set model")
    (propertize (string-trim (upcase (format " %s" model))))))

;; TODO +modeline-book-title

(defun druid-modeline-file-size ()
  "File size in human readable format"

  (if-let* ((file-name (buffer-file-name))
            (file-attributes (file-attributes file-name))
            (file-size (file-attribute-size file-attributes))
            (file-size (file-size-human-readable file-size)))
      (propertize (format "(%s)" file-size)
                  'face (druid-modeline-face 'primary))
    ""))

;; TODO +modeline-buffer-lines

;; TODO +modeline-info-current-file

;; NOTE lol
(defun druid-modeline-misc-info ()
  "Current value of `mode-line-misc-info'."

  (let ((misc-info (format-mode-line mode-line-misc-info)))
    misc-info))

;;;;; Secondary

;;;;;; Org Capture

(defun druid-modeline-org-capture-description ()
  "Org capture description"
  (let* ((header (nth 4 (org-heading-components)))
         (header (or header ""))
         (header (org-link-display-format header))
         (header (replace-regexp-in-string org-ts-regexp3 "" header))
         (header (string-trim header))
         (header (substring-no-properties header)))
    (propertize (format "(%s)" header)
                'face (druid-modeline-face 'primary))))

(defvar org-capture-mode-line-save-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-3] 'org-capture-finalize)
    map)
  "Keymap to put on the Org-Capture save button on the modeline.")
(defun druid-modeline-button-org-capture-save ()
  "Finalize the capture process."
  ;; (nano-modeline-button "SAVE"
  ;;                       #'org-capture-finalize
  ;;                       'active
  ;;                       "Finalize the capture process")
  (propertize "SAVE"
              'face (druid-modeline-face 'secondary)
              'help-echo "mouse-1: finalize capture"
              'mouse-face 'mode-line-highlight
              'local-map org-capture-mode-line-save-keymap))

(defvar org-capture-mode-line-kill-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-3] 'org-capture-kill)
    map)
  "Keymap to put on the Org-Capture kill button on the modeline.")
(defun druid-modeline-button-org-capture-kill ()
  "Abort the current capture process"
  ;; (nano-modeline-button "KILL"
  ;;                       #'org-capture-kill
  ;;                       'active
  ;;                       "Abort the current capture process")
  (propertize "KILL"
              'face (druid-modeline-face 'secondary)
              'help-echo "mouse-1: abort capture"
              'mouse-face 'mode-line-highlight
              'local-map org-capture-mode-line-kill-keymap))

(defvar org-capture-mode-line-refile-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-3] 'org-capture-refile)
    map)
  "Keymap to put on the Org-Capture refile button on the modeline.")

(defvar Info-mode-line-node-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'Info-mouse-scroll-up)
    (define-key map [mode-line mouse-3] 'Info-mouse-scroll-down)
    map)
  "Keymap to put on the Info node name in the mode line.")
(defun druid-modeline-button-org-capture-refile ()
  "Abort the current capture process"
  ;; (nano-modeline-button "REFILE"
  ;;                       #'org-capture-refile
  ;;                       'active
  ;;                       "Finalize the current capture and then refile the entry.")
  (propertize "REFILE"
              'face (druid-modeline-face 'secondary)
              'help-echo "mouse-1: finalize capture and refile"
              'mouse-face 'mode-line-highlight
              'local-map org-capture-mode-line-refile-keymap))

;;;;;; Org Agenda

(defun druid-modeline-calendar-date (&optional format)
  "Calendar date"
  (let* ((date (calendar-cursor-to-date))
         (date (when date
                 (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
         (format (or format "%d %B %Y")))
    (propertize (format-time-string format date)
                'face (druid-modeline-face 'secondary))))

;;;;;; TODO Selection

;;;;;; Default Directory

(defun druid-modeline-default-directory (&optional max-length)
  "Term current directory"

  (let* ((max-length (or max-length 32))
         (dir default-directory)
         (path (reverse (split-string (abbreviate-file-name dir) "/")))
         (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 0)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    (propertize output 'face (druid-modeline-face 'secondary))))

;;;;;; Cursor Position
(defun druid-modeline-cursor-position (&optional format)
  "Cursor position using given FORMAT."

  (let ((format (or format "%l:%c ")))
    (propertize (format-mode-line format)
                'face (druid-modeline-face 'secondary))))

;; TODO +modeline-scroll
(defun druid-modeline-scroll (&optional format)
  "Percent window travel through buffer using given FORMAT."

  (let ((format (or format "%o %% ")))
    (propertize (format-mode-line format)
                'face (druid-modeline-face 'secondary))))

;;;;;; PDF Page

(defun druid-modeline-pdf-page ()
  "PDF view mode page number / page total"

  (let ((page-current (image-mode-window-get 'page))
        (page-total (pdf-cache-number-of-pages)))
    (propertize (format "%d/%d " page-current page-total)
                'face (druid-modeline-face 'secondary))))

;;;;;;; Nov

(defun druid-modeline-nov-title ()
  "Nov title."
  (propertize
   (alist-get 'title nov-metadata)
   'face (druid-modeline-face 'primary)))

(defun druid-modeline-nov-chapter ()
  "Nov chatper title."
  (when (and (boundp 'header-line-format)
             (stringp header-line-format)
             (string-match ":\\s-*\\(.*\\)$" header-line-format))
    (propertize (match-string 1 header-line-format)
                'face (druid-modeline-face 'secondary))))

;;;;;; Info

(defun druid-modeline-info-file ()
  "The topic in the Info buffer"
  (propertize (if (stringp Info-current-file)
                  (replace-regexp-in-string
                   "%" "%%"
                   (file-name-sans-extension
                    (file-name-nondirectory Info-current-file)))
                (format "*%S*" Info-current-file))
              'face (druid-modeline-face 'primary)))

(defun druid-modeline-info-node ()
  "The node in the Info buffer"
  (when Info-current-node
    (propertize (replace-regexp-in-string
                 "%" "%%" Info-current-node)
                'face (druid-modeline-face 'secondary)
                'help-echo
                "mouse-1: scroll forward, mouse-3: scroll back"
                'mouse-face 'mode-line-highlight
                'local-map Info-mode-line-node-keymap)))

;;;;;; EWW

(defun druid-modeline-eww-title ()
  "EWW title"
  (propertize
   (plist-get eww-data :title)
   'face (druid-modeline-face 'primary)))

(defun druid-modeline-button-eww-back ()
  "Go to previous site."
  (if eww-history
      ;; (nano-modeline-button "BACK"
      ;;                       #'eww-back-url
      ;;                       'active
      ;;                       "Go to previous site")
      (propertize "BACK" 'face (druid-modeline-face 'secondary))
    ;; (nano-modeline-button "BACK" nil 'disabled "")
    (propertize "BACK" 'face (druid-modeline-face 'footer))))

;;;;;; Elpher

(defun druid-modeline-elpher-buffer-status ()
  "Elpher protocol"
  (let* ((protocol (elpher-address-protocol (elpher-page-address elpher-current-page)))
         (symbol (cond ((equal protocol "gemini") "GEM")
                       ((equal protocol "gopher") "/")
                       ((equal protocol "about") "/"))))
    (druid-modeline-buffer-status symbol)))

(defun druid-modeline-elpher-title ()
  "Epher page title"
  (propertize
   (elpher-page-display-string elpher-current-page)
   'face (druid-modeline-face 'primary)))

(defun druid-modeline-button-elpher-back ()
  "Go to previous site."
  (if elpher-history
      ;; (nano-modeline-button "BACK"
      ;;                       #'elpher-back
      ;;                       'active
      ;;                       "Go to previous site")
      (propertize "BACK" 'face (druid-modeline-face 'secondary))
    ;; (nano-modeline-button "BACK" nil 'disabled "")
    (propertize "BACK" 'face (druid-modeline-face 'footer))))

;;;;;; Window Dedicated

(defun druid-modeline-window-dedicated-status (&optional dedicated not-dedicated)
  "Pin symbol when window is dedicated"

  (propertize (if (window-dedicated-p)
                  (or dedicated (car druid-modeline-window-dedicated-symbol))
                (or not-dedicated (cdr druid-modeline-window-dedicated-symbol)))
              'face (druid-modeline-face 'secondary)))

;;;; Modes

;; ** init.el@host (#rewrite, edited) | <5L, 258C> 352:58 ◦
(defun druid-modeline-text-mode (&optional default)
  "Modeline for text modes. Can be made DEFAULT mode."

  (funcall druid-modeline-position
           '((druid-modeline-buffer-status) " "
             (druid-modeline-buffer-identification)
             ;; (druid-modeline-remote-status)
             " "
             (druid-modeline-vc-info)
             (druid-modeline-misc-info))
           '(;; (druid-modeline-selection-info)
             (druid-modeline-cursor-position)
             (druid-modeline-window-dedicated-status))
           default))

;; ** init.el@host (#rewrite, edited) | <5L, 258C> 352:58 ◦
(defun druid-modeline-prog-mode (&optional default)
  "Modeline for prog modes. Can be made DEFAULT mode."
  (funcall druid-modeline-position
           '((druid-modeline-buffer-status) " "
             (druid-modeline-buffer-identification)
             ;; (druid-modeline-remote-status)
             " "
             (druid-modeline-vc-info)
             (druid-modeline-misc-info))
           '(;; (druid-modeline-selection-info)
             (druid-modeline-cursor-position)
             (druid-modeline-window-dedicated-status))
           default))

;; ** how-to-study.org@host (#rewrite, edited) | <5L, 258C> 352:58 ◦
(defun druid-modeline-org-mode (&optional default)
  "Modeline for org-mode."
  (funcall druid-modeline-position
           '((druid-modeline-buffer-status) " "
             (druid-modeline-buffer-identification)
             ;; (druid-modeline-remote-status)
             " "
             ;; (druid-modeline-org-narrow-status) " "
             (druid-modeline-vc-info)
             (druid-modeline-misc-info))
           '(;; (druid-modeline-selection-info)
             (druid-modeline-cursor-position)
             (druid-modeline-window-dedicated-status))
           default))

;; ** Agenda | Tuesday 22 October 2024 ◦
(defun druid-modeline-org-agenda-mode ()
  (funcall druid-modeline-position
           '((druid-modeline-buffer-status) " "
             (druid-modeline-buffer-identification)
             (druid-modeline-misc-info))
           '((druid-modeline-calendar-date) " " ; "%A %-e %B %Y" as arg
             (druid-modeline-window-dedicated-status))))

;; NRW Capture (my org capture description) | KILL REFILE SAVE ◦
(defun druid-modeline-org-capture-mode ()
  "Modeline for Org Capture buffers."
  (funcall druid-modeline-position
           '((druid-modeline-buffer-status) " "
             (druid-modeline-buffer-identification) " "
             (druid-modeline-org-capture-description)
             (druid-modeline-misc-info))
           '((druid-modeline-button-org-capture-kill) " "
             (druid-modeline-button-org-capture-refile) " "
             (druid-modeline-button-org-capture-save) " "
             (druid-modeline-window-dedicated-status))))

;; PDF lets-go.pdf (4.5M) | 12/426 ◦
(defun druid-modeline-pdf-mode (&optional default)
  "Modeline for pdf-view-mode."
  (funcall druid-modeline-position
           '((druid-modeline-buffer-status "PDF") " "
             (druid-modeline-buffer-identification) " "
             (druid-modeline-file-size)
             (druid-modeline-misc-info))
           '((druid-modeline-pdf-page)
             (druid-modeline-window-dedicated-status))
           default))

;; NOV Build an Orchestrator in Go | 8 An API for the manager ◦
(defun druid-modeline-nov-mode ()
  "Modeline for nov-mode."
  (funcall druid-modeline-position
           '((druid-modeline-buffer-status "NOV") " "
             (druid-modeline-nov-title) " "
             (druid-modeline-file-size)
             (druid-modeline-misc-info))
           '((druid-modeline-cursor-position) " "
             (druid-modeline-nov-chapter) " "
             (druid-modeline-window-dedicated-status))))

;; TODO druid-modeline-info-mode
;; INFO coreutils | Output of entire files ◦
(defun druid-modeline-info-mode ()
  "Modeline for Info files."
  (funcall druid-modeline-position
           '((druid-modeline-buffer-status "INFO") " "
             (druid-modeline-info-file)
             (druid-modeline-misc-info))
           '((druid-modeline-cursor-position) " "
             (druid-modeline-info-node) " "
             (druid-modeline-window-dedicated-status))))

;; >_ *vterm*@host (#zsh, line mode) | <5L, 258C> .../nix-dotfiles/ ◦
(defun druid-modeline-shell-mode (&optional default)
  "Modeline for terminals and shells."
  (funcall druid-modeline-position
           '((druid-modeline-shell-status) " "
             (druid-modeline-buffer-identification)
             ;; (druid-modeline-remote-status)
             " "
             (druid-modeline-shell-name)
             (druid-modeline-misc-info))
           '(;; (druid-modeline-selection-info)
             (druid-modeline-default-directory) " "
             (druid-modeline-window-dedicated-status))
           default))

;; GEM auragem.lets.dev | BACK ◦
(defun druid-modeline-elpher-mode ()
  "Modeline for elpher."
  (funcall druid-modeline-position
           '((druid-modeline-elpher-buffer-status) " "
             (druid-modeline-elpher-title)
             (druid-modeline-misc-info))
           '((druid-modeline-button-elpher-back) " "
             (druid-modeline-window-dedicated-status))))

;; WEB Ink & Switch | BACK ◦
(defun druid-modeline-eww-mode ()
  "Modeline for eww."
  (funcall druid-modeline-position
           '((druid-modeline-buffer-status "WEB") " "
             (druid-modeline-eww-title)
             (druid-modeline-misc-info))
           '((druid-modeline-button-eww-back) " "
             (druid-modeline-window-dedicated-status))))

;; ** Ollama (typing) | MEDIA Context Model ◦
(defun druid-modeline-gptel-mode ()
  "Modeline for GPTel."
  (funcall druid-modeline-position
           '((druid-modeline-buffer-interactive) " "
             (druid-modeline-gptel-backend) " "
             (druid-modeline-gptel-query-status)
             (druid-modeline-misc-info))
           '(;; (druid-modeline-button-gptel-media) " "
             (druid-modeline-button-gptel-context) " "
             (druid-modeline-button-gptel-model) " "
             (druid-modeline-window-dedicated-status))))

;; ace-window number segment? from doom-modeline

;; dap debug states segment? from doom-modeline

;; NOTE repl mode
;; REPL CIDER (connected) | <5L, 258C> 352:58 ◦

;; NOTE xwidget mode
;; URL

;; NOTE notmuch headers mode
;; MAIL
;; NOTE notmuch message mode
;; FROM
;; NOTE notmuch compose mode
;; DRAFT Message

;; NOTE elfeed entry mode
;; NOTE elfeed search mode
;; NEWS

;; NOTE image mode
;; from doom-modeline

(provide 'druid-modeline)
;;; druid-modeline.el ends here
