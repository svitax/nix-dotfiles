;;; druid-modeline.el --- Modeline -*- lexical-binding: t -*-

;; Copyright (C) 2024 Svitax Erevos

;; This package was highly based on the druid-modeline package by Nicolas P. Rougier
;; <Nicolas.Rougier@inria.fr>

;; Author: Svitax Erevos
;; Maintainer: Svitax Erevos <svitaxiom@protonmail.com>
;; URL: https://github.com/svitax/druid-modeline
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, mode-line, header-line

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Druid modeline is a an alternative to the GNU/Emacs modeline. It can
;; There are several modelines that can be installed on a per-mode basis
;; or as the default one.
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

(defcustom druid-modeline-window-dedicated-symbol '(" " . "")
  "Pairs of strings showing a window is dedicated or not dedicated"
  :type '(cons
	  (string :tag "Window is dedicated")
          (string :tag "Window is not dedicated"))
  :group 'druid-modeline)

;;;; Faces

(defface druid-modeline-popout-i
  '((t (:foreground "#000000" 
	:background "#ffcfbf")))
  "Face for when line is active"
  :group 'druid-modeline)

(defface druid-modeline-faded
  '((t (:foreground "#585858")))
  "Face for when line is active"
  :group 'druid-modeline)

(defface druid-modeline-active
  `((t (:foreground "#000000"
        :background "#f2f2f2")))
  "Face for when line is active"
  :group 'druid-modeline)

(defface druid-modeline-inactive
  '((t (:inherit (druid-modeline-faded druid-modeline-active))))
  "Face for when line is inactive"
  :group 'druid-modeline)

(defface druid-modeline-status
  `((t (:foreground "#000000"
        :background "#e0e0e0"
        :inherit bold)))
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
    (name-active        . (bold))
    (narrow-active      . ())
    (primary-active     . ())
    (secondary-active   . (druid-modeline-faded)))
  "Druid line faces.

Each face defined here is used by the modeline depending on the current state (active / inactive). It is ok to define a face for a single state. In such case, the alternative state will use defaults."
  :type '(alist
	  :key-type (symbol :tag "Face")
          :value-type (repeat :tag "inherits" face)))

(defface druid-modeline--empty-face
  `((t (:foreground "#000000"
	:background "#f2f2f2")))
  "Empty face for setting mode-line / header-line."
  :group 'druid-modeline)

(defvar druid-modeline--selected-window nil
  "Selected window before mode-line was activated.")

(defun druid-modeline--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq druid-modeline--selected-window (selected-window)))

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
    (setq-local header-line-format (druid-modeline--make left right))))

(defun druid-modeline-footer (left &optional right default)
  "Install a footer line made of LEFT and RIGHT parts. Line can be
made DEFAULT."

  (if default
      (setq-default mode-line-format (druid-modeline--make left right 'footer))
    (setq-local mode-line-format (druid-modeline--make left right 'footer)))
  (face-remap-set-base 'header-line 'druid-modeline--empty-face)
  (face-remap-set-base 'mode-line 'druid-modeline--empty-face)
  (face-remap-set-base 'mode-line-inactive 'druid-modeline--empty-face)
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

(defun druid-modeline-buffer-status (&optional status padding)
  "Generic prefix to indicate buffer STATUS with vertical PADDING (top . bottom)"

  (let* ((padding (or padding druid-modeline-padding))
         (top (propertize " " 'display `(raise ,(car padding))))
         (bot (propertize " " 'display `(raise ,(- (cdr padding))))))
    (cond
     ((bound-and-true-p meow-mode)
      (let* ((variant (cond ((derived-mode-p '(eshell-mode vterm-mode)) 'status-RW)
			    (buffer-read-only 'status-RO)
			    ((buffer-modified-p) 'status-**)
			    (t 'status-RW))))
	(propertize (concat top (druid-modeline-meow-state-tag variant) bot)
		    'face (druid-modeline-face variant))))
     (buffer-read-only
      (propertize (concat top (or status "RO") bot)
		  'face (druid-modeline-face 'status-RO)))
     ((buffer-modified-p)
      (propertize (concat top (or status "**") bot)
		  'face (druid-modeline-face 'status-**)))
     (t
      (propertize (concat top (or status "RW") bot)
		  'face (druid-modeline-face 'status-RW))))))

;;;;; Name

;;;;;; Buffer Identification

(defun druid-modeline-buffer-identification (&optional name)
  "Buffer name"

  (propertize
   (cond (name name)
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

(defun druid-modeline-shell-name (&optional name)
  "Shell name"

  (propertize
   (cond (name name)
	 ;; NOTE eshell buffers already get named *eshell* so this is redundant information
	 ((derived-mode-p 'eshell-mode) "")
	 ;; TODO should i trim it? 
	 (t (format "(%s)" (file-name-base shell-file-name))))
   'face (druid-modeline-face 'primary)))

;;;;;; Shell Mode

;; TODO +modeline-shell-mode
;; TODO +modeline-vterm-shell-mode
;; TODO +modeline-eat-shell-mode

(defun druid-modeline-term-mode-name ()
  "Term mode name"

  (propertize (if (term-in-char-mode)
                  "(char mode)"
                "(line mode)")
              'face (druid-modeline-face 'primary)))

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

;;;;;; TODO Org Capture

;;;;;; TODO Org Agenda

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

;;;;;; Window Dedicated

(defun druid-modeline-window-dedicated-status (&optional dedicated not-dedicated)
  "Pin symbol when window is dedicated"

  (propertize (if (window-dedicated-p)
                  (or dedicated (car druid-modeline-window-dedicated-symbol))
                (or not-dedicated (cdr druid-modeline-window-dedicated-symbol)))
	      'face (druid-modeline-face 'secondary)))

;;;; Modes

(defun druid-modeline-text-mode (&optional default)
  "Druid line for text modes. Can be made DEFAULT mode."

  (funcall druid-modeline-position
           '((druid-modeline-buffer-status) " "
             (druid-modeline-buffer-identification) " "
	     (druid-modeline-narrow-status)
             (druid-modeline-vc-info)
	     (druid-modeline-misc-info))
           '((druid-modeline-cursor-position)
             (druid-modeline-window-dedicated-status))
           default))

(defun druid-modeline-prog-mode (&optional default)
  "Druid line for prog modes. Can be made DEFAULT mode."

  (funcall druid-modeline-position
           '((druid-modeline-buffer-status) " "
             (druid-modeline-buffer-identification) " "
	     (druid-modeline-narrow-status) " "
             (druid-modeline-vc-info)
	     (druid-modeline-misc-info))
           '((druid-modeline-cursor-position)
             (druid-modeline-window-dedicated-status))
           default))

(defun druid-modeline-org-mode (&optional default)
  "Druid line for org-mode."

  (funcall druid-modeline-position
           '((druid-modeline-buffer-status) " "
             (druid-modeline-buffer-identification) " "
	     (druid-modeline-org-narrow-status) " "
             (druid-modeline-vc-info)
	     (druid-modeline-misc-info))
           '((druid-modeline-cursor-position)
             (druid-modeline-window-dedicated-status))
           default))

(defun druid-modeline-pdf-mode (&optional default)
  "Druid line for pdf-view-mode."

  (funcall druid-modeline-position
           '((druid-modeline-buffer-status "PDF") " "
             (druid-modeline-buffer-identification) " "
	     (druid-modeline-file-size)
	     (druid-modeline-misc-info))
           '((druid-modeline-pdf-page)
             (druid-modeline-window-dedicated-status))
           default))

(defun druid-modeline-vterm-mode (&optional default)
  "Druid line for vterm-mode."

  (funcall druid-modeline-position
           '((druid-modeline-buffer-status) " "
             (druid-modeline-buffer-identification) " "
             (druid-modeline-shell-name)
	     (druid-modeline-misc-info))
           '((druid-modeline-default-directory 16) " "
             (druid-modeline-window-dedicated-status))
           default))

(defun druid-modeline-eshell-mode (&optional default)
  "Druid line for eshell-mode."

  (funcall druid-modeline-position
           '((druid-modeline-buffer-status) " "
             (druid-modeline-buffer-identification) " "
             (druid-modeline-shell-name)
	     (druid-modeline-misc-info))
           '((druid-modeline-default-directory 16) " "
             (druid-modeline-window-dedicated-status))
           default))

;; ace-window number segment? from doom-modeline

;; dap debug states segment? from doom-modeline

;; NOTE default mode
;; ** <N> init.el@host [narrow] (#rewrite, edited) | <5L, 258C> 352:58 ◦
;; buffer-status, modal, buffer-identification, remote-status, narrow-status, vc-branch, selection, cursor-position, window-dedicated-status

;; NOTE org mode
;; ** <N> how-to-study.org@host [Steps in a study session] (#rewrite, edited) | <5L, 258C> 352:58 ◦
;; buffer-status, modal, buffer-identification, remote-status, narrow-status (org), vc-branch, selection, cursor-position, winwow-dedicated-status

;; NOTE org capture mode
;; ORG <N> Capture [narrow] (my org capture description) | <5L, 258C> 352:58 ◦
;; buffer-status ("ORG"), modal, buffer-identification ("Capture"), narrow-status, org-capture, selection, cursor-position, window-dedicated-status

;; NOTE org agenda mode
;; ORG <N> Agenda | Tuesday 22 October 2024 ◦
;; buffer-status ("ORG"), modal, buffer-identification ("Agenda"), org-agenda, window-dedicated-status
;; (format-time-string "%A %-e %B %Y")

;; NOTE term mode (vterm, eat, eshell)
;; >_ <I> bash [narrow] (line mode) | <5L, 258C> .../home-manager/editor/emacs/ ◦
;; MO *vterm* (zsh) | <5L, 258C> 352:58 .../home-manager/editor/emacs/ ◦
;; buffer-status (">_"), modal, buffer-identification (shell name), narrow-status, shell-mode, selection, cursor-position, default-directory (only term), window-dedicated-status
;; (file-name-base vterm-shell)
;; cond term-in-char-mode or eat--semi-char-mode etc...
;; (abbreviate-file-name default-directory)

;; NOTE nov mode
;; NOV <M> 8 An API for the manager (Build an Orchestrator in Go) | 352 5% ◦
;; buffer-status ("NOV"), modal, buffer-identification (chapter title), book-title, cursor-position, scroll, window-dedicated-status

;; NOTE pdf mode
;; PDF <M> lets-go.pdf (4.5M) | 12/426 ◦
;; buffer-status ("PDF"), modal, buffer-identification, file-size, pdf-page, window-dedicated-status

;; NOTE message mode and git commit mode?
;; LOG <M> *Messages* [narrow] (178 lines) | <5L, 258C> 352:58 ◦
;; buffer-status ("LOG"), modal, buffer-identification, narrow-status, buffer-lines, selection, cursor-position, window-dedicated-status

;; NOTE magit mode
;; MAGIT <M> nix-dotfiles (#rewrite, edited) | ◦
;; buffer-status ("MAGIT"), buffer-identification (default-directory), vc-branch (magit current branch), window-dedicated-status
;; (file-name-directory
;;  (directory-file-name
;;   (file-name-directory default-directory)))

;; NOTE info mode
;; INFO <M> Output of entire files (coreutils) | 352 5% ◦
;; buffer-status ("INFO"), buffer-identification (info-current-node), info-current-file, cursor-position, scroll, window-dedicated-status
;; from doom-modeline

;; NOTE repl mode
;; REPL <N> CIDER (connected) | <5L, 258C> 352:58 ◦

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
