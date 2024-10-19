;;; vertico-extras.el -*- lexical-binding: t; -*-

(require 'vertico)
(require 'vertico-unobtrusive)

(defvar +vertico-multiform-minimal
  '(unobtrusive
    (vertico-flat-format . (:multiple ""
			    :single ""
			    :prompt ""
			    :separator ""
			    :ellipsis ""
			    :no-match "")))
  "List of configurations for minimal Vertico multiform.
The minimal view is intended to be more private or less revealing. This is
important when, for example, a prompt shows names of people. Of course, such a
view also provides a minimal style for general usage.

Toggle the vertical view with the `vertico-multiform-vertical' command or use
the commands `+vertico-minimal-next' and `vertico-minimal-previous', which
toggle the vertical view automatically.")

(defvar +vertico-multiform-maximal
  '((vertico-count . 10))
  "List of configurations for maximal Vertico multiform.")

(defun +vertico--match-directory (str)
  "Match directory delimeter in STR."
  (string-suffix-p "/" str))

;; From the Vertico documentation.
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
      (let ((vertico--index 0))
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
This is done to accommodate `+vertico-multiform-minimal'."
  (interactive)
  (if (and vertico-unobtrusive-mode (> vertico--total 1))
      (progn
	(minibuffer-complete)
	(vertico-multiform-vertical))
    (vertico-insert)))

;; NOTE: idk if i need anything past this anymore

(defun +vertico-multiform-monocle ()
  (interactive)
  (setq-local vertico-buffer-display-action '(display-buffer-full-frame))
  (vertico-multiform-buffer))

(defun +vertico-multiform-unobtrusive ()
  "Toggle between vertico-unobtrusive/vertico-flat and default vertico."
  (interactive)
  (vertico-multiform-vertical))

(defun +vertico-really-exit ()
  (interactive)
  (if minibuffer--require-match
      (minibuffer-complete-and-exit)
    (exit-minibuffer)))

(provide 'vertico-extras)
;;; vertico-extras.el ends here
