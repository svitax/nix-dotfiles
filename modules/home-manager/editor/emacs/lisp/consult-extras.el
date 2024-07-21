;;; consult-extras.el -*- lexical-binding: t; -*-

;; Combine `consult-imenu' and `consult-menu-multi'
(defun my/consult-imenu-all (&optional arg)
  "Call `consult-imenu'. With prefix-command ARG, call `consult-imenu-multi.'"
  (interactive "P")
  (if arg (consult-imenu-multi) (consult-imenu)))

(defun my/consult-mark-all (&optional arg)
  "Call `consult-mark'. With prefix-command ARG, call `consult-global-mark'"
  (interactive "P")
  (if arg (consult-global-mark) (consult-mark)))

(defun my/consult-shell-command ()
  (interactive)
  (let* ((candidates (split-string
		      (shell-command-to-string "compgen -c")
		      "\n"
		      t))
	 (command (consult--read
		   candidates
		   :prompt "Shell command: ")))
    (start-process-shell-command command nil command)))

;; TODO: i haven't written the code to automatically prefix certain buffers
;; once i do that i can easily target those buffers with consult
;; this is useful when you don't want to target an entire mode, but only a
;; certain subset of buffers with that mode
(defun my/consult-buffer-by-prefix (prefix caller show-preview)
  "Use consult to select a buffer prefixed by PREFIX#.

Show buffer previews if SHOW-PREVIEW is not nil."
  (let* ((consult--customize-alist
	  (if show-preview
	      (remove (list caller :preview-key nil) consult--customize-alist)
	    consult--customize-alist))
	 (my/consult--source-buffer-prefixed
	  `(:name ,(format "Buffers (%s)" prefix)
	    :category buffer
	    :face consult-buffer
	    :history buffer-name-history
	    :state ,#'consult--buffer-state
	    :default t
	    :items
	    ,(lambda ()
	       (consult--buffer-query
		:sort 'visibility
		:include (concat "^" prefix "#")
		:as #'buffer-name))))
	 (consult-buffer-sources (list my/consult--source-buffer-prefixed)))
    (consult-buffer)))

(defun my/consult-buffer-icecat (arg)
  "Use consult to select an Icecat buffer."
  (interactive "P")
  (my/consult-buffer-by-prefix "I" this-command arg))
(defun my/consult-buffer-ansi-term (arg)
  "Use consult to select an ansi-term buffer."
  (interactive "P")
  (my/consult-buffer-by-prefix "U" this-command arg))
(defun my/consult-buffer-detached-command (arg)
  "Use consult to select a compilation buffer."
  (interactive "P")
  (my/consult-buffer-by-prefix "D" this-command arg))

;; Don't preview Icecat buffers
(consult-customize my/consult-buffer-icecat :preview-key nil)

(provide 'consult-extras)
;;; consult-extras.el ends here
