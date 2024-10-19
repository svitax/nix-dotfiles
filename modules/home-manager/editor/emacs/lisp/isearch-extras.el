;;; isearch-extras.el --- . -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +isearch-delete ()
  "Delete the failed portion or last char if succesful search.

See also: https://emacs.stackexchange.com/a/10360/9198"
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string (substring
			  isearch-string 0 (or (isearch-fail-pos) (1- (length isearch-string))))
	  isearch-message (mapconcat 'isearch-text-char-description isearch-string ""))
    (funcall (or isearch-message-function #'isearch-message) nil t)
    (if isearch-other-end (goto-char isearch-other-end))
    (isearch-search)
    (isearch-push-state)
    (isearch-update)))

(defun +isearch-mark-and-exit ()
  "Mark the current search string and exit the search."
  (interactive)
  (push-mark isearch-other-end t 'activate)
  (setq deactivate-mark nil)
  (activate-mark)
  (isearch-done))

(defun my/isearch-repeat-forward (&optional arg)
  "Move forward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
  (interactive "p")
  (isearch-repeat-forward (or arg 1))
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defun my/isearch-repeat-backward (&optional arg)
  "Move backward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
  (interactive "p")
  (when (and (not isearch-forward) isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-backward (or arg 1)))

(defun my/isearch-abort-dwim ()
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

(defun my/isearch-other-end ()
  "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
  (interactive)
  (isearch-done)
  (goto-char isearch-other-end))

;;;###autoload
(defun my/isearch-region (&optional not-regexp no-recursive-edit)
  "If a region is active, make this the isearch default search pattern."
  (interactive "P\np")
  (when (use-region-p)
    (let ((search (buffer-substring-no-properties
		   (region-beginning)
		   (region-end))))
      (message "my/ir: %s %d %d" search (region-beginning) (region-end))
      (setq deactivate-mark t)
      (isearch-yank-string search))))

(defun my/isearch-mark-and-exit ()
  "Marks the current search string and exits the search. Can be used
as a building block for a more complex chain, such as to kill a
region, or place multiple cursors."
  (interactive)
  (push-mark isearch-other-end t 'activate)
  (setq deactivate-mark nil)
  (isearch-done))

;; TODO: make this into a `from-isearch' macro
(defun my/consult-line-from-isearch ()
  "Invoke `consult-line' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
		   isearch-string
		 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (consult-line query)))

(defun my/project-search-from-isearch ()
  "Invoke `consult-line' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
		   isearch-string
		 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (project-find-regexp query)))

(provide 'isearch-extras)
;;; isearch-extras.el ends here
