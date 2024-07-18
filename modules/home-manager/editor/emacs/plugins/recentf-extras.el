;;; recentf-extras.el -*- lexical-binding: t; -*-

(defun +recentf-delete-entry ()
	"Delete a recentf entry."
	(interactive)
	(let ((selection (completing-read "delete: " recentf-list)))
		(when (> (length selection) 0)
			(setq recentf-list
				(seq-remove (lambda (candidate)
											(string-equal candidate selection)) recentf-list))
			(message "%S removed from the list" selection))))

(defun +recentf--file-truename-fn (file)
	"Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
abbreviate $HOME -> ~ in filepaths (more portable, more readable, & saves
space)."
	(if (or (not (file-remote-p file))
				(equal "sudo" (file-remote-p file 'method)))
		(abbreviate-file-name (file-truename (tramp-file-name-localname file)))
		file))

(defun +recentf--touch-buffer-h ()
	"Bump file in recent file list when it is switched or written to."
	(when buffer-file-name
		(recentf-add-file buffer-file-name))
	;; Return nil for `write-file-functions'
	nil)

(defun +recentf--add-dired-directory-h ()
	"Add Dired directories to recentf file list."
	(recentf-add-file default-directory))

(provide 'recentf-extras)
;;; recentf-extras.el ends here
