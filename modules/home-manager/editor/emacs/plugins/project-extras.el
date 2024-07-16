;;; project-extras.el -*- lexical-binding: t; -*-

(defun my/project-remove-project ()
  "Remove project from `project--list' using completion."
  (interactive)
  (project--ensure-read-project-list)
  (let* ((projects project--list)
	 (dir (completing-read "REMOVE project from list: " projects nil t)))
    (setq project--list (delete (assoc dir projects) projects))))

(provide 'project-extras)
;;; project-extras.el ends here
