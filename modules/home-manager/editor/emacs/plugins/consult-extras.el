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

;; TODO: project-extras
(defun my/project-remove-project ()
  "Remove project from `project--list' using completion."
  (interactive)
  (project--ensure-read-project-list)
  (let* ((projects project--list)
	 (dir (completing-read "REMOVE project from list: " projects nil t)))
    (setq project--list (delete (assoc dir projects) projects))))

(provide 'consult-extras)
;;; consult-extras.el ends here
