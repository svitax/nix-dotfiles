;;; autorevert-extras.el -*- lexical-binding: t; -*-

(defun +window-state-state-change (state)
  (let* ((old-selected-window (old-selected-window))
	 (old-buffer (when old-selected-window
		       (window-buffer old-selected-window)))
	 (selected-window (selected-window))
	 (new-buffer (when selected-window
		       (window-buffer selected-window))))
    (when old-buffer
      (with-current-buffer old-buffer
	(when buffer-file-name
	  (auto-revert-mode -1))))
    (when new-buffer
      (with-current-buffer new-buffer
	(when buffer-file-name
	  (auto-revert-mode +1))))))

(provide 'autorevert-extras)
;;; autorevert-extras.el ends here
