;;; move-text-extras.el --- . -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +indent-region-advice (&rest ignored)
  "Re-indent the text in-and-around a text move."
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
	(indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(provide 'move-text-extras)
;;; move-text-extras.el ends here
