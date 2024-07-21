;;; vertico-quick-extras.el -*- lexical-binding: t; -*-

(defun +vertico-quick-embark (&optional arg)
  "Embark on candidate using quick keys."
  (interactive)
  (when (vertico-quick-jump)
    (embark-act arg)))

(provide 'vertico-quick-extras)
;;; vertico-quick-extras.el ends here
