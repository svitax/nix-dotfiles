;;; compile-extras.el -*- lexical-binding: t; -*-

(defun my/run-with-python ()
  "Set the default compile-command to run the current file with Python."
  (setq-local compile-command
	      (concat "python "
		      (when buffer-file-name
			(shell-quote-argument buffer-file-name)))))

(provide 'compile-extras)
