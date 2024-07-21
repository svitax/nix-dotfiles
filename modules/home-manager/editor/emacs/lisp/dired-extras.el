;;; dired-extras.el --- . -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +dired-home-directory ()
  (interactive)
  (dired-single-buffer (expand-file-name "~/")))

(defun +dired-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
	(wnd (current-window-configuration)))
    (if (<= (length files) 2)
	(let ((file1 (car files))
	      (file2 (if (cdr files)
			 (cadr files)
		       (read-file-name
			"file: "
			(dired-dwim-target-directory)))))
	  (if (file-newer-than-file-p file1 file2)
	      (ediff-files file2 file1)
	    (ediff-files file1 file2))
	  (add-hook 'ediff-after-quit-hook-internal
		    (lambda ()
		      (setq ediff-after-quit-hook-internal nil)
		      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

;; TODO: can't i just use scroll-other-window(-down) instead of wrapping it like this?
(defun +dired-scroll-other-window (&optional arg)
  "Scroll other window."
  (interactive "p")
  (let* ((scroll-error-top-bottom nil)
	 (num (if (= arg 1) nil arg)))
    (condition-case-unless-debug nil
	(scroll-other-window num))))

(defun +dired-scroll-other-window-down (&optional arg)
  "Scroll other window down."
  (interactive "p")
  (let* ((scroll-error-top-bottom nil)
	 (num (if (= arg 1) nil arg)))
    (condition-case-unless-debug nil
	(scroll-other-window-down num))))

(provide 'dired-extras)
;;; dired-extras.el ends here
