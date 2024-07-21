;;; hide-whitespace.el --- Configure displaying of trailing whitespace. -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure displaying of trailing whitespace. Enable it by default in every mode. Append to custom variables to disable.

;;; Code:

;; TODO: whitespace-exempt-modes not actually working

(defun +hide-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

;; Configuration variables used for determining whether to enable visible whitespace or not.
(defvar +whitespace-exempt-modes '(help-mode
				   Buffer-menu-mode
				   ibuffer-mode
				   eshell-mode
				   term-mode
				   minibuffer-mode
				   minibuffer-inactive-mode
				   messages-buffer-mode
				   eshell-mode)
  "Modes under which no trailing whitespace is shown.")

(defvar +whitespace-exempt-buffers
  (list (rx "*Ibuffer confirmation*")
	(rx "*Org Export Dispatcher*")
	(rx "*eldoc*")
	(rx "*Completions*"))
  "Regexp matching buffer names where no trailing whitespace is shown.")

(defun +set-trailing-whitespace--mode-p ()
  "Check the mode of the current buffer to see whether trailing whitespace
should be shown."
  (not (and +whitespace-exempt-modes
	    (cl-some (lambda (mode) (derived-mode-p mode)) +whitespace-exempt-modes))))

(defun +set-trailing-whitespace--name-p ()
  "Check the name of the current buffer to see whether trailing whitespace should be shown."
  (not
   (and +whitespace-exempt-buffers
	(let ((it (buffer-name)))
	  (cl-find-if (lambda (regexp)
			(string-match-p regexp it))
		      +whitespace-exempt-buffers)))))

(defun +set-trailing-whitespace (&rest _)
  (setq show-trailing-whitespace
	;; when both name and mode decide you can show whitespace,
	;; then show it. otherwise when at least one says no, hide it.
	(and (+set-trailing-whitespace--mode-p)
	     (+set-trailing-whitespace--name-p))))

(defun use-package-normalize/:hide-whitespace (name keyword args)
  (mapcar (lambda (arg)
	    (unless (symbolp arg)
	      (use-package-error (concat (symbol-name keyword) " args must be mode symbols")))
	    arg)
	  args))

(defun use-package-handler/:hide-whitespace (name keyword args rest state)
  (use-package-concat
   (use-package-process-keywords name rest state)
   (when args
     `((defvar +whitespace-exempt-modes)
       (with-eval-after-load 'hide-whitespace
	 (progn
	   ,@(cl-loop for arg in args collect
		      `(push (quote ,arg) +whitespace-exempt-modes))))))))

(defun use-package-normalize/:hide-whitespace-regexp (name keywords args)
  args)

(defun use-package-handler/:hide-whitespace-regexp (name keyword args rest state)
  (use-package-concat
   (use-package-process-keywords name rest state)
   (when args
     `((defvar +whitespace-exempt-buffers)
       (with-eval-after-load 'hide-whitespace
	 (progn
	   ,@(cl-loop for arg in args collect `(push ,arg +whitespace-exempt-buffers))))))))

(let ((tail (nthcdr (cl-position :init use-package-keywords)
		    use-package-keywords)))
  (setcdr tail (append (list :hide-whitespace :hide-whitespace-regexp)
		       (cdr tail))))

(provide 'hide-whitespace)
;;; hide-whitespace.el ends here
