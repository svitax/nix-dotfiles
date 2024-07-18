;;; indent.el --- Configure preferred indentation for buffers. -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure preferred indentation for buffers. I define an alist to store indent configurations and a command `set-indent-offset' which sets the desired indent.
;; `+indent-configuration' is the main user configuration variable for indentation. This is used when no alternative indent configuration (such as buffer-local variables or EditorConfig files) specify how to deal with the current buffer.

;;; Code:

(require 'editorconfig)

(defvar +indent-configuration '((t . 2))
  "Alist configuring preferred indentation for buffers.
The car of an entry is used to match which buffer it is applied to and cdr is
used to determine the value of it.

The car can be a symbol, a string, a function or some expression which evaluates
to t.
It it is a symbol, the major mode of the buffer is compared against it.
If it is a string, the name of the buffer is matched against it.

The cons can be a number, a function or some expression which evaluates to a
number.

NOTE: indentation is set exclusively when a buffer-mode change occurs, changing
the buffer name doesn't alter the indent.")

(defvar-local +indent nil
  "Override the local indent for the current file.")

(defun +indent-offset (&optional buffer)
  "Use `+indent-configuration' to find the preferred indent for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cl-loop with matcher = nil
	     with indent = nil
	     for cfg in +indent-configuration
	     do (setq matcher (car cfg))
	     if (cond
		 ((eq matcher t) t)
		 ((symbolp matcher) (eq matcher major-mode))
		 ((stringp matcher) (string-match-p matcher (buffer-name)))
		 ((functionp matcher) (funcall matcher))
		 (t (eval matcher)))
	     do (setq indent (cdr cfg))
	     and return (if (functionp indent)
			    (funcall indent)
			  indent))))

(defun +set-indent-offset (&optional indent tab-size)
  "Set the indentation level of the current buffer.
This method uses `+indent-configuration' to determine what the indentation of
the current buffer should be and then sets it. Also setting any mode dependent,
specific indent bindings alongside it.

You can pass a specific value for the indent as an argument and the current
buffers indentation will be set to that."
  (interactive (let ((size (read-number "Indent: ")))
		 (list size size)))
  (let* ((conf (or (ignore-errors
		     (funcall editorconfig-get-properties-function))
		   (make-hash-table :size 0)))
	 (style (or (gethash 'indent_style conf)
		    (if indent-tabs-mode "tab" "space"))))
    (setq indent (or indent
		     +indent
		     (gethash 'indent_size conf)
		     (+indent-offset))
	  tab-size (or tab-size
		       (gethash 'tab_width conf)))

    (when (numberp indent)
      (setq indent (number-to-string indent)))
    (when (numberp tab-size)
      (setq tab-size (number-to-string tab-size)))

    (if (not indent)
	(and (called-interactively-p 'interactive)
	     (user-error "set-indent-offset: unable to determine indent for current buffer."))
      (editorconfig-set-indentation style indent tab-size))))

(require 'use-package-core)

(defun use-package-normalize/:indent (name keyword args)
  (mapcar (lambda (arg)
	    (unless (and (listp args) (<= (length arg) 2) (>= (length arg) 1))
	      (use-package-error (concat (symbol-name keyword) " indent configuration should be (mode value).")))
	    arg)
	  args))

(defun use-package-handler/:indent (name keyword args rest state)
  (use-package-concat
   (use-package-process-keywords name rest state)
   (when args
     `((defvar +indent-configuration)
       (with-eval-after-load 'indent
	 (progn
	   ,@(cl-loop for arg in args collect
		      `(push (cons (quote ,(car arg)) ,(cadr arg))
			     +indent-configuration))))))))

(let ((tail (nthcdr (cl-position :init use-package-keywords)
		    use-package-keywords)))
  (setcdr tail (cons :indent (cdr tail))))

(provide 'indent)
;;; indent.el ends here
