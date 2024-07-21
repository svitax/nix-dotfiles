;;; completions.el -*- lexical-binding: t; -*-

(eval-when-compile (require 'cl-lib))

(defun use-package-normalize/:completions (name keyword args)
  (apply
   #'append
   (mapcar
    (lambda (arg)
      (let* ((result nil)
	     (mode (car arg))
	     (mode-hook (intern (concat (symbol-name mode) "-hook"))))
	(dolist (spec (cdr arg))
	  (push
	   (append
	    (list :mode mode :mode-hook mode-hook)
	    (pcase spec
	      ((pred symbolp) (list :capf spec))
	      ((pred consp)
	       (setq spec (append (list :capf) spec))
	       ;; Re-balance to ensure depth is always above the builtin and global
	       ;; hooks.
	       (let ((depth (or (plist-get spec :depth) 0)))
		 (plist-put spec :depth (- depth 49)))
	       spec)
	      (_ (use-package-error
		  (concat (symbol-name keyword) " capfs should be either a <symbol> or (<symbol> . <property-plist>)")))))
	   result))
	(nreverse result)))
    args)))

(defun use-package-handler/:completions (name keyword args rest state)
  (use-package-concat
   (use-package-process-keywords name rest state)
   (list
    (append
     (list 'progn)
     (let ((result nil))
       (dolist (it args)
	 (cl-destructuring-bind (&key capf mode-hook depth company &allow-other-keys) it
	   (let* ((add-capf-func-name
		   (cl-gentemp (concat "completion--add" (symbol-name capf) "-")))
		  (capf-to-add (if company
				   (cl-gentemp (concat "completion--company-adapt-" (symbol-name capf) "-"))
				 capf)))
	     (setq result
		   (append
		    result
		    `(,@ (when company
			   `((defalias ',capf-to-add
			       ;; Cape throws an error when adapting and using company-mode since
			       ;; there's a performance overhead. But there isn't a nice way to set
			       ;; priorities between capf and company backends so we ignore the check.
			       (let ((capf (cape-company-to-capf #',capf)))
				 (lambda (&rest args)
				   (let ((company-mode nil))
				     (apply capf args)))))))
			 (defun ,add-capf-func-name ()
			   (add-hook 'completion-at-point-functions ',capf-to-add ,depth t))
			 (add-hook ',mode-hook ',add-capf-func-name)))))))
       result)))))

(let ((tail (nthcdr (cl-position :hook use-package-keywords)
		    use-package-keywords)))
  (setcdr tail (cons :completions (cdr tail))))

(provide 'completions)
;;; completions.el ends here
