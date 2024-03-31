;;; cape-extras.el -*- lexical-binding: t; -*-

(defun my/ignore-elisp-keywords (cand)
  (or (not (keywordp cand))
      (eq (char-after (car completion-in-region--data)) ?:)))

(defalias 'my/elisp-ignore-keywords-completion-at-point
  (cape-capf-predicate
   #'elisp-completion-at-point
   #'my/ignore-elisp-keywords))

(defun my/setup-elisp-capf ()
  (setq-local completion-at-point-functions
	      (list #'my/elisp-ignore-keywords-completion-at-point
		    #'cape-file)))

(defun my/setup-jupyter-eglot-capf ()
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; Jupyter's completion-at-point function overshadows Eglot's.
  ;; We use a Cape super capf to have them at the same time.
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     ;; #'jupyter-completion-at-point
                     #'python-completion-at-point)
		    #'cape-file)))

(defun my/add-cape-capf ()
  "Add general Cape backends to `completion-at-point-functions'"
  (dolist (backend '(cape-file))
    (add-to-list 'completion-at-point-functions backend t)))

(provide 'cape-extras)
