;;; corfu-extras.el -*- lexical-binding: t; -*-

(defun my/corfu-combined-sort (candidates)
  "Sort CANDIDATES using both display-sort-function and corfu-sort-function."
  (let ((candidates
	 (let ((display-sort-func (corfu--metadata-get 'display-sort-function)))
	   (if display-sort-func
	       (funcall display-sort-func candidates)
	     candidates))))
    (if corfu-sort-function
	(funcall corfu-sort-function candidates)
      candidates)))

(defun my/corfu-send-shell (&rest _)
  "Send completion candidate when inside comint/eshell."
  (cond
   ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
    (eshell-send-input))
   ((and (derived-mode-p 'comint-mode) (fboundp 'comint-send-input))
    (comint-send-input))))

(advice-add #'corfu-insert :after #'my/corfu-send-shell)

(defun my/corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer."
  (when (local-variable-p 'completion-at-point-functions)
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popupinfo
		corfu-popupinfo-delay nil)
    (corfu-mode 1)))

(defun my/corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
	      (bound-and-true-p vertico--input)
	      (eq (current-local-map) read-passwd-map))
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popupinfo
		corfu-popupinfo-delay nil)
    (corfu-mode 1)))

(defun my/corfu-move-to-minibuffer ()
  (interactive)
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
	   completion-cycle-threshold completion-cycling)
       (consult-completion-in-region beg end table pred)))))

(add-to-list 'corfu-continue-commands #'my/corfu-move-to-minibuffer)

(provide 'corfu-extras)
