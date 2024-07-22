;;; corfu-extras.el -*- lexical-binding: t; -*-

(defvar +corfu-minibuffer-exclude-modes (list read-passwd-map)
  "Minibuffer-local keymaps for which `corfu' should be disabled.")

(defvar +corfu-minibuffer-exclude-commands '(org-ql-find)
  "Minibuffer commands for which `corfu' should be disabled.")

(defun +corfu-enable-always-in-minibuffer ()
  "Enable corfu in the minibuffer if `vertico' is not active."
  (unless (or (bound-and-true-p vertico--input)
	      (memq this-command +corfu-minibuffer-exclude-commands)
	      (memq (current-local-map)
		    +corfu-minibuffer-exclude-modes))
    (setq-local corfu-echo-delay nil
		corfu-popupinfo-delay nil)
    (corfu-mode 1)))

(defun +corfu-shell-settings ()
  (setq-local corfu-quit-no-match t
	      corfu-quit-at-boundary nil
	      corfu-auto nil)
  (corfu-mode))

(defun +corfu-insert-and-send ()
  (interactive)
  ;; 1. First insert the completed candidate
  (corfu-insert)
  ;; 2. Send the entire prompt input to the shell
  (cond
   ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
    (eshell-send-input))
   ((derived-mode-p 'comint-mode)
    (comint-send-input))))

(provide 'corfu-extras)
;;; corfu-extras.el ends here
