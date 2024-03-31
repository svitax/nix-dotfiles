;;; keycast-extras.el -*- lexical-binding: t; -*-

(require 'keycast)
(require 'embark)
(require 'avy)

(define-minor-mode keycast-mode
  "Show current command and its key binding in the mode line."
  :global t
  (if keycast-mode
      (progn
        (add-hook 'pre-command-hook 'keycast--update t)
        (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
    (remove-hook 'pre-command-hook 'keycast--update)
    (setq global-mode-string (remove '("" keycast-mode-line " ") global-mode-string))))

;; (custom-set-faces
;;  '(keycast-command ((t (:inherit doom-modeline-debug :height 0.9))))
;;  '(keycast-key ((t (:inherit custom-modified :height 1.1 :weight bold)))))

(setq keycast-separator-width 1)
(dolist (input '(self-insert-command
                 org-self-insert-command))
  (add-to-list 'keycast-substitute-alist `(,input "." "Typing!")))
(setf (alist-get 'strokes-do-stroke
                 keycast-substitute-alist)
      '("[mouse]" t))

(defun store-action-key+cmd (cmd)
  (setq keycast--this-command-keys (this-single-command-keys)
        keycast--this-command cmd)
  cmd)

(defun store-action-key-no-cmd (cmd)
  (setq keycast--this-command-keys (this-single-command-keys)
        keycast--this-command cmd))

(defun keycast-capture-avy-dispatch (char)
  (if-let ((cmd (assoc char avy-dispatch-alist)))
      (setq keycast--this-command-keys (make-vector 1 char)
            keycast--this-command (cdr cmd))))

(advice-add 'embark-keymap-prompter :filter-return #'store-action-key+cmd)
(advice-add 'avy-handler-default :before #'keycast-capture-avy-dispatch)

(defun force-keycast-update (&rest _)
  (force-mode-line-update t))

(dolist (cmd '(embark-act embark-become))
  (advice-add cmd :before #'force-keycast-update))

(provide 'keycast-extras)
