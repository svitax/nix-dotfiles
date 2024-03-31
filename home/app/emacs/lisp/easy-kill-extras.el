;;; easy-kill-extras.el -*- lexical-binding: t; -*-

(require 'easy-kill)
(require 'expreg)

(defun my/easy-kill-expand-region ()
  "Expand kill according to expand-region."
  (interactive)
  (let* ((thing (easy-kill-get nil))
         (bounds (easy-kill--bounds)))
    (save-mark-and-excursion
      (set-mark (cdr bounds))
      (goto-char (car bounds))
      ;; (er/expand-region 1)
      (expreg-expand)
      (deactivate-mark)
      (easy-kill-adjust-candidate thing (point) (mark)))))

(defun my/easy-kill-contract-region ()
  "Expand kill according to expand-region."
  (interactive)
  (let* ((thing (easy-kill-get nil))
         (bounds (easy-kill--bounds)))
    (save-mark-and-excursion
      (set-mark (cdr bounds))
      (goto-char (car bounds))
      ;; (er/contract-region 1)
      (expreg-contract)
      (deactivate-mark)
      (easy-kill-adjust-candidate thing (point) (mark))))))

(add-to-list 'easy-kill-alist '(62 page "\n"))
(add-to-list 'easy-kill-alist '(104 paragraph "\n"))
(add-to-list 'easy-kill-alist '(41 sentence "\n")))

(provide 'easy-kill-extras)
