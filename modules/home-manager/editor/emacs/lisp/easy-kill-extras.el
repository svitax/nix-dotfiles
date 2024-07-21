;;; easy-kill-extras.el -*- lexical-binding: t; -*-

(defun +easy-kill-expand-region ()
  "Expand kill according to expand-region."
  (interactive)
  (let* ((thing (easy-kill-get nil))
	 (bounds (easy-kill--bounds)))
    (save-mark-and-excursion
      (set-mark (cdr bounds))
      (goto-char (car bounds))
      (er/expand-region 1)
      (deactivate-mark)
      (easy-kill-adjust-candidate thing (point) (mark)))))

(defun +easy-kill-contract-region ()
  "Contract kill according to expand-region."
  (interactive)
  (let* ((thing (easy-kill-get nil))
	 (bounds (easy-kill--bounds)))
    (save-mark-and-excursion
      (set-mark (cdr bounds))
      (goto-char (car bounds))
      (er/contract-region 1)
      (deactivate-mark)
      (easy-kill-adjust-candidate thing (point) (mark)))))

(defsubst +did-mark (mark-fn)
  ;; Did mark-fn succeed in setting the mark?
  ;; Remove any existing mark
  (deactivate-mark)
  ;; Catch scan-error and deactivate-mark if we do
  (save-excursion
    (condition-case nil
	(progn (funcall mark-fn) (region-active-p))
      (scan-error (deactivate-mark)))))

(defmacro +er-easy-kill (thing)
  "Create a function `easy-kill-on-<thing>' which tests `er/mark-<thing>' to see if it set the region, and if so, adjusts the easy kill candidate."
  `(defun ,(intern (concat "easy-kill-on-" thing)) (_n)
     (when (+did-mark #',(intern (concat "er/mark-" thing)))
       (easy-kill-adjust-candidate ',(intern thing) (mark) (point)))))

(+er-easy-kill "symbol")
(+er-easy-kill "inside-pairs")
(+er-easy-kill "outside-pairs")
(+er-easy-kill "inside-quotes")
(+er-easy-kill "outsidn-quotes")

(provide 'easy-kill-extras)
;;; easy-kill-extras.el ends here
