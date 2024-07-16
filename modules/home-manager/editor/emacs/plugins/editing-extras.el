;;; editing-extras.el -*- lexical-binding: t; -*-

(defun my/back-to-indentation-or-beginning ()
  "Toggle between beginning of line and indentation."
  (interactive)
  (if (= (point) (progn (beginning-of-line-text) (point)))
      (beginning-of-line)))

(defun my/cycle-spacing-impatient (&optional n)
  "Call `cycle-spacing', but in fast mode."
  (interactive "*p")
  (cycle-spacing (if (= n 1) -1 n)))

;; Lifted from `crux.el' https://github.com/bbatsov/crux

(defmacro my/with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer.

Use to make commands like `indent-region' work on both the region
and the entire buffer (in the absense of a region)."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(defmacro my/with-region-or-line (func)
  "When called with no active region, call FUNC on current line."
  `(defadvice ,func (before with-region-or-line activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (line-beginning-position) (line-beginning-position 2))))))

(defmacro my/with-region-or-sexp-or-line (func)
  "When called with no active region, call FUNC on current sexp/string, or line."
  `(defadvice ,func (before with-region-or-sexp-or-line activate compile)
     (interactive
      (cond
       (mark-active (list (region-beginning) (region-end)))
       ((in-string-p) (flatten-list (bounds-of-thing-at-point 'string)))
       ((thing-at-point 'list) (flatten-list (bounds-of-thing-at-point 'list)))
       (t (list (line-beginning-position) (line-beginning-position 2)))))))

(defmacro my/with-region-or-point-to-eol (func)
  "When called with no active region, call FUNC from the point to the end of line."
  `(defadvice ,func (before with-region-or-point-to-eol activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point) (line-end-position))))))

(defun my/smart-kill-line ()
  "Kill to the end of the line and kill whole line on the next call."
  (interactive)
  (let ((orig-point (point)))
    (move-end-of-line 1)
    (if (= orig-point (point))
	(kill-whole-line)
      (goto-char orig-point)
      (kill-line))))

(defun my/zap-to-char-save (arg char)
  "Kill up to and including CHAR (optional) ARG number of times."
  (interactive "p\ncSave to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (save-excursion (message (char-to-string char))
		  (copy-region-as-kill (point)
				       (progn
					 (search-forward
					  (char-to-string char)
					  nil nil arg)
					 (point)))))

(defun my/zap-up-to-char-save (arg char)
  "Kill ring save up to, but not including CHAR (optional) ARG number of times."
  (interactive "p\ncSave to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (save-excursion (message (char-to-string char))
		  (copy-region-as-kill (point)
				       (progn
					 (forward-char)
					 (unwind-protect
					     (search-forward (char-to-string char) nil nil arg)
					   (backward-char))
					 (point)))))

(defun my--move-text (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          ;; ;; Account for changes to transpose-lines in Emacs 24.3
          ;; (when (and (eval-when-compile
          ;;              (not (version-list-<
          ;;                    (version-to-list emacs-version)
          ;;                    '(24 3 50 0))))
          ;;            (< arg 0))
          ;;   (forward-line -1))
          )
        (forward-line -1))
      (move-to-column column t)))))

(defun my/move-text-down (arg)
  "Move region (transient-mark-mode-active) or current line ARG lines down."
  (interactive "*p")
  (my--move-text arg))

(defun my/move-text-up (arg)
  "Move region (transient-mark-mode-active) or current line ARG lines up."
  (interactive "*p")
  (my--move-text (- arg)))

;; TODO: isearch-extras
(defun my/isearch-next ()
  "Go to next isearch match."
  (interactive)
  (let (isearch-lazy-highlight)
    (isearch-repeat 'forward))
  (isearch-exit))

(defun my/isearch-previous ()
  "Go to previous isearch match."
  (interactive)
  (let (isearch-lazy-highlight)
    (isearch-repeat 'backward))
  (isearch-exit))

;; TODO: window-extras
(defun my/scroll-down-half ()
  "Scroll text of selected window up by a half screen ARG times."
  (interactive)
  (scroll-down-command
   (floor
    (- (window-height)
       next-screen-context-lines)
    2)))

(defun my/scroll-up-half ()
  "Scroll text of selected window down by a half screen ARG times."
  (interactive)
  (scroll-up-command
   (floor
    (- (window-height)
       next-screen-context-lines)
    2)))

(defun my/scroll-down-half (arg)
  "Scroll text of selected window up by a half screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (forward-line (- dist))))

(defun my/scroll-up-half (arg)
  "Scroll text of selected window down by a half screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (forward-line dist)))

(provide 'editing-extras)
;;; editing-extras.el ends here

