;;; editing-extras.el --- Emacs editing extras -*- lexical-binding: t; -*-

(defun my/back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (progn (beginning-of-line-text) (point)))
      (beginning-of-line)))

;;; marking things

(defmacro define-thing-marker (fn-name things forward-thing &rest extra)
  `(defun ,fn-name (&optional arg allow-extend)
     ,(format "Mark ARG %s starting with the current one. If ARG is negative,
mark -ARG %s ending with the current one.

Interactively (or if ALLOW-EXTEND is non-nil), if this command is
repeated or (in Transient Mark mode) if the mark is active, it
marks the next ARG %s after the ones already marked." things things things)
     (interactive "p\np")
     (unless arg (setq arg 1))
     (if (and allow-extend
	      (or (and (eq last-command this-command) (mark t))
		  (and transient-mark-mode mark-active)))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (,forward-thing arg)
	    (point)))
       ,(plist-get extra :pre)
       (,forward-thing arg)
       ,(plist-get extra :post)
       (push-mark nil t t)
       (,forward-thing (- arg)))))

(define-thing-marker my/mark-line "lines" forward-line
		     :post (unless (= (preceding-char) ?\n)
			     (setq arg (1- arg))))

(define-thing-marker my/mark-char "characters" forward-char)

(define-thing-marker my/mark-my-word "words" forward-word
		     :pre (when (and (looking-at "\\>") (> arg 0))
			    (forward-word -1)))

(define-thing-marker my/mark-non-whitespace "vim WORDS"
		     forward-to-whitespace)

(defun my/open-next-line (arg)
  "Move to the next line and then opens a line.

 See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;;; behave like vi's O command
(defun my/open-previous-line (arg)
  "Open a new line before the current one.

  See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Bind C-w to backward-kill-word ulness region is active
(defun my/backward-kill-word-or-region (&optional arg)
  "Kill word backward if region is inactive; else kill region"
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

;;; Modifies kill-ring-save so that with no active mark, the current
;;; line is saved.
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

(defun my/zap-to-char-save (arg char)
  "Zap to CHAR, (optional) ARG number of times, but save instead of kill."
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

;; Cycle spacing: just-one-space to no-space to original-spacing
(setq cycle-spacing-actions
      '(just-one-space delete-all-space restore))

(defun my/cycle-spacing-impatient (&optional n)
  (interactive "*p")
  "Call `cycle-spacing', but in fast mode."
  ;; (cycle-spacing (if (= n 1) -1 n) preserve-nl-back 'fast)
  (cycle-spacing (if (= n 1) -1 n)))

(defun my/comment-dwim (n)
  "Comment N lines, defaulting to the current one.
When the region is active, comment its lines instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line n)))

(defun my/pipe-region (start end command)
  "Pipe region through shell command. If the mark is inactive,
pipe whole buffer."
  (interactive (append
		(if (use-region-p)
		    (list (region-beginning) (region-end))
		  (list (point-min) (point-max)))
		(list (read-shell-command "Pipe through: "))))
  (let ((ext-status (call-shell-region start end command t t)))
    (unless (equal 0 exit-status)
      (let ((error-msg (string-trim-right (buffer-substring (mark) (point)))))
	(undo)
	(cond
	 ((null exit-status)
	  (message "Unknown error"))
	 ((stringp exit-status)
	  (message "Signal %s" exit-status))
	 (t
	  (message "[%d] %s" exit-status error-msg)))))))

;; Functions to move blocks of text (or single lines) up and down
(defun move-text-internal (arg)
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
	  (transpose-lines arg))
	(forward-line -1))
      (move-to-column column t)))))

(defun my/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun my/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun my/indent-whole-buffer ()
  "Indent buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(provide 'editing-extras)
