;;; eshell-extras.el -*- lexical-binding: t; -*-

;; TODO: put this in window-extras
(defun +delete-window-if-not-single ()
  "Delete window if not the only one."
  (when (not (one-window-p))
    (delete-window)))

(defun +eshell-here ()
  "Opens up a new eshell in the directory associated with the current buffer's
file. The eshell is renamed to match that directory to make multiple eshell
windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
		     (file-name-directory (buffer-file-name))
		   default-directory))
	 (name (car (last (split-string parent "/" t)))))
    (if-let* ((eshell-name (concat "*eshell: " name "*"))
	      (existing-eshell-buffer (get-buffer eshell-name)))
	(select-window (display-buffer existing-eshell-buffer))
      (select-window (display-buffer (eshell "new")))
      (rename-buffer eshell-name))))

(defun +eshell-line-numbers-setup ()
  (make-local-variable
   'display-line-numbers-mode)
  (display-line-numbers-mode -1))

(defun +eshell-imenu-setup ()
  ;; TODO: Change this if the prompt changes
  (setq-local imenu-generic-expression
	      '(("Prompt" " $ \\(.*\\)" 1))))

(defun +eshell-outline-setup ()
  (setq outline-regexp eshell-prompt-regexp))

(defun +er/mark-eshell-command-and-output ()
  (when (re-search-forward eshell-prompt-regexp nil t)
    (goto-char (match-beginning 0))
    (push-mark)
    (when (re-search-backward eshell-prompt-regexp nil t)
      (goto-char (match-end 0)))))

(defun +eshell-expand-region-setup ()
  (when (require 'expand-region nil t)
    (make-local-variable 'er/try-expand-list)
    (add-to-list 'er/try-expand-list
		 #'+er/mark-eshell-command-and-output)))

(defun +eshell-send-detached-input (&optional arg)
  "Send the current Eshell input to a compilation buffer.
With universal prefix argument bury the compilation buffer and
send a notification when the process has exited."
  (interactive "p")
  (when-let* ((cmd (buffer-substring
                    eshell-last-output-end (point-max)))
              (cmd-present-p (not (string-empty-p cmd))))
    (let* ((hostname (or
                      (file-remote-p default-directory 'host)
                      (system-name)))
           (compile-command nil)
           (compilation-save-buffers-predicate 'ignore)
           (compilation-scroll-output nil)
           (compilation-buffer
            (compilation-start
             cmd
             nil
             (lambda (name-of-mode)
               (if (eq major-mode 'compilation-mode)
                   (buffer-name)
                 (generate-new-buffer-name (concat "D# " cmd)))))))
      (with-current-buffer compilation-buffer
        (setq list-buffers-directory default-directory)
        (when (equal arg 4)
          (switch-to-prev-buffer (get-buffer-window (current-buffer)))
          (setq-local compilation-finish-functions
                      `((lambda (buffer str)
                          (notifications-notify
                           :body (format "%s # %s" ,hostname ,cmd)
                           :timeout 8000
                           :category "detached_process"
                           :actions '("default" "Switch to buffer")
                           :on-action (lambda (id key) (switch-to-buffer-other-window ,(buffer-name compilation-buffer)))
                           :title (format "Process %s!" (string-chop-newline str))
                           :urgency (if (string-prefix-p "finished" str) 'normal 'critical))))))))
    (eshell-add-input-to-history cmd)
    (eshell-reset)))

(defun +eshell/emacs (&rest args)
  "Open a file (ARGS) in Emacs."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

;; TODO: make this work with eshell/up
(defun +eshell-add-input-to-history-a (orig-fun &rest r)
  "Cd to relative paths aren't that useful in history. Change to absolute paths."
  (require 'seq)
  (let* ((input (nth 0 r))
	 (args (progn
		 (set-text-properties 0 (length input) nil input)
		 (split-string input))))
    (if (and (equal "cd" (nth 0 args))
	     (not (seq-find (lambda (item)
			      ;; Don't rewrite "cd /ssh:" in history.
			      (string-prefix-p "/ssh:" item))
			    args))
	     (not (seq-find (lambda (item)
			      ;; Don't rewrite "cd -" in history.
			      (string-equal "-" item))
			    args)))
	(apply orig-fun (list (format "cd %s"
				      (expand-file-name (concat default-directory
								(nth 1 args))))))
      (apply orig-fun r))))

(defun +eshell-exec-visual-a (orig-fun &rest r)
  ;; Don't let visual commands keep creating multiple buffers.
  ;; Kill it first if it already exists.
  (cl-letf (((symbol-function #'generate-new-buffer)
	     (lambda (name &optional inhibit-buffer-hooks)
	       (when (get-buffer name)
		 (kill-buffer name))
	       (get-buffer-create (generate-new-buffer-name name)))))
    (apply orig-fun r)))

(defun +eshell-kill-ring-save-output (&optional arg)
  "Add to the `kill-ring' the last command's output.
With prefix argument ARG, also add the prompt and input."
  (interactive "P")
  (copy-region-as-kill (if arg (eshell-beginning-of-input)
                         (eshell-beginning-of-output))
                       (eshell-end-of-output))
  (message (if arg "Copied last input and output to kill ring."
             "Copied last output to kill ring.")))

(defun +eshell-export-output (&optional arg)
  "Export output of the last command to a buffer.
With prefix ARG, also copy the prompt and input."
  (interactive)
  (let ((orig (current-buffer))
        (beg (if arg (eshell-beginning-of-input)
               (eshell-beginning-of-output)))
        (end (eshell-end-of-output))
        (buffer (get-buffer-create
                 (format "*eshell export: %s*"
                         (buffer-substring-no-properties
                          (eshell-beginning-of-input)
                          (1- (eshell-beginning-of-output)))))))
    (with-current-buffer buffer
      (font-lock-mode)
      (insert-buffer-substring orig beg end)
      (goto-char (point-min)))
    ;; Taken from `eshell-kill-output'
    (goto-char (eshell-beginning-of-output))
    (insert (format "Exported to %S\n" buffer))
    (delete-region (point) (eshell-end-of-output))
    (goto-char (point-max))
    (pop-to-buffer buffer)))

(defun +eshell-redirect-to-buffer (buffer)
  "Complete the syntax for appending Eshell output to BUFFER."
  (interactive
   (list (read-buffer "Redirect to buffer: ")))
  (insert (format " >>> #<%s>" buffer)))

(defun +eshell-insert-args (&optional num)
  "Insert the NUMth argument of the previous command.

NUM counts from the end"
  (interactive "p")
  (let ((valid-pos)
        (N (length eshell-last-arguments)))
    (save-excursion
      (beginning-of-line)
      (if (looking-at eshell-prompt-regexp)
          (setq valid-pos t)))
    (if valid-pos
        (insert (substring-no-properties
                 (nth (- N num) eshell-last-arguments)))
      (call-interactively #'xref-find-definitions))))

;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
(defun +eshell-buffer-contents (buffer)
  "Return fontified buffer contents for BUFFER."
  (with-current-buffer buffer
    (font-lock-ensure (point-min) (point-max))
    (buffer-string)))

;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
(defun +eshell-file-contents (file)
  "Return fontified file contents for FILE."
  (let ((buffer (get-file-buffer file)))
    (if buffer
        (+eshell-buffer-contents buffer)
      (unwind-protect
          (+eshell-buffer-contents
           (setq buffer
                 (let ((inhibit-message t)
                       (non-essential t)
                       (enable-dir-local-variables nil)
                       (enable-local-variables (and enable-local-variables :safe)))
                   (find-file-noselect file))))
        (when buffer
          (kill-buffer buffer))))))

;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
;; can i get completion for this somehow?
(defun +eshell/b (regexp)
  "Output buffer content of buffer matching REGEXP."
  (cl-loop for buf in (buffer-list)
           thereis
           (and (string-match-p regexp (buffer-name buf))
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max))))))

;; From https://github.com/LemonBreezes/.doom.d/blob/master/modules/private/eshell/autoload.el
(defun +eshell/hat (&rest files)
  "Output FILES with highlighting."
  (dolist (f files)
    (eshell-print (+eshell-file-contents f))))

(defun +eshell/view (&optional file)
  (if (or (not file)
          (not (file-exists-p file))
          (file-directory-p file))
      (dired-other-window default-directory)
    (find-file-other-window file)
    (read-only-mode 1)
    (view-mode 1)))

(defun +eshell/z (&optional regexp)
  "Navigate to a previously visited directory in eshell."
  (let ((eshell-dirs (delete-dups (mapcar 'abbreviate-file-name
                                          (ring-elements eshell-last-dir-ring)))))
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (let* ((consult-dir--source-eshell `(:name "Eshell"
                                           :narrow ?e
                                           :category file
                                           :face consult-file
                                           :items ,eshell-dirs))
             (consult-dir-sources (cons consult-dir--source-eshell consult-dir-sources)))
        (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: ")))))
     (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                     (completing-read "cd: " eshell-dirs)))))))

(defun +consult-esh-dir-history ()
  (interactive)
  (eshell/cd (consult--read
	      (ring-elements eshell-last-dir-ring)
	      :prompt "Directory to change to: ")))

;; TODO: eshell-frame

;; (defvar +eshell-buffer-name "My Eshell")

;; (defvar +eshell--buffers (make-ring 25))

;; (defun +eshell-buffers ()
;;   (ring-elements +eshell--buffers))

;; (defun +eshell--unused-buffer (&optional new-p)
;;   (or (unless new-p
;;         (cl-loop for buf in (+eshell-buffers)
;; 		 if (and (buffer-live-p buf)
;; 			 (not (get-buffer-window buf t)))
;; 		 return buf))
;;       (generate-new-buffer +eshell-buffer-name)))

;; (defun +eshell-run-command (command &optional buffer)
;;   (let ((buffer (or buffer (if (eq major-mode 'eshell-mode)
;;                                (current-buffer)
;;                              (cl-find-if #'buffer-live-p (+eshell-buffers))))))
;;     (unless buffer
;;       (user-error "No living eshell buffers available"))
;;     (unless (buffer-live-p buffer)
;;       (user-error "Cannot operate on a dead buffer"))
;;     (with-current-buffer buffer
;;       (goto-char eshell-last-output-end)
;;       (goto-char (line-end-position))
;;       (insert command)
;;       (eshell-send-input nil t))))

;; (defun +eshell-frame (&optional command)
;;   "Open a frame dedicated to eshell, with COMMAND if specified.

;; Once the eshell process is killed, the previous frame layout is restored."
;;   (interactive "P")
;;   (let ((buf (+eshell--unused-buffer 'new)))
;;     (unless (frame-parameter nil 'saved-wconf)
;;       (set-frame-parameter nil 'saved-wconf (current-window-configuration)))
;;     (delete-other-windows)
;;     (with-current-buffer (switch-to-buffer buf)
;;       (eshell-mode)
;;       (when command
;;         (+eshell-run-command command buf)))
;;     buf))

(provide 'eshell-extras)
;;; eshell-extras.el ends here
