;;; eshell-extras.el -*- lexical-binding: t; -*-

(defun my/eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's
file. The eshell is renamed to match that directory to make multiple eshell
windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name   (car (last (split-string parent "/" t)))))
    (if-let* ((eshell-name (concat "*eshell: " name "*"))
              (existing-eshell-buffer (get-buffer eshell-name)))
        (select-window (display-buffer existing-eshell-buffer))
      (select-window (display-buffer (eshell "new")))
      (rename-buffer eshell-name)
      (insert (concat "ls"))
      (eshell-send-input))))

(defun shortened-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH.
Replacing parent directories with their initial characters to try to
get the character length of PATH (sans directory slashes) down to
MAX-LEN."
  (require 'cl-lib)
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len) (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/" (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

;; Tramp info
(defun my/eshell-remote-p ()
  "If you are in a remote machine."
  (tramp-tramp-file-p default-directory))
(defun my/eshell-remote-user ()
  "Return remote user name."
  (tramp-file-name-user (tram-dissect-file-name default-directory)))
(defun my/eshell-remote-host ()
  "Return remote host."
  ;; `tramp-file-name-real-host' is removed and replaced by
  ;; `tramp-file-name-host' in Emacs 26.
  (tramp-file-name-host (tram-dissect-file-name default-directory)))

(defun my/eshell-status-formatter (timestamp duration)
  "Return the status display for `my/eshell-status'.
TIMESTAMP is the value returned by `current-time' and DURATION is the floating
time the command took to complete in seconds."
  ;; (format "#[STATUS] End time %s, duration %.3fs\n"
  ;;         (format-time-string "%F %T" timestamp)
  ;;         duration)
  (concat
   " "
   (nerd-icons-mdicon "nf-md-timer")
   " "
   (format "%.1fs" duration)))

(defcustom my/eshell-status-min-duration 1
  "If a command takes more time than this, display its status with `epe-status'."
  :group 'sx
  :type 'number)

(defvar my/eshell-status-last-command-time nil)
(make-variable-buffer-local 'my/eshell-status-last-command-time)

(defun my/eshell-status-record ()
  "Record the time of the current command."
  (setq my/eshell-status-last-command-time (current-time)))

(defun my/eshell-status (&optional formatter min-duration)
  "Termination timestamp and duration of command.
Status is only returned if command duration was longer than MIN-DURATION \(defaults to `my/eshell-status-min-duration').
FORMATTER is a function of two arguments, TIMESTAMP and DURATION, that returns a string."
  (if my/eshell-status-last-command-time
      (let ((duration (time-to-seconds
                       (time-subtract (current-time) my/eshell-status-last-command-time))))
        (setq my/eshell-status-last-command-time nil)
        (if (> duration (or min-duration
                            my/eshell-status-min-duration))
            (funcall (or formatter
                         #'my/eshell-status-formatter)
                     (current-time)
                     duration)
          ""))
    (progn
      (add-hook 'eshell-pre-command-hook #'my/eshell-status-record)
      "")))

(defun my/eshell-prompt ()
  (concat
   (when (and (package-installed-p 'tramp) (my/eshell-remote-p))
     (propertize (concat (my/eshell-remote-user) "@" (my/eshell-remote-host) " ")
                 'face 'font-lock-comment-face))
   (when (package-installed-p 'envrc)
     (propertize (if (string= envrc--status 'none)
                     "" (concat
			 (nerd-icons-mdicon "nf-md-nix")
			 " "
                         (replace-regexp-in-string "-env$" "" (getenv "name"))))
                 'face 'font-lock-comment-face))
   (propertize (concat " " (shortened-path (eshell/pwd) 40)) 'face 'eshell-prompt)
   (propertize (if (car (vc-git-branches))
                   (concat
		    " "
		    (nerd-icons-octicon "nf-oct-git_branch")
		    " "
		    (car (vc-git-branches)))
                 "")
               'face 'vc-dir-status-up-to-date)
   (propertize (concat (my/eshell-status))
               'face 'font-lock-comment-face)
   (propertize " \n 𝝺 " 'face (if (zerop eshell-last-command-status) 'success 'error))))

(provide 'eshell-extras)
