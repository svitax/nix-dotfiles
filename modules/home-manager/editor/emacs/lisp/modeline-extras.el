;;; modeline-extras.el --- . -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A custom modeline based off of Protesilaos Stavrou's wonderful prot-modeline.el 
;;; Code:

(require 'nerd-icons)

(defgroup +modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup +modeline-faces nil
  "Faces for my custom modeline."
  :group '+modeline)

(defcustom +modeline-string-truncate-length 9
  "String length after which truncation should be done in small windows."
  :type 'natnum)

;;;; Faces

(defface +modeline-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with Prot's `spacious-padding'
package).")

(defface +modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group '+modeline-faces)

(defface +modeline-indicator-red-bg
  '((default :inherit (bold +modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for modeline indicators with a background."
  :group '+modeline-faces)

(defface +modeline-indicator-green
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#005f00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#73fa7f")
    (t :foreground "green"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group '+modeline-faces)

(defface +modeline-indicator-green-bg
  '((default :inherit (bold +modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background."
  :group '+modeline-faces)

(defface +modeline-indicator-yellow
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6f4000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f0c526")
    (t :foreground "yellow"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group '+modeline-faces)

(defface +modeline-indicator-yellow-bg
  '((default :inherit (bold +modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#805000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ffc800" :foreground "black")
    (t :background "yellow" :foreground "black"))
  "Face for modeline indicators with a background."
  :group '+modeline-faces)

(defface +modeline-indicator-blue
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#00228a")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88bfff")
    (t :foreground "blue"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group '+modeline-faces)

(defface +modeline-indicator-blue-bg
  '((default :inherit (bold +modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background."
  :group '+modeline-faces)

(defface +modeline-indicator-magenta
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6a1aaf")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e0a0ff")
    (t :foreground "magenta"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group '+modeline-faces)

(defface +modeline-indicator-magenta-bg
  '((default :inherit (bold +modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#6f0f9f" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#e3a2ff" :foreground "black")
    (t :background "magenta" :foreground "black"))
  "Face for modeline indicators with a background."
  :group '+modeline-faces)

(defface +modeline-indicator-cyan
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#004060")
    (((class color) (min-colors 88) (background dark))
     :foreground "#30b7cc")
    (t :foreground "cyan"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group '+modeline-faces)

(defface +modeline-indicator-cyan-bg
  '((default :inherit (bold +modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for modeline indicators with a background."
  :group '+modeline-faces)

(defface +modeline-indicator-gray
  '((t :inherit shadow))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group '+modeline-faces)

(defface +modeline-indicator-gray-bg
  '((default :inherit (bold +modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#808080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#a0a0a0" :foreground "black")
    (t :inverse-video t))
  "Face for modeline indicatovrs with a background."
  :group '+modeline-faces)

;;;; Common helper functions

(defun +common-window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))

(defun +modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (if (string-empty-p str)
      str
    (and (+common-window-narrow-p)
         (> (length str) +modeline-string-truncate-length)
         (not (one-window-p :no-minibuffer)))))

(defun +modeline--truncate-p ()
  "Return non-nil if truncation should happen.
This is a more general and less stringent variant of
`+modeline--string-truncate-p'."
  (and (+common-window-narrow-p)
       (not (one-window-p :no-minibuffer))))

(defun +modeline-string-cut-end (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the end of STR by counting from its start up to
`+modeline-string-truncate-length'."
  (if (+modeline--string-truncate-p str)
      (concat (substring str 0 +modeline-string-truncate-length) "...")
    str))

(defun +modeline-string-cut-beginning (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the beginning of STR by counting from its end up to
`+modeline-string-truncate-length'."
  (if (+modeline--string-truncate-p str)
      (concat "..." (substring str (- +modeline-string-truncate-length)))
    str))

(defun +modeline-string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`+modeline-string-truncate-length' both from its beginning
and end."
  (let ((half (floor +modeline-string-truncate-length 2)))
    (if (+modeline--string-truncate-p str)
        (concat (substring str 0 half) "..." (substring str (- half)))
      str)))

(defun +modeline--first-char (str)
  "Return first character from STR."
  (substring str 0 1))

(defun +modeline-string-abbreviate (str)
  "Abbreviate STR individual hyphen or underscore separated words.
Also see `+modeline-string-abbreviate-but-last'."
  (if (+modeline--string-truncate-p str)
      (mapconcat #'+modeline--first-char (split-string str "[_-]") "-")
    str))

(defun +modeline-string-abbreviate-but-last (str nthlast)
  "Abbreviate STR, keeping NTHLAST words intact.
Also see `+modeline-string-abbreviate'."
  (if (+modeline--string-truncate-p str)
      (let* ((all-strings (split-string str "[_-]"))
             (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
             (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
             (first-component (mapconcat #'+modeline--first-char nbutlast-strings "-"))
             (last-component (mapconcat #'identity last-strings "-")))
        (if (string-empty-p first-component)
            last-component
          (concat first-component "-" last-component)))
    str))

;;;; Keyboard macro indicator

(defvar-local +modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face '+modeline-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

;;;; Narrow indicator

(defvar-local +modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face '+modeline-indicator-cyan-bg)))
  "Mode line construct to report the buffer's narrowed state.")

;;;; Input method

(defvar-local +modeline-input-method
    '(:eval
      (when current-input-method-title
        (propertize (format " %s " current-input-method-title)
                    'face '+modeline-indicator-green-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct to report the multilingual environment.")

;;;; Buffer status

;; TODO 2023-07-05: What else is there beside remote files?  If
;; nothing, this must be renamed accordingly.
(defvar-local +modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    'face '+modeline-indicator-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

;;;; Meow state

(defvar meow-state)
(defvar meow-visual-selection)

(defconst +modeline-meow-state-tags
  '((normal :short "<N>" :long "NORMAL")
    (insert :short "<I>" :long "INSERT")
    (beacon :short "<B>" :long "BEACON")
    (motion :short "<M>" :long "MOTION")
    (keypad :short "<K>" :long "KEYPAD")
    (expand :short "<E>" :long "EXPAND")
    (structural :short "<S>" :long "STRUCT")
    (simple-motion :short "<m>" :long "SMOTION"))
  "Short and long tags for Meow states.")

(defun +modeline--meow-get-tag (state variant)
  "Get Meow STATE tag of VARIANT :short or :long.
VARIANT of the state tag is either :short or :long, as defined in
`+modeline-meow-state-tags'."
  (let ((tags (alist-get state +modeline-meow-state-tags)))
    (plist-get tags (or variant :short))))

(defun +modeline--meow-get-format-specifier (variant)
  "Return a `format' specifier for VARIANT.
VARIANT of the state tag is either :short or :long, as defined in
`+modeline-meow-state-tags'."
  (if (eq variant :short)
      " %-5s"
    " %-8s"))

(defun +modeline--meow-propertize-tag (state variant &optional face)
  "Propertize STATE tag of VARIANT with optional FACE.
VARIANT of the state tag is either :short or :long, as defined in
`+modeline-meow-state-tags'. If FACE is nil, fall back to `default'."
  (propertize
   (format (+modeline--meow-get-format-specifier variant) (+modeline--meow-get-tag state variant))
   'face (or face 'mode-line)
   'mouse-face 'mode-line-highlight
   'help-echo (format "Meow `%s' state" state)))

(defun +modeline-meow-state-tag (variant)
  "Return mode line tag VARIANT depending on the Meow state.
VARIANT of the state tag is either :short or :long, as defined in
`+modeline-meow-state-tags'."
  (pcase (meow--current-state)
    ('normal (+modeline--meow-propertize-tag 'normal variant '+modeline-indicator-blue))
    ('insert (+modeline--meow-propertize-tag 'insert variant '+modeline-indicator-magenta))
    ('beacon (+modeline--meow-propertize-tag 'beacon variant '+modeline-indicator-red))
    ('expand (+modeline--meow-propertize-tag 'expand variant '+modeline-indicator-yellow))
    ('motion (+modeline--meow-propertize-tag 'motion variant '+modeline-indicator-cyan))
    ('simple-motion (+modeline--meow-propertize-tag 'simple-motion variant '+modeline-indicator-cyan))
    ('keypad (+modeline--meow-propertize-tag 'keypad variant '+modeline-indicator-blue))
    ('structural (+modeline--meow-propertize-tag 'structural variant '+modeline-indicator-gray))))

(defvar +modeline-meow-state
  '(:eval
    (if (and (mode-line-window-selected-p) (bound-and-true-p meow-mode))
	(let ((variant (if (+modeline--truncate-p) :short :long)))
	  (+modeline-meow-state-tag variant))
      " "))
  "Mode line construct to display the Meow state.")

;;;; Dedicated window

(defvar-local +modeline-window-dedicated-status
    '(:eval
      (when (window-dedicated-p)
        (propertize " = "
                    'face '+modeline-indicator-gray-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for dedicated window indicator.")

;;;; Buffer name and modified status

(defun +modeline-buffer-identification-face ()
  "Return appropriate face or face list for `+modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun +modeline--buffer-name ()
  "Return `buffer-name', truncating it if necessary.
See `+modeline-string-cut-middle'."
  (when-let ((name (buffer-name)))
    (+modeline-string-cut-middle name)))

(defun +modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (+modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun +modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `+modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local +modeline-buffer-identification
    '(:eval
      (propertize (+modeline-buffer-name)
                  'face (+modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (+modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

;;;; Position

(defvar-local +modeline-position
    '(:eval
      (propertize (format-mode-line "%l:%c")
		  'face 'shadow
		  'mouse-face 'mode-line-highlight))
  "Mode line construct for displaying the position in the buffer.
Displays the line number and the column number.")

;;;; Major mode

(defun +modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'dired-mode) (nerd-icons-icon-for-mode major-mode))
                    ((derived-mode-p 'nix-mode) (nerd-icons-flicon "nf-linux-nixos"))
                    ((derived-mode-p 'org-mode) (nerd-icons-icon-for-file (buffer-file-name)))
                    ;; ((derived-mode-p 'comint-mode) (nerd-icons-faicon "nf-fa-terminal")) ; ">_"
                    ;; ((derived-mode-p 'eshell-mode) (nerd-icons-faicon "nf-fa-terminal")) ; ">_"
                    ((derived-mode-p 'text-mode) (nerd-icons-faicon "nf-fa-file_text_o")) ; "§"
		    ;; (t (nerd-icons-icon-for-file (buffer-file-name)))
                    ;; (t (nerd-icons-octicon "nf-oct-dot")) ; "◦"
		    (t (nerd-icons-icon-for-mode major-mode)))))
    (propertize indicator)))

(defun +modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun +modeline-major-mode-help-echo ()
  "Return `help-echo' value for `+modeline-major-mode'."
  (if-let ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local +modeline-major-mode
    (list
     (propertize "%[" 'face '+modeline-indicator-red)
     '(:eval
       (concat
        (+modeline-major-mode-indicator)
        " "
        (propertize
         (+modeline-string-abbreviate-but-last
          (+modeline-major-mode-name)
          2)
         'mouse-face 'mode-line-highlight
         'help-echo (+modeline-major-mode-help-echo))))
     (propertize "%]" 'face '+modeline-indicator-red))
  "Mode line construct for displaying major modes.")

(defvar-local +modeline-process
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

;;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun +modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    (capitalize branch)))

;; NOTE 2023-07-27: This is a good idea, but it hardcodes Git, whereas
;; I want a generic VC method.  Granted, I only use Git but I still
;; want it to work as a VC extension.

;; (defun +modeline-diffstat (file)
;;   "Return shortened Git diff numstat for FILE."
;;   (when-let* ((output (shell-command-to-string (format "git diff --numstat %s" file)))
;;               (stats (split-string output "[\s\t]" :omit-nulls "[\s\f\t\n\r\v]+"))
;;               (added (nth 0 stats))
;;               (deleted (nth 1 stats)))
;;     (cond
;;      ((and (equal added "0") (equal deleted "0"))
;;       "")
;;      ((and (not (equal added "0")) (equal deleted "0"))
;;       (propertize (format "+%s" added) 'face 'shadow))
;;      ((and (equal added "0") (not (equal deleted "0")))
;;       (propertize (format "-%s" deleted) 'face 'shadow))
;;      (t
;;       (propertize (format "+%s -%s" added deleted) 'face 'shadow)))))

(declare-function vc-git-working-revision "vc-git" (file))

(defvar +modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map)
  "Keymap to display on VC indicator.")

(defun +modeline--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun +modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (nerd-icons-devicon "nf-dev-git_branch")) ; (char-to-string #xE0A0)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (+modeline--vc-help-echo file)
               'local-map +modeline-vc-map)
   ;; " "
   ;; (+modeline-diffstat file)
   ))

(defun +modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (+modeline-string-cut-end
   (+modeline--vc-text file branch face)))

(defvar +modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun +modeline--vc-get-face (key)
  "Get face from KEY in `+modeline--vc-faces'."
  (alist-get key +modeline--vc-faces 'up-to-date))

(defun +modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (+modeline--vc-get-face (vc-state file backend)))

(defvar-local +modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  ;; ((vc-git-registered file))
                  (branch (+modeline--vc-branch-name file backend))
                  (face (+modeline--vc-face file backend)))
        (+modeline--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")

;;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

;; Based on `flymake--mode-line-counter'.
(defun +modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar +modeline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line down-mouse-3] 'flymake-show-project-diagnostics)
    map)
  "Keymap to display on Flymake indicator.")

(defmacro +modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "+modeline-flymake-%s" type)) ()
     (when-let ((count (+modeline-flymake-counter
                        ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'shadow)
        (propertize count
                    'face ',(or face type)
                    'mouse-face 'mode-line-highlight
                    ;; FIXME 2023-07-03: Clicking on the text with
                    ;; this buffer and a single warning present, the
                    ;; diagnostics take up the entire frame.  Why?
                    'local-map +modeline-flymake-map
                    'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")
	" "))))

(+modeline-flymake-type error "x") ; "☣"
(+modeline-flymake-type warning "!") ; "!"
(+modeline-flymake-type note "i" success) ; "·"

(defvar-local +modeline-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         ;; See the calls to the macro `+modeline-flymake-type'
         '(:eval (+modeline-flymake-error))
         '(:eval (+modeline-flymake-warning))
         '(:eval (+modeline-flymake-note)))))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

;;;; Eglot

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local +modeline-eglot
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

;;;; Miscellaneous

(defvar-local +modeline-notmuch-indicator
    '(notmuch-indicator-mode
      (" "
       (:eval (when (mode-line-window-selected-p)
                notmuch-indicator--counters))))
  "The equivalent of `notmuch-indicator-mode-line-construct'.
Display the indicator only on the focused window's mode line.")

(defvar-local +modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

;;;; Risky local variables

;; NOTE 2023-04-28: The `risky-local-variable' is critical, as those
;; variables will not work without it.
(dolist (construct '(+modeline-kbd-macro
                     +modeline-narrow
                     +modeline-input-method
                     +modeline-buffer-status
                     +modeline-window-dedicated-status
		     +modeline-meow-state
                     +modeline-buffer-identification
		     +modeline-position
                     +modeline-major-mode
                     +modeline-process
                     +modeline-vc-branch
                     +modeline-flymake
                     +modeline-eglot
                     +modeline-notmuch-indicator
                     +modeline-misc-info))
  (put construct 'risky-local-variable t))

(provide 'modeline-extras)
;;; modeline-extras.el ends here
