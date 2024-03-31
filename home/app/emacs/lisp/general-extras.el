;;; general-extras.el -*- lexical-binding: t; -*-

(require 'general)

;; Create SPC leader key, to be used in the macro.
(general-create-definer global-definer
  :keymaps 'override
  :states  '(normal hybrid motion visual operator)
  :prefix  "SPC"
  :non-normal-prefix "C-SPC")

;; Add a definer for each of the major-modes
(general-create-definer major-mode-definer
  :keymaps 'override
  :states '(normal hybrid motion visual operator)
  :prefix ","
  "" '(:ignore t :which-key "localleader"))

;; Add an additional minor-mode-definer, for each of the modes.
;; It is key to remember that in this case, the :keymaps option refers to the minor-mode,
;; not the keymap.
(general-create-definer minor-mode-definer
  :keymaps 'override
  :definer 'minor-mode
  :states '(normal hybrid motion visual operator)
  :prefix ",")

;; Macro to define all key-pockets. It adapts to the name passed, and defines additonal macros to be
;; used to define keybindings. See `general-global-buffer' below.
(defmacro +general-global-menu! (name infix-key &rest body)
  "Create a definer named +general-global-NAME wrapping global-definer.
      Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
  (declare (indent 2))
  `(progn
     (general-create-definer ,(intern (concat "+general-global-" name))
       :wrapping global-definer
       :prefix-map (quote ,(intern (concat "+general-global-" name "-map")))
       :infix ,infix-key
       :wk-full-keys nil
       "" '(:ignore t :which-key ,name))
     (,(intern (concat "+general-global-" name))
      ,@body)))

(provide 'general-extras)
