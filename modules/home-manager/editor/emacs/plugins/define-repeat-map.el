;;; define-repeat-map.el --- Easy-define repeat-maps -*- lexical-binding: t -*-

;; Copyright (C) 2021 Case Duckworth

;; Author: Case Duckworth <acdw@acdw.net>
;; Keywords: convenience
;; URL: https://tildegit.org/acdw/define-repeat-map.el/

;;; License:

;; Everyone is permitted to do whatever with this software, without
;; limitation.  This software comes without any warranty whatsoever,
;; but with two pieces of advice:
;; - Don't hurt yourself.
;; - Make good choices.

;;; Commentary:

;; Emacs 28 comes built-in with repeat.el (which see), which allows users to
;; define their own maps to repeat common commands easily.  This package
;; attempts to make the definition of those maps a one-sexp affair, through the
;; macro `define-repeat-map'.  See its docstring for details.

;;; Code:

(defun define-repeat-map--make-alias (cmd map)
  "Internal.  Make an alias for CMD in `repeat-map' MAP."
  (intern (concat (symbol-name cmd) "|"
                  (symbol-name map))))

(defun define-repeat-map--map-commands (fn args)
  "Internal.  Map FN over ARGS, whch are commands in MAP."
  (let (res)
    (dolist (arg args)
      (unless (stringp arg)
        (push (funcall fn arg) res)))
    (reverse res)))

(defun define-repeat-map--define-keys (map fn args)
  "Internal.  Map `define-key' in MAP over ARGS, transorming them with FN."
  (unless (zerop (mod (length args) 2))
    (signal 'wrong-number-of-arguments (length args)))
  (let (res)
    (while args
      (let ((key (pop args))
            (cmd (funcall fn (pop args))))
        (push `(define-key ,map (kbd ,key) #',cmd)
              res)))
    (reverse res)))

;;;###autoload
(defmacro define-repeat-map (name &rest keys)
  "Define a `repeat-map', NAME -repeat-map, and bind KEYS to it.
Each ARG is a list of lists containing keybind definitions of
the form (KEY DEFINITION) KEY is anything `kbd' can recognize,
and DEFINITION is passed directly to `define-key'.

Optionally, the car of an arglist can contain the following
symbols, which changes the behavior of the key definitions in the
rest of the list:

:enter - Provided commands can enter the `repeat-map', but aren't
bound in the map.  They need to be bound elsewhere, however.

:exit - Keys are bound in the `repeat-map', but can't enter the
map.  Their invocation exits the `repeat-map'.

:continue - Keys are bound in the `repeat-map', but can't enter the
map.  However, their invocations keep the `repeat-map' active."
  (declare (indent 1))
  (let ((define-repeat-map--result)
        (map (intern (concat (symbol-name name) "-repeat-map"))))
    ;; Create the keymap
    (push `(defvar ,map (make-sparse-keymap)
             "Defined by `define-repeat-map'.")
          define-repeat-map--result)

    ;; Iterate through KEYS
    (dolist (arg keys)
      (pcase (car arg)
        (:enter
         ;; Add the map to the commands' repeat-map property.
         (push `(progn
                  ,@(define-repeat-map--map-commands
                      (lambda (cmd) `(put ',cmd 'repeat-map ',map))
                      (cdr arg)))
               define-repeat-map--result))
        
        (:exit
         ;; Bind the commands in the map.
         (push `(progn
                  ,@(define-repeat-map--define-keys
                      `,map #'identity (cdr arg)))
               define-repeat-map--result))

        (:continue
         ;; Make an alias for each command, and process that alias like the
         ;; default, below.
         (push `(progn
                  ,@(define-repeat-map--define-keys
                      `,map
                      (lambda (cmd) (define-repeat-map--make-alias cmd map))
                      (cdr arg))
                  ,@(define-repeat-map--map-commands
                      (lambda (cmd)
                        (let ((alias (define-repeat-map--make-alias cmd map)))
                          `(progn
                             (defalias ',alias ',cmd
                               "Defined by `define-repeat-map'.")
                             (put ',alias
                                  'repeat-map ',map))))
                      (cdr arg)))
               define-repeat-map--result))

        (_
         ;; Default: bind the commands in the map, and add the map to the
         ;; commands' repeat-map property.
         (push `(progn
                  ,@(define-repeat-map--define-keys `,map #'identity arg)
                  ,@(define-repeat-map--map-commands
                      (lambda (cmd) `(put ',cmd 'repeat-map ',map))
                      arg))
               define-repeat-map--result))))

    `(add-hook 'repeat-mode-hook
               (lambda nil
                 ,@(reverse define-repeat-map--result)))))

(provide 'define-repeat-map)
;;; define-repeat-map.el ends here
