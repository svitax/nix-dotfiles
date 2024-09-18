;;; use-package-bind.el --- Bind support for use-package -*- lexical-binding: t; -*-

;; Copyright (C) 2024 svitax, repelliuss

;; Author: svitax <https://github.com/svitax, repelliuss <https://github.com/repelliuss>,
;; Maintainer: svitax <svitaxiom@protonmail.com>
;; Created: September 14, 2024
;; Modified: September 14, 2024
;; Version: 0.0.1
;; Package-Requires: ((bind "0.9") (use-package "2.4") (emacs "25.1"))

;; Homepage: https://github.com/svitax/use-package-bind

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds support for bind.el in use-package
;; `:main-file' metadata inserted so `bind-autoload' doesn't need explicit file name.
;; Active map of current use-package context is inserted to *first FORM of `bind' FORM if it
;; is insertable.  It is insertable if no map is given or a list of maps are given explicitly.
;; For example,
;; (bind c-mode-map "c" #'foo ...), the map won't be inserted.
;; (bind "c" #'foo), the map will be inserted and act like (bind pkg-mode-map "c" #'foo)
;; (bind (c-mode-map) "c" #'foo), the map will be inserted and act like (bind (pkg-mode-map c-mode-map) "c" #'foo).
;; (bind (my-function-returning-maps) "c" #'foo), the map won't be inserted.
;; Note that if there are multiple `bind' FORMS, the first one will be selected.

;;; Code:

(require 'use-package)
(require 'bind)
(require 'cl-lib)

(defun use-package-bind--normalize (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label form)
      (let ((map (make-symbol (concat (symbol-name name-symbol) "-mode-map"))))
	(if (bind--singularp form)
	    (pcase (bind--map-insertable-formp form)
	      ('no form)
	      ('yes `(,map ,@form))
	      ('yes-merge `((,map ,@(car form))
			    ,@(cdr form))))
	  (pcase (bind--map-insertable-formp (car form))
	    ('no form)
	    ('yes `((,map ,@(car form))
		    ,@(cdr form)))
	    ('yes-merge `(((,map ,@(caar form))
			   ,@(cdar form))
			  ,@(cdr form)))))))))

(defalias 'use-package-normalize/:bind #'use-package-bind--normalize)

(defun use-package-bind--handler (name-symbol _keyword form rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    (use-package-concat
     body
     `((bind-with-metadata (:main-file ,(symbol-name name-symbol))
	 (bind ,@form))))))

(defalias 'use-package-handler/:bind #'use-package-bind--handler)

(defun use-package-bind--add-to-use-package-keywords (keyword)
  (unless (member keyword use-package-keywords)
    (setopt use-package-keywords
	    (--splice-list (equal it :custom) `(:custom ,keyword) use-package-keywords))))

(use-package-bind--add-to-use-package-keywords :bind)

(provide 'use-package-bind)
