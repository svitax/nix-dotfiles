;;; org-noter-extras.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; TODO: docstring for +org-noter-denote--create-note
(defun +org-noter-denote--create-note (&optional title keywords)
  ""
  (list (denote title
		(append (split-string keywords "_") '("literature"))
		denote-file-type
		nil
		nil
		nil
		nil)))

;; NOTE: citar-denote integration (_ (citar-denote-add-citekey))
(defun +org-noter-denote--find-note (doc-path)
  "Find a denote file whose `org-noter-property-doc-file' property
(default :NOTER_DOCUMENT:) matches the document we are trying to open.

Returns the path to the note's file."
  ;; Extracts the file name from the doc-path
  (let* ((title (denote-retrieve-filename-title doc-path))
	 (keywords (denote-retrieve-filename-keywords doc-path))
	 (denote-files (directory-files denote-directory t nil t))
	 (regex-list `("_literature" ,(concat "-" title)))
	 (filtered-files (seq-filter (lambda (file)
				       (seq-every-p (lambda (regex)
						      (string-match-p regex (file-name-nondirectory file)))
						    regex-list))
				     denote-files)))
    (if (eq filtered-files nil)
	(+org-noter-denote--create-note title keywords)
      filtered-files)))
(defun +org-noter-denote--create-top-level-heading (heading doc-path)
  "Create a top level heading for document at DOC-PATH and return it's
position."
  (goto-char (point-max))
  (insert (if (save-excursion (beginning-of-line) (looking-at "[[:space:]]*$")) "" "\n")
	  "* "
	  org-noter-headline-title-decoration
	  heading
	  org-noter-headline-title-decoration)
  (org-entry-put nil org-noter-property-doc-file
		 (expand-file-name doc-path))
  (org-id-get-create)
  (point))
(defun +org-noter-denote--find-top-level-heading-position (doc-path)
  (let ((found-heading-position nil))
    (org-with-point-at (point-min)
      (condition-case nil
	  (while (and (not found-heading-position)
		      (re-search-forward (org-re-property org-noter-property-doc-file)))
	    (let ((current-file-name (expand-file-name (match-string 3)))
		  (looking-for-filename (expand-file-name doc-path)))
	      (when (file-equal-p current-file-name looking-for-filename)
		(setq found-heading-position (point)))))
	(search-failed
	 (message "This buffer doesn't seem to have a matching NOTER_DOCUMENT heading.") nil)))
    found-heading-position))
(defun +org-noter-denote--find-top-level-heading (doc-path)
  "In current buffer, look for a top level heading for document at DOC-PATH.
If one is not found, create and return it's position."
  (let* ((top-level-heading-position (+org-noter-denote--find-top-level-heading-position doc-path))
	 (title (denote-retrieve-filename-title doc-path)))
    (if (eq top-level-heading-position nil)
	(+org-noter-denote--create-top-level-heading title doc-path)
      top-level-heading-position)))

(defun +org-noter-denote--create-session-from-document-file
    (&optional arg doc-path)
  "Main point of integration with org-noter.

This is a hook function that is to be assigbned to
`org-noter-create-session-from-document-hook' to enable denote integration:

	`(setq org-noter-create-session-from-document-hook '(+org-noter-denote--create-session-from-document-file))

ARG is not currently used but here for compatibility reasons.
DOC-PATH is the path to the document (pdf, epub, djvu)."
  ;; Check if a denote file for specified document path already exists.
  ;; If it doesn't, ask the user to specify one.
  (let* ((file-path-for-denote-file (car (+org-noter-denote--find-note doc-path)))
	 ;; Create or find a top level heading for the document and return it.
	 (top-level-heading-position (with-current-buffer (find-file-noselect file-path-for-denote-file)
				       (+org-noter-denote--find-top-level-heading doc-path))))
    (with-current-buffer (find-file-noselect file-path-for-denote-file)
      (goto-char top-level-heading-position)
      (org-noter))))

(provide 'org-noter-extras)
;;; org-noter-extras.el ends here
