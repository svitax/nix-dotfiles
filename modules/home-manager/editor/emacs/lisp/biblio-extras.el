;;; biblio-extras.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +biblio-lookup ()
  "Combines biblio-lookup and biblio-doi-insert-bibtex."
  (interactive)
  (let* ((dbs (biblio--named-backends))
         (db-list (append dbs '(("DOI" . biblio-doi-backend))))
         (db-selected (biblio-completing-read-alist
                       "Database:"
                       db-list)))
    (if (eq db-selected 'biblio-doi-backend)
        (let ((doi (read-string "DOI: ")))
          (biblio-doi-insert-bibtex doi))
      (biblio-lookup db-selected))))

(defun +biblio-bibtex-lookup ()
  "Select BibTeX file, perform a lookup with Biblio and insert entry."
  (interactive)
  (let ((bibfile (completing-read
                  "BibTeX file:"
                  (citar--bibliography-files))))
    (find-file bibfile)
    (goto-char (point-max))
    (sx-biblio-lookup)
    (save-buffer)))

(provide 'biblio-extras)
;;; biblio-extras.el ends here
