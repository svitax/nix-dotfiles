;;; cape-extras.el -*- lexical-binding: t; -*-

(defun +cape-wrap-max-prefix-length (capf length)
  "Call CAPF and ensure that prefix length is greater or equal than LENGTH.
If the prefix is long enough, enforce auto completion."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     (when (<= (- end beg) length)
       `(,beg ,end ,table
	 :company-prefix-length t
	 ,@plist)))))

(defun +cape-capf-max-prefix-length (capf length)
  (lambda () (funcall #'+cape-wrap-max-prefix-length capf length)))

(defalias '+cape-keyword-with-length-limit (+cape-capf-max-prefix-length #'cape-keyword 2))

(provide 'cape-extras)
;;; cape-extras.el ends here
