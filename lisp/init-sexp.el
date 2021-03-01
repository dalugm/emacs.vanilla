;;; init-sexp.el --- S-expression -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for dealing with S-expressions.
;;

;;; Code:

;; https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
(defun my/endless-sharp ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

(define-key emacs-lisp-mode-map "#" #'my/endless-sharp)

(provide 'init-sexp)

;;; init-sexp.el ends here
