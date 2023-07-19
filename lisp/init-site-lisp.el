;;; init-site-lisp.el --- Support elisp manually installed in the site-lisp dir -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Third party elisp management.
;;

;;; Code:

(defun my--add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (cl-remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
           load-path))))

;; Add both site-lisp and its immediate subdirs to `load-path'.
(let ((site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)))
  (add-to-list 'load-path site-lisp-dir)
  (my--add-subdirs-to-load-path site-lisp-dir))

(provide 'init-site-lisp)

;;; init-site-lisp.el ends here
