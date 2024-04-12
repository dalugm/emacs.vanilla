;;; init-term.el --- terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Terminal inside Emacs.
;;


;;; Code:

;; eshell
(setq eshell-aliases-file (expand-file-name "lisp/eshell/aliases"
                                            user-emacs-directory)
      eshell-rc-script (expand-file-name "lisp/eshell/rc"
                                         user-emacs-directory)
      eshell-login-script (expand-file-name "lisp/eshell/login"
                                            user-emacs-directory))

(provide 'init-term)
;;; init-term.el ends here
