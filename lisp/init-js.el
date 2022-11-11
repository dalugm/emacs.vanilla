;;; init-js.el --- JavaScript programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  JavaScript configuration.
;;

;;; Code:

(add-to-list 'auto-mode-alist
             (cons "\\.\\(m\\|c\\)?js\\'" 'js-mode))

(with-eval-after-load 'js
  (setq js-indent-level 2))

(provide 'init-js)

;;; init-js.el ends here
