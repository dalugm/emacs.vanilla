;;; init-check.el --- checker for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Syntax and spelling check.
;;

;;; Code:

(with-eval-after-load 'flymake
  (global-set-key (kbd "C-c ! n") #'flymake-goto-next-error)
  (global-set-key (kbd "C-c ! p") #'flymake-goto-prev-error)
  (global-set-key (kbd "C-c ! d") #'flymake-show-diagnostics-buffer)
  (global-set-key (kbd "C-c ! s") #'flymake-start))

(with-eval-after-load 'flyspell
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args
        '("--sug-mode=ultra" "--lang=en_US" "--camel-case")))

(provide 'init-check)

;;; init-check.el ends here
