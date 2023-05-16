;;; init-check.el --- checker for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Syntax and spelling check.
;;

;;; Code:

(with-eval-after-load 'flymake
  (global-set-key (kbd "C-c ! b") #'flymake-show-buffer-diagnostics)
  (global-set-key (kbd "C-c ! p") #'flymake-show-project-diagnostics))

(with-eval-after-load 'flyspell
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args
          '("--sug-mode=ultra" "--lang=en_US" "--camel-case")))))

(provide 'init-check)

;;; init-check.el ends here
