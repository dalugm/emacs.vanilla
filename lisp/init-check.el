;;; init-check.el --- checker for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Syntax and spelling check.
;;

;;; Code:

(dolist (hook '(c++-mode-hook c-mode-hook))
  (add-hook hook #'flymake-mode))

(with-eval-after-load 'flymake
  (global-set-key (kbd "C-c ! n") #'flymake-goto-next-error)
  (global-set-key (kbd "C-c ! p") #'flymake-goto-prev-error)
  (global-set-key (kbd "C-c ! d") #'flymake-show-buffer-diagnostics)
  (global-set-key (kbd "C-c ! D") #'flymake-show-project-diagnostics)
  (global-set-key (kbd "C-c ! s") #'flymake-start))

(with-eval-after-load 'flyspell
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args
          '("--sug-mode=ultra" "--lang=en_US" "--camel-case")))))

(provide 'init-check)

;;; init-check.el ends here
