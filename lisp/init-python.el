;;; init-python.el --- programming in python -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Python configuration.
;;

;;; Code:

(defun my//python-mode-hook-setup ()
  "Default configuration for python."
  ;; CamelCase aware editing operations
  (subword-mode +1)
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "python3"))

(add-hook 'python-mode-hook #'my//python-mode-hook-setup)

(provide 'init-python)

;;; init-python.el ends here
