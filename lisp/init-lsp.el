;;; init-lsp.el --- lsp configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Language Server Protocol (LSP) configuration.
;;

;;; Code:

(global-set-key (kbd "C-c l l") #'eglot)

(with-eval-after-load 'eglot
  (global-set-key (kbd "C-c l a") #'eglot-code-actions)
  (global-set-key (kbd "C-c l c") #'eglot-show-workspace-configuration)
  (global-set-key (kbd "C-c l f") #'eglot-format)
  (global-set-key (kbd "C-c l q") #'eglot-shutdown)
  (global-set-key (kbd "C-c l Q") #'eglot-shutdown-all)
  (global-set-key (kbd "C-c l r") #'eglot-rename))

(provide 'init-lsp)

;;; init-lsp.el ends here
