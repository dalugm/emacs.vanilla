;;; init-prog.el --- Programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Generic Programming Configuration.
;;

;;; Code:

;; Eldoc.
(with-eval-after-load 'eldoc
  ;; Multi-line message should not display too soon.
  (setq eldoc-idle-delay 1)
  (setq eldoc-echo-area-use-multiline-p t))

;; Tags.
;; Don't ask before rereading the TAGS files if they have changed.
(setq tags-revert-without-query t)

(when (treesit-available-p)
  (require 'treesit)

  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c.git"))
          (cmake      . ("https://github.com/uyha/tree-sitter-cmake.git"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
          (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css.git"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git"))
          (elixir     . ("https://github.com/elixir-lang/tree-sitter-elixir.git"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go.git"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (heex       . ("https://github.com/phoenixframework/tree-sitter-heex.git"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html.git"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json.git"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python.git"))
          (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby.git"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml.git"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml.git"))))

  (dolist (list `((cmake      . (,(rx (or "CMakeLists.txt" ".cmake") eos) . cmake-ts-mode))
                  (dockerfile . (,(rx "Dockerfile" (opt "." (zero-or-more nonl)) eos) . dockerfile-ts-mode))
                  (elixir     . (,(rx (or ".elixir" (seq ".ex" (opt "s")) "mix.lock") eos) . elixir-ts-mode))
                  (go         . (,(rx ".go" eos) . go-ts-mode))
                  (gomod      . (,(rx "/go.mod" eos) . go-mod-ts-mode))
                  (heex       . (,(rx "." (opt (any "hl")) "eex" eos) . heex-ts-mode))
                  (rust       . (,(rx ".rs" eos) . rust-ts-mode))
                  (tsx        . (,(rx ".tsx" eos) . tsx-ts-mode))
                  (typescript . (,(rx ".ts" eos) . typescript-ts-mode))
                  (yaml       . (,(rx ".y" (opt "a") "ml" eos) . yaml-ts-mode))))
    (let ((parser (car list))
          (alist (cdr list)))
      (when (treesit-ready-p parser 'message)
        (add-to-list 'auto-mode-alist alist))))

  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (c-or-c++-mode   . c-or-c++-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (csharp-mode     . csharp-ts-mode)
          (css-mode        . css-ts-mode)
          (html-mode       . html-ts-mode)
          (java-mode       . java-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (js-mode         . js-ts-mode)
          (python-mode     . python-ts-mode)
          (ruby-mode       . ruby-ts-mode)
          (sh-mode         . bash-ts-mode))))

(defvar my-last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(with-eval-after-load 'compile
  ;; Save before compiling.
  (setq compilation-ask-about-save nil)
  ;; Kill old compile processes before starting the new one.
  (setq compilation-always-kill t)
  ;; Automatically scroll to first error.
  (setq compilation-scroll-output 'first-error)

  (defun my--save-compilation-buffer (&rest _)
    "Save the compilation buffer to find it later."
    (setq my-last-compilation-buffer next-error-last-buffer))

  (advice-add 'compilation-start :after #'my--save-compilation-buffer)

  (defun my--find-prev-compilation (orig &optional edit-command)
    "Find the previous compilation buffer, if present, and recompile there."
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             my-last-compilation-buffer
             (buffer-live-p (get-buffer my-last-compilation-buffer)))
        (with-current-buffer my-last-compilation-buffer
          (funcall orig edit-command))
      (funcall orig edit-command)))

  (advice-add 'recompile :around #'my--find-prev-compilation))

(global-set-key (kbd "C-c c k") #'compile)
(global-set-key (kbd "C-c c r") #'recompile)

;; Colorize output of Compilation Mode.
;; https://stackoverflow.com/a/3072831/355252
(with-eval-after-load 'compile
  (require 'ansi-color)

  (defun my--colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    ;; We don't want to mess with child modes such as grep-mode, ack, ag, etc.
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))

  (add-hook 'compilation-filter-hook #'my--colorize-compilation-buffer))

(defun my--generic-prog-mode-hook-setup ()
  "Generic configuration for `prog-mode'."
  ;; Camel case aware editing operations.
  (subword-mode +1))

(add-hook 'prog-mode-hook #'my--generic-prog-mode-hook-setup)

(provide 'init-prog)

;;; init-prog.el ends here
