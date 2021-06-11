;;; init.el --- Emacs start file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Emacs start-up configuration.
;;

;;; Code:

;;; Packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; HTTPS URLs should be used where possible
;; as they offer superior security
(with-eval-after-load 'package
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    (setq package-archives
      `(;; emacs-china
         ,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
         ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))
         ,(cons "org"   (concat proto "://elpa.emacs-china.org/org/"))
         ;; official
         ;; ,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
         ;; ,(cons "melpa" (concat proto "://melpa.org/packages/"))
         ;; ,(cons "org"   (concat proto "://orgmode.org/elpa/"))
         ))))

;;; Garbage Collection
;; https://www.reddit.com/r/emacs/comments/brc05y/is_lspmode_too_slow_to_use_for_anyone_else/eofulix/
(defvar my--gc-cons-threshold-up-limit (* 100 1024 1024)
  "Best up-limit GC threshold value.  Should NOT be too big!")

(defvar my--gc-cons-threshold-default (* 20 1024 1024)
  "Default GC threshold value.")

(defun my//inc-gc-cons-threshold ()
  "Increase `gc-cons-threshold' to `my--gc-cons-threshold-up-limit'."
  (setq gc-cons-threshold my--gc-cons-threshold-up-limit))

(defun my//reset-gc-cons-threshold ()
  "Rest `gc-cons-threshold' to `my--gc-cons-threshold-default'."
  (setq gc-cons-threshold my--gc-cons-threshold-default))

;; Avoid Emacs do GC during the initializing
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(progn (my//inc-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
    (lambda ()
      (my//reset-gc-cons-threshold)
      (add-hook 'minibuffer-setup-hook #'my//inc-gc-cons-threshold)
      (add-hook 'minibuffer-exit-hook  #'my//reset-gc-cons-threshold))))

;; For speedup initialize
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Normally value of `file-name-handler-alist' is
;;   (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;;   ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;;   ("\\`/:" . file-name-non-special))
;; which means on every .el and .elc file loaded during start up, it has to run
;; those regexps against the filename.

(let ((file-name-handler-alist nil))

  (require 'init-utils)
  (require 'init-ui)
  (require 'init-funcs)
  (require 'init-dired)
  (require 'init-org)
  (require 'init-filetype)
  (require 'init-ibuffer)
  (require 'init-prog)
  (require 'init-misc)
  (require 'init-term)
  (require 'init-ido)

  ;; program
  (require 'init-sexp)
  (require 'init-cc)
  (require 'init-ruby)
  (require 'init-python)

  ;; personal setup, other major-mode specific setup need it.
  (load (expand-file-name "~/.custom.el") t nil)

  ;; https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
  ;; See `custom-file' for details.
  (load (setq custom-file (expand-file-name (concat user-emacs-directory "custom-set-variables.el"))) t t))

;; Local Variables:
;; coding: utf-8
;; End:
;;; init.el ends here
