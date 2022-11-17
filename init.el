;;; init.el --- Emacs start file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Emacs start-up configuration.
;;

;;; Code:

(setq user-init-file (or load-file-name buffer-file-name))
(setq user-emacs-directory (file-name-directory user-init-file))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(when (< emacs-major-version 27)
  (load (expand-file-name "early-init.el" user-emacs-directory)))

(let ((old-file-name-handler-alist file-name-handler-alist)
      (old-gc-cons-threshold gc-cons-threshold))
  (setq file-name-handler-alist nil)
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'emacs-startup-hook
            (lambda ()
              "Speed up startup."
              (setq file-name-handler-alist
                    (delete-dups (append file-name-handler-alist
                                         old-file-name-handler-alist)))
              ;; if x10, half of cpu time is spent on gc when scrolling
              (setq gc-cons-threshold (* 100 old-gc-cons-threshold)))))

(require 'init-utils)
(require 'init-modeline)
(require 'init-funcs)
(require 'init-dired)
(require 'init-org)
(require 'init-ibuffer)

(when (display-graphic-p)
  (require 'init-gui))

(require 'init-package)
(require 'init-ido)
(require 'init-prog)
(require 'init-check)

;; handy tools though not must have
(require 'init-misc)
(require 'init-term)

;; program
(require 'init-tex)
(require 'init-sexp)
(require 'init-js)
(require 'init-python)

;; personal setup
(load (expand-file-name "~/.custom.el") t nil)

(load
 (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
 t t)

(message "*** Emacs loaded in %s with %d garbage collections. ***"
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))
         gcs-done)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init.el ends here
