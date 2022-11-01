;;; init.el --- Emacs start file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Emacs start-up configuration.
;;

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq user-init-file (or load-file-name buffer-file-name))
(setq user-emacs-directory (file-name-directory user-init-file))

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

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

;;; Configuration

;; Load configs for specific features and modes
(require 'init-utils)
(require 'init-modeline)
(require 'init-funcs)
(require 'init-dired)
(require 'init-org)
(require 'init-ibuffer)
(require 'init-ido)
(require 'init-prog)
(require 'init-check)

(require 'init-package)

;; handy tools though not must have
(when (display-graphic-p)
  (require 'init-gui))
(require 'init-misc)
(require 'init-term)

;; program
(require 'init-tex)
(require 'init-sexp)
(require 'init-cc)
(require 'init-js)
(require 'init-python)

;; personal setup, other major-mode specific setup need it.
(load (expand-file-name "~/.custom.el") t nil)

;; https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
;; See `custom-file' for details.
(load (setq custom-file
            (expand-file-name (concat user-emacs-directory
                                      "custom-set-variables.el")))
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
