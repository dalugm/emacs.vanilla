;;; init-prog.el --- program configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Basic configuration for prog-mode.
;;

;;; Code:

;; ---------------------------------------------------------
;; compile
;; ---------------------------------------------------------
;; Compilation from Emacs
(defun my//colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
;; Just save before compiling
(setq compilation-ask-about-save nil)
;; Just kill old compile processes before starting the new one
(setq compilation-always-kill t)
;; Automatically scroll to first error
(setq compilation-scroll-output 'first-error)

;; Colorize output of Compilation Mode
;; https://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'my//colorize-compilation-buffer)

(provide 'init-prog)

;;; init-prog.el ends here
