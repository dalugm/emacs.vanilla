;;; init-ui.el --- Emacs user interface -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  UI configuration.
;;

;;; Code:

;; NOTE: `tool-bar-mode' and `scroll-bar-mode' are not defined in some cases
;; https://emacs-china.org/t/topic/5159/12
;; https://github.com/vijaykiran/spacemacs/commit/b2760f33e5c77fd4a073bc052e7b3f95eedae08f
;; removes the GUI elements
;; NO scroll-bar, tool-bar
(when window-system
  (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1))
    (tool-bar-mode -1))
  (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1))
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1)))

;; NO menu-bar
;; BUT there's no point in hiding the menu bar on mac, so let's not do it
(unless sys/mac-x-p
  (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1))
    (menu-bar-mode -1)))

;;; `mode-line'

;; ;; If you want to customize time format, read document of `format-time-string'
;; ;; and customize `display-time-format'.
;; (setq display-time-format "%a %b %e")

;; see `info'
(setq system-time-locale "C")
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; do NOT display the load average
(setq display-time-default-load-average nil)
;; show date in mode-line
(display-time)

;; line-column
;; To make the position number update correctly in all cases
(line-number-mode +1)
(column-number-mode +1)

;; make displayed column number to count from 1
(setq column-number-indicator-zero-based nil)

;; human readable representation of file size in mode-line
(size-indication-mode +1)

(provide 'init-ui)

;;; init-ui.el ends here
