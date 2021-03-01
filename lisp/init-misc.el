;;; init-misc.el --- misc config for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; misc configuration.
;;

;;; Code:

;; show fortune in Emacs
(with-eval-after-load 'fortune
  (when (or sys/macp sys/linuxp)
    (let ((fortune (cond
                     (sys/macp "/usr/local/Cellar/fortune/9708/share/games/fortunes")
                     (sys/linuxp "/usr/share/games/fortunes"))))
      (setq fortune-file fortune))))

;; network proxy
(defvar my-proxy "127.0.0.1:1087"
  "Network proxy.")

;; allow access from `emacsclient'
(add-hook 'after-init-hook
  (lambda ()
    (require 'server)
    (unless (server-running-p)
      (message "Starting a server...")
      (server-start))))

;; calendar
(setq calendar-chinese-all-holidays-flag t)
(setq holiday-local-holidays `((holiday-fixed 3 8  "Women's Day")
                               (holiday-fixed 3 12 "Arbor Day")
                               ,@(cl-loop for i from 1 to 3
                                          collect `(holiday-fixed 5 ,i "International Workers' Day"))
                               (holiday-fixed 5 4  "Chinese Youth Day")
                               (holiday-fixed 6 1  "Children's Day")
                               (holiday-fixed 9 9  "Mourn of Mao's Death")
                               (holiday-fixed 9 10 "Teachers' Day")
                               ,@(cl-loop for i from 1 to 7
                                          collect `(holiday-fixed 10 ,i "National Day"))
                               (holiday-fixed 12 26 "Mao's Birthday")))

(provide 'init-misc)

;;; init-misc.el ends here
