;;; init-misc.el --- misc config for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; misc configuration.
;;

;;; Code:

;; show fortune in Emacs
(with-eval-after-load 'fortune
  (when (or my-mac-p my-linux-p)
    (let ((fortune
           (cond
            (my-mac-p "/usr/local/Cellar/fortune/9708/share/games/fortunes")
            (my-linux-p "/usr/share/games/fortunes"))))
      (setq fortune-file fortune))))

;; network proxy
(defcustom my-http-proxy "127.0.0.1:1087"
  "Network proxy."
  :group 'convenience
  :type 'string)

(defcustom my-socks-proxy "127.0.0.1:1080"
  "SOCKS proxy."
  :group 'convenience
  :type 'string)

(defcustom my-wsl-socks-proxy
  (concat
   (shell-command-to-string
    "cat /etc/resolv.conf | grep nameserver | awk '{ printf $2 }'")
   ":"
   "10810")
  "SOCKS proxy in WSL."
  :group 'convenience
  :type 'string)

;; allow access from emacsclient
(run-with-idle-timer 3 nil
                     (lambda ()
                       (require 'server)
                       (unless (server-running-p)
                         (message "Starting a server...")
                         (server-start))))

;; calendar
(setq calendar-chinese-all-holidays-flag t)
(setq holiday-local-holidays
      `((holiday-fixed 3 8  "Women's Day")
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
