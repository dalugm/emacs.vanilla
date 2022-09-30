;;; init-complete.el --- Complete setup -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Complete-related configuration.
;;

;;; Code:

(if (fboundp 'fido-mode)
    (progn
      (fido-mode +1)
      (when (fboundp 'fido-vertical-mode)
        (fido-vertical-mode +1)))
  (progn
    (ido-mode +1)
    (ido-everywhere +1)))

(provide 'init-complete)

;;; init-complete.el ends here
