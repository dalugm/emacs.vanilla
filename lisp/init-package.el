;;; init-package.el --- config for manage packages -*- lexical-binding:t ; -*-

;;; Commentary:
;;
;; Package management.
;;

;;; Code:

(with-eval-after-load 'package
  (setq package-install-upgrade-built-in t)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    (setq package-archives
          `(

            ;; ;; Official.
            ;; ,(cons "gnu"    (concat proto "://elpa.gnu.org/packages/"))
            ;; ,(cons "nongnu" (concat proto "://elpa.nongnu.org/nongnu/"))
            ;; ,(cons "gnu-devel" (concat proto "://elpa.gnu.org/devel/"))
            ;; ,(cons "nongnu-devel" (concat proto "://elpa.nongnu.org/nongnu-devel/"))
            ;; ,(cons "melpa"  (concat proto "://melpa.org/packages/"))
            ;; ;; ,(cons "melpa-stable" (concat proto "://stable.melpa.org/packages/"))

            ;; ;; Emacs-china.
            ;; ,(cons "gnu"    (concat proto "://elpa.emacs-china.org/gnu/"))
            ;; ,(cons "nongnu" (concat proto "://elpa.emacs-china.org/nongnu/"))
            ;; ,(cons "gnu-devel" (concat proto "://elpa.emacs-china.org/gnu-devel/"))
            ;; ,(cons "nongnu-devel" (concat proto "://elpa.emacs-china.org/nongnu-devel/"))
            ;; ,(cons "melpa"  (concat proto "://elpa.emacs-china.org/melpa/"))
            ;; ;; ,(cons "melpa-stable" (concat proto "://elpa.emacs-china.org/stable-melpa/"))

            ;; ;; 163.
            ;; ,(cons "gnu"    (concat proto "://mirrors.163.com/elpa/gnu/"))
            ;; ,(cons "nongnu" (concat proto "://mirrors.163.com/elpa/nongnu/"))
            ;; ,(cons "gnu-devel" (concat proto "://mirrors.163.com/elpa/gnu-devel/"))
            ;; ,(cons "nongnu-devel" (concat proto "://mirrors.163.com/elpa/nongnu-devel/"))
            ;; ,(cons "melpa"  (concat proto "://mirrors.163.com/elpa/melpa/"))
            ;; ;; ,(cons "melpa-stable" (concat proto "://mirrors.163.com/elpa/stable-melpa/"))

            ;; Tuna.
            ,(cons "gnu"    (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
            ,(cons "nongnu" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"))
            ,(cons "gnu-devel" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu-devel/"))
            ,(cons "nongnu-devel" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu-devel/"))
            ,(cons "melpa"  (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
            ;; ,(cons "melpa-stable" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/"))

            ))))

(provide 'init-package)

;;; init-package.el ends here
