;;; init-utils.el --- utility -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Utility configuration.
;;

;;; Code:

;;;; Const.

(defconst my-linux-p (eq system-type 'gnu/linux)
  "Running on GNU/Linux.")

(defconst my-mac-p (eq system-type 'darwin)
  "Running on Mac system.")

(defconst my-win-p (eq system-type 'windows-nt)
  "Running on Windows system.")

(defconst my-wsl-p (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
  "Running on WSL (Windows Subsystem for Linux).")

;;;; Utility.

(if (and my-mac-p
         (< emacs-major-version 30)
         (executable-find "gls"))
    (setq insert-directory-program "gls")
  ;; Suppres Dired warning when not using GNU ls.
  (setq dired-use-ls-dired nil))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html
(prefer-coding-system 'utf-8)

;; Warn when opening files bigger than 100MB.
(setq-default large-file-warning-threshold (* 100 1000 1000))

;; https://emacs-china.org/t/topic/25811/9
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(setq auth-sources '("~/.authinfo.gpg"))

;; Shutdown the startup screen.
(setq inhibit-startup-screen t)

(defun my--initial-scratch-message ()
  "Customize `initial-scratch-message'."
  (format
   ;; Comment first line and add two new lines in the end.
   ";; %s\n\n"
   (replace-regexp-in-string
    ;; Comment each line below first line.
    "\n"
    "\n;; "
    (if (executable-find "fortune")
        (replace-regexp-in-string
         ;; Remove extra escape sequences.
         (rx (or (seq ?\n eol)
                 (seq ?\C-\[ ?\[ (0+ digit) ?m)
                 (seq ?\\ (0+ digit))))
         ""
         (shell-command-to-string "fortune"))
      (string-join
       '("Now, trailblazers"
         "Keep credos in mind"
         "(I won't say it twice!)"
         "One! Stop staying within the lines"
         "Two! We always align"
         "Three! Even if we don't gain the upper hand, we'll fight for right"
         "Four! Never care a rap for hindsight"
         "Five! Let us light the night"
         "Six! Even when there are wheels within wheels, go ahead!"
         "Get it pulverized")
       "\n")))))

(setq-default initial-scratch-message (my--initial-scratch-message))

;; Nice scrolling.
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Disable the annoying bell ring.
(setq ring-bell-function #'ignore)

;; Repeating C-SPC after popping mark pops it again.
(setq set-mark-command-repeat-pop t)

;; Do NOT make backups of files, not safe.
;; https://github.com/joedicastro/dotfiles/tree/master/emacs
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Use y/n instead of yes/no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Pass `C-u' to `recenter' to put point in the window's center.
(setq next-error-recenter '(4))

;; Indent with spaces.
(setq-default indent-tabs-mode nil)

;; Smart tab behavior - indent or complete.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; TAB cycle if there are only few candidates.
(setq completion-cycle-threshold 3)

;;;; Useful modes.

;; Ediff.
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; Disable annoying blink.
(blink-cursor-mode -1)

;; Delete the selection with a key press.
(delete-selection-mode +1)

;; Fix Emacs performance when edit so-long files.
(global-so-long-mode +1)

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode +1)

;; Automatically reload files was modified by external program.
(setq auto-revert-verbose nil)
(global-auto-revert-mode +1)

;; Pairs...
(electric-pair-mode +1)

;; Show matching parentheses.
(show-paren-mode +1)
(setq show-paren-context-when-offscreen 'overlay)

;; Meaningful names for buffers with the same name.
(require 'uniquify)
;; Rename after killing uniquified.
(setq uniquify-after-kill-buffer-p t)
;; Don't muck with special buffers.
(setq uniquify-ignore-buffers-re "^\\*")

;; Clean up obsolete buffers automatically.
(require 'midnight)

;; Undo (and redo) changes about the window.
(require 'winner)
(setq winner-boring-buffers '("*Apropos*" "*Buffer List*"
                              "*Completions*" "*Compile-Log*"
                              "*Help*" "*Ibuffer*"
                              "*inferior-lisp*"))
(winner-mode +1)

;;;;; Keep track of recently opened files.
(require 'recentf)
(setq recentf-max-saved-items 100)
(dolist (regexp '("^/\\(?:ssh\\|su\\|sudo\\)?x?:"
                  "/\\.?TAGS\\'" "/\\.?tags\\'"))
  (add-to-list 'recentf-exclude regexp))
;; Disable `recentf-cleanup' on recentf start,
;; because it can be laggy with remote files.
(setq recentf-auto-cleanup 'never)
(recentf-mode +1)
(global-set-key (kbd "C-c f f") #'recentf-open-files)
(global-set-key (kbd "C-c f l") #'recentf-load-list)

;;;;; Whitespace.
(global-set-key (kbd "C-c t w") #'whitespace-mode)
(with-eval-after-load 'whitespace
  ;; Search {zero,full}-width space also.
  (setq whitespace-space-regexp "\\( +\\|　+\\|​+\\)")
  ;; Show zero-width space.
  (add-to-list 'whitespace-display-mappings '(space-mark #x200b [?.])))

;;;;; Tramp.
(setq tramp-default-method "ssh")

;;;;; Tab-bar.
(setq tab-bar-show nil)
(setq tab-bar-new-tab-choice "*scratch*")

;;;; Commands.

;; Enable narrowing commands.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Enabled change region case commands.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable erase-buffer command.
(put 'erase-buffer 'disabled nil)

;;;; Keybindings.

;; Be able to M-x without meta.
(global-set-key (kbd "C-c m x") #'execute-extended-command)

;; Zero width space.
(global-set-key (kbd "C-c 8 z") (lambda ()
                                  (interactive)
                                  (insert-char #x200b)))
;; Ideographic space.
(global-set-key (kbd "C-c 8 i") (lambda ()
                                  (interactive)
                                  (insert-char #x3000)))

;; Toggle.
(global-set-key (kbd "C-c t A") #'abbrev-mode)
(global-set-key (kbd "C-c t a") #'auto-fill-mode)
(global-set-key (kbd "C-c t f f") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c t f m") #'toggle-frame-maximized)
(global-set-key (kbd "C-c t g") #'glasses-mode)
(global-set-key (kbd "C-c t h") #'global-hl-line-mode)
(global-set-key (kbd "C-c t i") #'display-fill-column-indicator-mode)
(global-set-key (kbd "C-c t j") #'toggle-truncate-lines)
(global-set-key (kbd "C-c t k") #'visual-line-mode)
(global-set-key (kbd "C-c t l") #'display-line-numbers-mode)
(global-set-key (kbd "C-c t r") #'cua-rectangle-mark-mode)
(global-set-key (kbd "C-c t s") #'subword-mode)
(global-set-key (kbd "C-c t t") #'load-theme)
(global-set-key (kbd "C-c t v") #'view-mode)

;; Search.
(global-set-key (kbd "C-c s d") #'find-dired)
(global-set-key (kbd "C-c s i") #'imenu)
(global-set-key (kbd "C-c s g") #'grep)

;; Isearch.
(global-set-key (kbd "C-M-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") #'isearch-backward-regexp)
;; Activate occur easily inside isearch.
(define-key isearch-mode-map (kbd "C-o") #'isearch-occur)

;; Align code in a pretty way.
;; http://ergoemacs.org/emacs/emacs_align_and_sort.html
(global-set-key (kbd "C-x \\") #'align-regexp)

;; Open header file under cursor.
(global-set-key (kbd "C-x C-o") #'ffap)

;; A complementary binding to the apropos-command (C-h a).
(define-key 'help-command "A" #'apropos)

(define-key 'help-command (kbd "C-f") #'find-function)
(define-key 'help-command (kbd "C-k") #'find-function-on-key)
(define-key 'help-command (kbd "C-v") #'find-variable)
(define-key 'help-command (kbd "C-l") #'find-library)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

(global-set-key [remap just-one-space] #'cycle-spacing)
(global-set-key (kbd "C-x M-u") #'revert-buffer)
(global-set-key (kbd "C-x M-c") #'capitalize-region)

(global-set-key (kbd "M-z") #'zap-up-to-char)
(global-set-key (kbd "M-Z") #'zap-to-char)

(global-set-key (kbd "M-s M-j") #'scroll-other-window)
(global-set-key (kbd "M-s M-k") #'scroll-other-window-down)

;;;; Advice.

(define-advice delete-indentation (:around (fn &rest args) chinese)
  "Add Chinese characters support for `fixup-whitespace'.

Use `cl-letf' to change the behavior of `fixup-whitespace' only when
called from `delete-indentation'."
  (cl-letf (((symbol-function #'fixup-whitespace)
             (lambda ()
               (save-excursion
                 (delete-horizontal-space)
                 (if (or (looking-at "^\\|\\s)")
                         (save-excursion (forward-char -1)
                                         (looking-at "\\cc\\|$\\|\\s(\\|\\s'")))
                     nil
                   (insert ?\s))))))
    (apply fn args)))

(provide 'init-utils)
;;; init-utils.el ends here
