;;; init.el --- `user-init-file' -*- lexical-binding: t; -*-

(when (version< emacs-version "24.3")
  (error "This configuration requires Emacs 24.3 or newer"))

(require 'cl-lib)
(require 'rx)
(require 'advice)
(require 'dired)
(require 'pulse)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(progn                                  ; startup
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))

  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq ring-bell-function #'ignore)

  (defun my--initial-scratch-message ()
    "Customize `initial-scratch-message'."
    (format
     ;; Comment first line and add two new lines in the end
     ";; %s\n\n"
     (replace-regexp-in-string
      ;; Comment each line below first line
      "\n"
      "\n;; "
      (let ((fortune-prog (executable-find "fortune")))
        (if fortune-prog
            (replace-regexp-in-string
             ;; Remove extra escape sequences
             (rx (or (seq ?\n eol)
                     (seq ?\C-\[ ?\[ (0+ digit) ?m)))
             ""
             (shell-command-to-string fortune-prog))
          (mapconcat #'identity
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
                     "\n"))))))

  (setq initial-scratch-message (my--initial-scratch-message))

  (prefer-coding-system 'utf-8))

;; Ensure saved customizations are applied correctly during startup
(load
 (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
 t t)


;;; Package

(eval-after-load 'package
  '(progn
     (defconst my-package-archive-source-alist
       '((official
          ("gnu"    . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa"  . "https://melpa.org/packages/"))
         (emacs-china
          ("gnu"    . "https://elpa.emacs-china.org/gnu/")
          ("nongnu" . "https://elpa.emacs-china.org/nongnu/")
          ("melpa"  . "https://elpa.emacs-china.org/melpa/"))
         (163
          ("gnu"    . "https://mirrors.163.com/elpa/gnu/")
          ("nongnu" . "https://mirrors.163.com/elpa/nongnu/")
          ("melpa"  . "https://mirrors.163.com/elpa/melpa/"))
         (tuna
          ("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
          ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
       "ELPA archives provided by each package archive source.")

     (defun my--set-package-archive-source (symbol value)
       "Set SYMBOL to package archive source VALUE."
       (let ((archives (cdr (assq value my-package-archive-source-alist))))
         (unless archives
           (setq value 'official
                 archives (cdr (assq value my-package-archive-source-alist))))
         (set-default symbol value)
         (setq package-archives
               (mapcar (lambda (archive)
                         (cons (copy-sequence (car archive))
                               (copy-sequence (cdr archive))))
                       archives))))

     (defcustom my-package-archive-source 'official
       "ELPA source for package installation.

Supported values:
`official'    - Official GNU ELPA and MELPA repositories
`tuna'        - Tsinghua University TUNA mirror
`163'         - NetEase 163 mirror
`emacs-china' - Emacs China community mirror

See also: `package-archives'."
       :type '(choice
               (const :tag "official"    official)
               (const :tag "tuna"        tuna)
               (const :tag "163"         163)
               (const :tag "emacs-china" emacs-china))
       :set #'my--set-package-archive-source
       :group 'convenience)

     (setq package-install-upgrade-built-in t)))

;;; Long tail

;; Handle large files
;; https://emacs-china.org/t/topic/25811/9
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      large-file-warning-threshold (* 100 1000 1000)
      syntax-wholeline-max 1000)

;; Nice scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Respect kinsoku rules when wrapping CJK text visually.
(setq word-wrap-by-category t)

;; Repeating C-SPC after popping mark pops it again
(setq set-mark-command-repeat-pop t)

;; Do NOT make backups of files, not safe
;; https://github.com/joedicastro/dotfiles/tree/master/emacs
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Pass `C-u' to `recenter' to put point in the window's center
(setq next-error-recenter '(4))

;; Indent with spaces
(setq-default indent-tabs-mode nil)

;; Smart tab behavior - indent or complete
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Ediff
(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; Disable annoying blink
(blink-cursor-mode -1)

;; Delete the selection with a key press
(delete-selection-mode +1)

;; Protect editing performance in buffers with very long lines.
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode +1))

;; Remember the last visited position in files.
(require 'saveplace)
(if (fboundp 'save-place-mode)
    (save-place-mode +1)
  (setq-default save-place t))

;; Automatically reload files was modified by external program
(setq auto-revert-verbose nil)
(global-auto-revert-mode +1)

;; Pairs...
(electric-pair-mode +1)

;; Sub-word
(global-subword-mode +1)

;; Enable repeatable commands when supported.
(when (fboundp 'repeat-mode)
  (repeat-mode +1))

;; Show matching parentheses
(show-paren-mode +1)
(setq show-paren-context-when-offscreen 'overlay)

;; Meaningful names for buffers with the same name
(require 'uniquify)
;; Rename after killing uniquified
(setq uniquify-after-kill-buffer-p t)
;; Don't muck with special buffers
(setq uniquify-ignore-buffers-re "^\\*")

;; Clean up obsolete buffers automatically
(require 'midnight)
(customize-set-variable 'midnight-mode t)

;; Undo (and redo) changes about the window
(require 'winner)
(setq winner-boring-buffers '("*Apropos*" "*Buffer List*"
                              "*Completions*" "*Compile-Log*"
                              "*Help*" "*Ibuffer*"
                              "*inferior-lisp*"))
(winner-mode +1)

;;;;; Keep track of recently opened files
(require 'recentf)
(setq recentf-max-saved-items 100)
(dolist (regexp '("^/\\(?:ssh\\|su\\|sudo\\)?x?:"
                  "/\\.?TAGS\\'" "/\\.?tags\\'"))
  (add-to-list 'recentf-exclude regexp))
;; Disable `recentf-cleanup' on recentf start,
;; because it can be laggy with remote files
(setq recentf-auto-cleanup 'never)
(recentf-mode +1)
(global-set-key (kbd "C-c f f") #'recentf-open-files)
(global-set-key (kbd "C-c f l") #'recentf-load-list)

;;;;; Whitespace
(global-set-key (kbd "C-c t w") #'whitespace-mode)
(eval-after-load 'whitespace
  '(progn
     ;; Search {zero,full}-width space also.
     (setq whitespace-space-regexp "\\( +\\|　+\\|​+\\)")
     ;; Show zero-width space.
     (add-to-list 'whitespace-display-mappings '(space-mark #x200b [?.]))))

;;;;; Tab-bar
(setq tab-bar-show nil)
(setq tab-bar-new-tab-choice "*scratch*")

;; Enable dangerous commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(put 'erase-buffer 'disabled nil)

;; Be able to M-x without meta
(global-set-key (kbd "C-c m x") #'execute-extended-command)

;; Zero width space
(global-set-key (kbd "C-c 8 z") (lambda ()
                                  (interactive)
                                  (insert-char #x200b)))
;; Ideographic space
(global-set-key (kbd "C-c 8 i") (lambda ()
                                  (interactive)
                                  (insert-char #x3000)))


(when (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-c t f f") #'toggle-frame-fullscreen))
(when (fboundp 'toggle-frame-maximized)
  (global-set-key (kbd "C-c t f m") #'toggle-frame-maximized))

(global-set-key (kbd "C-c t t") #'load-theme)

(global-set-key (kbd "C-c s d") #'find-dired)
(global-set-key (kbd "C-c s i") #'imenu)
(global-set-key (kbd "C-c s g") #'grep)

;; Align code in a pretty way
;; http://ergoemacs.org/emacs/emacs_align_and_sort.html
(global-set-key (kbd "C-x \\") #'align-regexp)

;; Open header file under cursor
(global-set-key (kbd "C-x C-o") #'ffap)

(global-set-key (kbd "C-x C-j") #'dired-jump)
(global-set-key (kbd "C-x 4 C-j") #'dired-jump-other-window)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" #'apropos)

(define-key 'help-command (kbd "C-f") #'find-function)
(define-key 'help-command (kbd "C-k") #'find-function-on-key)
(define-key 'help-command (kbd "C-v") #'find-variable)
(define-key 'help-command (kbd "C-l") #'find-library)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

(when (fboundp 'cycle-spacing)
  (global-set-key [remap just-one-space] #'cycle-spacing))

(global-set-key (kbd "C-x M-u") #'revert-buffer)

(unless (boundp 'ctl-x-x-map)
  (define-prefix-command 'ctl-x-x-map)
  (define-key ctl-x-map "x" 'ctl-x-x-map)

  (unless (fboundp 'revert-buffer-quick)
    (defun my-revert-buffer-quick (&optional auto-save)
      "Revert without confirmation if the file-visiting buffer is unmodified.
With prefix AUTO-SAVE, offer to revert from the latest auto-save file."
      (interactive "P")
      (if (and buffer-file-name (not (buffer-modified-p)))
          (progn
            (revert-buffer (not auto-save) t)
            (message "Reverted buffer"))
        (revert-buffer (not auto-save)))))

  (define-key ctl-x-x-map "g"
    (if (fboundp 'revert-buffer-quick)
        #'revert-buffer-quick
      #'my-revert-buffer-quick))
  (define-key ctl-x-x-map "f"
    (if (fboundp 'font-lock-update)
        #'font-lock-update
      #'font-lock-fontify-buffer))
  (define-key ctl-x-x-map "r" #'rename-buffer)
  (define-key ctl-x-x-map "u" #'rename-uniquely)
  (define-key ctl-x-x-map "n" #'clone-buffer)
  (define-key ctl-x-x-map "i" #'insert-buffer)
  (define-key ctl-x-x-map "t" #'toggle-truncate-lines))

(global-set-key (kbd "C-x M-c") #'capitalize-region)

(global-set-key (kbd "M-z") #'zap-up-to-char)
(global-set-key (kbd "M-Z") #'zap-to-char)

(global-set-key (kbd "M-s M-j") #'scroll-other-window)
(global-set-key (kbd "M-s M-k") #'scroll-other-window-down)

(defadvice delete-indentation (around my-chinese-spacing activate)
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
    ad-do-it))


(eval-after-load 'epg
  '(setq epg-pinentry-mode 'loopback))

(eval-after-load 'fortune
  '(progn
     (setq fortune-dir "~/fortunes/")
     (setq fortune-file (expand-file-name "." fortune-dir))))

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

;;; Isearch

(global-set-key (kbd "C-M-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") #'isearch-backward-regexp)
;; Activate occur easily inside isearch.
(define-key isearch-mode-map (kbd "C-o") #'isearch-occur)

;;; Mode-line
;; https://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html#Mode-Line-Format

;; Unify the eol mnemonics for all systems.
(setq eol-mnemonic-unix ":"
      eol-mnemonic-mac "/"
      eol-mnemonic-dos "\\")

;; ;; Check `format-time-string'
;; (setq display-time-format "%a %b %e")

;; Time format.
(setq system-time-locale "C"
      display-time-24hr-format t
      display-time-day-and-date t
      ;; Do NOT display the load average.
      display-time-default-load-average nil)

;; ;; Show date on mode-line
;; (display-time-mode +1)

;; Make the position number update correctly in all cases
(line-number-mode +1)
(column-number-mode +1)

;; Human readable representation of file size in mode-line
(size-indication-mode +1)

(defmacro my-defvar-risky (var value &optional docstring)
  "Define VAR with VALUE and mark it as risky.

Unless the symbol has a non-nil `risky-local-variable'
property, all properties in any strings, as well as all `:eval' and
`:propertize' forms in the value, are ignored."
  (declare (doc-string 3)
           (debug t)
           (indent defun))
  `(progn
     (defvar ,var ,value ,docstring)
     (put ',var 'risky-local-variable t)))

;; Buffer name
(my-defvar-risky my--mode-line-buffer-identification
  '(:propertize " %b ")
  "Buffer name, instead of `mode-line-buffer-identification'.")

;; Position: line and column
(my-defvar-risky my--mode-line-position
  '(" %l:%C ")
  "Line and column numbers.")

;; File info: percentage, remote, size
(my-defvar-risky my--mode-line-file-info
  '((:propertize "%p" 'face nil)
    (:propertize "%@" 'face nil)
    (:propertize "%I" 'face nil))
  "File position, remote indicator, and size.")

;; Modes: major mode, process, narrow...
(my-defvar-risky my--mode-line-modes
  (let ((recursive-edit-help-echo
         "Recursive edit, type C-M-c to get out"))
    (list (propertize "%[" 'help-echo recursive-edit-help-echo)
          `(:propertize ("" mode-name)
                        help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                        mouse-face mode-line-highlight
                        local-map ,mode-line-major-mode-keymap)
          '("" mode-line-process)
          (propertize "%]" 'help-echo recursive-edit-help-echo)))
  "Major mode and status indicators.")

(defvar my-mode-line-format-left
  (list ""
        'mode-line-front-space
        'mode-line-mule-info
        'mode-line-client
        'mode-line-modified
        'mode-line-remote
        'my--mode-line-buffer-identification
        'my--mode-line-position
        'my--mode-line-file-info
        'mode-line-frame-identification)
  "Left side mode-line.")

(defvar my-mode-line-format-right
  (list ""
        'mode-line-misc-info
        'my--mode-line-modes
        '(vc-mode vc-mode)
        " ")
  "Right side mode-line.")

(setq-default mode-line-format
              (list "%e"
                    my-mode-line-format-left
                    '(:eval
                      (let ((mode-line-right-width
                             (string-width
                              (format-mode-line my-mode-line-format-right))))
                        (propertize
                         " "
                         'display
                         `((space :align-to (- right
                                               ,mode-line-right-width))))))
                    my-mode-line-format-right))

;;; Org

(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o b") #'org-switchb)
(global-set-key (kbd "C-c o o") #'org-open-at-point-global)


(eval-after-load 'org
  '(progn
     (setq org-agenda-files (list org-directory))
     (setq org-default-notes-file (expand-file-name "notes.org" org-directory))

     ;; Respect property lines.
     (setq org-startup-folded 'nofold)

     ;; Fontify source code in code blocks.
     (setq org-src-fontify-natively t)

     (setq org-edit-src-content-indentation 0)

     ;; Save state changes in the LOGBOOK drawer.
     (setq org-log-into-drawer t)

     ;; {} must exist to denote this is a subscript.
     (setq org-use-sub-superscripts (quote {}))
     (setq org-export-with-sub-superscripts (quote {}))

     (setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("@school" . ?s)
                           ("@code" . ?c) ("TOC" . ?T) ("noexport" . ?n)))

     (global-set-key (kbd "C-c o d") #'org-demote-subtree)
     (global-set-key (kbd "C-c o p") #'org-promote-subtree)

     (global-set-key (kbd "C-c o t") #'org-toggle-link-display)
     (global-set-key (kbd "C-c o l") #'org-store-link)
     (when (fboundp 'org-insert-structure-template)
       (global-set-key (kbd "C-c o i") #'org-insert-structure-template))

;;;; Org todo
     ;; `X/Y', X means action when enters the state, Y means action when
     ;; leaves the state. Use `@' to add notes and status information
     ;; (including time), use `!' to add status information only.

     ;; | DONE(d@)   | add notes when entering                            |
     ;; | DONE(d/!)  | add status when leaving                            |
     ;; | DONE(d@/!) | add note when entering and add status when leaving |
     ;; | DONE(d@/)  | WARNING: illegal                                   |

     ;; NOTE: When leaving state A to state B, if A has a leaving action
     ;; and B has an entering action. A's leaving action won't be triggered
     ;; instead of executing B's entering action.

     (setq org-todo-keywords
           '((sequence "TODO(t)" "STARTED(s!/!)" "HANGUP(h@)"
                       "|"
                       "DONE(d)" "ABORT(a@/!)")
             (sequence "REPORT(r)" "BUG(b@)" "KNOWNCAUSE(k@)"
                       "|"
                       "FIXED(f!)")
             (sequence "WAITING(w@/!)" "SOMEDAY(S@)" "PROJECT(P@)"
                       "|"
                       "CANCELLED(c@/!)")))))

;;;; Org-archive
(eval-after-load 'org-archive
  '(progn
     (defun my-org-archive-done-tasks ()
       "Archive DONE tasks."
       (interactive)
       (org-map-entries
        (lambda ()
          (org-archive-subtree)
          (setq org-map-continue-from (outline-previous-heading)))
        "/DONE" 'file)
       (org-map-entries
        (lambda ()
          (org-archive-subtree)
          (setq org-map-continue-from (outline-previous-heading)))
        "/ABORT" 'file)
       (org-map-entries
        (lambda ()
          (org-archive-subtree)
          (setq org-map-continue-from (outline-previous-heading)))
        "/CANCELLED" 'file))

     (setq org-archive-mark-done nil)
     (setq org-archive-location "%s_archive::* Archive")))

(global-set-key (kbd "C-c o c") #'org-capture)

;;;; Org-capture
(eval-after-load 'org-capture
  '(progn
     (defun my--org-capture-find-month-tree ()
       "Go to current month heading."
       (let ((heading-list (split-string (format-time-string "%Y %m")))
             (level 1)
             end)
         (unless (derived-mode-p 'org-mode)
           (user-error "Target buffer `%s' should be in org mode"
                       (current-buffer)))
         (goto-char (point-min))
         ;; Locate YEAR headline, then MONTH headline.
         (dolist (heading heading-list)
           (let ((re (format org-complex-heading-regexp-format
                             (regexp-quote heading)))
                 found)
             (while (and (not found) (re-search-forward re end t))
               (setq found (save-excursion
                             (beginning-of-line)
                             (= (org-outline-level) level))))
             (if found
                 (goto-char (line-beginning-position))
               ;; Append inside the parent subtree, or at EOF for a new year.
               (goto-char (or end (point-max)))
               (or (bolp) (insert "\n"))
               (insert (make-string level ?*) " " heading "\n")
               (forward-line -1)))
           (cl-incf level)
           (setq end (save-excursion (org-end-of-subtree t t))))
         (org-end-of-subtree)))

     ;; |                org-capture-templates common used entry               |
     ;; |--------+-------------------------------------------------------------|
     ;; | %a     | annotation, normally the link created with `org-store-link' |
     ;; | %i     | initial content, copied from the active region              |
     ;; | %^g    | tag                                                         |
     ;; | %t     | timestamp, date only                                        |
     ;; | %T     | timestamp, with date and time                               |
     ;; | %u，%U | timestamp, but inactive                                     |
     ;; | %?     | cursor location after completing the template               |
     ;; NOTE: inactive timestamp will not be added to agenda.

     (setq org-capture-templates
           '(("b" "Bill" plain
              (file+function "bill.org" my--org-capture-find-month-tree)
              "| %U | %^{category} | %^{desc} | %^{price} |" :kill-buffer t)
             ("c" "Capture" plain
              (file+datetree org-default-notes-file)
              "* %^{capture}\n   %u\n")
             ("t" "Todo" entry
              (file+headline "todo.org" "Todo")
              "* TODO %^{todo}\n")
             ("w" "Work" entry
              (file+headline "work.org" "Work")
              "* %^{task name}\n   %t\n"
              :clock-in t :clock-resume t)
             ("r" "Read" entry
              (file+headline "read.org" "Book")
              "* %^{book name}\n   %u\n"
              :clock-in t :clock-resume t)))))

;;;; Org-clock
(eval-after-load 'org-clock
  '(progn
     ;; Save clock data and notes in the LOGBOOK drawer.
     (setq org-clock-into-drawer t)
     ;; Remove clocked tasks with 0:00 duration.
     (setq org-clock-out-remove-zero-time-clocks t)))

;;; GUI

(when (or (display-graphic-p) (daemonp))

;;;; Frame

  (setq-default frame-title-format "GNU Emacs %@ %b")

  ;; Move more smoothly when precision scrolling is available.
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode +1))

  (when (featurep 'ns)
    (setq ns-command-modifier 'meta
          ns-alternate-modifier 'super))

  (defun my-set-window-transparency (value)
    "Set the VALUE of transparency of the frame window."
    (interactive "nSet transparency (0 is transparent - 100 is opaque): ")
    (set-frame-parameter (selected-frame) 'alpha value))

  (global-set-key (kbd "C-c w p") #'my-set-window-transparency)

  (defun my-set-line-spacing (space)
    "Set the line SPACE of the current window."
    (interactive "nLine Space: ")
    (setq line-spacing space))

  (global-set-key (kbd "C-c w l") #'my-set-line-spacing)

;;;; Font

  ;; ;; https://archive.casouri.cat/note/2019/emacs-%E5%AD%97%E4%BD%93%E4%B8%8E%E5%AD%97%E4%BD%93%E9%9B%86/index.html
  ;; ;; http://ergoemacs.org/emacs/emacs_list_and_set_font.html
  ;; ;;
  ;; ;; Emacs use `symbola' (https://dn-works.com/ufas/) as the default
  ;; ;; fallback font. Install to avoid traversing all fonts.
  ;; ;;
  ;; ;; NOTE: I am using `my-load-font' to handle this now.
  ;; ;;
  ;; ;; Default font.
  ;; (set-face-attribute 'default nil :font (font-spec :family "Unifont" :size 16))
  ;; ;;
  ;; ;; East Asia: 你好，こんにちは，안녕하세요。
  ;; ;;
  ;; ;; ¯\_(ツ)_/¯
  ;; (dolist (charset '(han cjk-misc))
  ;;   (set-fontset-font t charset "LXGW WenKai Mono"))
  ;; (set-fontset-font t 'kana "LXGW WenKai Mono")
  ;; (set-fontset-font t 'hangul "LXGW WenKai Mono")

  (defcustom my-font-alist
    '(("Maple" "Maple Mono CN" nil 1)
      ("Maple Normal" "Maple Mono Normal CN" nil 1)
      ("霞鹜文楷等宽" "LXGW WenKai Mono" nil 1)
      ("Fira Code" "Fira Code" "LXGW WenKai Mono" 1)
      ("Jetbrains Mono" "Jetbrains Mono" "Maple Mono NF CN" 1)
      ("Unifont" "Unifont" nil 1)
      ("更纱黑体" "Sarasa Gothic SC" nil 1)
      ("等距更纱黑体" "Sarasa Mono SC" nil 1)
      ("霞鹜文楷" "LXGW WenKai" nil 1))
    "An alist of all the fonts you can switch between by `my-load-font'.

Each element is like

    (FONT-NAME . (ASCII-NAME CJK-NAME CJK-SCALE ASCII-SPEC CJK-SPEC))

FONT-NAME is the display name, ASCII-NAME is the ASCII font
family name, CJK-NAME is the CJK font family name, CJK-SCALE is
the CJK font rescale ratio.  ASCII-SPEC and CJK-SPEC are
additional font spec for ASCII and CJK font."
    :type '(repeat (list (string :tag "Font display name")
                         (string :tag "ASCII font name")
                         (choice (string :tag "CJK font name") (const nil))
                         (number :tag "CJK font rescale ratio")
                         (choice (plist :tag "ASCII font spec"
                                        :key-type symbol
                                        :value-type natnum)
                                 (const nil))
                         (choice (plist :tag "CJK font spec"
                                        :key-type symbol
                                        :value-type natnum)
                                 (const nil))))
    :group 'convenience)

  (defcustom my-font-size 13
    "Default font size."
    :type '(natnum :tag "Font size")
    :group 'convenience)

  (defun my--create-fontset (ascii-spec cjk-spec)
    "Create a fontset NAME with ASCII-SPEC and CJK-SPEC font."
    (let* ((font-hash (sxhash (list ascii-spec cjk-spec)))
           ;; If two fontset have the same ASCII spec and different CJK
           ;; spec, the fontset description is the same, we need to
           ;; differentiate between the two, hence the hash.
           (fontset-name
            (format "fontset-%s+%x"
                    (downcase (plist-get ascii-spec :family))
                    ;; Don't want negative sign ("-").
                    (abs font-hash)))
           ;; ASCII font.
           (fontset
            (create-fontset-from-fontset-spec
             (font-xlfd-name
              (apply #'font-spec :registry fontset-name ascii-spec)))))
      ;; CJK font.
      (dolist (charset '(han kana hangul cjk-misc))
        (set-fontset-font fontset charset (apply #'font-spec cjk-spec)))
      fontset))

  (defun my--font-expand-spec (font-spec size)
    "Translate FONT-SPEC and SIZE to (ASCII-SPEC CJK-SPEC).

FONT-SPEC should be a list (ASCII-FAMILY CJK-FAMILY CJK-SCALE
ASCII-SPEC CJK-SPEC), where ASCII-FAMILY is a ASCII font family,
CJK-FAMILY is the CJK font family, and SCJK-SCALE is the scale
factor of CJK font. ASCII-SPEC and CJK-SPEC are extra spec for
ASCII and CJK.

If CJK is nil, the returned CJK-SPEC is nil. If SIZE is nil,
don't add size attributes to the two font spec. If SIZE or
SCJK-SCALE is nil, don't add size attributes to the CJK spec."
    (let* ((ascii-family (nth 0 font-spec))
           (cjk-family (nth 1 font-spec))
           (cjk-scale (nth 2 font-spec))
           (ascii-extra-spec
            (and size (append `(:size ,size) (nth 3 font-spec))))
           (cjk-extra-spec
            (and size cjk-scale (append `(:size ,(* cjk-scale size))
                                        (nth 4 font-spec))))
           (ascii-spec (and ascii-family
                            `(:family ,ascii-family ,@ascii-extra-spec)))
           (cjk-spec (and cjk-family
                          `(:family ,cjk-family ,@cjk-extra-spec))))
      (list ascii-spec cjk-spec)))

  (defun my--font-name-to-spec (&optional font-name)
    "Translate FONT-NAME to font-spec.

If FONT-NAME is nil, use the first font in `my-font-alist'."
    (or (cdr (assoc font-name my-font-alist))
        (cdar my-font-alist)))

  (defun my-load-default-font (font-name size &rest attrs)
    "Load FONT-NAME for default face with SIZE and ATTRS.

More details are inside `my-load-font'."
    ;; We use a separate function for default font because Emacs has a
    ;; bug that prevents us from setting a fontset for the default face
    ;; (although `set-frame-parameter' works). So we just set default
    ;; face with ASCII font and use default fontset for Unicode font.
    (interactive
     (list (completing-read "Font: " (mapcar #'car my-font-alist) nil t)
           (read-number "Size: " my-font-size)))
    (let* ((spec (my--font-expand-spec
                  (my--font-name-to-spec font-name)
                  size))
           (ascii (apply #'font-spec (car spec)))
           (cjk (apply #'font-spec (cadr spec))))
      (apply #'set-face-attribute 'default nil :font ascii attrs)
      (set-fontset-font t 'han cjk)
      (set-fontset-font t 'kana cjk)
      (set-fontset-font t 'hangul cjk)
      (set-fontset-font t 'cjk-misc cjk)
      (set-fontset-font t 'symbol cjk nil 'append)))

  (global-set-key (kbd "C-c m f") #'my-load-default-font)

  (defun my-load-font (face font-name size &rest attrs)
    "Load FONT-NAME for FACE with SIZE and ATTRS.

If FONT-NAME is nil, use the first font in `my-font-alist'.
SIZE is the font size in pt.  Add additional face attributes in
ATTRS."
    (interactive
     (list (intern (completing-read "Face: " (face-list) nil t))
           (completing-read "Font: " (mapcar #'car my-font-alist) nil t)
           (read-number "Size: " my-font-size)))
    (let* ((spec (my--font-name-to-spec font-name))
           (fontset (apply #'my--create-fontset
                           (my--font-expand-spec spec size))))
      (if (eq face 'default)
          (apply #'my-load-default-font font-name size attrs)
        (apply #'set-face-attribute face nil
               :font fontset
               :fontset fontset
               attrs))))

  (global-set-key (kbd "C-c m F") #'my-load-font)

  ;; Fonts are global defaults and are inherited by newly created frames.
  (my-load-default-font nil my-font-size))

;;; Dired

(if (and (eq system-type 'darwin)
         (executable-find "gls"))
    (setq insert-directory-program "gls")
  ;; Suppres Dired warning when not using GNU ls
  (setq dired-use-ls-dired nil))

(setq dired-listing-switches "-alh")

;; Search file names when point is initially on a file name.
(setq dired-isearch-filenames 'dwim)

;; Reuse Dired buffers when navigating to another directory.
(setq dired-kill-when-opening-new-dired-buffer t)

;; Make dired "guess" target directory.
(setq dired-dwim-target t)

(defun my--ediff-restore-window-configuration (configuration)
  "Restore window CONFIGURATION after the current Ediff session."
  (add-hook 'ediff-after-quit-hook-internal
            (lambda () (set-window-configuration configuration))
            nil t))

(defun my-ediff-files ()
  "Run Ediff on the two marked files.

URL `https://oremacs.com/2017/03/18/dired-ediff/'."
  (interactive)
  (require 'dired-aux)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (cond
     ((<= (length files) 2)
      (let ((file1 (car files))
            (file2 (if (cdr files)
                       (cadr files)
                     (read-file-name
                      "File: "
                      (dired-dwim-target-directory))))
            (startup-hooks
             (list (apply-partially
                    #'my--ediff-restore-window-configuration
                    wnd))))
	(if (file-newer-than-file-p file1 file2)
	    (ediff-files file2 file1 startup-hooks)
	  (ediff-files file1 file2 startup-hooks))))
     (t
      (user-error "Mark more than 2 files")))))

(defun my-dired-cycle-space-underscore-hyphen ()
  "Cycle marked files name between space, hyphen and underscore."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (let* ((directory (file-name-directory file))
           (name (file-name-nondirectory file))
           (new-name
            (cond
             ((string-match-p " " name)
              (replace-regexp-in-string " " "-" name))
             ((string-match-p "-" name)
              (replace-regexp-in-string "-" "_" name))
             ((string-match-p "_" name)
              (replace-regexp-in-string "_" " " name)))))
      (when new-name
        (rename-file file (expand-file-name new-name directory)))))
  (revert-buffer))

(defun my-dired-open-externally (&optional arg)
  "Open marked or current file in OS's default application."
  (interactive "P")
  (dired-map-over-marks
   (my-open-externally (dired-get-file-for-visit))
   arg))

(defun my--dired-mode-hook-setup ()
  "Setup for Dired."
  (local-set-key (kbd ",") #'dired-up-directory)
  (local-set-key (kbd "e") #'my-dired-open-externally)
  (local-set-key (kbd "_") #'my-dired-cycle-space-underscore-hyphen)
  (local-set-key (kbd "C-c C-e") #'my-ediff-files)
  (local-set-key (kbd "C-c C-p") #'wdired-change-to-wdired-mode))

(add-hook 'dired-mode-hook #'my--dired-mode-hook-setup)

;;; Ibuffer

;; Replace `buffer-menu' with `ibuffer'.
(global-set-key [remap list-buffers] #'ibuffer)

;;; Minibuffer completion

(cond
 ((fboundp 'fido-vertical-mode)
  (fido-vertical-mode +1))
 ((fboundp 'fido-mode)
  (fido-mode +1))
 (t
  (ido-mode +1)
  (ido-everywhere +1)))

;;; Prog

;;;; elisp-mode

(defun my-endless-sharp ()
  "Insert #\\=' unless in a string or comment.

URL `https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html'."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

(define-key emacs-lisp-mode-map "#" #'my-endless-sharp)

(defun my-eval-print-last-sexp (&optional arg)
  "Evaluate sexp before point, insert output below following an arrow.
With a `\\[universal-argument]' prefix argument ARG, delete the
sexp before point and insert output into current position."
  (interactive "P")
  (let ((value (eval (save-excursion
                       (backward-sexp)
                       (read (current-buffer))))))
    (save-excursion
      (cond
       ((not arg)
        (newline-and-indent)
        (if (and (stringp value) (string-match-p "\n" value))
            ;; if return value is a multiline string
            (insert (format
                     ";; =>\n;; %S"
                     (replace-regexp-in-string "\n" "\n;; " value)))
          (insert (format "%s%S" ";; => " value))))
       ((equal arg '(4))
        (backward-kill-sexp)
        (insert (format "%S" value)))))))

(dolist (map (list emacs-lisp-mode-map
                   lisp-interaction-mode-map))
  (define-key map (kbd "C-c C-p") #'my-eval-print-last-sexp))

;;;; JavaScript and JSON

(add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . js-mode))
;; Emacs 24.3 has no separate JSON mode; JavaScript handles its syntax.
(add-to-list 'auto-mode-alist '("\\.jsonc?\\'" . js-mode))
(eval-after-load 'js
  '(progn
     (setq js-indent-level 2)))

;;;; Python

(add-to-list 'auto-mode-alist '("\\.[cir]py\\'" . python-mode))
(eval-after-load 'python
  '(progn
     (setq python-indent-guess-indent-offset nil
           python-indent-offset 4)))

;;;; Ruby

(add-to-list 'auto-mode-alist '("\\(?:\\.axlsx\\|/Fastfile\\)\\'" . ruby-mode))

;;;; tex-mode

(eval-after-load 'tex-mode
  '(progn
     (setq tex-command "xelatex")
     (add-to-list 'tex-compile-commands '("xelatex %f" t "%r.pdf"))))

;;;; Compile

(defvar my-last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(eval-after-load 'compile
  '(progn
     (require 'ansi-color)
     (when (fboundp 'ansi-color-compilation-filter)
       (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

     ;; Save before compiling
     (setq compilation-ask-about-save nil)
     ;; Automatically scroll to first error
     (setq compilation-scroll-output 'first-error)
     ;; Kill old compile processes before starting the new one
     (setq compilation-always-kill t)))

(global-set-key (kbd "C-c c k") #'compile)
(global-set-key (kbd "C-c c r") #'recompile)

;;;; Vendor


(eval-after-load 'flymake
  '(progn
     (when (fboundp 'flymake-show-buffer-diagnostics)
       (global-set-key (kbd "C-c ! b") #'flymake-show-buffer-diagnostics))
     (when (fboundp 'flymake-show-project-diagnostics)
       (global-set-key (kbd "C-c ! p") #'flymake-show-project-diagnostics))))

(eval-after-load 'flyspell
  '(progn
     (when (executable-find "aspell")
       (setq ispell-program-name "aspell")
       (setq ispell-extra-args '("--sug-mode=ultra"
                                 "--lang=en_US"
                                 "--camel-case")))))

;;; Version control

;; Visit version controlled symlink without asking
(setq vc-follow-symlinks t)

(defun my-smerge-resolve-all (keep)
  "Resolve all conflicts while keeping KEEP side.

KEEP is one of `upper', `base', `lower'."
  (interactive
   (list (intern (completing-read "Keep: " '(upper base lower)))))
  (let ((resolve-func
         (pcase keep
           (`upper #'smerge-keep-mine)
           (`base  #'smerge-keep-base)
           (`lower #'smerge-keep-other)
           (_ (error "Invalid keep value: %s" keep))))
        (resolve-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^<<<<<<< " nil t)
        (beginning-of-line)
        (funcall resolve-func)
        (cl-incf resolve-count)))
    (if (> resolve-count 0)
        (message "Resolved %d conflict(s) using `%s'" resolve-count keep)
      (message "No conflicts found"))))

(eval-after-load 'smerge-mode
  '(progn
     (define-key smerge-basic-map "R" #'my-smerge-resolve-all)))

;;; Functions

(defun my-toggle-two-split-window ()
  "Toggle two window layout vertically or horizontally."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  #'split-window-horizontally
                #'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (when this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (when this-win-2nd (other-window 1))))
    (user-error "There should be only two windows in current frame")))

(global-set-key (kbd "C-c w t") #'my-toggle-two-split-window)

(defun my-rotate-windows ()
  "Rotate buffers clockwise through the current frame's windows."
  (interactive)
  (let* ((windows (window-list nil 'nomini))
         (states (mapcar (lambda (window)
                           (list (window-buffer window)
                                 (window-start window)
                                 (window-point window)))
                         windows)))
    (when (= (length windows) 1)
      (user-error "Cannot rotate a single window"))
    (when (cl-some #'window-dedicated-p windows)
      (user-error "Cannot rotate dedicated windows"))
    (cl-mapc (lambda (window state)
               (set-window-buffer window (nth 0 state))
               (set-window-start window (nth 1 state))
               (set-window-point window (nth 2 state)))
             (append (cdr windows) (list (car windows)))
             states)))

(global-set-key (kbd "C-c w r") #'my-rotate-windows)

(defun my-toggle-full-window()
  "Toggle full view of selected window."
  (interactive)
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))

(global-set-key (kbd "C-c w f") #'my-toggle-full-window)

(defun my-rename-this-file (&optional arg)
  "Rename both current buffer and file.
With a prefix ARG, rename based on current name."
  (interactive "P")
  (let ((filename (buffer-file-name)))
    (unless filename
      (user-error "Buffer `%s' is not visiting a file" filename))
    (let ((new-name (read-string
                     "New name: "
                     (when arg (file-name-nondirectory filename)))))
      (when (file-exists-p filename)
        (rename-file filename new-name +1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))
    (save-buffer)))

(global-set-key (kbd "C-c f r") #'my-rename-this-file)

(defun my-copy-file-name ()
  "Copy file name to clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (directory-file-name default-directory)
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new (file-name-nondirectory filename))
          (message "Copied `%s'." (file-name-nondirectory filename)))
      (user-error "Current buffer is not attached to a file"))))

(global-set-key (kbd "C-c f c") #'my-copy-file-name)

(defun my-browse-this-file ()
  "Open current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (if (file-remote-p file-name)
            (user-error "Cannot open remote file")
          (browse-url-of-file file-name))
      (user-error "Current buffer is not visiting a file"))))

(global-set-key (kbd "C-c f b") #'my-browse-this-file)

(defun my-open-externally (file)
  "Open FILE or url using system's default application."
  (interactive "sOpen externally: ")
  (unless (string-match-p "\\`[a-z]+://" file)
    (setq file (expand-file-name file)))
  (message "Opening `%s' externally..." file)
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" file)
    (call-process (pcase system-type
                    (`darwin "open")
                    (`cygwin "cygstart")
                    (_ "xdg-open"))
                  nil 0 nil file)))

(global-set-key (kbd "C-c m e") #'my-open-externally)

(defun my-open-file-externally (file)
  "Open FILE using system's default application."
  (interactive "fOpen externally: ")
  (my-open-externally file))

(global-set-key (kbd "C-c f o") #'my-open-file-externally)

(defun my-delete-this-file ()
  "Delete current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (user-error "No file is currently being edited"))
  (when (yes-or-no-p
         (format "Really delete `%s'?"
                 (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(global-set-key (kbd "C-c f d") #'my-delete-this-file)

(defun my-delete-file (file)
  "Recursively delete files named FILE under `default-directory'.
Do not descend into symbolic links."
  (interactive "sFile name: ")
  (let ((count 0))
    (cl-labels ((visit (directory)
                  (dolist (path (directory-files directory t directory-files-no-dot-files-regexp))
                    (cond
                     ((file-directory-p path)
                      (unless (file-symlink-p path) (visit path)))
                     ((equal (file-name-nondirectory path) file)
                      (delete-file path)
                      (cl-incf count))))))
      (visit default-directory))
    (message "%d file(s) named `%s' deleted." count file)))

(global-set-key (kbd "C-c f D") #'my-delete-file)

(defun my--sudo-file-path (file)
  "Get FILE's path with sudo."
  (unless file
    (user-error "Current buffer is not visiting a file or directory"))
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if (file-remote-p file 'user)
                              (concat (file-remote-p file 'user) "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

(defun my-sudo-edit-file ()
  "Edit current file as root."
  (interactive)
  (find-file
   (my--sudo-file-path
    (or buffer-file-name
        (when (or (derived-mode-p 'dired-mode)
                  (derived-mode-p 'wdired-mode))
          default-directory)))))

(global-set-key (kbd "C-c f s") #'my-sudo-edit-file)

(defun my-sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (my--sudo-file-path file)))

(global-set-key (kbd "C-c f S") #'my-sudo-find-file)

(defun my-set-window-margins (margin)
  "Set the MARGIN of the current window."
  (interactive "nMargin Value: ")
  (set-window-margins (selected-window) margin margin))

(global-set-key (kbd "C-c w m") #'my-set-window-margins)

(defcustom my-http-proxy "127.0.0.1:1080"
  "HTTP proxy."
  :type 'string
  :group 'convenience)

(defcustom my-socks-proxy '("127.0.0.1" 1080)
  "SOCKS proxy."
  :type '(list (string :tag "Host")
               (integer :tag "Port"))
  :group 'convenience)

(defun my-show-http-proxy ()
  "Show http/https proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (message "Current HTTP proxy is \"%s\"." my-http-proxy)
    (message "No HTTP proxy.")))

(defun my-enable-http-proxy ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,my-http-proxy)
          ("https" . ,my-http-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (setenv "http_proxy" (format "http://%s" my-http-proxy))
  (setenv "https_proxy" (format "http://%s" my-http-proxy))
  (my-show-http-proxy))

(defun my-disable-http-proxy ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (setenv "http_proxy" "")
  (setenv "https_proxy" "")
  (my-show-http-proxy))

(defun my-toggle-http-proxy ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (my-disable-http-proxy)
    (my-enable-http-proxy)))

(global-set-key (kbd "C-c t p h") #'my-toggle-http-proxy)
(global-set-key (kbd "C-c t p H") #'my-show-http-proxy)

(defun my-show-socks-proxy ()
  "Show SOCKS proxy."
  (interactive)
  (if (eq (bound-and-true-p url-gateway-method) 'socks)
      (message "Current SOCKS%d proxy is \"%s:%d\"."
               (nth 3 socks-server)
               (cadr   socks-server)
               (nth 2 socks-server))
    (message "No SOCKS proxy.")))

(defun my-enable-socks-proxy ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (let ((host (car my-socks-proxy))
        (port (cadr my-socks-proxy)))
    (setq url-gateway-method 'socks
          socks-server `("Default server" ,host ,port 5)
          socks-noproxy '("localhost"))
    (setenv "all_proxy" (format "socks5://%s:%s" host port)))
  (my-show-socks-proxy))

(defun my-disable-socks-proxy ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native)
  (setenv "all_proxy" "")
  (my-show-socks-proxy))

(defun my-toggle-socks-proxy ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (eq (bound-and-true-p url-gateway-method) 'socks)
      (my-disable-socks-proxy)
    (my-enable-socks-proxy)))

(global-set-key (kbd "C-c t p s") #'my-toggle-socks-proxy)
(global-set-key (kbd "C-c t p S") #'my-show-socks-proxy)

(defun my-toggle-selective-display (column)
  "Quick and dirty code folding with COLUMN.

URL `https://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/'"
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(global-set-key (kbd "C-c m h") #'my-toggle-selective-display)

(defun my-switch-scratch-buffer ()
  "Create or switch to the *scratch* buffer.

URL `https://www.emacswiki.org/emacs/RecreateScratchBuffer'"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (when (eq major-mode 'fundamental-mode)
    (funcall initial-major-mode)))

(global-set-key (kbd "C-c X") #'my-switch-scratch-buffer)

(defun my-occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (let* ((default-input (if (use-region-p)
                            (buffer-substring-no-properties
                             (region-beginning)
                             (region-end))
                          (let ((sym (thing-at-point 'symbol)))
                            (when sym (regexp-quote (substring-no-properties sym))))))
         (prompt (if default-input
                     (format
                      "List lines matching (default %s): "
                      default-input)
                   "List lines matching: ")))
    (occur (read-regexp prompt default-input))))

(global-set-key (kbd "C-c m o") #'my-occur-dwim)

(defun my-hide-dos-eol ()
  "Do not show CR in files with mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\C-m []))

(defun my-dos2unix ()
  "Replace DOS eolns CR LF with Unix eolns CR."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match "" nil t))))

(defun my-load-theme (x)
  "Disable current theme and load theme X."
  (interactive (list
                (completing-read
                 "Choose a theme: "
                 (mapcar #'symbol-name (custom-available-themes)))))
  (let ((enabled-themes (copy-sequence custom-enabled-themes)))
    (condition-case err
        (progn
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme (intern x) t))
      (error
       (mapc #'disable-theme custom-enabled-themes)
       (mapc (lambda (theme) (load-theme theme t))
             (reverse enabled-themes))
       (message "Problem loading theme %s: %s"
                x (error-message-string err))))))

(global-set-key (kbd "C-c m t") #'my-load-theme)

(defun my-load-default-theme ()
  "Load default Emacs theme."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(global-set-key (kbd "C-c m T") #'my-load-default-theme)

(defun my-kill-other-buffers-without-special-ones ()
  "Keep all buffers but the current one.
Do NOT mess with special buffers."
  (interactive)
  (when (y-or-n-p "Kill all buffers but current with special ones? ")
    (mapc #'kill-buffer
          (remove
           (current-buffer)
           (cl-remove-if-not #'buffer-file-name (buffer-list))))))

(global-set-key (kbd "C-c m k") #'my-kill-other-buffers-without-special-ones)

;; https://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun my-kill-other-buffers-with-special-ones ()
  "Kill all other buffers, including special buffers."
  (interactive)
  (when (y-or-n-p "Kill all buffers but current one? ")
    (mapc #'kill-buffer (remq (current-buffer) (buffer-list)))))

(global-set-key (kbd "C-c m K") #'my-kill-other-buffers-with-special-ones)

(defun my-insert-date (&optional arg)
  "Insert current date at point.

Without ARG, use full ISO 8601 format.
With a `\\[universal-argument]' prefix argument ARG, use unix timestamp.
With a `\\[universal-argument] \\[universal-argument]' prefix \
argument ARG, use common timestamp.
With a `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]' prefix argument ARG, use locale's timestamp."
  (interactive "P")
  (let ((format (cond
                 ((not arg) "%FT%T%z")
                 ((equal arg '(4)) "%s")
                 ((equal arg '(16)) "%a %b %e %T %Y %z")
                 ((equal arg '(64)) "%c"))))
    (insert (format-time-string format))))

(global-set-key (kbd "C-c m 1") #'my-insert-date)

(defun my-insert-user-information (arg)
  "Insert user information at point.

Without ARG, insert user's name and email.
With a `\\[universal-argument]' prefix argument ARG, insert email only.
With a `\\[universal-argument] \\[universal-argument]' prefix \
argument ARG, insert name only."
  (interactive "P")
  (let ((format (cond
                 ((not arg) (concat user-full-name " <"
                                    user-mail-address ">"))
                 ((equal arg '(4)) user-mail-address)
                 ((equal arg '(16)) user-full-name))))
    (insert format)))

(global-set-key (kbd "C-c m 2") #'my-insert-user-information)

(defun my-delete-blank-lines ()
  "Delete blank lines.
When region is active, delete the blank lines in region only."
  (interactive)
  (save-excursion
    (let ((regexp "^[[:space:]]*$"))
      (if (use-region-p)
          (delete-matching-lines regexp
                                 (region-beginning)
                                 (region-end))
        (delete-matching-lines regexp
                               (point-min)
                               (point-max))))))

(global-set-key (kbd "C-c m d") #'my-delete-blank-lines)

(defun my-delete-visual-blank-lines ()
  "Delete all visual blank lines."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (window-start) (window-end))
      (delete-matching-lines "^[[:space:]]*$"
                             (point-min)
                             (point-max)))))

(global-set-key (kbd "C-c m D") #'my-delete-visual-blank-lines)

(defun my-delete-invisible-chars ()
  "Query and replace some invisible Unicode chars.

The chars replaced are:
 ZERO WIDTH NO-BREAK SPACE (65279, #xfeff)
 ZERO WIDTH SPACE (codepoint 8203, #x200b)
 RIGHT-TO-LEFT MARK (8207, #x200f)
 RIGHT-TO-LEFT OVERRIDE (8238, #x202e)
 LEFT-TO-RIGHT MARK ‎(8206, #x200e)
 OBJECT REPLACEMENT CHARACTER (65532, #xfffc)

Begin at buffer beginning, respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/elisp_unicode_replace_invisible_chars.html'
Version: 2018-09-07 2022-09-13."
  (interactive)
  (save-excursion
    (let ((case-replace nil)
          (case-fold-search nil))
      (goto-char (point-min))
      (while (re-search-forward
              "\ufeff\\|\u200b\\|\u200f\\|\u202e\\|\u200e\\|\ufffc"
              nil t)
        (replace-match "")))))

(defun my-recompile-init ()
  "Byte-compile dotfiles again."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun my-indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(global-set-key (kbd "C-M-z") #'my-indent-defun)

(defun my--adjust-point-after-click (event &optional _)
  "Adjust point.  Click more accurate in line with intuition.
Adjust point depending on which portion of the character the
cursor clicked on, if on the right half, move point after.
EVENT is the mouse event."
  (let* ((posn (event-end event))
         (x (car (posn-object-x-y posn)))
         (w (car (posn-object-width-height posn))))
    ;; `mouse-set-point' is called twice when you click mouse
    ;; first in `down-mouse-1', called by `mouse-drag-region' ->
    ;; `mouse-drag-track' to set point, second in `mouse-1', when
    ;; mouse released and Emacs realized that this is a click event.
    ;; We want to adjust point in both cases.
    (when (and (null (posn-object posn))
               (numberp x) (numberp w)
               (> x (/ w 2))
               (not (eobp))
               (not (eq (char-after) ?\n)))
      (forward-char))))

(define-minor-mode my-delicate-click-mode
  "Accurate point position on click.
That is, if you click on the right half of a character, the point
is set to after it."
  :global t
  :lighter ""
  :group 'convenience
  (if my-delicate-click-mode
      (ad-enable-advice 'mouse-set-point 'after 'my-delicate-click)
    (ad-disable-advice 'mouse-set-point 'after 'my-delicate-click))
  (ad-activate 'mouse-set-point))

(defadvice mouse-set-point (after my-delicate-click disable)
  "Adjust point according to the clicked half of the character."
  (my--adjust-point-after-click (ad-get-arg 0)))

(defadvice kill-ring-save (before my-pulse-highlight-region activate)
  "Momentarily highlight the region being copied."
  (pulse-momentary-highlight-region (ad-get-arg 0) (ad-get-arg 1)))

(defcustom my-pangu-spacing-excluded-puncuation
  "，。！？、；：‘’“”『』「」【】（）《》"
  "Excluded puncuation when pangu spacing buffer."
  :type 'string
  :group 'convenience)

(defcustom my-pangu-spacing-regexp
  (rx-to-string
   `(or (and (or (group-n 3 (any ,my-pangu-spacing-excluded-puncuation))
                 (group-n 1 (or (category ?C)
                                (category japanese-hiragana-two-byte)
                                (category japanese-katakana-two-byte)
                                (category korean-hangul-two-byte))))
             (group-n 2 (in "a-zA-Z0-9")))
        (and (group-n 1 (in "a-zA-Z0-9"))
             (or (group-n 3 (any ,my-pangu-spacing-excluded-puncuation))
                 (group-n 2 (or (category ?C)
                                (category japanese-hiragana-two-byte)
                                (category japanese-katakana-two-byte)
                                (category korean-hangul-two-byte))))))
   t)
  "Regexp to find Chinese character around English character.

Group 1 contains the character before the potential pangu
spacing, and group 2 the character after that. A space is needed
when both group 1 and group 2 are non-nil. Group 3 exists as a
workaround for excluded puncuation.

Since rx does not support matching text that satisfy two regexp
at the same time (we want to match all Chinese two byte
characters, but not the punctuation), we first try to match
excluded puncuation, then the characters that need
pangu-spacing. The excluded puncuation will be matched to group
3, and shortcut the matching for Chinese characters.  Thus group
1 and group 2 will both be non-nil when a pangu space is needed."
  :type 'regexp
  :group 'convenience)

(defun my-pangu-spacing-current-buffer ()
  "Pangu space current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward my-pangu-spacing-regexp nil t)
      (when (and (match-beginning 1)
                 (match-beginning 2))
        (replace-match "\\1 \\2" nil nil)
        (backward-char)))))

(global-set-key (kbd "C-c m p") #'my-pangu-spacing-current-buffer)

(defcustom my-search-engine-alist
  '((baidu         . "https://www.baidu.com/s?wd=")
    (bilibili      . "https://search.bilibili.com/all?keyword=")
    (bing          . "https://www.bing.com/search?q=")
    (duckduckgo    . "https://www.duckduckgo.com/?q=")
    (github        . "https://www.github.com/search?q=")
    (google        . "https://www.google.com/search?q=")
    (longman       . "https://www.ldoceonline.com/dictionary/")
    (stackoverflow . "https://stackoverflow.com/search?q=")
    (vocabulary    . "https://www.vocabulary.com/dictionary/")
    (wikipedia     . "https://www.wikipedia.org/wiki/Special:Search?go=Go&search=")
    (youtube       . "https://www.youtube.com/results?search_query=")
    (zhihu         . "https://www.zhihu.com/search?type=content&q="))
  "An alist of all the engines you can search by.
Key is a symbol as the name, value is a plist specifying the search url."
  :type '(alist :key-type symbol :value-type string)
  :group 'convenience)

(defun my-search-online (&optional search-engine)
  "Search a query or region if any by using SEARCH-ENGINE."
  (interactive (list
                (completing-read "Choose a search engine: "
                                 (mapcar #'car my-search-engine-alist))))
  (let ((search-url (cdr (assq (intern search-engine) my-search-engine-alist))))
    (browse-url
     (concat search-url
             (url-hexify-string
              (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (read-string
                 (message "%s Search: " (capitalize search-engine)))))))))

(defun my-print-ascii-table (&optional arg)
  "Print the ASCII table.

Print character codes 0 through 255 by default.
With a prefix ARG, prompt for the number of character codes to print."
  (interactive "P")
  (let ((num (if arg
                 (read-number "Input a number: ")
               256)))
    (switch-to-buffer "*ASCII*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Character codes from 0 to %d.\n" (1- num)))
      (dotimes (i num)
        (insert (format "%4d %c\n" i i))))
    (special-mode)
    (goto-char (point-min))))

(defun my-pingan-emacs ()
  "建议击毙, @平安 Emacs."
  (interactive)
  (message
   (let ((list '()))
     (mapatoms (lambda (sym)
                 (when (and (fboundp sym)
                            (subrp (symbol-function sym))
                            (eq (cdr (subr-arity (symbol-function sym)))
                                'unevalled))
                   (push sym list))))
     (mapconcat (lambda (sym)
                  (format "%s 平安" sym))
                list
                "，"))))

;;; Tequila worms

(load (expand-file-name "~/.custom.el") t nil)

(message "Emacs ready in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract (or after-init-time (current-time))
                                 before-init-time)))
         gcs-done)

;;; _
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
