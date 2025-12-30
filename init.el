;;; init.el --- `user-init-file' -*- lexical-binding: t; -*-

(progn                                  ; startup
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))

  (when (< emacs-major-version 27)
    (load (expand-file-name "early-init.el" user-emacs-directory)))

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
      (if-let* ((fortune-prog (executable-find "fortune")))
          (replace-regexp-in-string
           ;; Remove extra escape sequences
           (rx (or (seq ?\n eol)
                   (seq ?\C-\[ ?\[ (0+ digit) ?m)))
           ""
           (shell-command-to-string fortune-prog))
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

  (setq initial-scratch-message (my--initial-scratch-message))

  (prefer-coding-system 'utf-8))

;; Ensure saved customizations are applied correctly during startup
(load
 (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
 t t)

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

;; Fix Emacs performance when edit so-long files
(global-so-long-mode +1)

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode +1)

;; Automatically reload files was modified by external program
(setq auto-revert-verbose nil)
(global-auto-revert-mode +1)

;; Pairs...
(electric-pair-mode +1)

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
(with-eval-after-load 'whitespace
  ;; Search {zero,full}-width space also.
  (setq whitespace-space-regexp "\\( +\\|　+\\|​+\\)")
  ;; Show zero-width space.
  (add-to-list 'whitespace-display-mappings '(space-mark #x200b [?.])))

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

(global-set-key (kbd "C-c t f f") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c t f m") #'toggle-frame-maximized)
(global-set-key (kbd "C-c t t") #'load-theme)

(global-set-key (kbd "C-c s d") #'find-dired)
(global-set-key (kbd "C-c s i") #'imenu)
(global-set-key (kbd "C-c s g") #'grep)

;; Align code in a pretty way
;; http://ergoemacs.org/emacs/emacs_align_and_sort.html
(global-set-key (kbd "C-x \\") #'align-regexp)

;; Open header file under cursor
(global-set-key (kbd "C-x C-o") #'ffap)

;; A complementary binding to the apropos-command (C-h a)
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

(if (>= emacs-major-version 27)
    (with-eval-after-load 'epg
      (setq epg-pinentry-mode 'loopback))
  (with-eval-after-load 'epa
    (setq epa-pinentry-mode 'loopback)))

(with-eval-after-load 'fortune
  (setq fortune-dir "~/fortunes/")
  (setq fortune-file (expand-file-name "." fortune-dir)))

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
        'evil-mode-line-tag
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

(setq org-export-backends '(ascii beamer html latex md))

(with-eval-after-load 'org
  (setq org-agenda-files (list org-directory))
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))

  ;; Respect property lines.
  (setq org-startup-folded 'nofold)

  ;; Fontify source code in code blocks.
  (setq org-src-fontify-natively t)

  (setq org-edit-src-content-indentation 0)

  ;; Make Emacs respect kinsoku rules when wrapping lines visually.
  (setq word-wrap-by-category t)

  ;; Save state changes in the LOGBOOK drawer.
  (setq org-log-into-drawer t)

  ;; ;; After v9.2 [[https://orgmode.org/Changes.html][changelog]]
  ;; ;; Org comes with a new template expansion mechanism,
  ;; ;; `org-insert-structure-template'. Default keybinding is `\C-c\C-,'.
  ;; ;; If prefer using previous patterns, e.g. `<s',
  ;; ;; check `org-tempo.el' for more information.
  ;; (add-to-list 'org-modules 'org-tempo)

  ;; {} must exist to denote this is a subscript.
  (setq org-use-sub-superscripts (quote {}))
  (setq org-export-with-sub-superscripts (quote {}))

  (setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("@school" . ?s)
                        ("@code" . ?c) ("TOC" . ?T) ("noexport" . ?n)))

  (global-set-key (kbd "C-c o d") #'org-demote-subtree)
  (global-set-key (kbd "C-c o p") #'org-promote-subtree)

  (global-set-key (kbd "C-c o t") #'org-toggle-link-display)
  (global-set-key (kbd "C-c o l") #'org-store-link)
  (global-set-key (kbd "C-c o i") #'org-insert-structure-template)

;;;; Org timestamp
  ;; -----------------------------------------
  ;; C-c . \+1w RET ;; => <2020-05-23 Sat +1w>
  ;; C-c . \-1w RET ;; => <2020-05-23 Sat -1w>
  ;; -----------------------------------------
  (define-advice org-time-stamp (:around (fn &rest args) insert-escaped-repeater)
    "Insert escaped repeater for org timestamp."
    (apply fn args)
    (when (string-match (rx "\\" (group (any "+\\-") (0+ nonl)))
                        org-read-date-final-answer)
      (save-excursion
        (backward-char)
        (insert " "
                (string-trim-right
                 (match-string 1 org-read-date-final-answer))))))

;;;; Org preview
  ;; Preview LaTeX in Org.
  (setq org-preview-latex-process-alist
        '((dvisvgm
           :programs ("xelatex" "dvisvgm")
           :description "xdv > svg"
           :message "you need to install the programs: xelatex and dvisvgm."
           :image-input-type "xdv"
           :image-output-type "svg"
           :image-size-adjust (1.7 . 1.5)
           :latex-compiler
           ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
           :image-converter
           ;; use `-e' to compute exact glyph bounding boxes
           ("dvisvgm %f -e -n -b min -c %S -o %O"))
          (dvipng
           :programs ("latex" "dvipng")
           :description "dvi > png"
           :message "you need to install the programs: latex and dvipng."
           :image-input-type "dvi"
           :image-output-type "png"
           :image-size-adjust (1.0 . 1.0)
           :latex-compiler
           ("latex -interaction nonstopmode -output-directory %o %f")
           :image-converter
           ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
          (imagemagick
           :programs ("latex" "convert")
           :description "pdf > png"
           :message "you need to install the programs: latex and imagemagick."
           :image-input-type "pdf"
           :image-output-type "png"
           :image-size-adjust (1.0 . 1.0)
           :latex-compiler
           ("pdflatex -interaction nonstopmode -output-directory %o %f")
           :image-converter
           ("convert -density %D -trim -antialias %f -quality 100 %O"))))
  (setq org-preview-latex-default-process 'dvisvgm)

  ;; ;; Enlarge the preview magnification.
  ;; (plist-put org-format-latex-options :scale 1.5)

  ;; https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/Justifying-LaTeX-preview-fragments-in-org-mode/
  ;; Use center or right, anything else means left-justified as the default.
  (plist-put org-format-latex-options :justify 'right)

  (defun my--org-justify-fragment-overlay-h (beg end)
    "Adjust the justification of a LaTeX fragment horizontally.
The justification is set by :justify in `org-format-latex-options'.
Only equations at the beginning of a line are justified.

URL `https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/Justifying-LaTeX-preview-fragments-in-org-mode/'."
    (let* ((position (plist-get org-format-latex-options :justify))
           (ov (car (overlays-at (/ (+ beg end) 2) t)))
           (width (car (image-size (overlay-get ov 'display))))
           offset)
      (cond
       ((and (eq 'center position)
             (= beg (line-beginning-position)))
        (setq offset (floor (- (/ fill-column 2)
                               (/ width 2))))
        (when (< offset 0)
          (setq offset 0))
        (overlay-put ov 'before-string (make-string offset #x20)))
       ((and (eq 'right position)
             (= beg (line-beginning-position)))
        (setq offset (floor (- fill-column width)))
        (when (< offset 0)
          (setq offset 0))
        (overlay-put ov 'before-string (make-string offset #x20))))))

  (advice-add 'org--make-preview-overlay
              :after #'my--org-justify-fragment-overlay-h)

  (defun my-org-toggle-justify-fragment-overlay-h ()
    "Toggle justify LaTeX fragment horizontally."
    (interactive)
    (if (advice-member-p #'my--org-justify-fragment-overlay-h
                         'org--make-preview-overlay)
        (advice-remove 'org--make-preview-overlay
                       #'my--org-justify-fragment-overlay-h)
      (advice-add 'org--make-preview-overlay
                  :after #'my--org-justify-fragment-overlay-h)))

  (defun my--org-justify-fragment-overlay-v (beg end)
    "Adjust the justification of a LaTeX fragment vertically."
    (let* ((ov (car (overlays-at (/ (+ beg end) 2) t)))
           (img (cdr (overlay-get ov 'display)))
           (new-img (plist-put img :ascent 95)))
      (overlay-put ov 'display (cons 'image new-img))))

  (defun my-org-toggle-justify-fragment-overlay-v ()
    "Toggle justify LaTeX fragment vertically."
    (interactive)
    (if (advice-member-p #'my--org-justify-fragment-overlay-v
                         'org--make-preview-overlay)
        (advice-remove 'org--make-preview-overlay
                       #'my--org-justify-fragment-overlay-v)
      (advice-add 'org--make-preview-overlay
                  :after #'my--org-justify-fragment-overlay-v)))

  (defun my--org-renumber-fragment (orig-func &rest args)
    "Number equations in LaTeX fragment.

URL `https://kitchingroup.cheme.cmu.edu/blog/2016/11/07/Better-equation-numbering-in-LaTeX-fragments-in-org-mode/'."
    (let ((counter -1)
          equation-number
          results)
      (setq results (cl-loop for (begin . env)
                             in (org-element-map
                                 (org-element-parse-buffer)
                                 'latex-environment
                                 (lambda (env)
                                   (cons
                                    (org-element-property :begin env)
                                    (org-element-property :value env))))
                             collect
                             (cond
                              ((and (string-match "\\\\begin{equation}" env)
                                    (not (string-match "\\\\tag{" env)))
                               (cl-incf counter)
                               (cons begin counter))
                              ((and (string-match "\\\\begin{align}" env)
                                    (string-match "\\\\notag" env))
                               (cl-incf counter)
                               (cons begin counter))
                              ((string-match "\\\\begin{align}" env)
                               (prog2
                                   (cl-incf counter)
                                   (cons begin counter)
                                 (with-temp-buffer
                                   (insert env)
                                   (goto-char (point-min))
                                   ;; `\\' is used for a new line.
                                   ;; Each one leads to a number.
                                   (cl-incf counter (count-matches "\\\\$"))
                                   ;; Unless there are nonumbers.
                                   (goto-char (point-min))
                                   (cl-decf counter
                                            (count-matches "\\nonumber")))))
                              (t
                               (cons begin nil)))))
      (when (setq equation-number (cdr (assoc (point) results)))
        (setf (car args)
              (concat
               (format "\\setcounter{equation}{%s}\n" equation-number)
               (car args)))))
    (apply orig-func args))

  (defun my-org-toggle-renumber-fragment ()
    "Toggle renumber LaTeX fragment behavior."
    (interactive)
    (if (advice-member-p #'my--org-renumber-fragment
                         'org-create-formula-image)
        (advice-remove 'org-create-formula-image
                       #'my--org-renumber-fragment)
      (advice-add 'org-create-formula-image
                  :around #'my--org-renumber-fragment)))

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
                    "CANCELLED(c@/!)"))))

;;;; Org-babel
(with-eval-after-load 'ob
  (define-advice org-babel-execute-src-block (:around (fn &rest args) lazy-load-languages)
    "Load languages when needed."
    (let* ((language (org-element-property :language (org-element-at-point)))
           (lang-cons (assoc (intern language) org-babel-load-languages)))
      (unless (cdr lang-cons)
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-load-languages)))
    (apply fn args)))

;;;; Org-archive
(with-eval-after-load 'org-archive
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
  (setq org-archive-location "%s_archive::* Archive"))

(global-set-key (kbd "C-c o c") #'org-capture)

;;;; Org-capture
(with-eval-after-load 'org-capture
  (defun my--org-capture-find-month-tree ()
    "Go to current month heading."
    (let ((heading-list (string-split (format-time-string "%Y %m")))
          (level 1)
          end)
      (unless (derived-mode-p 'org-mode)
        (user-error "Target buffer `%s' should be in org mode"
                    (current-buffer)))
      (goto-char (point-min))
      ;; Locate YEAR headline, then MONTH headline.
      (dolist (heading heading-list)
        (let ((re (format org-complex-heading-regexp-format
                          (regexp-quote heading))))
          (if (re-search-forward re end t)
              (goto-char (line-beginning-position))
            ;; Not found, create a new headline at EOF.
            (progn
              (goto-char (point-max))
              (or (bolp) (insert "\n"))
              (when (/= (point) (point-min)) (org-end-of-subtree t t))
              (insert (make-string level ?*) " " heading "\n"))))
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
           (file+olp+datetree org-default-notes-file)
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
           :clock-in t :clock-resume t))))

;;;; Org-clock
(with-eval-after-load 'org-clock
  ;; Save clock data and notes in the LOGBOOK drawer.
  (setq org-clock-into-drawer t)
  ;; Remove clocked tasks with 0:00 duration.
  (setq org-clock-out-remove-zero-time-clocks t))

;;;; Ox-latex
(with-eval-after-load 'ox-latex
  ;; Export org in Chinese into PDF.
  ;; https://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"))
  (add-to-list 'org-latex-classes
               '("ctexart" "\\documentclass[11pt]{ctexart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-default-class "ctexart")
  ;; Compared to `pdflatex', `xelatex' supports unicode and can use
  ;; system's font.
  (setq org-latex-compiler "xelatex"))

;;; GUI

(when (display-graphic-p)

;;;; Frame

  (setq-default frame-title-format "GNU Emacs %@ %b")

  ;; Move more smoothly.
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode +1))

  (when (featurep 'ns)
    ;; Make NS behavior the same as other platforms.
    (setq ns-command-modifier 'meta)
    (setq ns-alternate-modifier 'super)

    (push '(ns-transparent-titlebar . t) default-frame-alist)

    (defun my--set-frame-ns-appearance (frame &rest _)
      "Set ns-appearance frame parameter for FRAME."
      (when (display-graphic-p frame)
        (let ((mode (frame-parameter frame 'background-mode)))
          (modify-frame-parameters frame `((ns-appearance . ,mode))))))

    (defun my--set-all-frames-ns-appearance (&rest _)
      "Set ns-appearance frame parameter for all frames."
      (mapc #'my--set-frame-ns-appearance (frame-list)))

    (add-hook 'after-init-hook #'my--set-all-frames-ns-appearance)
    (add-hook 'after-make-frame-functions #'my--set-frame-ns-appearance)
    (advice-add 'frame-set-background-mode :after #'my--set-frame-ns-appearance))

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
                                 (const nil)))))

  (defcustom my-font-size 13
    "Default font size."
    :type '(natnum :tag "Font size"))

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
    (or (alist-get font-name my-font-alist nil nil #'equal)
        (cdar my-font-alist)))

  (defun my--load-font-spec (face font-name size &rest attrs)
    "Load FONT-SPEC for FACE.

FONT-SPEC should be a list (ASCII-FAMILY CJK-FAMILY CJK-SCALE
ASCII-SPEC CJK-SPEC), where ASCII-FAMILY is a ASCII font family,
CJK-FAMILY is the CJK font family, and SCJK-SCALE is the scale
factor of CJK font.  ASCII-SPEC and CJK-SPEC are extra spec for
ASCII and CJK."
    (if (eq face 'default)
        (apply #'my-load-default-font font-name size attrs)
      (let ((fontset
             (apply #'my--create-fontset
                    (my--font-expand-spec
                     (my--font-name-to-spec font-name) size))))
        (apply #'set-face-attribute face nil
               ;; We must set both `:font' and `fontset' for both ASCII
               ;; and non-ascii spec to take effect.
               :font fontset
               :fontset fontset
               attrs))))

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

  (my-load-default-font nil my-font-size)

  ;; Emoji display.
  (set-fontset-font t 'emoji
                    (font-spec
                     :family (string-join
                              (list (if (eq system-type 'darwin) "Apple" "Noto")
                                    "Color"
                                    "Emoji")
                              " "))
                    nil 'prepend))

;;; Dired

(if (and (eq system-type 'darwin)
         (< emacs-major-version 30)
         (executable-find "gls"))
    (setq insert-directory-program "gls")
  ;; Suppres Dired warning when not using GNU ls
  (setq dired-use-ls-dired nil))

(setq dired-listing-switches "-alh")

;; Search file name only when focus is over filename.
(setq dired-isearch-filenames 'dwim)

;; Kill current dired buffer when selecting a new directory.
(setq dired-kill-when-opening-new-dired-buffer t)

;; Make dired "guess" target directory.
(setq dired-dwim-target t)

(defun my-ediff-files ()
  "Run Ediff on the two marked files.

URL `https://oremacs.com/2017/03/18/dired-ediff/'."
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (cond
     ((<= (length files) 2)
      (let ((file1 (car files))
            (file2 (if (cdr files)
                       (cadr files)
                     (read-file-name
                      "File: "
                      (dired-dwim-target-directory)))))
        (if (file-newer-than-file-p file1 file2)
            (ediff-files file2 file1)
          (ediff-files file1 file2))
        (add-hook 'ediff-after-quit-hook-internal
                  (lambda ()
                    (setq ediff-after-quit-hook-internal nil)
                    (set-window-configuration wnd)))))
     (t
      (user-error "Mark more than 2 files")))))

(defun my-dired-cycle-space-underscore-hyphen ()
  "Cycle marked files name between space, hyphen and underscore."
  (interactive)
  (mapc (lambda (x)
          (let ((x (file-name-nondirectory x)))
            (cond
             ((string-match " " x)
              (rename-file x (replace-regexp-in-string " " "-" x)))
             ((string-match "-" x)
              (rename-file x (replace-regexp-in-string "-" "_" x)))
             ((string-match "_" x)
              (rename-file x (replace-regexp-in-string "_" " " x))))))
        (dired-get-marked-files))
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

(with-eval-after-load 'ibuffer

  ;; Display vc status info in the ibuffer list.
  (defun ibuffer-vc--state (file)
    "Return the `vc-state' for FILE, or `nil' if unregistered."
    (ignore-errors (vc-state file)))

  (defun ibuffer-vc--status-string ()
    "Return a short string to represent the current buffer's status."
    (when buffer-file-name
      (let ((state (ibuffer-vc--state buffer-file-name)))
        (if state
            (symbol-name state)
          "-"))))

  ;; Use human readable Size column instead of original one.
  (define-ibuffer-column my--size
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000)
      (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000)
      (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t
      (format "%8d" (buffer-size)))))

  (define-ibuffer-column vc-status-mini
    (:name "V")
    (if buffer-file-name
        (let ((state (ibuffer-vc--state buffer-file-name)))
          (cond
           ((eq 'added state) "A")
           ((eq 'removed state) "D")
           ((eq 'up-to-date state) "U")
           ((eq 'edited state) "E")
           ((eq 'needs-update state) "N")
           ((memq state '(conflict needs-merge unlocked-changes)) "C")
           ((eq 'ignored state) "!")
           ((memq state '(() unregistered missing)) "?")))
      " "))

  (define-ibuffer-column vc-status
    (:name "VC status")
    (ibuffer-vc--status-string))

  ;; Modify the default ibuffer-formats.
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (my--size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))

  (defun my--ibuffer-get-major-modes-list ()
    "Get all major modes based on opened buffers."
    (mapcar
     (lambda (buffer)
       (buffer-local-value 'major-mode (get-buffer buffer)))
     (buffer-list (selected-frame))))

  (defun my--ibuffer-generate-filter-groups-alist (mm-list result-list)
    "Create an alist of filtering groups to switch between."
    (if mm-list
        (let* ((cur-mm (car mm-list))
               (next-res-list-el `(,(capitalize
                                     ;; trim `-mode' string
                                     (substring (symbol-name cur-mm) 0 -5))
                                   (mode . ,cur-mm))))
          (my--ibuffer-generate-filter-groups-alist
           (cdr mm-list) (cons next-res-list-el result-list)))
      result-list))

  (defun my--ibuffer-generate-filter-groups-by-major-mode ()
    "Generate `ibuffer-saved-filter-groups' by major mode."
    (let* ((ignore-modes '(Buffer-menu-mode
                           compilation-mode
                           minibuffer-inactive-mode
                           ibuffer-mode
                           magit-process-mode
                           messages-buffer-mode
                           fundamental-mode
                           completion-list-mode
                           help-mode
                           Info-mode))
           (groups
            (list
             (cons "default"
                   (my--ibuffer-generate-filter-groups-alist
                    ;; Created by major mode.
                    (cl-set-difference
                     (cl-remove-duplicates
                      (my--ibuffer-get-major-modes-list))
                     ignore-modes)
                    ;; Manually created add here.
                    '(("Modified" (predicate buffer-modified-p
                                             (current-buffer)))))))))
      (setq ibuffer-saved-filter-groups groups)
      (ibuffer-switch-to-saved-filter-groups "default")))

  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-display-summary nil)

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
  ;; Update filter group when calling `ibuffer'.
  (add-hook 'ibuffer-hook #'my--ibuffer-generate-filter-groups-by-major-mode))

;; Replace `buffer-menu' with `ibuffer'.
(global-set-key [remap list-buffers] #'ibuffer)

;;; Ido

(if (fboundp 'fido-mode)
    (progn
      (fido-mode +1)
      (when (fboundp 'fido-vertical-mode)
        (fido-vertical-mode +1)))
  (progn
    (ido-mode +1)
    (ido-everywhere +1)))

;;; Version control

;; Visit version controlled symlink without asking
(setq vc-follow-symlinks t)

;;; Program

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'treesit)

  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (doxygen "https://github.com/tree-sitter-grammars/tree-sitter-doxygen")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
          (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src")
          (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src")
          (php "https://github.com/tree-sitter/tree-sitter-php" nil "php/src")
          (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

  ;; Add `*-ts-mode' to `auto-mode-alist'.
  (dolist (list `((cmake      . (,(rx (or "CMakeLists.txt" ".cmake") eos) . cmake-ts-mode))
                  (dockerfile . (,(rx "Dockerfile" eos) . dockerfile-ts-mode))
                  (elixir     . (,(rx (or ".elixir" (seq ".ex" (opt "s")) "mix.lock") eos) . elixir-ts-mode))
                  (go         . (,(rx ".go" eos) . go-ts-mode))
                  (gomod      . (,(rx "/go.mod" eos) . go-mod-ts-mode))
                  (heex       . (,(rx "." (opt (any "hl")) "eex" eos) . heex-ts-mode))
                  (lua        . (,(rx ".lua" eos) . lua-ts-mode))
                  (tsx        . (,(rx "." (opt (any "jt")) "sx" eos) . tsx-ts-mode))
                  (typescript . (,(rx ".ts" eos) . typescript-ts-mode))
                  (yaml       . (,(rx ".y" (opt "a") "ml" eos) . yaml-ts-mode))))
    (let ((parser (car list))
          (alist (cdr list)))
      (when (treesit-ready-p parser 'message)
        (add-to-list 'auto-mode-alist alist))))

  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (c-or-c++-mode   . c-or-c++-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (csharp-mode     . csharp-ts-mode)
          (css-mode        . css-ts-mode)
          (html-mode       . html-ts-mode)
          (java-mode       . java-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (js-mode         . js-ts-mode)
          (mhtml-mode      . mhtml-ts-mode)
          (python-mode     . python-ts-mode)
          (ruby-mode       . ruby-ts-mode)
          (sh-mode         . bash-ts-mode))))

(defvar my-last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(with-eval-after-load 'compile
  ;; Colorize output of Compilation Mode
  ;; https://stackoverflow.com/a/3072831/355252
  (if (>= emacs-major-version 28)
      (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
    (progn
      (defun my--colorize-compilation-buffer ()
        "Colorize a compilation mode buffer."
        (let ((inhibit-read-only t))
          (ansi-color-apply-on-region compilation-filter-start (point))))
      (add-hook 'compilation-filter-hook #'my--colorize-compilation-buffer)))

  ;; Save before compiling
  (setq compilation-ask-about-save nil)
  ;; Automatically scroll to first error
  (setq compilation-scroll-output 'first-error)
  ;; Kill old compile processes before starting the new one
  (setq compilation-always-kill t))

(global-set-key (kbd "C-c c k") #'compile)
(global-set-key (kbd "C-c c r") #'recompile)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'text-mode-hook #'subword-mode)

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)

;;;; Major modes

(add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . js-mode))
(with-eval-after-load 'js
  (setq js-indent-level 2))

(add-to-list 'auto-mode-alist '("\\.[cir]py\\'" . python-mode))
(with-eval-after-load 'python
  (setq python-indent-guess-indent-offset nil)
  (setq python-indent-offset 4))

(with-eval-after-load 'tex-mode
  (setq tex-command "xelatex")
  (add-to-list 'tex-compile-commands '("xelatex %f" t "%r.pdf")))

(with-eval-after-load 'lua-ts-mode
  (setq lua-ts-indent-offset 3))

;;; S-expression

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
  (let ((value (eval (elisp--preceding-sexp))))
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

;;; Lsp

(when (fboundp 'eglot)
  (global-set-key (kbd "C-c l l") #'eglot)

  (with-eval-after-load 'eglot
    (global-set-key (kbd "C-c l a") #'eglot-code-actions)
    (global-set-key (kbd "C-c l c") #'eglot-show-workspace-configuration)
    (global-set-key (kbd "C-c l d") #'eglot-find-declaration)
    (global-set-key (kbd "C-c l f") #'eglot-format)
    (global-set-key (kbd "C-c l h") #'eldoc)
    (global-set-key (kbd "C-c l i") #'eglot-find-implementation)
    (global-set-key (kbd "C-c l n") #'eglot-rename)
    (global-set-key (kbd "C-c l q") #'eglot-shutdown)
    (global-set-key (kbd "C-c l t") #'eglot-find-typeDefinition)
    (global-set-key (kbd "C-c l R") #'eglot-reconnect)
    (global-set-key (kbd "C-c l Q") #'eglot-shutdown-all)))

;;; Check

(with-eval-after-load 'flymake
  (global-set-key (kbd "C-c ! b") #'flymake-show-buffer-diagnostics)
  (global-set-key (kbd "C-c ! p") #'flymake-show-project-diagnostics))

(with-eval-after-load 'flyspell
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra"
                              "--lang=en_US"
                              "--camel-case"))))

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
  "Rotate windows in clock-wise direction."
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (user-error "Cannot rotate a single window"))
   (t
    (let ((i 1)
          (window-num (count-windows)))
      (while (< i window-num)
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i window-num) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (cl-incf i)))))))

(global-set-key (kbd "C-c w r") #'my-rotate-windows)

(defun my-toggle-full-window()
  "Toggle full view of selected window."
  (interactive)
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))

(global-set-key (kbd "C-c w f") #'my-toggle-full-window)

;;;; File.

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
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name +1))
        (set-visited-file-name new-name)
        (rename-buffer new-name)))
    (save-buffer)))

(global-set-key (kbd "C-c f r") #'my-rename-this-file)

(defun my-copy-file-name ()
  "Copy file name to clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
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
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (user-error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

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
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
                  nil 0 nil file)))

(global-set-key (kbd "C-c f e") #'my-open-externally)
(global-set-key (kbd "C-c m e") #'my-open-externally)

(defun my-delete-this-file ()
  "Delete current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (user-error "No file is currently being edited"))
  (when (yes-or-no-p
         (format-message "Really delete `%s'?"
                         (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(global-set-key (kbd "C-c f d") #'my-delete-this-file)

(defun my-delete-file (file)
  "Delete FILE under current working directory."
  (interactive "sFile name: ")
  (shell-command
   (format "find . -depth -name %s -print0 | xargs -0 rm" file))
  (message "`%s' under current working directory deleted." file))

(global-set-key (kbd "C-c f D") #'my-delete-file)

(defun my--sudo-file-path (file)
  "Get FILE's path with sudo."
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
  :group 'convenience
  :type 'string)

(defcustom my-socks-proxy
  (list
   (if (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
       (if (file-exists-p "/etc/resolv.conf")
           (shell-command-to-string
            "cat /etc/resolv.conf | grep nameserver | awk '{ printf $2 }'")
         "0.0.0.0")
     "127.0.0.1")
   1080)
  "SOCKS proxy."
  :group 'convenience
  :type '(list (string :tag "Host")
               (integer :tag "Port")))

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
  (if (eq url-gateway-method 'socks)
      (message "Current SOCKS%d proxy is \"%s:%d\"."
               (cadddr socks-server)
               (cadr   socks-server)
               (caddr  socks-server))
    (message "No SOCKS proxy.")))

(defun my-enable-socks-proxy ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (let ((host (car  my-socks-proxy))
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
  (if (bound-and-true-p socks-noproxy)
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
  (pop-to-buffer-same-window (get-scratch-buffer-create)))

(global-set-key (kbd "C-c X") #'my-switch-scratch-buffer)

(defun my-occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (let* ((default-input (if (use-region-p)
                            (buffer-substring-no-properties
                             (region-beginning)
                             (region-end))
                          (when-let* ((sym (thing-at-point 'symbol t)))
                            (regexp-quote sym))))
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
  (condition-case _
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme (intern x) t))
    (error "Problem loading theme %s" x)))

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
    (seq-do #'kill-buffer
            (remove
             (current-buffer)
             (seq-filter #'buffer-file-name (buffer-list))))))

(global-set-key (kbd "C-c m k") #'my-kill-other-buffers-without-special-ones)

;; https://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun my-kill-other-buffers-with-special-ones ()
  "Keep all buffers (include special buffers) but the current one."
  (interactive)
  (when (y-or-n-p "Kill all buffers but current one? ")
    (mapc #'kill-buffer (cdr (buffer-list (current-buffer))))))

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
               (> x (/ w 2))
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
      (advice-add 'mouse-set-point :after #'my--adjust-point-after-click)
    (advice-remove 'mouse-set-point #'my--adjust-point-after-click)))

(defcustom my-pangu-spacing-excluded-puncuation
  "，。！？、；：‘’“”『』「」【】（）《》"
  "Excluded puncuation when pangu spacing buffer."
  :group 'convenience
  :type 'string)

(defvar my-pangu-spacing-regexp
  (rx-to-string
   `(or (and (or (group-n 3 (any ,my-pangu-spacing-excluded-puncuation))
                 (group-n 1 (or (category chinese-two-byte)
                                (category japanese-hiragana-two-byte)
                                (category japanese-katakana-two-byte)
                                (category korean-hangul-two-byte))))
             (group-n 2 (in "a-zA-Z0-9")))
        (and (group-n 1 (in "a-zA-Z0-9"))
             (or (group-n 3 (any ,my-pangu-spacing-excluded-puncuation))
                 (group-n 2 (or (category chinese-two-byte)
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
1 and group 2 will both be non-nil when a pangu space is needed.")

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
  (let ((search-url (alist-get (intern search-engine)
                               my-search-engine-alist
                               nil nil #'equal)))
    (browse-url
     (url-encode-url
      (concat search-url
              (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (read-string
                 (message "%s Search: " (capitalize search-engine)))))))))

(defun my-print-ascii-table (&optional arg)
  "Print the ASCII table.

Default print to 256.  With a prefix ARG, print to specified
number."
  (interactive "P")
  (let ((num (if arg
                 (read-number "Input a number: ")
               256))
        (i 0))
    (switch-to-buffer "*ASCII*")
    (erase-buffer)
    (insert (format "ASCII characters up to number %d.\n" num))
    (while (< i num)
      (cl-incf i)
      (insert (format "%4d %c\n" i i)))
    (special-mode)
    (goto-char (point-min))))

(defun my-pingan-emacs ()
  "建议击毙, @平安 Emacs."
  (interactive)
  (message
   (let ((list '()))
     (mapatoms (lambda (sym)
                 (when (special-form-p sym)
                   (push sym list))))
     (mapconcat (lambda (sym)
                  (format "%s 平安" sym))
                list
                "，"))))

;;; Server

(defcustom my-run-emacs-as-a-server nil
  "Non-nil means to run Emacs as a server process, which allows
access from `emacsclient'."
  :group 'convenience
  :type 'boolean)

(when my-run-emacs-as-a-server
  (run-with-idle-timer 3 nil
                       (lambda ()
                         "Run Emacs as a server process."
                         (require 'server)
                         (unless (server-running-p)
                           (message "Starting a server...")
                           (server-start)))))

;;; Site-Lisp

(defun my--add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (cl-remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
           load-path))))

;; Add both site-lisp and its immediate subdirs to `load-path'
(let ((site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)))
  (add-to-list 'load-path site-lisp-dir)
  (my--add-subdirs-to-load-path site-lisp-dir))

;;; Tequila worms

(load (expand-file-name "~/.custom.el") t nil)

(message "Emacs ready in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))
         gcs-done)

;;; _
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
