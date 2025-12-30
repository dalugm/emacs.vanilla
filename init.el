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
  (setq fortune-dir
        (cond
         (my-mac-p "/usr/local/Cellar/fortune/9708/share/games/fortunes/")
         (my-linux-p "/usr/share/games/fortunes/")
         (t "~/fortunes/")))
  (setq fortune-file (expand-file-name "." fortune-dir)))

(setq abbrev-file-name (expand-file-name "lisp/abbrev.el" user-emacs-directory))

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

;;; Modular files

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-modeline)
(require 'init-funcs)
(require 'init-org)

(when (display-graphic-p)
  (require 'init-gui))

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
   (my-open-file-externally (dired-get-file-for-visit))
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

;;; Eshell

(setq eshell-aliases-file (expand-file-name "lisp/eshell/aliases"
                                            user-emacs-directory)
      eshell-rc-script (expand-file-name "lisp/eshell/rc"
                                         user-emacs-directory)
      eshell-login-script (expand-file-name "lisp/eshell/login"
                                            user-emacs-directory))

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

;;; Tequila worms

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
