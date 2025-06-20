;;; init-org.el --- org configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ORGanize.
;;

;;; Code:

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

;;;; Timestamp.
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

;;;; Preview.
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

  (advice-add 'org--make-preview-overlay :after #'my--org-justify-fragment-overlay-h)

  (defun my-org-toggle-justify-fragment-overlay-h ()
    "Toggle justify LaTeX fragment horizontally."
    (interactive)
    (if (advice-member-p #'my--org-justify-fragment-overlay-h 'org--make-preview-overlay)
        (advice-remove 'org--make-preview-overlay #'my--org-justify-fragment-overlay-h)
      (advice-add 'org--make-preview-overlay :after #'my--org-justify-fragment-overlay-h)))

  (defun my--org-justify-fragment-overlay-v (beg end)
    "Adjust the justification of a LaTeX fragment vertically."
    (let* ((ov (car (overlays-at (/ (+ beg end) 2) t)))
           (img (cdr (overlay-get ov 'display)))
           (new-img (plist-put img :ascent 95)))
      (overlay-put ov 'display (cons 'image new-img))))

  (defun my-org-toggle-justify-fragment-overlay-v ()
    "Toggle justify LaTeX fragment vertically."
    (interactive)
    (if (advice-member-p #'my--org-justify-fragment-overlay-v 'org--make-preview-overlay)
        (advice-remove 'org--make-preview-overlay #'my--org-justify-fragment-overlay-v)
      (advice-add 'org--make-preview-overlay :after #'my--org-justify-fragment-overlay-v)))

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
    (if (advice-member-p #'my--org-renumber-fragment 'org-create-formula-image)
        (advice-remove 'org-create-formula-image #'my--org-renumber-fragment)
      (advice-add 'org-create-formula-image :around #'my--org-renumber-fragment)))

;;;; Todo.
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

;;;; Org-babel.
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

;;;; Org-archive.
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

;;;; Org-capture.
(with-eval-after-load 'org-capture
  (defcustom my--org-todo-file (expand-file-name "todo.org" org-directory)
    "My org todo file."
    :type 'string)

  (defcustom my--org-work-file (expand-file-name "work.org" org-directory)
    "My org work file."
    :type 'string)

  (defcustom my--org-read-file (expand-file-name "read.org" org-directory)
    "My org reading record file."
    :type 'string)

  (defcustom my--org-bill-file (expand-file-name "bill.org" org-directory)
    "My org billing file."
    :type 'string)

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
        `(("b" "Bill" plain
           (file+function my--org-bill-file my--org-capture-find-month-tree)
           "| %U | %^{category} | %^{desc} | %^{price} |" :kill-buffer t)
          ("c" "Capture" plain (file+olp+datetree org-default-notes-file))
          ("t" "Todo" entry
           (file+headline my--org-todo-file "Todo")
           "* TODO %^{todo}\n")
          ("w" "Work" entry
           (file+headline my--org-work-file "Work")
           "* %^{task name}\n   %t\n" :clock-in t :clock-resume t)
          ("r" "Read" entry
           (file+headline my--org-read-file "Book")
           "* %^{book name}\n   %u\n" :clock-in t :clock-resume t))))

;;;; Org-clock.
(with-eval-after-load 'org-clock
  ;; Save clock data and notes in the LOGBOOK drawer.
  (setq org-clock-into-drawer t)
  ;; Remove clocked tasks with 0:00 duration.
  (setq org-clock-out-remove-zero-time-clocks t))

;;;; Ox-latex.
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

(provide 'init-org)
;;; init-org.el ends here
