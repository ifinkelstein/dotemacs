;; my-setup-latex.el  -*- lexical-binding: t -*-

(message "Setting up LaTeX settings...")
;;* Latex Packages
;; Basic settings
;; inspiration: https://jwiegley.github.io/use-package/keywords/#defer-demand
(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  ;; Autoload every command surfaced in `my-transient-latex' so the menu
  ;; works even when opened before auctex is loaded (it is on a global
  ;; leader, not just LaTeX-mode-map). Custom my-LaTeX-* helpers live in
  ;; this package's :config, so they load alongside these.
  :commands (LaTeX-environment TeX-insert-macro
                        LaTeX-narrow-to-environment
                        TeX-fold-dwim
                        TeX-fold-buffer
                        TeX-fold-clearout-buffer
                        TeX-command-run-all
                        TeX-view
                        TeX-next-error
                        TeX-kill-job
                        TeX-recenter-output-buffer
                        TeX-clean
                        LaTeX-section
                        LaTeX-mark-section
                        LaTeX-insert-item
                        LaTeX-fill-environment)

  :hook ((LaTeX-mode . variable-pitch-mode)
         (LaTeX-mode . LaTeX-preview-setup)
         (LaTeX-mode . outline-minor-mode) ;; clobbers TAB expansion of yas-snippets
         (LaTeX-mode . olivetti-mode)
         (LaTeX-mode . hl-todo-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . TeX-fold-mode) ;; enable hiding various things
         (LaTeX-mode . electric-indent-local-mode)) ;; trying to see if I like this mode
  ;; introduce dummy variables to silence compile and other warnings
  ;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-latex.el
  :defines (TeX-auto-save
            TeX-parse-self
            TeX-electric-escape
            TeX-PDF-mode
            TeX-source-correlate-method
            TeX-newline-function
            TeX-view-program-list
            TeX-view-program-selection
            TeX-mode-map)
  :bind (:map LaTeX-mode-map
              ("C-M-u" . LaTeX-backward-up-list)
              ("C-M-e" . LaTeX-forward-environment)
              ("C-M-a" . LaTeX-backward-environment)
              ("M-RET" . LaTeX-insert-item)
              ("C-c l" . my-LaTeX-wrap-list)
              ("s-u" . (lambda () (interactive) (yas-expand-snippet (yas-lookup-snippet "underline"))))
              ("s-b" . (lambda () (interactive) (yas-expand-snippet (yas-lookup-snippet "textbf"))))
              ("s-i" . (lambda () (interactive) (yas-expand-snippet (yas-lookup-snippet "emph"))))
              ("s-h" . (lambda () (interactive) (yas-expand-snippet (yas-lookup-snippet "highlight"))))
              ("s-$" . (lambda () (interactive) (yas-expand-snippet (yas-lookup-snippet "inline math"))))
              ("s-d" . my-TeX-delete-current-macro)
              ("M-i" . tab-to-tab-stop)
              ("C-)" . puni-slurp-forward)
              ("C-(" . puni-slurp-backward)
              ("M-," . embark-act)
              ("M-." . embark-dwim))

  :custom
  ;; https://emacs.stackexchange.com/questions/3083/how-to-indent-items-in-latex-auctex-itemize-environments
  (LaTeX-indent-level 4) ;; set reasonable indentation for lists
  (LaTeX-item-indent -2) ;; set reasonable indentation for lists

  (TeX-error-overview-open-after-TeX-run nil) ; do not open the error overview automatically after running TeX.

  (TeX-parse-self t) ;; this should auto-detect when biber is needed for C-c C-a
  (TeX-electric-escape nil) ; if true, offer auto-completion when I type /
  ;; for navigation menu
  (reftex-toc-split-windows-fraction 0.35)
  (reftex-toc-split-windows-horizontally t)

  ;; disable reftex from prompting for how to cite
  (reftex-ref-macro-prompt nil)

  ;; fold blocks between comments using outline-minor-mode in TeX-mode
  (TeX-outline-extra
   '(("%%" 1)
     ("%%%" 2)
     ("%%%%" 3)
     ("%%%%%" 4)
     ("%chapter" 1)
     ("%section" 2)
     ("%subsection" 3)
     ("%subsubsection" 4)
     ("%paragraph" 5)))

  ;; outline-minor-mode settings
  (outline-minor-mode-cycle t)

  ;; click on a PDF to see the TeX source
  (TeX-source-correlate-mode t)

  (TeX-newline-function 'reindent-then-newline-and-indent)

  ;; this sets the TeX engine to luatex for new plugins
  ;; this seems to work by using PdfLatex engine
  (TeX-engine 'luatex)

  (TeX-auto-save t) ;; save style info w/ buffer (?)
  (TeX-save-query nil)
  (TeX-master nil)
  :config
  ;; add font locking to the section-comment headers
  (font-lock-add-keywords
   'LaTeX-mode
   '(("^%\\(chapter\\|\\(sub\\|subsub\\)?section\\|paragraph\\)"
      0 'font-lock-keyword-face t)
     ("^%chapter{\\(.*\\)}"       1 'font-latex-sectioning-1-face t)
     ("^%section{\\(.*\\)}"       1 'font-latex-sectioning-2-face t)
     ("^%subsection{\\(.*\\)}"    1 'font-latex-sectioning-3-face t)
     ("^%subsubsection{\\(.*\\)}" 1 'font-latex-sectioning-4-face t)
     ("^%paragraph{\\(.*\\)}"     1 'font-latex-sectioning-5-face t)))

  ;; Fold all citation macros to a static [C].
  ;; AUCTeX 14 defaults map cite/textcite/parencite/footcite to dynamic
  ;; display functions that render the citation keys; the fold resolver lets
  ;; later-processed specs win, so a plain add-to-list (which prepends) loses
  ;; to them. Strip those function specs first, then add our [C] spec.
  (with-eval-after-load 'tex-fold
    (setq TeX-fold-macro-spec-list
          (seq-remove (lambda (spec)
                        (memq (car-safe (car spec))
                              '(TeX-fold-cite-display
                                TeX-fold-textcite-display
                                TeX-fold-parencite-display
                                TeX-fold-footcite-display)))
                      TeX-fold-macro-spec-list))
    (add-to-list 'TeX-fold-macro-spec-list
                 '("[C]" ("cite" "Cite" "citep" "citet" "citealt" "citealp"
                          "citeauthor" "citeyear" "autocite" "Autocite"
                          "parencite" "Parencite" "textcite" "Textcite"
                          "footcite" "footcitetext" "smartcite" "supercite"
                          "fullcite" "nocite"))))

  (advice-add 'TeX-view :around #'my-widen-first) ; fixes bug in TeX-view
  (put 'LaTeX-narrow-to-environment 'disabled nil) ;; disable warning when using this function
  (add-to-list 'TeX-file-extensions "tex\\.~[0-9a-f]+~") ;; for backup files too

   ;;;; Helper functions
  ;; source: https://emacs.stackexchange.com/questions/6045/how-to-delete-a-latex-macro-while-preserving-its-text-content/7997#7997
  ;; more useful than C-c C-f C-d
  (defun my-TeX-delete-current-macro (&optional arg)
    "Remove the current macro.
With an optional argument ARG, delete just the ARG-th macro
starting from the innermost."
    (interactive "*p")
    (let (macro end)
      (when
          (dotimes (i arg macro)
            (goto-char (TeX-find-macro-start))
            (setq macro (TeX-current-macro)
                  end (TeX-find-macro-end))
            ;; If we need to look for an outer macro we have to "exit" from the
            ;; current one.
            (backward-char))
        ;; Return to the beginning of the macro to be deleted.
        (forward-char)
        (re-search-forward
         (concat (regexp-quote TeX-esc) macro "\\(?:\\[[^]]*\\]\\)?"
                 TeX-grop "\\(\\(.\\|\n\\)*\\)")
         end t)
        (replace-match "\\1")
        ;; Delete the closing brace.
        (delete-char -1))))

  ;; citation for the two functions below:https://www.reddit.com/r/emacs/comments/5f99nv/help_with_auctex_how_to_delete_an_environment/
  (defun my-LaTeX-delete-macro ()
    "Remove current macro and return `t'.  If no macro at point,
return `nil'."
    (interactive)
    (when (TeX-current-macro)
      (let ((bounds (TeX-find-macro-boundaries))
            (brace  (save-excursion
                      (goto-char (1- (TeX-find-macro-end)))
                      (TeX-find-opening-brace))))
        (delete-region (1- (cdr bounds)) (cdr bounds))
        (delete-region (car bounds) (1+ brace)))
      t))

  (defun my-LaTeX-delete-environment ()
    (interactive)
    (when (LaTeX-current-environment)
      (save-excursion
        (let* ((begin-start (save-excursion
                              (LaTeX-find-matching-begin)
                              (point)))
               (begin-end (save-excursion
                            (goto-char begin-start)
                            (search-forward-regexp "begin{.*?}")))
               (end-end (save-excursion
                          (LaTeX-find-matching-end)
                          (point)))
               (end-start (save-excursion
                            (goto-char end-end)
                            (1- (search-backward-regexp "\\end")))))
          ;; delete end first since if we delete begin first it shifts the
          ;; location of end
          (delete-region end-start end-end)
          (delete-region begin-start begin-end)))))

  ;; Wrap a region (or the current line) in a LaTeX list environment,
  ;; turning each line into an \item.  AUCTeX's own `LaTeX-environment'
  ;; (C-c C-e) already wraps a region in any environment but only seeds a
  ;; single \item; this adds the per-line \item conversion that the
  ;; abandoned `latex-wrap' package did (https://github.com/abo-abo/latex-wrap),
  ;; layered on top of AUCTeX completion and indentation instead of Helm.
  (defun my-LaTeX-wrap-list (environment)
    "Wrap the active region (or current line) in a LaTeX list ENVIRONMENT.
Each non-empty line becomes an \\item, with leading/trailing whitespace
trimmed.  Interactively, prompt for ENVIRONMENT (default \"itemize\").
With no real content, leave point at an empty \\item ready to type."
    (interactive
     (list (completing-read "List environment: "
                            '("itemize" "enumerate" "description")
                            nil nil nil nil "itemize")))
    (let* ((region (use-region-p))
           (beg (if region (region-beginning) (line-beginning-position)))
           (end (if region (region-end) (line-end-position)))
           (items (delq nil
                        (mapcar (lambda (l)
                                  (let ((s (string-trim l)))
                                    (unless (string-empty-p s) s)))
                                (split-string (buffer-substring-no-properties beg end)
                                              "\n"))))
           item-point)
      (delete-region beg end)
      (goto-char beg)
      (insert (format "\\begin{%s}\n" environment))
      (if items
          (dolist (s items)
            (insert (format "\\item %s\n" s)))
        (insert "\\item ")
        (setq item-point (copy-marker (point)))
        (insert "\n"))
      (insert (format "\\end{%s}" environment))
      (indent-region beg (point))
      (when item-point
        (goto-char item-point)
        (set-marker item-point nil))))

  ;; view generated PDF with `pdf-tools'.
  (unless (assoc "PDF Tools" TeX-view-program-list)
    (add-to-list 'TeX-view-program-list
                 '("PDF Tools" TeX-pdf-tools-sync-view)))
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "PDF Tools"))) ;;auctex use-package
;;** reftex
(use-package reftex
  :after auctex
  :commands (turn-on-reftex reftex-citation reftex-reference reftex-toc
                            reftex-view-crossref)
  :config
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-insert-label-flags '("sf" "sfte"))
  ;; (setq reftex-ref-style-default-list '("Default" "AMSMath" "Cleveref"))
  (setq reftex-use-multiple-selection-buffers t))

;;** latex-change-env
(use-package latex-change-env
  :after latex
  :bind (:map LaTeX-mode-map ("C-c r" . latex-change-env)))


;;** consult-reftex
(use-package consult-reftex
  :vc (:url "https://github.com/karthink/consult-reftex" :branch "main" :rev :newest)
  :after (reftex consult embark)
  :bind (:map reftex-mode-map
              ("C-c )"   . consult-reftex-insert-reference)
              ("C-c M-." . consult-reftex-goto-label)
              :map org-mode-map
              ("C-c (" . consult-reftex-goto-label)
              ("C-c )"   . consult-reftex-insert-reference))
  :config
  (setq consult-reftex-preview-function
        #'consult-reftex-make-window-preview
        consult-reftex-preferred-style-order
        '("\\eqref" "\\ref"))
  (consult-customize consult-reftex-insert-reference
                     :preview-key (list :debounce 0.3 'any)))
;;** bibtex
(use-package bibtex
  :after auctex
  :custom
  (bibtex-align-at-equal-sign t)
  :config
  (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120))))

(with-eval-after-load 'font-latex
  (set-face-attribute 'font-latex-sedate-face nil :inherit 'fixed-pitch)
  (set-face-attribute 'font-latex-math-face nil :inherit 'fixed-pitch))

;;** ox-latex
;; https://jakebox.github.io/youtube/org_latex_video.html
(with-eval-after-load 'ox-latex
  (setq org-latex-compiler "lualatex") ;; change org-latex output. Also check org-latex-to-pdf-process
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;;* Prettify latex
;; https://github.com/karthink/.emacs.d/blob/master/lisp/pretty-latex.el
;; Set the LaTeX prettify table buffer-locally from LaTeX-mode-hook only.
;; Hanging it on the GLOBAL prettify-symbols-mode-hook corrupted every other
;; buffer (e.g. $->·, --->– in code), so we scope it to LaTeX-mode.
(defun prettify-symbols-latex-symbols ()
  "Enable prettify-symbols-mode with a LaTeX-specific symbol table.
Uses the core table from tex-mode.el (`tex--prettify-symbols-alist',
private API, recheck after Emacs upgrades) plus a few entries it lacks."
  (require 'tex-mode)
  (setq-local prettify-symbols-alist
              (append '(("$" . ?\N{MIDDLE DOT})
                        ("\\newline" . ?\N{LINE SEPARATOR})
                        ("\\par" . ?\N{PARAGRAPH SEPARATOR}))
                      tex--prettify-symbols-alist))
  (prettify-symbols-mode 1))

(add-hook 'LaTeX-mode-hook #'prettify-symbols-latex-symbols)



;;* Helpful functions
;;** control how reftex toc shows up
(autoload 'imenu-list-display-buffer "imenu-list")
(add-to-list 'display-buffer-alist
             '("^\\*toc\\*" imenu-list-display-buffer))

(defun my-LaTeX-mark-inside-environment ()
  "Like `LaTeX-mark-environment' but marks the inside of the environment.
Skips past [] and {} arguments to the environment.
Adapted by the er/mark-LaTeX-inside-environment function"
  (interactive)
  (LaTeX-mark-environment)
  (when (looking-at "\\\\begin{")
    (forward-sexp 2)
    ;; Assume these are arguments
    (while (looking-at "[ \t\n]*[{[]")
      (forward-sexp 1))
    ;; Go to next line if there is nothing interesting on this one
    (skip-syntax-forward " ") ;; newlines are ">" i.e. end comment
    (when (looking-at "%\\|$")
      (forward-line))
    ;; Clean up the end portion
    (exchange-point-and-mark)
    (backward-sexp 2)
    (skip-syntax-backward " ")
    (exchange-point-and-mark)))

;;* Embark: LaTeX macro argument targets
;; Extensible embark integration for LaTeX macros like \input{}, \include{},
;; \includegraphics{}, etc. Uses AUCTeX primitives for robust parsing.

(require 'cl-lib)

;; Forward declarations for AUCTeX functions
(declare-function TeX-current-macro "tex")
(declare-function TeX-find-macro-boundaries "tex")
(defvar TeX-esc)

;; Forward declarations for embark
(defvar embark-general-map)
(defvar embark-target-finders)
(defvar embark-keymap-alist)
(defvar embark-default-action-overrides)

(defun my-TeX-macro-arguments ()
  "Return list of arguments for macro at point.
Each element is (CONTENT START END DELIM) where DELIM is ?{ or ?[."
  (when-let* ((bounds (TeX-find-macro-boundaries))
              (macro-start (car bounds))
              (macro-end (cdr bounds)))
    (save-excursion
      (goto-char macro-start)
      ;; Skip past \macroname
      (forward-char (length TeX-esc))
      (skip-chars-forward "A-Za-z@*")
      (let (args)
        (while (< (point) macro-end)
          (skip-chars-forward " \t\n")
          (when (and (< (point) macro-end)
                     (memq (char-after) '(?\{ ?\[)))
            (let* ((open-char (char-after))
                   (arg-start (point))
                   (arg-end (save-excursion
                              (forward-sexp 1)
                              (point)))
                   (content (buffer-substring-no-properties
                             (1+ arg-start) (1- arg-end))))
              (push (list content arg-start arg-end open-char) args)
              (goto-char arg-end))))
        (nreverse args)))))

(defun my-TeX-arg-at-point ()
  "Return info about macro argument at point.
Returns (MACRO-NAME ARG-INDEX CONTENT START END) or nil."
  (when-let* ((macro (TeX-current-macro))
              (args (my-TeX-macro-arguments)))
    (cl-loop for (content start end _delim) in args
             for idx from 1
             when (and (>= (point) start) (<= (point) end))
             return (list macro idx content start end))))

(defvar my-embark-latex-macro-targets
  '((("input" . 1)           . latex-input-file)
    (("include" . 1)         . latex-input-file)
    (("subfile" . 1)         . latex-input-file)
    (("includeonly" . 1)     . latex-input-file)
    (("includegraphics" . 1) . latex-graphics-file)
    (("includegraphics" . 2) . latex-graphics-file)
    (("bibliography" . 1)    . latex-bib-file)
    (("addbibresource" . 1)  . latex-bib-file))
  "Alist mapping (MACRO . ARG-INDEX) to embark target types.
Add entries here to extend embark support for additional macros.")

(defun my-embark-target-latex-macro-arg ()
  "Embark target finder for LaTeX macro arguments.
Recognizes macros registered in `my-embark-latex-macro-targets'."
  (when (derived-mode-p 'latex-mode 'LaTeX-mode)
    (when-let* ((info (my-TeX-arg-at-point))
                (macro (nth 0 info))
                (idx   (nth 1 info))
                (content (nth 2 info))
                (start (1+ (nth 3 info)))
                (end   (1- (nth 4 info)))
                (key (cons (downcase macro) idx))
                (type (alist-get key my-embark-latex-macro-targets
                                 nil nil #'equal)))
      `(,type ,content . (,start . ,end)))))

;;** Embark actions

(defun my-latex-open-input-file (filename)
  "Open FILENAME as LaTeX input file, adding .tex if needed.
Searches relative to the current buffer's directory."
  (let* ((dir (file-name-directory (or buffer-file-name default-directory)))
         (file (expand-file-name filename dir))
         (candidates (list file (concat file ".tex") (concat file ".ltx"))))
    (if-let* ((found (cl-find-if #'file-exists-p candidates)))
        (find-file found)
      (if (y-or-n-p (format "Create %s.tex? " filename))
          (find-file (concat file ".tex"))
        (user-error "File not found: %s" filename)))))

(defun my-latex-open-input-file-other-window (filename)
  "Open FILENAME in other window, adding .tex if needed."
  (let* ((dir (file-name-directory (or buffer-file-name default-directory)))
         (file (expand-file-name filename dir))
         (candidates (list file (concat file ".tex") (concat file ".ltx"))))
    (if-let* ((found (cl-find-if #'file-exists-p candidates)))
        (find-file-other-window found)
      (user-error "File not found: %s" filename))))

(defun my-latex-open-graphics-file (filename)
  "Open FILENAME as graphics file.
Tries common extensions: pdf, png, jpg, jpeg, eps."
  (let* ((dir (file-name-directory (or buffer-file-name default-directory)))
         (file (expand-file-name filename dir))
         (extensions '("" ".pdf" ".png" ".jpg" ".jpeg" ".eps"))
         (candidates (mapcar (lambda (ext) (concat file ext)) extensions)))
    (if-let* ((found (cl-find-if #'file-exists-p candidates)))
        (find-file found)
      (user-error "Graphics file not found: %s" filename))))

(defun my-latex-open-bib-file (filename)
  "Open FILENAME as bibliography file, adding .bib if needed."
  (let* ((dir (file-name-directory (or buffer-file-name default-directory)))
         (file (expand-file-name filename dir))
         (with-ext (if (string-suffix-p ".bib" file) file (concat file ".bib"))))
    (if (file-exists-p with-ext)
        (find-file with-ext)
      (user-error "Bib file not found: %s" with-ext))))

;;** Embark keymaps and registration

(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders #'my-embark-target-latex-macro-arg)

  (defvar-keymap embark-latex-input-file-map
    :doc "Embark actions for LaTeX input files."
    :parent embark-general-map
    "RET" #'my-latex-open-input-file
    "o"   #'my-latex-open-input-file
    "4"   #'my-latex-open-input-file-other-window)

  (defvar-keymap embark-latex-graphics-file-map
    :doc "Embark actions for LaTeX graphics files."
    :parent embark-general-map
    "RET" #'my-latex-open-graphics-file
    "o"   #'my-latex-open-graphics-file)

  (defvar-keymap embark-latex-bib-file-map
    :doc "Embark actions for LaTeX bibliography files."
    :parent embark-general-map
    "RET" #'my-latex-open-bib-file
    "o"   #'my-latex-open-bib-file)

  (add-to-list 'embark-keymap-alist '(latex-input-file . embark-latex-input-file-map))
  (add-to-list 'embark-keymap-alist '(latex-graphics-file . embark-latex-graphics-file-map))
  (add-to-list 'embark-keymap-alist '(latex-bib-file . embark-latex-bib-file-map))

  (setf (alist-get 'latex-input-file embark-default-action-overrides)
        #'my-latex-open-input-file)
  (setf (alist-get 'latex-graphics-file embark-default-action-overrides)
        #'my-latex-open-graphics-file)
  (setf (alist-get 'latex-bib-file embark-default-action-overrides)
        #'my-latex-open-bib-file))

;;* provide my-setup-latex
(provide 'my-setup-latex)
;;; my-setup-latex.el ends here
