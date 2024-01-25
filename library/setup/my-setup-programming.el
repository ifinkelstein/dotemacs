;;; my-setup-programming.el --- personal setup  -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein


;;; Commentary:
;; Settings for better programming/coding. This includes delimiters, languages,
;; indentation, linting, documentation, and compilation.

;;; Code:
(message "Setting up programming settings...")

;;;; Show Pretty Symbols
(use-package prog-mode
  :ensure nil
  :defer t
  :custom
  ;; Show markup at point
  (prettify-symbols-unprettify-at-point t)
  :config
  ;; Pretty symbols
  (global-prettify-symbols-mode +1))

;;;; Delimiters & Identifiers
;;;;; Visualization of Delimiters (Rainbow Delimiters)
;; https://github.com/Fanael/rainbow-delimiters Useful package that will highlight
;; delimiters such as parentheses, brackets or braces according to their depth. Each
;; successive level is highlighted in a different color. This makes it easy to spot
;; matching delimiters, orient yourself in the code, and tell which statements are at
;; a given depth.
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; https://github.com/Fanael/rainbow-identifiers Rainbow identifiers mode is an Emacs
;; minor mode providing highlighting of identifiers based on their names. Each
;; identifier gets a color based on a hash of its name.
(use-package rainbow-identifiers
  :commands rainbow-identifiers-mode)

;;;;; Pair Delimiters
(use-package elec-pair
  :ensure nil
  :defer 1
  :config (electric-pair-mode 1))

;;;;; Surround & Change Delimiters
;; TODO: Add  hydra or transient to expose more embrace commander commands
(use-package embrace
  ;; :bind (("C-M-s-#" . embrace-commander))
  :hook ((org-mode . embrace-org-mode-hook)
         (markdown-mode . embrace-markdown-mode-hook))
  :config
  (defun embrace-markdown-mode-hook ()
    (dolist (lst '((?* "*" . "*")
                   (?\ "\\" . "\\")
                   (?$ "$" . "$")
                   (?/ "/" . "/")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst)))))

;;;;; Structural Editing: Edit & Traverse Delimiters
;; TODO: Write a transient for puni bindings
(use-package puni
  :bind (:map puni-mode-map
         ;; Add slurp and bark bindings
         ("C-(" . #'puni-slurp-backward)
         ("C-)" . #'puni-slurp-forward)
         ("C-{" . #'puni-barf-backward)
         ("C-}" . #'puni-barf-forward))
  :hook ((prog-mode
          tex-mode
          org-mode markdown-mode
          eval-expression-minibuffer-setup) . puni-mode))

;;;;;; Tree Sitter
;; See https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; Set load-path
(setq treesit-extra-load-path `(,(concat my-var-dir "tree-sitter/")))
;; List languages for install
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Install all languages
(defun my-install-treesit-lang-grammar ()
  "Install tree-sitter language grammars in alist."
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

;; Remap major modes to tree-sitter
;; NOTE: this is on a per-mode basis and hooks may need to be altered
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)))

;;;; Multiple Cursors
(use-package iedit
  :bind (:map my+search-keys
         ("c" . iedit-mode)))

;;;; Languages

;;;;; Applescript
(use-package applescript-mode
  :mode (("\\.scpt\\'" . applescript-mode)
         ("\\.applescript\\'"       . applescript-mode))
  :commands (applescript-mode))

;;;;; Elisp
;;;;;; Lisp Packages
(use-package lisp-mode
  :ensure nil
  :commands lisp-mode)

(use-package emacs-lisp-mode
  :ensure nil
  :mode (("\\.el$" . emacs-lisp-mode))
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package eldoc
  :ensure nil
  :commands eldoc-mode
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
  :diminish eldoc-mode
  :config
  ;; Show ElDoc messages in the echo area immediately, instead of after 1/2 a second.
  (setq eldoc-idle-delay 0))

;; better jump to definition
(use-package elisp-def
  :commands (elisp-def elisp-def-mode)
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
    (add-hook hook #'elisp-def-mode)))

;; Elisp hook
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
  (add-hook hook (lambda ()
                   (setq show-trailing-whitespace t)
                   (setq show-paren-context-when-offscreen t)
                   (prettify-symbols-mode 1)
                   (eldoc-mode 1)
                   (yas-minor-mode 1)
                   (rainbow-delimiters-mode 1))))

;; Show matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0))

;;;;;; Lisp Functions
;; idea from http://www.reddit.com/r/emacs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
(defun my-eval-current-form ()
  "Looks for the current def* or set* command then evaluates, unlike `eval-defun', does not go to topmost function"
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

(defun my-nav-find-elisp-thing-at-point-other-window ()
  "Find thing under point and go to it another window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb
             (not (equal symb 0))
             (not (fboundp symb)))
        (find-variable-other-window symb)
      (find-function-at-point))))

;;;;;; Fix Parentheses
(defun my-fix-lonely-parens ()
  "Move all closing parenthesis at start of indentation to previous line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*)" nil t)
      (delete-indentation))))

;;;;;; Elisp indentation
;; Fix the indentation of keyword lists in Emacs Lisp. See [1] and [2].
;;
;; Before:
;;  (:foo bar
;;        :baz quux)
;;
;; After:
;;  (:foo bar
;;   :bar quux)
;;
;; [1]: https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L12-L94
;; [2]: http://emacs.stackexchange.com/q/10230/12534

(with-eval-after-load 'el-patch
  (el-patch-defun lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (el-patch-let (($cond (and (elt state 2)
                               (el-patch-wrap 1 1
                                 (or (not (looking-at "\\sw\\|\\s_"))
                                     (looking-at ":")))))
                   ($then (progn
                            (if (not (> (save-excursion (forward-line 1) (point))
                                        calculate-lisp-indent-last-sexp))
                                (progn (goto-char calculate-lisp-indent-last-sexp)
                                       (beginning-of-line)
                                       (parse-partial-sexp (point)
                                                           calculate-lisp-indent-last-sexp 0 t)))
                            ;; Indent under the list or under the first sexp on the same
                            ;; line as calculate-lisp-indent-last-sexp.  Note that first
                            ;; thing on that line has to be complete sexp since we are
                            ;; inside the innermost containing sexp.
                            (backward-prefix-chars)
                            (current-column)))
                   ($else (let ((function (buffer-substring (point)
                                                            (progn (forward-sexp 1) (point))))
                                method)
                            (setq method (or (function-get (intern-soft function)
                                                           'lisp-indent-function)
                                             (get (intern-soft function) 'lisp-indent-hook)))
                            (cond ((or (eq method 'defun)
                                       (and (null method)
                                            (> (length function) 3)
                                            (string-match "\\`def" function)))
                                   (lisp-indent-defform state indent-point))
                                  ((integerp method)
                                   (lisp-indent-specform method state
                                                         indent-point normal-indent))
                                  (method
                                   (funcall method indent-point state))))))
      (let ((normal-indent (current-column))
            (el-patch-add
              (orig-point (point))))
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (el-patch-swap
          (if $cond
              ;; car of form doesn't seem to be a symbol
              $then
            $else)
          (cond
           ;; car of form doesn't seem to be a symbol, or is a keyword
           ($cond $then)
           ((and (save-excursion
                   (goto-char indent-point)
                   (skip-syntax-forward " ")
                   (not (looking-at ":")))
                 (save-excursion
                   (goto-char orig-point)
                   (looking-at ":")))
            (save-excursion
              (goto-char (+ 2 (elt state 1)))
              (current-column)))
           (t $else)))))))

;;;;; Shell Scripts
(use-package sh-script
  :ensure nil
  :commands sh-script-mode
  :init
  (progn
    ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms.
    (dolist (pattern '("\\.zsh\\'"
                       "zlogin\\'"
                       "zlogout\\'"
                       "zpreztorc\\'"
                       "zprofile\\'"
                       "zshenv\\'"
                       "zshrc\\'"))
      (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))))

(defun spacemacs//setup-shell ()
  (when (and buffer-file-name
             (string-match-p "\\.zsh\\'" buffer-file-name))
    (sh-set-shell "zsh")))
(add-hook 'sh-mode-hook 'spacemacs//setup-shell)

;;;; Indentation
(use-package aggressive-indent
  :preface
  (defun my-aggressive-indent-mode-off ()
    (aggressive-indent-mode 0))
  :hook
  ((css-mode . aggressive-indent-mode)
   (emacs-lisp-mode . aggressive-indent-mode)
   (lisp-interaction-mode . aggressive-indent-mode)
   (lisp-mode . aggressive-indent-mode)
   (js-mode . aggressive-indent-mode)
   (sgml-mode . aggressive-indent-mode))
  :config
  (setq-default aggressive-indent-comments-too t)
  (add-to-list 'aggressive-indent-protected-commands 'comment-dwim))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq-default highlight-indent-guides-method 'character
                highlight-indent-guides-character ?\│
                ;; default is \x2502 but it is very slow on Mac
                ;; highlight-indent-guides-character ?\xFFE8
                highlight-indent-guides-responsive 'top
                highlight-indent-guides-auto-odd-face-perc 5
                highlight-indent-guides-auto-even-face-perc 5
                highlight-indent-guides-auto-character-face-perc 15
                highlight-indent-guides-auto-enabled t))

;;;; Linting/Error Checking (Flymake)
;; Both Flycheck and Flymake are good linters, but let's stick with the built-in Flymake
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-start-on-flymake-mode t)
  (flymake-no-changes-timeout nil)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-wrap-around nil)
  ;; Customize mode-line
  (flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))
  (flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters)))

;; Linting for emacs package libraries
(use-package package-lint
  :commands (package-lint-batch-and-exit
             package-lint-current-buffer
             package-lint-buffer)
  :config
  (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)
  ;; Avoid`package-not-installable' errors
  ;; See https://github.com/purcell/package-lint/issues/153
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'package-archive-contents)))

;; A collection of flymake backends
(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))

;; Use Consult with Flymake
(use-package consult-flymake
  ;; comes with consult
  :ensure nil
  :bind (:map my+flymake-keys
         ("c" . consult-flymake)))

;;;; Compiling

;;;;; Multi-Compile
(use-package multi-compile
  :commands (compile multi-compile-run)
  :custom
  (multi-compile-history-file (concat my-cache-dir "multi-compile.cache"))
  (multi-compile-completion-system 'default)
  :config
  ;; Use for book compiling
  (defun string/starts-with (string prefix)
    "Return t if STRING starts with prefix."
    (and (stringp string) (string-match (rx-to-string `(: bos ,prefix) t) string))))

;;;;; Compile with Nearest Makefile
;; See https://www.emacswiki.org/emacs/CompileCommand
(defun my-upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
  manage to find it, return the containing directory. Else if we
  get to the toplevel directory and still can't find it, return
  nil. Start at startdir or . if startdir not given"

  (let ((dirname (expand-file-name
                  (if startdir startdir ".")))
        (found nil) ; found is set as a flag to leave loop if we find it
        (top nil))  ; top is set when we get
                                        ; to / so that we only check it once

                                        ; While we've neither been at the top last time nor have we found
                                        ; the file.
    (while (not (or found top))
                                        ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
          (setq top t))

                                        ; Check for the file
      (if (file-exists-p (expand-file-name filename dirname))
          (setq found t)
                                        ; If not, move up a directory
        (setq dirname (expand-file-name ".." dirname))))
                                        ; return statement
    (if found dirname nil)))

(defun my-compile-next-makefile ()
  (interactive)
  (let* ((default-directory (or (my-upward-find-file "Makefile") "."))
         (compile-command (concat "cd " default-directory " && "
                                  compile-command)))
    (compile compile-command)))


;;;; highlight-sexp
(use-package highlight-sexp
  :vc (:fetcher github :repo daimrod/highlight-sexp)
  :hook
  (emacs-lisp-mode . highlight-sexp-mode)
  (help-mode . highlight-defined-mode)
  :config
  ;;change the sexp overlap to match a light theme
  ;; adapted from:https://sachachua.com/blog/2023/01/making-highlight-sexp-follow-modus-themes-toggle/
  (defun my-hl-sexp-update-overlay ()
    (when (overlayp hl-sexp-overlay)
      (overlay-put
       hl-sexp-overlay
       'face
       `(:background
         ,"#e6e6fa"))))
  ;; reset all sexp highlights in all buffers
  (defun my-hl-sexp-update-all-overlays ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when highlight-sexp-mode
          (my-hl-sexp-update-overlay)))))

  (advice-add 'hl-sexp-create-overlay :after 'my-hl-sexp-update-overlay))


;;;; highlight-defined
;; highlights defined Emacs Lisp symbols in source code.
(use-package highlight-defined
  :hook
  (emacs-lisp-mode . highlight-defined-mode)
  (help-mode . highlight-defined-mode))

(use-package highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

;;;; dwim-shell-command
;; convenient commands for working with the CLI
(use-package dwim-shell-command
  :vc (:fetcher github :repo xenodium/dwim-shell-command)
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  (defun dwim-shell-commands-unzip ()
    "Unzip all marked archives (of any kind) using `atool'."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Unzip" "atool --extract --explain '<<f>>'"
     :utils "atool"))

  (defun dwim-shell-commands-zip ()
    "Zip all marked files into archive.zip."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Zip" (if (eq 1 (seq-length (dwim-shell-command--files)))
               "zip -r '<<fne>>.<<e>>' '<<f>>'"
             "zip -r '<<archive.zip(u)>>' '<<*>>'")
     :utils "zip"))

  (defun dwim-shell-command-print ()
    "Spool selected files to default printer."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Print file(s) to default printer"
     "lp \"<<f>>\""
     :utils "lp"))

  (defun dwim-shell-command-ai-to-pdf ()
    "Convert AI files to PDF."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert AI to PDF"
     "gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=\"<<fne>>.pdf\" \"<<f>>\""
     :utils "gs"))

  (defun dwim-shell-command-split-FLAC ()
    "Split a FLAC album file into tracks using xld and a CUE file in the same folder."
    (interactive)

    (dwim-shell-command-on-marked-files
     "Split FLAC album file into tracks"
     "xld -c \"<<fne>>.cue\" -f FLAC \"<<f>>\""
     :utils "xld"))

  (defun dwim-shell-command-reduce-PDF-size ()
    "Reduce PDF file size; images downsampled to 150 dpi.

If this doesn't work, then the file can be reduced further in Adobe Acrobat."
    (interactive)
    ;; https://askubuntu.com/questions/113544/how-can-i-reduce-the-file-size-of-a-scanned-pdf-file
    (dwim-shell-command-on-marked-files
     "Reduce PDF file size"
     "  gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET -dBATCH -sOutputFile=\"<<fne>>.reduced.pdf\" \"<<f>>\""
     :utils "gs"))

  (defun dwim-shell-commands-converst-to-docx ()
    "Convert file(s) to docx using custom template."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert to DOCX"
     "pandoc --reference-doc '/Users/ilya/Work/80-89 other writing/80 letter-templates/generic-word-template.docx' -i '<<f>>' -o '<<fne>>.docx'"
     :utils "pandoc"))

  (defun dwim-shell-commands-pdf-to-txt ()
    "Convert pdf to txt."
    (interactive)
    (dwim-shell-command-on-marked-files
     "pdf to txt"
     "pdftotext -layout '<<f>>' '<<fne>>.txt'"
     :utils "pdftotext"))

  (defun dwim-shell-commands-files-combined-size ()
    "Get files combined file size."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Get files combined file size"
     "du -csh '<<*>>'"
     :utils "du"
     :on-completion (lambda (buffer _process)
                      (with-current-buffer buffer
                        (message "Total size: %s"
                                 (progn
                                   (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
                                   (match-string 1))))
                      (kill-buffer buffer))))

  (defun dwim-shell-command-rename-with-date ()
    "Rename the file with the current date between the filename and extension"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Rename with date"
     (concat "mv \"<<f>>\" \"<<fne>>." (format-time-string "%Y%m%d") ".<<e>>\" ")
     :utils "mv"))
  ) ;;dwim-shell-command

;;;; gnuplot
;; (use-package gnuplot-mode
;;   :vc (:fetcher github :repo emacs-gnuplot/gnuplot)
;;   :config
;;   ;; specify the gnuplot executable (if other than /usr/bin/gnuplot)
;;   (setq gnuplot-program "/opt/homebrew/bin/gnuplot")

;;   ;; automatically open files ending with .gp or .gnuplot in gnuplot mode
;;   (setq auto-mode-alist
;;         (append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)) auto-mode-alist)) )

;;;; prism to color programming blocks
(use-package prism
  :vc (:fetcher github :repo alphapapa/prism.el)
  :hook
  (emacs-lisp-mode . prism-mode)
  (latex-mode . prism-mode)
  :config
  (prism-set-colors :num 16
    :desaturations (cl-loop for i from 0 below 16
                            collect (* i 2.5))
    :lightens (cl-loop for i from 0 below 16
                       collect (* i 2.5))
    :colors (list "dark blue" "DarkGreen" "goldenrod4" "turquoise4")
    :comments-fn
    (lambda (color)
      (prism-blend color
                   (face-attribute 'font-lock-comment-face :foreground) 0.25))
    :strings-fn
    (lambda (color)
      (prism-blend color "gray" 0.5))))

;;;; yaml-pro mode
;; TODO: need to fix
;; (use-package yaml-pro
;;   :hook
;;   (yaml-mode . yaml-pro-mode)
;;   (yaml-mode . (lambda() (hungry-delete-mode -1))))

;;;; Custom functions and useful utilities
;; ref: https://xenodium.com/deleting-from-emacs-sequence-vars/
(defun my-remove-from-list-variable ()
  "Remove an item from a list using completing-read"
  (interactive)
  (let* ((var (intern
               (completing-read "From variable: "
                                (let (symbols)
                                  (mapatoms
                                   (lambda (sym)
                                     (when (and (boundp sym)
                                                (seqp (symbol-value sym)))
                                       (push sym symbols))))
                                  symbols) nil t)))
         (values (mapcar (lambda (item)
                           (setq item (prin1-to-string item))
                           (concat (truncate-string-to-width
                                    (nth 0 (split-string item "\n"))
                                    (window-body-width))
                                   (propertize item 'invisible t)))
                         (symbol-value var)))
         (index (progn
                  (when (seq-empty-p values) (error "Already empty"))
                  (seq-position values (completing-read "Delete: " values nil t)))))
    (unless index (error "Eeek. Something's up."))
    (set var (append (seq-take (symbol-value var) index)
                     (seq-drop (symbol-value var) (1+ index))))
    (message "Deleted: %s" (truncate-string-to-width
                            (seq-elt values index)
                            (- (window-body-width) 9)))))

;;; Provide
(provide 'my-setup-programming)
;;; my-setup-programming.el ends here
