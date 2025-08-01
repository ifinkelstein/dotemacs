;; my-setup-programming.el  -*- lexical-binding: t -*-

(message "Setting up programming settings...")

;;* Prettify and rice code
;;** prism to color programming blocks
(use-package prism
  :vc (:url "https://github.com/alphapapa/prism.el" :rev :latest)
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

;;** Show Pretty Symbols
;; Show matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0))

(use-package prog-mode
  :ensure nil
  :defer t
  :custom
  ;; Show markup at point
  (prettify-symbols-unprettify-at-point t)
  :config
  ;; Pretty symbols
  (global-prettify-symbols-mode +1))

;;** highlight various things in code
;;** highlight-sexp
(use-package highlight-sexp
  :vc (:url "https://github.com/daimrod/highlight-sexp")
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

;; highlights defined Emacs Lisp symbols in source code.
(use-package highlight-defined
  :hook
  (emacs-lisp-mode . highlight-defined-mode)
  (help-mode . highlight-defined-mode))

(use-package highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

;;** Visualization of Delimiters
;; highlight delimiters such as parentheses, brackets or braces
;; according to their depth. Each successive level is highlighted in a
;; different color. This makes it easy to spot matching delimiters,
;; orient yourself in the code, and tell which statements are at a
;; given depth.
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :commands rainbow-identifiers-mode)

(use-package elec-pair
  :ensure nil
  :defer 1
  :config (electric-pair-mode 1))

;; Surround & Change Delimiters
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

(use-package embrace
  :bind (("s-e" . embrace-add)
         ("s-E" . embrace-delete))
  :hook ((org-mode . embrace-org-mode-hook) ;; defined in embrace.el
         (markdown-mode . embrace-markdown-mode-hook) ;; defined below
	     (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
	     (LaTeX-mode . embrace-LaTeX-mode-hook))

  :config

  (defun embrace-parens ()
    (interactive)
    (embrace--add-internal (region-beginning) (region-end) ?\( ))
  (defun embrace-angle ()
    (interactive)
    (embrace--add-internal (region-beginning) (region-end) ?< ))
  
  (defun embrace-double-quotes ()
    (interactive)
    (embrace--add-internal (region-beginning) (region-end) ?\" ))

  (defun embrace-single-quotes ()
    (interactive)
    (embrace--add-internal (region-beginning) (region-end) ?\'))

  (defun embrace-markdown-mode-hook ()
    (dolist (lst '((?* "*" . "*")
                   (?\ "\\" . "\\")
                   (?$ "$" . "$")
                   (?/ "/" . "/")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
  ) ;; use-package embrace

;;* eglot and treesitter for language server protocols (LSP)
(use-package eglot
  :ensure nil
  :hook (prog-mode . eglot-ensure)
  :init
  (setq eglot-stay-out-of '(flymake))
  :bind (:map
         eglot-mode-map
         ("C-c c a" . eglot-code-actions)
         ("C-c c o" . eglot-code-actions-organize-imports)
         ("C-c c r" . eglot-rename)
         ("C-c c f" . eglot-format)))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))
;;* Programming modes
;; reminder to run eglot-upgrade-eglot every so often
;;** Python
(use-package elpy
  :vc (:url "https://github.com/jorgenschaefer/elpy")
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")

  :init
  (elpy-enable))



;;** json and jq
(use-package json-mode)
;;** org-babel integration--need to decide if I want this
(use-package jq-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode)))

;;** Applescript
(use-package applescript-mode
  :mode (("\\.scpt\\'" . applescript-mode)
         ("\\.applescript\\'"       . applescript-mode))
  :commands (applescript-mode))

(use-package ob-applescript
  :vc (:url "https://github.com/stig/ob-applescript.el"))

(use-package applescript-mode)

;;** YAML
(use-package yaml-mode)

(use-package outline-yaml
  :disabled t
  :vc (:url "https://github.com/jamescherti/outline-yaml.el")
  :hook
  ((yaml-mode . outline-yaml-minor-mode)
   (yaml-ts-mode . outline-yaml-minor-mode)))


;;** Elisp (emacs-lisp)
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
  :config
  ;; Show ElDoc messages in the echo area immediately, instead of after 1/2 a second.
  (setq eldoc-idle-delay 0))

;; better jump to definition
;; TODO: re-write hook using use-package syntax
(use-package elisp-def
  :commands (elisp-def elisp-def-mode)
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . elisp-def-mode))

;; Elisp hook
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
  (add-hook hook (lambda ()
                   (setq show-trailing-whitespace t)
                   (setq show-paren-context-when-offscreen t)
                   (prettify-symbols-mode 1)
                   (eldoc-mode 1)
                   (yas-minor-mode 1)
                   (rainbow-delimiters-mode 1))))

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

;; (with-eval-after-load 'el-patch
;;   (el-patch-defun lisp-indent-function (indent-point state)
;;     "This function is the normal value of the variable `lisp-indent-function'.
;; The function `calculate-lisp-indent' calls this to determine
;; if the arguments of a Lisp function call should be indented specially.
;; INDENT-POINT is the position at which the line being indented begins.
;; Point is located at the point to indent under (for default indentation);
;; STATE is the `parse-partial-sexp' state for that position.
;; If the current line is in a call to a Lisp function that has a non-nil
;; property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
;; it specifies how to indent.  The property value can be:
;; * `defun', meaning indent `defun'-style
;;   (this is also the case if there is no property and the function
;;   has a name that begins with \"def\", and three or more arguments);
;; * an integer N, meaning indent the first N arguments specially
;;   (like ordinary function arguments), and then indent any further
;;   arguments like a body;
;; * a function to call that returns the indentation (or nil).
;;   `lisp-indent-function' calls this function with the same two arguments
;;   that it itself received.
;; This function returns either the indentation to use, or nil if the
;; Lisp function does not specify a special indentation."
;;     (el-patch-let (($cond (and (elt state 2)
;;                                (el-patch-wrap 1 1
;;                                  (or (not (looking-at "\\sw\\|\\s_"))
;;                                      (looking-at ":")))))
;;                    ($then (progn
;;                             (if (not (> (save-excursion (forward-line 1) (point))
;;                                         calculate-lisp-indent-last-sexp))
;;                                 (progn (goto-char calculate-lisp-indent-last-sexp)
;;                                        (beginning-of-line)
;;                                        (parse-partial-sexp (point)
;;                                                            calculate-lisp-indent-last-sexp 0 t)))
;;                             ;; Indent under the list or under the first sexp on the same
;;                             ;; line as calculate-lisp-indent-last-sexp.  Note that first
;;                             ;; thing on that line has to be complete sexp since we are
;;                             ;; inside the innermost containing sexp.
;;                             (backward-prefix-chars)
;;                             (current-column)))
;;                    ($else (let ((function (buffer-substring (point)
;;                                                             (progn (forward-sexp 1) (point))))
;;                                 method)
;;                             (setq method (or (function-get (intern-soft function)
;;                                                            'lisp-indent-function)
;;                                              (get (intern-soft function) 'lisp-indent-hook)))
;;                             (cond ((or (eq method 'defun)
;;                                        (and (null method)
;;                                             (> (length function) 3)
;;                                             (string-match "\\`def" function)))
;;                                    (lisp-indent-defform state indent-point))
;;                                   ((integerp method)
;;                                    (lisp-indent-specform method state
;;                                                          indent-point normal-indent))
;;                                   (method
;;                                    (funcall method indent-point state))))))
;;       (let ((normal-indent (current-column))
;;             (el-patch-add
;;               (orig-point (point))))
;;         (goto-char (1+ (elt state 1)))
;;         (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
;;         (el-patch-swap
;;           (if $cond
;;               ;; car of form doesn't seem to be a symbol
;;               $then
;;             $else)
;;           (cond
;;            ;; car of form doesn't seem to be a symbol, or is a keyword
;;            ($cond $then)
;;            ((and (save-excursion
;;                    (goto-char indent-point)
;;                    (skip-syntax-forward " ")
;;                    (not (looking-at ":")))
;;                  (save-excursion
;;                    (goto-char orig-point)
;;                    (looking-at ":")))
;;             (save-excursion
;;               (goto-char (+ 2 (elt state 1)))
;;               (current-column)))
;;            (t $else)))))))

;;** lua

;;* Shell
;;** Inherit env variables into emacs on Mac OSX
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;;** Shell Scripts
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

;;* Indentation
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

;; (use-package highlight-indent-guides
;;   :hook (prog-mode . highlight-indent-guides-mode)
;;   :config
;;   (setq-default highlight-indent-guides-method 'character
;;                 highlight-indent-guides-character ?\│
;;                 ;; default is \x2502 but it is very slow on Mac
;;                 ;; highlight-indent-guides-character ?\xFFE8
;;                 highlight-indent-guides-responsive 'top
;;                 highlight-indent-guides-auto-odd-face-perc 5
;;                 highlight-indent-guides-auto-even-face-perc 5
;;                 highlight-indent-guides-auto-character-face-perc 15
;;                 highlight-indent-guides-auto-enabled t))

;;;; Linting/Error Checking (Flymake)
;; Both Flycheck and Flymake are good linters, but let's stick with the built-in Flymake
;; (use-package flymake
;;   :ensure nil
;;   :hook (prog-mode . flymake-mode)
;;   :custom
;;   (flymake-fringe-indicator-position 'left-fringe)
;;   (flymake-suppress-zero-counters t)
;;   (flymake-start-on-flymake-mode t)
;;   (flymake-no-changes-timeout nil)
;;   (flymake-start-on-save-buffer t)
;;   (flymake-proc-compilation-prevents-syntax-check t)
;;   (flymake-wrap-around nil)
;;   ;; Customize mode-line
;;   (flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))
;;   (flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters)))

;; Linting for emacs package libraries
;; (use-package package-lint
;;   :commands (package-lint-batch-and-exit
;;              package-lint-current-buffer
;;              package-lint-buffer)
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)
;;   ;; Avoid`package-not-installable' errors
;;   ;; See https://github.com/purcell/package-lint/issues/153
;;   (with-eval-after-load 'savehist
;;     (add-to-list 'savehist-additional-variables 'package-archive-contents)))

;; A collection of flymake backends
;; (use-package flymake-collection
;;   :hook (after-init . flymake-collection-hook-setup))

;; Use Consult with Flymake
;; (use-package consult-flymake
;;   ;; comes with consult
;;   :ensure nil
;;   :bind (:map my+flymake-keys
;;          ("c" . consult-flymake)))


;;* Provide my-setup-programming.el
(provide 'my-setup-programming)
;;; my-setup-programming.el ends here
