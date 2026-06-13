;; my-setup-programming.el  -*- lexical-binding: t -*-

(message "Setting up programming settings...")

;;* Prettify and rice code
;;** prism to color programming blocks
(use-package prism
  :hook
  (emacs-lisp-mode . prism-mode)
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
;; show-paren-mode is global by default since Emacs 28
(setopt show-paren-delay 0)

;; Pretty symbols
(setopt prettify-symbols-unprettify-at-point t)
(global-prettify-symbols-mode 1)

;;** highlight various things in code
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
  :hook (prog-mode . rainbow-delimiters-mode))

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

;; Tree-sitter-aware expand region — replaces expand-region
;; Expansion order: subword → word → symbol → list/string → treesit node → defun/paragraph
;; In text modes, sentence expansion is added between word and paragraph.
(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract))
  :custom
  (expreg-restore-point-on-quit t)
  :config
  (defun my-expreg--enable-sentence ()
    "Add sentence expansion to `expreg-functions' for text modes."
    (setq-local expreg-functions
                (append expreg-functions '(expreg--sentence))))
  (dolist (hook '(text-mode-hook org-mode-hook markdown-mode-hook))
    (add-hook hook #'my-expreg--enable-sentence)))

;;* eglot and treesitter for language server protocols (LSP)
(use-package eglot
  :ensure nil
  :hook (prog-mode . my/maybe-eglot-ensure)
  :init
  ;; No LSP server exists for emacs-lisp-mode; built-in eldoc, xref, and
  ;; completion-at-point already cover what an LSP would provide.
  (defun my/maybe-eglot-ensure ()
    "Start eglot only in modes that have an LSP server configured."
    (unless (derived-mode-p 'emacs-lisp-mode)
      (eglot-ensure)))
  :bind (:map
         eglot-mode-map
         ("C-c c a" . eglot-code-actions)
         ("C-c c o" . eglot-code-actions-organize-imports)
         ("C-c c r" . eglot-rename)
         ("C-c c f" . eglot-format)))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

;;** Tree-sitter
;; Configure grammar sources and install them if missing.
;; Remap classic major modes to their tree-sitter equivalents.
(use-package treesit
  :ensure nil
  :when (treesit-available-p)
  :custom
  (treesit-font-lock-level 4)
  ;; Auto-install missing grammars on demand (Emacs 31+)
  (treesit-auto-install-grammar 'always)
  :config
  ;; Grammar sources — each entry is (LANG . (URL [REVISION [SOURCE-DIR]])
  (setopt treesit-language-source-alist
          '((python     "https://github.com/tree-sitter/tree-sitter-python")
            (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
            (bash       "https://github.com/tree-sitter/tree-sitter-bash")
            (org        "https://github.com/milisims/tree-sitter-org")
            (markdown   "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                        nil "tree-sitter-markdown/src")
            (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                             nil "tree-sitter-markdown-inline/src")
            (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
            (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
            (toml       "https://github.com/tree-sitter/tree-sitter-toml")
            (json       "https://github.com/tree-sitter/tree-sitter-json")
            ;; NOTE: latex grammar requires `tree-sitter generate' before
            ;; compile (no parser.c in repo).  The dylib was manually built
            ;; and placed in tree-sitter/.  If it goes missing, run:
            ;;   cd /tmp && git clone https://github.com/latex-lsp/tree-sitter-latex
            ;;   cd tree-sitter-latex && npx tree-sitter generate
            ;;   cc -shared -fPIC -o libtree-sitter-latex.dylib src/parser.c src/scanner.c -Isrc -Os
            ;;   cp libtree-sitter-latex.dylib ~/.config/.emacs/tree-sitter/
            (latex      "https://github.com/latex-lsp/tree-sitter-latex")))

  ;; Remap classic modes to tree-sitter modes where available
  (setopt major-mode-remap-alist
          '((python-mode     . python-ts-mode)
            (sh-mode         . bash-ts-mode)
            (js-json-mode    . json-ts-mode)
            (dockerfile-mode . dockerfile-ts-mode)
            (conf-toml-mode  . toml-ts-mode))))

;;* Programming modes
;; reminder to run eglot-upgrade-eglot every so often
;;** Python
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

;;** json and jq
(use-package jq-mode
  :mode "\\.jq\\'")

;;** Applescript
(use-package applescript-mode
  :mode (("\\.scpt\\'" . applescript-mode)
         ("\\.applescript\\'"       . applescript-mode)))

(use-package ob-applescript
  :vc (:url "https://github.com/stig/ob-applescript.el")
  :commands (org-babel-execute:applescript))

;;** Elisp (emacs-lisp)
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0))

;; better jump to definition
(use-package elisp-def
  :commands (elisp-def elisp-def-mode)
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . elisp-def-mode))

;; Elisp hook
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
  (add-hook hook (lambda ()
                   (setq show-trailing-whitespace t)
                   (setq show-paren-context-when-offscreen t)
                   (yas-minor-mode 1))))

;;* Shell
;;** Inherit env variables into emacs on Mac OSX
(use-package exec-path-from-shell
  :config
  (when (or (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))


;;** Shell Scripts
(use-package sh-script
  :ensure nil
  :init
  ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms.
  (dolist (pattern '("zlogin\\'"
                     "zlogout\\'"
                     "zpreztorc\\'"
                     "zprofile\\'"
                     "zshenv\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'sh-mode))))

;;* Indentation
(use-package aggressive-indent
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

;;* Provide my-setup-programming.el
(provide 'my-setup-programming)
;;; my-setup-programming.el ends here
