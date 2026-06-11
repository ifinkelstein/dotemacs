;;; my-setup-completion.el  -*- lexical-binding: t -*-

;; All packages related to core narrowing and completion functions

(message "Setting up completion packages...")

;;* Narrowing Completion

;;** Vertico
;; Enable vertico for vertical completion
(use-package vertico
  :bind (:map vertico-map
              ("C-l"      . vertico-previous-group  )
              ("C-n"      . vertico-next            )
              ("C-e"      . vertico-previous        )
              ("C-k"      . vertico-next-group      )
              ("<escape>" . minibuffer-keyboard-quit)
              ("M-TAB"    . minibuffer-complete)
              ("M-RET"    . vertico-exit-input))
  :hook (emacs-startup . vertico-mode)
  :config
  ;; Cycle through candidates
  (setq vertico-cycle t)

  ;; Don't resize buffer
  (setq vertico-resize nil)

  ;; try the `my-completion-category-sort-function' first
  (advice-add #'vertico--sort-function :before-until #'my-completion-category-sort-function)

  (defun my-completion-category-sort-function ()
    (alist-get (vertico--metadata-get 'category)
               my-completion-category-sort-function-overrides))

  (defvar my-completion-category-sort-function-overrides
    '((file . my-directories-before-files))
    "Completion category-specific sorting function overrides.")

  (defun my-directories-before-files (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-history-length-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files))))

;;***  Vertico Packages
;; Use vertico in buffer
(use-package vertico-buffer
  :ensure nil
  :after vertico
  :custom
  (vertico-buffer-hide-prompt t)
  :config
  ;; put minibuffer at top -- this is the more natural place to be looking!
  (setq vertico-buffer-display-action
        '(display-buffer-in-side-window
          (window-height . 12)
          (side . top)))
  (vertico-buffer-mode 1))

;; Vertico repeat last command
(use-package vertico-repeat
  :ensure nil
  :hook (minibuffer-setup . vertico-repeat-save)
  :commands (vertico-repeat-last))

;; Configure directory extension
(use-package vertico-directory
  :ensure nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;avy-like quick keys navigation
(use-package vertico-quick
  :ensure nil
  :after vertico
  :bind (:map vertico-map
         ("M-q" . vertico-quick-insert)
         ("C-q" . vertico-quick-exit))
  :custom
  (vertico-quick1 "arstg")
  (vertico-quick2 "neiom"))

;; A few more useful configurations...
;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Grow and shrink minibuffer
(setq resize-mini-windows t)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable recursive minibuffers
(setf enable-recursive-minibuffers t)

;; Persist history over Emacs restarts with savehist mode. Vertico sorts by history position.
;; Savehist is set up in my-settings-ui.el

;;* Ordering
;; Setup for vertico
;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;;* Embark
;; Actions on narrowed candidates
(use-package embark
  :hook ((embark-collect-mode . hl-line-mode))
  :bind (("M-," . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-completion-map
         ("C-S-o" . embark-act)
         ;; https://www.reddit.com/r/emacs/comments/19ec8v5/weekly_tips_tricks_c_thread/
         ;; see if this works, not clear that its doing anything
         :map minibuffer-local-map
         ("C-SPC" . (lambda ()
                      (interactive)
                      (embark-select)
                      (vertico-next)))
         :map completion-list-mode-map
         (";" . embark-act)
         :map embark-file-map
         ("x" . embark-open-externally)
         ("O" . xah-open-in-external-app)
         ;; When using the Embark package, you can bind `marginalia-cycle' as an Embark action
         :map embark-general-map
         ("A"  . marginalia-cycle)
         ("G" . my-embark-google-search))
  :custom
  ;; Use which-key
  ;; Don't display extra embark buffer
  (embark-indicators '(embark-which-key-indicator
                       embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Useful Functions
  (define-key embark-file-map (kbd "D") 'my-dired-here)
  (defun my-dired-here (file)
    "Open dired in this directory"
    (dired (file-name-directory file))) ;; embark

  (define-key embark-file-map (kbd "g") 'my-consult-rg-here)
  (defun my-consult-rg-here (file)
    "consult-ripgrep in this directory."
    (let ((default-directory (file-name-directory file)))
      (consult-ripgrep)))

  (defun my-embark-google-search (term)
    "Search Google for a given term.
Borrowed from: https://github.com/oantolin/embark/wiki/Additional-Actions"
    (interactive "sSearch Term: ")
    (browse-url
     (format "http://google.com/search?q=%s" term))))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;* Marginalia
;; Enable richer annotations using the Marginalia package
;; Info about candidates pulled from metadata
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (setq marginalia-align 'center))

;;* Consult
(use-package consult
  :commands (consult-line
             consult-line-multi
             consult-buffer
             consult-project-buffer
             consult-find
             consult-apropos
             consult-yank-pop
             consult-goto-line
             consult-outline
             consult-org-agenda
             consult-org-heading
             consult-flymake)
  :bind (:map project-prefix-map
         ("b" . consult-project-buffer)
         ("m" .  consult-bookmark)
         :map global-map
         ("C-h i" . consult-info))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<")
  ;; Previewing
  ;; https://github.com/minad/consult#live-previews
  ;; This setting previews on any key but with a very short delay (.2 sec)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-recent-file
   consult-source-project-recent-file consult-theme
   :preview-key '(:debounce 0.2 any))

  ;; Configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; search settings
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")

  ;; Make consult locate work with macos spotlight
  (setq consult-locate-args "mdfind -name")

  (setq consult-async-min-input 2)

  ;; Consult info functions
  (defun consult-info-emacs ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "efaq" "elisp" "cl" "compat"))

  (defun consult-info-org ()
    "Search through the Org info page."
    (interactive)
    (consult-info "org"))

  (defun consult-info-completion ()
    "Search through completion info pages."
    (interactive)
    (consult-info "vertico" "consult" "marginalia" "orderless" "embark"
                  "corfu" "cape" "tempel")))

;; Search at point with consult
(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;;** Consult Dir
;; Consult-dir allows you to easily select and switch between "active" directories.
(use-package consult-dir
  :commands (consult-dir
             consult-dir-jump-file)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;* In-Buffer Completion with corfu
;; Corfu
(use-package corfu
  :bind
  (:map corfu-map
   ("C-j"      . corfu-next)
   ("C-k"      . corfu-previous)
   ("M-l"      . corfu-show-location)
   ("M-SPC" . corfu-insert-separator)
   ("<escape>" . corfu-quit)
   ("TAB" . corfu-insert)
   ([tab] . corfu-insert))
  :custom
  ;; auto-complete
  (corfu-auto t) ;; Enable auto completion
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.4)

  (corfu-min-width 25)
  (corfu-max-width 100)
  (corfu-count 10)
  (corfu-scroll-margin 5)
  (corfu-cycle t)
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)
  (corfu-separator ?\s) ;; Use space as separator
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current t)  ;; Preview current candidate?
  (corfu-popupinfo-delay '(0.4 0.2)) ;; delay for info popup; (initial subsequent)
  :init
  (global-corfu-mode)

  :config
  ;; Enable corfu history
  (corfu-history-mode 1)

  ;; Enable Corfu completion for commands like M-: (eval-expression) or M-!
  ;; (shell-command)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  ;; Avoid press RET twice in shell
  ;; https://github.com/minad/corfu#completing-in-the-eshell-or-shell
  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))

  (advice-add #'corfu-insert :after #'corfu-send-shell)

  ;; Completion in eshell
  (defun my-corfu-eshell-setup ()
    "Configure corfu for eshell."
    (setq-local corfu-quit-no-match t
                corfu-quit-at-boundary t
                corfu-auto nil)
    (corfu-mode))
  (add-hook 'eshell-mode-hook #'my-corfu-eshell-setup)

  ;; Display popup info
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1))


;; Use dabbrev with Corfu!
(use-package dabbrev
  :bind (("C-M-/" . dabbrev-expand)))

;;**  Corfu Extensions (Cape)
;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; this is a super-capf that combines two capf

  (defalias 'cape-dabbrev+dict
    (cape-capf-super #'cape-dabbrev #'cape-dict))
  (add-to-list 'completion-at-point-functions #'cape-dabbrev+dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;;** Kind Icon (For Corfu)
(use-package kind-icon
  :defer 1
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)
  ;; NOTE kind-icon' depends on `svg-lib' which creates a cache directory that
  ;; defaults to the `user-emacs-directory'. Here, I change that directory to
  ;; the cache location.
  (svg-lib-icons-dir (concat my-cache-dir  "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; NOTE 2022-02-05: Add hook to reset cache so the icon colors match the theme
  ;; If this isn't done, then the backgound color will remain the same, meaning
  ;; it will not match the background color corresponding to the current theme.
  ;; This hook is already set in the `my-setup-themes.el' file, but you could
  ;; set it here if you prefer:
  ;; e.g. (add-hook 'after-load-theme-hook #'kind-icon-reset-cache)
  ;; kind-icon needs to have its cache flushed after theme change
  (add-hook 'lambda-themes-after-load-theme-hook #'kind-icon-reset-cache))

;;* Yasnippet
(use-package yasnippet
  :defer 10
  :bind (:map yas-minor-mode-map
         ("C-'" . yas-expand))
  :custom
  (yas-snippet-dirs `(;; custom snippets in library/my-snippets/
                      ,(concat my-library-dir "my-snippets/")))
  :config
  ;; see https://emacs.stackexchange.com/a/30150/11934
  (defun my-yas-org-mode-hook ()
    (setq-local yas-buffer-local-condition
                '(not (org-in-src-block-p t))))
  (add-hook 'org-mode-hook #'my-yas-org-mode-hook)
  ;; suppress warnings when expanding
  (with-eval-after-load 'warnings
    (push '(yasnippet backquote-change) warning-suppress-types))
  (yas-global-mode 1))

(provide 'my-setup-completion)
;;; my-setup-completion.el ends here
