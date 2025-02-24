;;; my-setup-search.el -*- lexical-binding: t -*-

(message "Setting up search settings...")

;;* Search
;;** consult-ripgrep-all (rga)
;; from this gist:https://gist.github.com/jthaman/c4eb411defc98f82cfd85c8c0d4c67e0#file-consult-ripgrep-all-el
;; (load "/Users/ilya/.config/.emacs/library/consult-ripgrep-all.el")

;;** Ag -- the silver searcher
(use-package ag
  :defer 10
  :config
  (progn
    (defun ag/jump-to-result-if-only-one-match ()
      "Jump to the first ag result if that ag search came up with just one match."
      (let (only-one-match)
        (when (member "--stats" ag-arguments)
          (save-excursion
            (goto-char (point-min))
            (setq only-one-match (re-search-forward "^1 matches\\s-*$" nil :noerror)))
          (when only-one-match
            (next-error)
            (kill-buffer (current-buffer))
            (message (concat "ag: Jumping to the only found match and "
                             "killing the *ag* buffer."))))))
    (add-hook 'ag-search-finished-hook #'ag/jump-to-result-if-only-one-match)

    ;; Set default ag arguments
    ;; It looks like the ~/.agignore is used when launching ag from emacs too.
    ;; So the ignores from ~/.agignore don't have to be set here again.
    (setq ag-highlight-search t)
    ;; By default, ag.el will open results in a different window in the frame, so
    ;; the results buffer is still visible. You can override this so the results
    ;; buffer is hidden and the selected result is shown in its place:
    (setq ag-reuse-window nil)
    ;; reuse the same *ag* buffer for all your searches
    (setq ag-reuse-buffers t)
    ;; ;; To save buffer automatically when `wgrep-finish-edit'
    (setq wgrep-auto-save-buffer t)))

;;** Deadgrep
;; Deadgrep uses ripgrep for extremely fast text searches and provides a
;; separate buffer for results.
(use-package deadgrep)

;;** Ripgrep
;; Ripgrep is a replacement for both grep like (search one file) and ag like
;; (search many files) tools. It's fast and versatile and written in Rust.
(use-package rg :commands rg)

;;** Xref
;; Built-in library for cross-referencing
(use-package xref
  :ensure nil
  :defer 10)

;;** wgrep to edit grep results like wdired
(use-package wgrep
  :vc (:url "https://github.com/mhayashi1120/Emacs-wgrep"))

;;* Replace
;;** visual regexp
(use-package visual-regexp
  :commands (vr/query-replace))

(use-package visual-regexp-steroids
  :commands (vr/select-query-replace))

;;** substitute -- rapidly change search/replace in buffer
(use-package  substitute
  :commands (substitute-target-in-buffer)
  :config
  ;; Set this to nil if you do not like visual feedback on the matching
  ;; target.  Default is t.
  (setq substitute-highlight t)

  ;; Set this to t if you want to always treat the letter casing
  ;; literally.  Otherwise each command accepts a `C-u' prefix
  ;; argument to do this on-demand.
  (setq substitute-fixed-letter-case t)

  ;; If you want a message reporting the matches that changed in the
  ;; given context.  We don't do it by default.
  (add-hook 'substitute-post-replace-functions #'substitute-report-operation))

;;* File search

;;** fzf wrapper
(use-package fzf
  :bind
  ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;;** Fuzzy File Finding with affe
(use-package affe
  :after orderless
  :commands (affe-find)
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-.")

  ;; The default regular expression transformation of Consult is limited.
  ;; Configure Orderless as affe-regexp-compiler in Consult.
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))


;;** Provide
(provide 'my-setup-search)
;; my-setup-search.el ends here
