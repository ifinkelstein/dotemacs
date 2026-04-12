;;; my-setup-search.el -*- lexical-binding: t -*-

(message "Setting up search settings...")

;;* Search
;;** consult-ripgrep-all (rga)
;; from this gist:https://gist.github.com/jthaman/c4eb411defc98f82cfd85c8c0d4c67e0#file-consult-ripgrep-all-el
;; (load "/Users/ilya/.config/.emacs/library/consult-ripgrep-all.el")

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

;;** re-builder
;; Use string syntax in re-builder so regexps look like normal Emacs
;; regexps (no double-escaping)
(setq reb-re-syntax 'string)

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

;;** Fuzzy File Finding with affe
(use-package affe
  :after orderless
  :commands (affe-find)
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-."))


;;** Provide
(provide 'my-setup-search)
;; my-setup-search.el ends here
