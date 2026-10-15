;;; my-setup-search.el -*- lexical-binding: t -*-

(message "Setting up search settings...")

;;* Search
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
(use-package substitute
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
  :commands (affe-find affe-grep)
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-."))


;;** Provide
(provide 'my-setup-search)
;; my-setup-search.el ends here
