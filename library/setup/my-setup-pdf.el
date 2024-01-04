;;; my-setup-pdf.el --- summary -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein

;;; Commentary:

;;; Code:
(message "Setting up PDF support...")
;; PDF Management

;;;; PDF-Tools
;; good but often problematic pdf reader and annotator
;; need to run M-x pdf-tools-install after the first install to set up the server
(use-package pdf-tools
  :mode (("\\.pdf$" . pdf-view-mode))
  :commands (pdf-view-mode)
  ;; :init
  ;; (pdf-loader-install :no-query)
  :bind (:map pdf-view-mode-map
         ;; Navigation
         ("j"  . pdf-view-next-line-or-next-page)
         ("k"  . pdf-view-previous-line-or-previous-page)
         ("l"  . pdf-view-next-page-command)
         ("h"  . pdf-view-previous-page-command)
         ("g"  . pdf-view-first-page)
         ("G"  . pdf-view-last-page)
         ("t"  . pdf-view-goto-page)
         ("l"  . pdf-view-goto-label)
         ;; Search
         ("/"  . isearch-forward)
         ("?"  . isearch-backward)
         ;; Actions
         ("-"  . pdf-view-shrink)
         ("+"  . pdf-view-enlarge)
         ("="  . pdf-view-fit-page-to-window)
         ("r"  . pdf-view-revert-buffer)
         ("o"  . pdf-links-action-perform)
         ("O"  . pdf-outline)
         ("!"  . my-pdf-no-filter))
  :config
  ;; initialise
  (pdf-tools-install-noverify)
  ;; HiDPI
  (setq pdf-view-use-imagemagick t
        pdf-view-use-scaling t)

  (defun my-pdf-no-filter ()
    "View pdf without colour filter."
    (interactive)
    (pdf-view-midnight-minor-mode -1))

  ;; tex hook
  ;; see https://github.com/politza/pdf-tools#auto-revert
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; other hooks
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (blink-cursor-mode -1)
                                  (pulsing-cursor-mode -1)
                                  (display-line-numbers-mode -1)
                                  (column-number-mode -1)
                                  (auto-revert-mode -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-setup-pdf)
