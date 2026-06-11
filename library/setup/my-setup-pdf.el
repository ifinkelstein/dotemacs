;;; my-setup-pdf.el -*- lexical-binding: t -*-

;; PDF Management
(message "Setting up PDF support...")

;;* PDF-Tools
;; need to run M-x pdf-tools-install after the first install to set up the server
(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :bind (:map pdf-view-mode-map
              ;; Navigation
              ("j"  . pdf-view-next-line-or-next-page)
              ("k"  . pdf-view-previous-line-or-previous-page)
              ("l"  . pdf-view-next-page-command)
              ("h"  . pdf-view-previous-page-command)
              ("g"  . pdf-view-first-page)
              ("G"  . pdf-view-last-page)
              ("t"  . pdf-view-goto-page)
              ("L"  . pdf-view-goto-label)
              ("s-c"  . pdf-view-kill-ring-save)
              ("S"    . xah-open-in-external-app)
              ("s-p"  . dwim-shell-command-print)
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

  (defun my-pdf-no-filter ()
    "View pdf without colour filter."
    (interactive)
    (pdf-view-midnight-minor-mode -1))

  ;; tex hook
  ;; see https://github.com/politza/pdf-tools#auto-revert
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; disable ctrlf in pdf-isearch (ctrlf is being kept but may not be loaded yet)
  (add-hook 'pdf-isearch-minor-mode-hook
            (lambda ()
              (when (fboundp 'ctrlf-local-mode)
                (ctrlf-local-mode -1))))
  (add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (blink-cursor-mode -1)
                                  (line-number-mode -1)
                                  (display-line-numbers-mode -1)
                                  (column-number-mode -1)
                                  (auto-revert-mode -1))))


(provide 'my-setup-pdf)
