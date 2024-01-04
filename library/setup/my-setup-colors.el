;;; colors.el --- summary -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein

;;; Commentary:

;; Setup how emacs handles colors

;;; Code:

;;;; Color
(setq-default ns-use-srgb-colorspace t)

;;;; Show Colors
;; https://github.com/emacsmirror/rainbow-mode
;; Colorize color names in buffers
(use-package rainbow-mode
  :commands rainbow-mode)

(provide 'my-setup-colors)
;;; colors.el ends here
