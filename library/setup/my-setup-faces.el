;;; faces.el --- Setup for faces          -*- lexical-binding: t; -*-
;; Author: Ilya Finkelstein

;;; Commentary:
;;
;;; Code:
(message "Setting up faces settings...")

;;;; Outline Faces
;; Make outline faces look better
(use-package outline-minor-faces
  :after outline
  :hook ((emacs-lisp-mode lisp-interaction-mode lisp-mode) . outline-minor-faces-mode))

;;;; What Face?
;; https://stackoverflow.com/a/66287459/6277148
(defun what-face (pos)
  "State the face at point POS in the minibuffer."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;;; Underline
(customize-set-variable 'x-underline-at-descent-line t)

;;;; Dim inactive windows
(use-package dimmer
  :hook (after-init . dimmer-mode)
  :custom
  (dimmer-prevent-dimming-predicates '(window-minibuffer-p))
  (dimmer-fraction 0.5)
  (dimmer-adjustment-mode :foreground)
  (dimmer-use-colorspace :rgb)
  (dimmer-watch-frame-focus-events nil)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (dimmer-configure-posframe)
  (dimmer-configure-vertico))

(defun dimmer-configure-vertico ()
  "Convenience settings for Dimmer & Vertico users."
  (with-no-warnings
    (add-to-list
     'dimmer-buffer-exclusion-regexps "^ \\*Vertico\\*$")))

;;;; Cursor
;; don't show cursor in inactive windows
(customize-set-variable 'cursor-in-non-selected-windows nil)

;;;; Reveal Mode
;; Toggle uncloaking of invisible text near point, including folded org headlines (Reveal mode).
(use-package reveal
  :ensure nil
  :defer 1
  :config
  (setq reveal-auto-hide nil)
  (global-reveal-mode))

;;;; SVG Library (For Tags/Labels/etc.)
  ;;; SVG Tag Mode
(use-package svg-tag-mode
  :when (image-type-available-p 'svg)
  :hook (prog-mode . svg-tag-mode)
  :config
  (setq svg-tag-tags
        '(;; Replaces any occurence of :XXX: with a dynamic SVG tag displaying XXX
          ("\\(:[A-Z]+:\\)" . ((lambda (tag)
                                 (svg-tag-make tag :face 'success :inverse t :beg 1 :end -1))))
          ;; other tags
          ("DONE:"  . ((lambda (tag) (svg-tag-make "DONE:"  :face 'fringe  :inverse t ))))
          ("FIXME:" . ((lambda (tag) (svg-tag-make "FIXME:" :face 'error   :inverse t))))
          ("HACK:"  . ((lambda (tag) (svg-tag-make "HACK:"  :face 'warning :inverse t))))
          ("NOTE:"  . ((lambda (tag) (svg-tag-make "NOTE:"  :face 'warning :inverse t))))
          ("TODO:"  . ((lambda (tag) (svg-tag-make "TODO:"  :face 'warning :inverse t)))))))

;;;; Widgets
(use-package wid-edit
  :ensure nil
  :defer 1
  :custom
  ;; No ugly button for checkboxes
  (widget-image-enable nil))

;;;; Highlight

;;;;; Highlight Numbers & TODOS
(use-package highlight-numbers
  :defer t
  :commands highlight-numbers-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package hl-todo
  :commands hl-todo-mode
  :hook ((prog-mode . hl-todo-mode)
         (markdown-mode . hl-todo-mode)
         (LaTeX-mode . hl-todo-mode))
  :custom
  (hl-todo-keyword-faces '(("zzz"   . "#FF0000")
                           ("ZZZ"   . "#FF0000")
	                       ("FIXME"  . "#FF0000")
                           ("todo"  . "#A020F0")
	                       ("TODO"  . "#A020F0"))))
;; hydra for TODOs
(with-eval-after-load 'hydra
  (defhydra my-hydra-todo
    (:pre
     (hl-todo-mode 1)
     :post
     (hl-todo-mode -1))
    "Todo"
    ("n" hl-todo-next "Next")
    ("p" hl-todo-previous "Previous")
    ("o" hl-todo-occur "Occur")
    ("q" nil "Quit" :color blue :exit t)))


;;;;; Highlight Cursor Line with Pulse
;; From https://karthinks.com/software/batteries-included-with-emacs/
;; Replace external package with internal command

(use-package pulse
  :bind
  ("C-<return>" . pulse-line)
  :commands (pulse-line pulse-momentary-highlight-one-line)
  :config
  (setq pulse-delay 0.08)
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (interactive)
    (pulse-momentary-highlight-one-line (point)))
  ;; pulse for commands
  (dolist (command '(scroll-up-command scroll-down-command
                                       recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line))
  ;; pulse on window change
  (push 'pulse-line window-selection-change-functions))

;;;; Goggles (Highlight Changes)
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

;;;; Empty Lines
;; Don't show empty lines.
;; .. Allows you to tell if there are blank lines at the end of the file.
(setq-default indicate-empty-lines nil)


(provide 'my-setup-faces)
;;; faces.el ends here
