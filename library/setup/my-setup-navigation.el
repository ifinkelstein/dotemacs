;; my-setup-navigation.el --- summary -*- lexical-binding: t -*-

;; Setup code for "navigating" Emacs, in the sense of outlines, goto functions,
;; saving place, recent files, etc.

(message "Setting up navigation libraries...")


;;* Imenu list outline
;; Make a useful outline buffer
;; Binding set to toggle "o"
(use-package imenu-list
  :commands (imenu-list-smart-toggle imenu-list-minor-mode)
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-position 'left))

;;* Persist cursors and other variables across seasons
;; This package saves the last visited location in files and restores them when they are reopened.
(use-package saveplace
  :ensure nil
  :hook (emacs-startup . save-place-mode)
  :custom
  (save-place-autosave-interval 300)   ; Emacs 31: persist even on daemon crash
  :config
  (setq save-place-file (concat my-cache-dir "saved-places"))
  (setq save-place-forget-unreadable-files nil)
  ;; Recenter view after save-place jumps to restored position,
  ;; so the cursor isn't stranded at the edge of the window
  (advice-add 'save-place-find-file-hook :after
              (lambda (&rest _)
                (when buffer-file-name (ignore-errors (recenter))))))

;; This package keeps track of recently opened files.
(use-package recentf
  :ensure nil
  :defer 2
  :custom
  (recentf-save-file (concat my-cache-dir "recentf"))
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 10)
  (recentf-autosave-interval 300)      ; Emacs 31: persist even on daemon crash
  :config
  (recentf-mode 1))

;;* Go To Change
;; Navigate to last edits.
(use-package goto-chg
  :bind (("M-'" . goto-last-change)
         ("M-\"" . goto-last-change-reverse)
         :map org-mode-map
         ("M-'" . org-goto-last-change)
         ("M-\"" . org-goto-last-change-reverse))
  :config
  ;; unfold org headings if change was inside them
  ;; https://www.reddit.com/r/emacs/comments/1blnbzx/go_to_last_change_with_consult/
  (defun org-goto-last-change ()
    (interactive)
    (call-interactively 'goto-last-change)
    (when (org-invisible-p)
      (org-fold-show-context 'default)))

  (defun org-goto-last-change-reverse ()
    (interactive)
    (call-interactively 'goto-last-change-reverse)
    (when (org-invisible-p)
      (org-fold-show-context 'default))))


;; Jump in Buffer
(defun my-jump-in-buffer ()
  "Jump between headlines in buffer using consult"
  (interactive)
  (call-interactively (if (derived-mode-p 'org-mode)
                          #'consult-org-heading
                        #'consult-outline)))


;;;; mwim -- move where i mean
;; Provide several commands to switch between various line positions,
;; like moving to the beginning/end of code, line or comment.
(use-package mwim
  :bind (([remap move-beginning-of-line] . 'mwim-beginning)
	     ([remap move-end-of-line] . 'mwim-end)))

;;* avy
(use-package avy
  :bind   (("C-c C-j" . avy-resume)
           ("C-c j" . avy-goto-char-timer)
           ("s-J" . avy-goto-char-timer)
           ("s-j" . avy-goto-word-1)
           ;; use avy with isearch
           (:map isearch-mode-map
                 ("C-j" . avy-isearch)))

  :custom
  (avy-all-windows 'all-frames)
  ;; show a letter for a single candidate jump for invoking an action first
  (avy-single-candidate-jump nil)
  ;;change how many seconds to wait for char timeout
  (avy-timeout-seconds 0.3)
  ;; may need to inactivate some avy jump keys if I have too many actions
  (avy-keys '(?n ?e ?i ?o ?t ?s ?r ?a ?l ?u ?y ?p ?f ?w)) ;; old: ?a ?r ?s ?t ?g ?m ?n ?e ?i ?o
  (avy-orders-alist
   '((avy-goto-char . avy-order-closest)
     (avy-goto-char-timer . avy-order-closest)
     (avy-goto-line . avy-order-closest)))
  (avy-dispatch-alist '((?w . avy-action-mark)
                        (?z . avy-action-zap-to-char)
                        (?x . avy-action-exchange)
                        (?k . avy-action-kill-word-jump)
                        (?K . avy-action-kill-sentence-jump)
                        (?F . avy-action-mark-to-char)
                        (?W . avy-action-copy-whole-line)
                        (?Y . avy-action-yank-end-of-line)))


  :config
  ;; lots of useful actions
  ;; see: https://github.com/xl666/avy-conf/blob/main/avy.org

  ;; run this after loading embark
  (with-eval-after-load 'embark
    ;; Add the option to run embark when using avy
    ;; ref: https://codeberg.org/ashton314/emacs-bedrock/src/branch/main/extras/base.el
    (defun my-avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    ;; After invoking avy-goto-char-timer, hit "'" to run embark at the next
    ;; candidate you select
    (setf (alist-get ?' avy-dispatch-alist) 'my-avy-action-embark))

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char (+ 1 pt)))

  ;; Jump and correct spelling with the jinx cycler (replaces flyspell action).
  ;; Applies the top suggestion and leaves point on the corrected word so C-.
  ;; cycles to the other suggestions in place. The pre-jump location is saved
  ;; to a register (and the mark ring), so after cycling you can return with
  ;; `C-x r j j' (or `C-u C-SPC').
  (defvar my-avy-jinx-return-register ?j
    "Register where `avy-action-jinx' stores the pre-jump location.")

  (defun avy-action-jinx (pt)
    (with-selected-window (cdr (ring-ref avy-ring 0))
      (point-to-register my-avy-jinx-return-register)
      (push-mark nil t))           ; silent; integrates with C-u C-SPC
    (goto-char pt)
    (setq my-jinx-cycle--state nil) ; force a fresh correction at PT
    (my-jinx-correct-cycle)
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-jinx)

  ;; Useful actions here
  ;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-avy.el
  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-end-of-line (pt)
    "Copy to the end of the line from point."
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill pt end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-kill-word-jump (pt)
    (goto-char pt)
    (kill-word 1)
    (cycle-spacing)
    t)

  (defun avy-action-kill-sentence-jump (pt)
    (goto-char pt)
    (my-kill-sentence-dwim)
    t)

  (defun avy-action-yank-end-of-line (pt)
    (avy-action-copy-end-of-line pt)
    (save-excursion (yank))
    t)

  ;; additional useful avy actions
  ;; https://github.com/abo-abo/avy/issues/312
  (defun avy-goto-symbol-at-point (&optional arg)
    "Jump to a visible occurance of symbol-at-point.
The window scope is determined by `avy-all-windows' (ARG negates it)."
    (interactive "P")
    (let ((avy-all-windows (if arg
                               (not avy-all-windows)
                             avy-all-windows)))
      (avy-with avy-goto-symbol-at-point
        (avy-process
         (avy--regex-candidates (regexp-quote (thing-at-point 'symbol t)))))))

  ) ;;use-package avy

;;* Browse-url
;; Route DocuSign URLs to Chrome (they don't work with Firefox security settings)
(setq browse-url-chrome-program
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
(setq browse-url-handlers
      '(("docusign\\.net" . browse-url-chrome)
        ("docusign\\.com" . browse-url-chrome)))

;;* Link-Hint
;; use avy to navigate links
(use-package link-hint
  :after avy
  :commands (link-hint-open-link
             link-hint-copy-link)
  :config
  ;;show avy shortcuts before the link instead of on top of them
  (setq link-hint-avy-style 'pre))

;;* avy-like text zapping use avy-style navigation for zapping parts
;; of something TODO: patch this repo to add a function that defines
;; which avy function to use for the search
(use-package avy-zap
  :vc (:url "https://github.com/ifinkelstein/avy-zap")
  :bind (("M-Z" . avy-zap-to-char-dwim)
         ("M-z" . avy-zap-up-to-char-dwim))
  :config
  (setopt avy-zap-jump-function 'avy-goto-char-timer))

;;* CTRLF
;; improves on Isearch
(use-package ctrlf
  :bind ("s-f" . ctrlf-forward-default)
  :hook (after-init . ctrlf-mode))

;;* easy-kill
;; Easy kill and mark commands
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))


;;** easy-kill-extras
(use-package easy-kill-extras
  :after (easy-kill)
  :config
  ;; Integrate `expand-region' functionality with easy-kill
  (define-key easy-kill-base-map (kbd "o") 'easy-kill-er-expand)
  (define-key easy-kill-base-map (kbd "i") 'easy-kill-er-unexpand)

  ;; Add the following tuples to `easy-kill-alist', using customize-variable
  ;; or add-to-list as shown here.
  (add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?b buffer ""))
  (add-to-list 'easy-kill-alist '(?< buffer-before-point ""))
  (add-to-list 'easy-kill-alist '(?> buffer-after-point ""))
  (add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
  (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward "")))

;;* keyfreq to remember command usage
(use-package keyfreq
  :config
  ;; where to store info
  (setq keyfreq-file (expand-file-name ".emacs.keyfreq" my-library-dir))
  (setq keyfreq-file-lock (expand-file-name ".emacs.keyfreq.lock" my-library-dir))
  ;;ignore these common commands
  (setq keyfreq-excluded-commands
        '(self-insert-command
          org-self-insert-command
          forward-char
          backward-char
          previous-line
          left-char
          right-char
          newline
          next-line
          meow-next
          meow-prev
          meow-left
          meow-right
          meow-insert-exit
          meow-insert
          meow-keypad-self-insert
          mwheel-scroll
          mouse-set-point
          puni-backward-delete-char
          ultra-scroll
          eat-self-input
          ghostel--self-insert
          ghostel--scroll-intercept-down
          ghostel--scroll-intercept-up
          ghostel--send-event
          pdf-util-image-map-mouse-event-proxy
          handle-select-window
          handle-switch-frame
          mouse-drag-region
          mouse-set-region ))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;** Provide
(provide 'my-setup-navigation)

;;; navigation.el ends here
