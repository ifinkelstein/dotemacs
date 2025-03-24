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
  :config
  (setq save-place-file (concat my-cache-dir "saved-places"))
  (setq save-place-forget-unreadable-files nil))

;; This package keeps track of recently opened files.
(use-package recentf
  :ensure nil
  :defer 2
  :custom
  (recentf-save-file (concat my-cache-dir "recentf"))
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 10)
  :config
  (recentf-mode 1))

;;* Go To Change
;; Navigate to last edits.
(use-package goto-chg
  :vc (:url "https://github.com/emacs-evil/goto-chg")
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

;;;; Goto Address
;; This package allows you to click or hit a key sequence while on a
;; URL or e-mail address, and either load the URL into a browser of
;; your choice using the browse-url package, or if it's an e-mail
;; address, to send an e-mail to that address.
;; (use-package goto-addr
;;   :ensure nil
;;   :hook ((compilation-mode . goto-address-mode)
;;          (prog-mode . goto-address-prog-mode)
;;          (eshell-mode . goto-address-mode)
;;          (text-mode . goto-address-mode)
;;          (shell-mode . goto-address-mode))
;;   :bind (:map goto-address-highlight-keymap
;;          ("<RET>"  . goto-address-at-point)
;;          ("M-<RET>" . newline))
;;   :commands (goto-address-prog-mode
;;              goto-address-mode))


;; Jump in Buffer
(defun my-jump-in-buffer ()
  "Jump between headlines in buffer using consult"
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (call-interactively 'consult-org-heading))
   (t
    (call-interactively 'consult-outline))))


;;;; mwim -- move where i mean
;; Provide several commands to switch between various line positions,
;; like moving to the beginning/end of code, line or comment.
(use-package mwim
  :bind (([remap move-beginning-of-line] . 'mwim-beginning)
	     ([remap move-end-of-line] . 'mwim-end)))

;;* Dogears for auto-bookmarks
(use-package dogears
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-r" . dogears-remember)
              ("M-g M-D" . dogears-sidebar))
  :config
  ;; auto-remember the place every 5 sec
  (dogears-mode 1))

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
                        ;; (?i . avy-action-ispell)
                        (?z . avy-action-zap-to-char)
                        ;; (?  . avy-action-embark)
                        ;; (?= . avy-action-define)
                        ;; (67108896 . avy-action-mark-to-char)
                        ;; (67108925 . avy-action-tuxi)
                        ;; (?W . avy-action-tuxi)
                        ;; (?h . avy-action-helpful)
                        (?x . avy-action-exchange)

                        ;; (11 . avy-action-kill-line)
                        ;; (25 . avy-action-yank-line)

                        (?k . avy-action-kill-word-jump)
                        (?K . avy-action-kill-sentence-jump)
                        (?F . avy-action-mark-to-char)

                        ;; (?w . avy-action-easy-copy)
                        ;; (?c . avy-action-kill-stay)
                        ;; (?y . avy-action-yank)
                        ;; (?t . avy-action-teleport)
                        ;; (?T . avy-action-teleport-whole-line)

                        (?W . avy-action-copy-whole-line)
                        ;; (?M . avy-action-kill-whole-line)
                        (?Y . avy-action-yank-end-of-line)))


  :config
  ;; lots of useful actions
  ;; see: https://github.com/xl666/avy-conf/blob/main/avy.org
  ;; Base function actions
  ;; For generic actions
  ;; Like actions that only execute a command at point and stay
  ;; This is useful for more complex actions

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

  (defun avy-generic-command-action (action-f)
    "Excecutes action-f at point and stays"
    (save-excursion
      (goto-char pt)
      (funcall action-f))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-generic-command-action-no-stay (action-f)
    "Excecutes action-f at point and returns to original position"
    (goto-char pt)
    (funcall action-f)
    t)

  ;; Actions from Avy can do anything
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char (+ 1 pt)))

  ;; (defun avy-action-helpful (pt)
  ;;   (avy-generic-command-action #'helpful-at-point))
  ;; (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-flyspell)

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

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
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

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-yank-end-of-line (pt)
    (avy-action-copy-end-of-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

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

  ;; additional useful avy actions
  ;; https://github.com/abo-abo/avy/issues/312
  (defun avy-goto-string (string)
    "Jump to a visible occurace of string."
    (interactive (list (read-from-minibuffer "String: " nil nil nil nil (thing-at-point 'symbol t)))
                 (avy-with avy-goto-string
                   (avy-process
                    (avy--regex-candidates (regexp-quote string))))))
  ) ;;use-package avy

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

;; patch avy-zap to use avsingle char
;; (el-patch-feature avy-zap)
;; (with-eval-after-load 'avy-zap
;;   (el-patch-defun avy-zap--internal (&optional zap-up-to-char-p)
;;     "Patched to use avy-goto-char-timer. If ZAP-UP-TO-CHAR-P, perform `zap-up-to-char'."
;;     (let ((start (point))
;;           avy-all-windows)
;;       (avy-zap--flet-if
;;           avy-zap-forward-only
;;           (window-start (&optional window) (point))
;;         (if (member avy-zap-function avy-zap--function-list)
;; 	        (when (call-interactively 'avy-goto-char-timer)
;; 	          (and (avy-zap--xor (<= start (point)) zap-up-to-char-p)
;; 		           (forward-char))
;; 	          (funcall avy-zap-function start (point)))
;;           (error "Invalid `avy-zap-function' value `%s' is not in the valid list: %s"
;; 	             avy-zap-function avy-zap--function-list))))))

;;* CTRLF
;; improves on Isearch
(use-package ctrlf
  :bind ("s-f" . ctrlf-forward-default)
  ;; :bind (:map ctrlf-mode-map ("C-j" . avy-isearch))
  :config
  (ctrlf-mode +1))

;;* easy-kill
;; Easy kill and mark commands
(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))


;;** easy-kill-extras
(use-package easy-kill-extras
  :vc (:url "https://github.com/knu/easy-kill-extras.el")
  :after (easy-kill)
  :config
  ;; Integrate `expand-region' functionality with easy-kill
  (define-key easy-kill-base-map (kbd "o") 'easy-kill-er-expand)
  (define-key easy-kill-base-map (kbd "i") 'easy-kill-er-unexpand)

  ;; Add the following tuples to `easy-kill-alist', preferrably by
  ;; using `customize-variable'.
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
  :vc (:url "https://github.com/dacap/keyfreq")
  :config
  ;; where to store info
  (setq keyfreq-file (expand-file-name "kefreq.txt" my-etc-dir))
  (setq keyfreq-file-lock (expand-file-name "kefreq.lock" my-etc-dir))
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
          vterm--self-insert
          puni-backward-delete-char ))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;** Provide
(provide 'my-setup-navigation)

;;; navigation.el ends here
