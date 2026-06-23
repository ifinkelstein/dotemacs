;; my-setup-writing.el -*- lexical-binding: t -*-
;; Author: Ilya Finkelstein
(message "Setting up writing settings...")

;; Personal tweaks to improve writing and other text-based manipulations

;;* UI improvements and sane defaults

;; Virtually every program out there will delete the
;; selected/highlighted text as soon as the user types something.
;; Emacs does not do this by default, even though it has the
;; functionality available. Let us then enable it:

(use-package delsel
  :ensure nil ; no need to install it as it is built-in
  :hook (after-init . delete-selection-mode))

;;* Smerge mode
;; built in
(use-package smerge-mode
  :ensure nil
  :bind (:map smerge-mode-map
              ("M-o" . my-smerge-transient))

  :config
  (transient-define-prefix my-smerge-transient ()
    "Smerge commands transient."
    ["Smerge Actions"
     ;; Navigation
     ["Navigation"
      ("p" "Prev conflict" smerge-prev)                 ;; C-c ^ p
      ("n" "Next conflict" smerge-next)]                ;; C-c ^ n

     ;; Keep Commands
     ["Keep"
      ("t" "Keep current (reTurn)" smerge-keep-current) ;; C-c ^ RET
      ("a" "Keep all" smerge-keep-all)                  ;; C-c ^ a
      ("b" "Keep base" smerge-keep-base)                ;; C-c ^ b
      ("l" "Keep lower" smerge-keep-lower)              ;; C-c ^ l
      ("u" "Keep upper" smerge-keep-upper)]             ;; C-c ^ u

     ;; Actions
     ["Actions"
      ("C" "Combine with next" smerge-combine-with-next);; C-c ^ C
      ("R" "Refine" smerge-refine)                      ;; C-c ^ R (Shift-r)
      ("r" "Resolve" smerge-resolve)]                   ;; C-c ^ r (lowercase r)

     ;; View/Diff
     ["View/Diff"
      ("E" "Ediff" smerge-ediff)                        ;; C-c ^ E (Shift-e)
      ("<" "Diff base - upper" smerge-diff-base-upper)  ;; C-c ^ = <
      ("=" "Diff upper - lower" smerge-diff-upper-lower);; C-c ^ = =
      (">" "Diff base - lower" smerge-diff-base-lower)] ;; C-C ^ = >
     ]))


;;* Dictionary
(use-package dictionary
  :ensure nil
  :bind ("M-#" . dictionary-lookup-definition)
  :custom
  (dictionary-server "dict.org"))

;;* Spelling
(use-package ispell
  :commands (ispell-word ispell-region ispell-buffer)
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
    (setq ispell-personal-dictionary "~/.config/.aspell.en.pws")))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-;" . jinx-correct)
         ("C-M-$" . jinx-languages)
         (:map text-mode-map ("C-." . my-jinx-correct-cycle)))
  :custom (jinx-languages "en_US")
  :config
  ;; jinx has no built-in in-place candidate cycler (only the `jinx-correct'
  ;; menu), so reproduce `flyspell-auto-correct-word': replace the misspelled
  ;; word at point with its top suggestion, and on repeat cycle to the next.
  (defvar-local my-jinx-cycle--state nil
    "State for `my-jinx-correct-cycle': (BEG END CANDS IDX).")

  (defvar my-jinx-cycle-max-suggestions 8
    "How many spelling suggestions `my-jinx-correct-cycle' cycles through.
The original misspelled word is always appended after these, so the full
cycle length is at most this many suggestions plus one.  Set higher to see
more candidates, or nil for all of them.")

  (defun my-jinx--overlay-at-point ()
    "Return the jinx misspelling overlay at or just before point."
    (seq-find (lambda (o) (eq (overlay-get o 'category) 'jinx-overlay))
              (append (overlays-at (point))
                      (and (> (point) (point-min))
                           (overlays-at (1- (point)))))))

  (defun my-jinx--overlay-before-point ()
    "Return the nearest jinx misspelling overlay ending at/before point.
The search stays on the current line and skips over correctly-spelled
words, so it lands on the most recent misspelling behind point (like
`flyspell-auto-correct-previous-word')."
    (let ((bol (line-beginning-position)))
      (car
       (last
        (sort
         (seq-filter (lambda (o)
                       (and (eq (overlay-get o 'category) 'jinx-overlay)
                            (<= (overlay-end o) (point))))
                     (overlays-in bol (point)))
         (lambda (a b) (< (overlay-end a) (overlay-end b))))))))

  (defun my-jinx--find-overlay ()
    "Return the jinx overlay to correct.
Prefers the misspelling at or just before point (so the avy jump action
and a cursor sitting on a word both work); otherwise falls back to the
nearest misspelling earlier on the line, reaching across whitespace,
punctuation and correctly-spelled words."
    (or (my-jinx--overlay-at-point)
        (my-jinx--overlay-before-point)))

  (defun my-jinx--word-suggestions (word)
    "Return plain-string spelling suggestions for WORD.
Drops jinx's \"accept and save\" entries (tagged with face `jinx-save')."
    (delete-dups
     (delq nil
           (mapcar (lambda (s)
                     (unless (eq (get-text-property 0 'face s) 'jinx-save)
                       (substring-no-properties s)))
                   (jinx--correct-suggestions word)))))

  (defun my-jinx--replace (beg end word)
    "Replace region BEG..END with WORD; return the new end position."
    (goto-char beg)
    (delete-region beg end)
    (insert word)
    (point))

  (defun my-jinx--echo (cands idx)
    "Echo all CANDS in the minibuffer, highlighting the one at IDX.
The final entry (the original misspelled word) is shown dimmed."
    (let ((message-log-max nil)
          (last (1- (length cands)))
          (n -1))
      (message "%s"
               (mapconcat
                (lambda (c)
                  (setq n (1+ n))
                  (cond ((= n idx) (propertize c 'face 'highlight))
                        ((= n last) (propertize c 'face 'shadow))
                        (t c)))
                cands "  "))))

  (defun my-jinx-correct-cycle ()
    "Correct the misspelled word at point, cycling suggestions on repeat.
First call replaces it with the most likely suggestion; repeating the
command cycles through the rest in place, like `flyspell-auto-correct-word'.
At most `my-jinx-cycle-max-suggestions' candidates are offered.  The last
entry in the cycle is the original misspelled word, so one more repeat
restores it before wrapping back to the top suggestion.  Each step echoes
the candidate list with the current pick highlighted."
    (interactive)
    ;; Continue cycling while point still sits on the just-corrected word.
    ;; Position-based (not `last-command'-based) so it also resumes after the
    ;; avy jump action lands point there, and after the word loses its overlay.
    (if (and my-jinx-cycle--state
             (<= (nth 0 my-jinx-cycle--state) (point) (nth 1 my-jinx-cycle--state)))
        (pcase-let ((`(,beg ,end ,cands ,idx) my-jinx-cycle--state))
          (setq idx (mod (1+ idx) (length cands)))
          (let* ((cand (nth idx cands))
                 (new-end (my-jinx--replace beg end cand)))
            (setq my-jinx-cycle--state (list (copy-marker beg) (copy-marker new-end) cands idx))
            (my-jinx--echo cands idx)))
      (let ((ov (my-jinx--find-overlay))
            (orig (point)))
        (unless ov (user-error "No misspelled word at point"))
        ;; When the correction sits behind point (reached across whitespace,
        ;; punctuation or earlier words), leave a mark at the original spot so
        ;; `C-u C-SPC' returns there after cycling.
        (when (> orig (overlay-end ov))
          (push-mark orig t))
        (let* ((beg (overlay-start ov))
               (end (overlay-end ov))
               (word (buffer-substring-no-properties beg end))
               (sugg (my-jinx--word-suggestions word))
               ;; Cap to `my-jinx-cycle-max-suggestions'; the original word is
               ;; the last entry, so cycling eventually restores it before
               ;; wrapping back to the top suggestion.
               (cands (append (if my-jinx-cycle-max-suggestions
                                  (seq-take sugg my-jinx-cycle-max-suggestions)
                                sugg)
                              (list word))))
          (unless (cdr cands) (user-error "No suggestions for %S" word))
          (let ((new-end (my-jinx--replace beg end (car cands))))
            (setq my-jinx-cycle--state (list (copy-marker beg) (copy-marker new-end) cands 0))
            (my-jinx--echo cands 0)))))))

;;* Grammar

;;** LTeX+ (LanguageTool via LSP, fully offline)
(use-package eglot-ltex-plus
  :vc (:url "https://github.com/emacs-languagetool/eglot-ltex-plus")
  :defer t ; lambda hooks can't autoload, so without this the block loads eagerly
  :hook ((org-mode markdown-mode LaTeX-mode)
         . (lambda ()
             (require 'eglot-ltex-plus)
             (eglot-ensure)))
  :init
  (setq eglot-ltex-plus-server-path (expand-file-name "~/bin/ltex-ls-plus/bin/ltex-ls-plus")
        eglot-ltex-plus-communication-channel 'stdio)
  ;; LanguageTool spell-checks via the MORFOLOGIK rule; disable it so it
  ;; doesn't double up on jinx, which owns spelling here.  This MUST be the
  ;; global value, not buffer-local: eglot answers the server's
  ;; `workspace/configuration' request from inside a `with-temp-buffer'
  ;; (see `eglot--workspace-configuration-plist'), so a value set in a mode
  ;; hook is invisible and eglot replies null.  The top-level key must match
  ;; the section ltex-ls requests, which is "ltex".
  (setq-default eglot-workspace-configuration
                '(:ltex (:disabledRules (:en-US ["MORFOLOGIK_RULE_EN_US"])))))

;;* Abbreviations (abbrev)
(use-package abbrev
  :ensure nil
  :hook (text-mode . abbrev-mode)
  :config
  (setq abbrev-file-name (concat my-library-dir ".abbrev_defs")
        save-abbrevs 'silently)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))) 

;; fancy dabbrev replacement
(use-package fancy-dabbrev
  :bind ("M-/" . fancy-dabbrev-expand))

;; replace undo with hippie expand
(global-set-key (kbd "C-/") 'hippie-expand)

;; C-w with no active region kills the previous word (shell-style)
;; instead of signaling an error (Emacs 31).
(setopt kill-region-dwim 'emacs-word)

;;* Move-Text
;; move the current line using M-up / M-down (or any other bindings you choose) if a region is marked, it will move the region instead.
(use-package move-text
  :config
  ;; note: this may clobber some keybindings
  (move-text-default-bindings))

;;* Emacs Everywhere
;; Write with emacs everywhere
;; https://github.com/tecosaur/emacs-everywhere
(use-package emacs-everywhere
  :commands (emacs-everywhere))

;;* Capitalization
(use-package fix-word
  :config
  (global-set-key (kbd "M-u") #'fix-word-upcase)
  (global-set-key (kbd "M-l") #'fix-word-downcase)
  (global-set-key (kbd "M-c") #'fix-word-capitalize))

;;* Hungry space delete
;; deleting a whitespace character will delete all whitespace until the next non-whitespace character.
(use-package hungry-delete
  :config
  (setq hungry-delete-join-reluctantly t)
  (global-set-key (kbd "s-<backspace>") #'hungry-delete-backward)
  (global-hungry-delete-mode))

;;* Olivetti
;; this package isn't extreme/opinionated as writeroom-mode
;; I prefer to use this package for writing in text-mode(s)
(use-package olivetti
  :commands (olivetti-mode)
  :hook
  (org-mode . olivetti-mode)
  ;; (slack-mode . olivetti-mode)
  (markdown-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 0.75)

  ;; unload some annoying key conflicts with LaTeX-mode
  (define-key olivetti-mode-map (kbd "C-c {") nil)
  (define-key olivetti-mode-map (kbd "C-c }") nil))

;;* Mixed Pitch
(use-package mixed-pitch
  :hook ((markdown-mode . mixed-pitch-mode)
         (gfm-mode . mixed-pitch-mode))
  :config
  ;; markdown faces not covered by mixed-pitch defaults
  (dolist (face '(markdown-table-face
                  markdown-pre-face))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;;* Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :hook ((markdown-mode . auto-fill-mode)
         (markdown-mode . hl-todo-mode))
  :bind (:map markdown-mode-map
              ("s-*"        . markdown-insert-list-item)
              ("s-b"        . markdown-insert-bold)
              ("s-i"        . markdown-insert-italic)
              ("<M-right>"  . markdown-demote)
              ("<M-left>"   . markdown-promote)
              ("<M-up>"     . markdown-move-up)
              ("<M-down>"   . markdown-move-down)
              ("<C-return>" . markdown-insert-header-dwim))
  :config
  (setq markdown-enable-math nil
        markdown-enable-wiki-links t
        markdown-nested-imenu-heading-index t
        markdown-footnote-location 'immediately
        markdown-unordered-list-item-prefix "-   "
        markdown-header-scaling t
        markdown-use-pandoc-style-yaml-metadata t
        markdown-asymmetric-header t)
  (setq markdown-live-preview-window-function 'my--markdown-live-preview-window-xwidget)

  (defun my--markdown-live-preview-window-xwidget (file)
    "Preview file with xwidget browser"
    (xwidget-webkit-browse-url (concat "file://" file))
    (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
      (when (buffer-live-p buf)
        (and (eq buf (current-buffer)) (quit-window))
        (pop-to-buffer buf))))

  ;; for use with meow point movement
  (modify-syntax-entry ?@ "_" markdown-mode-syntax-table)

  )


;;* Helper Functions
;; TODO: move to functions.el
;; custom kill-sentence behavior
;; erase up to the period from within a sentence, but includes punctuation from start of sentence
;; source: https://emacs.stackexchange.com/questions/12266/how-change-behavior-of-kill-sentence-based-on-position-in-sentence
(defun my-forward-to-sentence-end ()
  "Move point to just before the end of the current sentence."
  (forward-sentence)
  (backward-char)
  (unless (looking-back "[[:alnum:]]" (1- (point)))
    (backward-char)))

(defun my-beginning-of-sentence-p ()
  "Return  t if point is at the beginning of a sentence."
  (let ((start (point))
        (beg (save-excursion (forward-sentence) (forward-sentence -1))))
    (eq start beg)))

(defun my-kill-sentence-dwim ()
  "Kill the current sentence up to and possibly including the punctuation.
When point is at the beginning of a sentence, kill the entire
sentence. Otherwise kill forward but preserve any punctuation at the sentence end."
  (interactive)
  (if (my-beginning-of-sentence-p)
      (progn
        (kill-sentence)
        (just-one-space)
        (when (looking-back "^[[:space:]]+" (line-beginning-position)) (delete-horizontal-space)))
    (kill-region (point) (progn (my-forward-to-sentence-end) (point)))
    (just-one-space 0)))

(define-key (current-global-map) [remap kill-sentence] 'my-kill-sentence-dwim)

(defun my-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://xahlee.info/emacs/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if (or (null line-spacing) (< line-spacing 0.2))
      (setq line-spacing 0.5)
    (setq line-spacing 0.1))
  (redraw-frame (selected-frame)))


;; end my-setup-writing
(provide 'my-setup-writing)

