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

;;* Dictionary
;; built in
(use-package dictionary
  :ensure nil
  :custom
  (dictionary-server "dict.org"))

;; quick dictionary access
;; To retrieve the word under the cursor and display its definition in a buffer:
;; (quick-sdcv-search-at-point)
;; To prompt the user for a word and display its definition in a buffer:
;; (quick-sdcv-search-input)
(use-package quick-sdcv
  :bind ("M-#" . dictionary-lookup-definition)
  :custom
  (quick-sdcv-dictionary-prefix-symbol "►")
  (quick-sdcv-ellipsis " ▼ ")
  :config
  (setq quick-sdcv-hist-size 100))

;;* Spelling
(use-package ispell
  :commands (ispell-word ispell-region ispell-buffer)
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(use-package flyspell
  :config
  (setq flyspell-abbrev-p t
        flyspell-use-global-abbrev-table-p t
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  :hook ((markdown-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;; completion of spellings
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
         ("C-;" . flyspell-correct-previous)
         ("C-:" . flyspell-correct-at-point))
  :custom
  (flyspell-correct-interface #'flyspell-correct-completing-read))

;; use avy for flyspell corrections
(use-package flyspell-correct-avy-menu
  :after flyspell-correct)

;; Completion of misspelled words in buffer
(use-package consult-flyspell
  :after flyspell
  :config
  (setq consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil
        ;; Apply flyspell-correct-at-point directly after selecting candidate
        ;; and jump back to consult-flyspell.
        consult-flyspell-select-function
        (lambda () (flyspell-correct-at-point) (consult-flyspell))))

;; Spelling Goto Next Error
(defun my-flyspell-ispell-goto-next-error ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))


;; use avy interface for flyspell
(use-package flyspell-correct-avy-menu
  :after flyspell-correct)

;; Spelling Goto Next Error
(defun my-flyspell-ispell-goto-next-error ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

;;* Grammar
;; This is apparently a non-trivial problem.
;; Some solutions for general text:
;; https://www.reddit.com/r/emacs/comments/ril2m4/grammar_checker_for_scientific_writing/

;; https://github.com/mhayashi1120/Emacs-langtool
;; with this to remove annoyances:
;; https://github.com/cjl8zf/langtool-ignore-fonts
;; or
;; https://github.com/amperser/proselint
(use-package langtool
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :init (setq langtool-default-language "en-US")
  :custom-face
  ;; add subtle line under the error as opposed to the ugliness before
  (langtool-errline ((t (:background nil :foreground nil :inherit 'flyspell-duplicate))))

  :config
  (setq langtool-java-user-arguments '("-Dfile.encoding=UTF-8"))
  ;; disable some pesky rules
  ;; MORFOLOGIK_RULE_EN_US -- spell checker, ispell has my dictionary
  ;; EN_QUOTES -- smart quotes
  ;; EN_DIACRITICS_REPLACE -- suggest diacritics (blase ->blasé )
  ;; WANT -- ???
  ;; other rules:
  ;; https://community.languagetool.org/rule/list?lang=en
  (setq langtool-user-arguments '("--disable" "MORFOLOGIK_RULE_EN_US,WANT,EN_QUOTES,EN_DIACRITICS_REPLACE"))
  (unless (or langtool-bin
              langtool-language-tool-jar
              langtool-java-classpath)
    (cond ((setq langtool-bin
                 (or (executable-find "languagetool-commandline")
                     (executable-find "languagetool"))))  ; for nixpkgs.languagetool
          ((featurep :system 'macos)
           (cond
            ;; is user using home brew?
            ((file-directory-p "/usr/local/Cellar/languagetool")
             (setq langtool-language-tool-jar
                   (locate-file "libexec/languagetool-commandline.jar"
                                (doom-files-in "/usr/local/Cellar/languagetool"
                                               :type 'dirs
                                               :depth 2))))
            ;; macports compatibility
            ((file-directory-p "/opt/local/share/java/LanguageTool")
             (setq langtool-java-classpath "/opt/local/share/java/LanguageTool/*"))))
          ((featurep :system 'linux)
           (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")))))


;;* Search/Replace
(use-package substitute
  :config
  ;; Set this to nil if you do not like visual feedback on the matching
  ;; target.  Default is t.
  (setq substitute-highlight t)
  ;; Set this to t if you want to always treat the letter casing
  ;; argument to do this on-demand.
  (setq substitute-fixed-letter-case t)

  ;; If you want a message reporting the matches that changed in the
  ;; given context.  We don't do it by default.
  (add-hook 'substitute-post-replace-functions #'substitute-report-operation))

;;* Abbreviations (abbrev)
(use-package abbrev
  :ensure nil
  :defer 2
  :hook (text-mode . abbrev-mode)
  :config
  (setq abbrev-file-name (concat my-var-dir "abbrev/.abbrev_defs")
        save-abbrevs 'silently)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))) 

;; fancy dabbrev replacement
(use-package fancy-dabbrev
  :bind ("M-/" . fancy-dabbrev-expand)
  :config
  ;; define functions here
  (setq fancy-dabbrev-preview-delay 0.3)
  (global-fancy-dabbrev-mode)
  (global-set-key (kbd "TAB") 'fancy-dabbrev-expand-or-indent)
  (global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward))

;; disable adding flyspell auto-corrected text to abbrev mode file
(with-eval-after-load 'flyspell
  (setq flyspell-abbrev-p nil))

;; replace undo with hippie expand
(global-set-key (kbd "C-/") 'hippie-expand)

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

;;* Rotate words at point with grugru
;; TODO: re-enable after org debug
(use-package grugru
  :disabled t
  :bind ("C-c r" . grugru)   ; global keymap
  :config
  ;; Define grugru keeping case by default
  (customize-set-variable 'grugru-strings-metagenerator #'grugru-metagenerator-keep-case)

  ;; define useful word rotations
  (grugru-define-multiple
    (org-mode
     (symbol "tomorrow" "today")
     (symbol "TODO" "NEXT" "DONE")
     (symbol "deep" "shallow"))
    (text-mode
     (symbol "true" "false")
     (symbol "left" "right")
     (symbol "yes" "no"))
    (latex-mode
     (symbol "\small" "\tiny")
     (symbol "draft" "final")
     (symbol "disable" "enable"))
    (LaTeX-mode
     (symbol "\\small" "\\tiny")
     (symbol "draft" "final")
     (symbol "disable" "enable"))
    (symbol "enable" "disable")
    (symbol "true" "false")
    (symbol "yes" "no"))
  (grugru-highlight-mode))

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

;;* Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
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
        markdown-open-command "~/bin/mark.sh"
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

  (defun my--markdown-settings ()
    "settings for markdown mode"
    (progn
      (turn-on-flyspell)
      (auto-fill-mode)
      (hl-todo-mode)))

  ;; markdown hooks
  (add-hook 'markdown-mode-hook 'my--markdown-settings)

  ;; for use with meow point movement
  (modify-syntax-entry ?@ "_" markdown-mode-syntax-table))

;;** Markdown TOC
(use-package markdown-toc
  :after markdown
  :hook (markdown-mode . markdown-toc))



;; macro: delete backslashes in paragraph to cleanup markdown conversion
(fset 'my-md-delete-backslash
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\361\361f\\x" 0 "%d")) arg)))


;;* Helper Functions
;; TODO: move to functions.el
;; custom kill-sentence behavior
;; erase up to the period from within a sentence, but includes punctuation from start of sentence
;; source: https://emacs.stackexchange.com/questions/12266/how-change-behavior-of-kill-sentence-based-on-position-in-sentence
(defun my-forward-to-sentence-end ()
  "Move point to just before the end of the current sentence."
  (forward-sentence)
  (backward-char)
  (unless (looking-back "[[:alnum:]]")
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
        (when (looking-back "^[[:space:]]+") (delete-horizontal-space)))
    (kill-region (point) (progn (my-forward-to-sentence-end) (point)))
    (just-one-space 0)))

(define-key (current-global-map) [remap kill-sentence] 'my-kill-sentence-dwim)

(defun my-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://xahlee.info/emacs/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if (< line-spacing 0.2)
      (setq line-spacing 0.5)
    (setq line-spacing 0.1))
  (redraw-frame (selected-frame)))


;; end my-setup-writing
(provide 'my-setup-writing)

