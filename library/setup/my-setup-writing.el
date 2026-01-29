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
      ("o" "Keep lOwer (other)" smerge-keep-lower)      ;; C-c ^ o
      ("m" "Keep upper (mine)" smerge-keep-upper)       ;; C-c ^ m
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
      (hl-todo-mode)
      (variable-pitch-mode)))

  ;; markdown hooks
  (add-hook 'markdown-mode-hook 'my--markdown-settings)

  ;; for use with meow point movement
  (modify-syntax-entry ?@ "_" markdown-mode-syntax-table)

  ;;** Markdown Citation Folding
  ;; ============================================================================
  ;; Citation Folding for Markdown Mode
  ;; ============================================================================
  ;;
  ;; This section provides functionality to fold/hide Pandoc-style citations
  ;; in markdown documents using overlays. Citations are elements of the form
  ;; [@key] or [@key1; @key2] and will be folded to display as [C].
  ;;
  ;; Inspired by AUCTeX's TeX-fold-mode (see `tex-fold.el'), this implementation
  ;; uses overlays to hide citation content while preserving the underlying text.
  ;; Overlays are non-destructive - the actual buffer content is unchanged.
  ;;
  ;; Key Features:
  ;; - Toggle individual citations with `markdown-fold-citation-at-point'
  ;; - Fold all citations in buffer with `markdown-fold-citations-buffer'
  ;; - Fold citations in region with `markdown-fold-citations-region'
  ;; - Smart DWIM command that folds/unfolds based on context
  ;; - Auto-reveal when point enters a folded citation
  ;; - Mouse hover shows full citation text via help-echo
  ;;
  ;; The implementation follows the overlay-based approach from AUCTeX:
  ;; 1. Search for citation patterns using regexp
  ;; 2. Create overlays spanning the citation text
  ;; 3. Set the 'display property to show replacement text
  ;; 4. Use post-command-hook for auto-reveal behavior
  ;;
  ;; Main interactive commands:
  ;;   M-x markdown-fold-citations-dwim      - Smart fold/unfold based on context
  ;;   M-x markdown-fold-citations-buffer    - Fold all citations in buffer
  ;;   M-x markdown-fold-citations-clearout-buffer - Unfold all citations
  ;; ============================================================================

  (defgroup markdown-fold nil
    "Fold citations and other elements in Markdown documents.

This group contains customization options for the citation folding
feature in `markdown-mode'. The folding functionality allows hiding
Pandoc-style citations (e.g., [@author2023]) behind a shorter display
string (e.g., [C]) to reduce visual clutter while writing.

The implementation uses Emacs overlays, similar to AUCTeX's TeX-fold-mode,
ensuring that the underlying buffer content remains unchanged."
    :group 'markdown
    :prefix "markdown-fold-")

  (defcustom markdown-fold-citation-display-string "[C]"
    "Display string used when a citation is folded.

This string replaces the visible representation of citations like
[@key] or [@key1; @key2] when folding is active. The actual buffer
content is preserved - only the display is affected.

Common alternatives:
- \"[C]\" (default) - compact indicator
- \"[…]\" - ellipsis style
- \"📚\" - emoji indicator
- \"[cite]\" - more explicit

The string should be short to maximize the visual benefit of folding."
    :type 'string
    :group 'markdown-fold)

  (defcustom markdown-fold-citation-help-echo-max-length 80
    "Maximum length of the help-echo tooltip for folded citations.

When hovering over a folded citation, the original text is shown
in a tooltip. If the citation exceeds this length, it will be
truncated with an ellipsis (...).

Set to 0 to disable help-echo tooltips entirely."
    :type 'integer
    :group 'markdown-fold)

  (defcustom markdown-fold-auto-reveal t
    "Whether to automatically unfold citations when point enters them.

When non-nil (the default), moving point into a folded citation
will temporarily reveal its contents. The citation folds again
when point moves away.

This mimics the behavior of AUCTeX's TeX-fold-mode and provides
a convenient way to inspect citations without manual toggling.

When nil, citations remain folded until explicitly unfolded."
    :type 'boolean
    :group 'markdown-fold)

  (defface markdown-fold-citation-face
    '((((class color) (background light))
       (:foreground "SlateBlue" :weight bold))
      (((class color) (background dark))
       (:foreground "SlateBlue1" :weight bold))
      (t (:slant italic :weight bold)))
    "Face used for the display string of folded citations.

This face is applied to the replacement text (e.g., [C]) that
appears in place of the folded citation. The default uses a
blue-ish color to distinguish it from regular text while
remaining unobtrusive."
    :group 'markdown-fold)

  (defface markdown-fold-citation-unfolded-face
    '((((class color) (background light))
       (:background "#f2f0fd"))
      (((class color) (background dark))
       (:background "#38405d"))
      (t (:inverse-video t)))
    "Face for temporarily unfolded citation content.

When `markdown-fold-auto-reveal' is enabled and point enters
a folded citation, this face highlights the revealed text to
indicate its special status."
    :group 'markdown-fold)

  (defvar-local markdown-fold--open-spots nil
    "List of currently revealed (temporarily unfolded) citation overlays.

Each element is a cons cell (WINDOW . OVERLAY) tracking which
overlays are revealed in which windows. This allows the same
buffer to have different fold states in different windows.

This variable is managed internally by the post-command-hook
and should not be modified directly.")

  (defconst markdown-fold--citation-regexp
    "\\[\\(?:[^][@]*\\)?@[^]]+\\]"
    "Regular expression matching Pandoc-style citations.

This pattern matches citations in the following formats:
- Simple: [@key]
- With prefix: [see @key]
- With suffix: [@key, p. 10]
- Multiple: [@key1; @key2]
- Combined: [see @key1; @key2, p. 10]

The regexp breakdown:
- \\\\[ - Opening bracket
- \\\\(?:[^][@]*\\\\)? - Optional prefix (text before @, no brackets)
- @[^]]+ - At least one @key (@ followed by non-bracket chars)
- \\\\] - Closing bracket

Note: This intentionally matches the entire bracket expression,
not just the @key portion, as the whole citation should be folded.")

  (defun markdown-fold--make-overlay (start end)
    "Create a fold overlay for a citation from START to END.

Creates an overlay with the following properties:
- category: `markdown-fold-citation' (for identification)
- display: the folded representation (from `markdown-fold-citation-display-string')
- face: `markdown-fold-citation-face'
- help-echo: the original citation text (for tooltip)
- evaporate: t (auto-delete when text is deleted)
- priority: calculated to handle nested overlays

The overlay is returned for further manipulation if needed.

Arguments:
  START - Buffer position where the citation begins (inclusive)
  END   - Buffer position where the citation ends (exclusive)

Returns:
  The newly created overlay object."
    (let* ((priority (- (buffer-size) (- end start))) ; Smaller spans get higher priority
           (original-text (buffer-substring-no-properties start end))
           (help-echo (if (and (> markdown-fold-citation-help-echo-max-length 0)
                               (> (length original-text)
                                  markdown-fold-citation-help-echo-max-length))
                          (concat (substring original-text 0
                                            markdown-fold-citation-help-echo-max-length)
                                  "...")
                        original-text))
           (ov (make-overlay start end nil t nil)))
      (overlay-put ov 'category 'markdown-fold-citation)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'priority priority)
      (overlay-put ov 'markdown-fold-original-text original-text)
      (overlay-put ov 'help-echo help-echo)
      (overlay-put ov 'mouse-face 'highlight)
      ov))

  (defun markdown-fold--hide-overlay (ov)
    "Set the display properties on overlay OV to hide the citation.

This function applies the visual folding by setting the overlay's
display property to `markdown-fold-citation-display-string' and
applying the appropriate face.

Arguments:
  OV - An overlay created by `markdown-fold--make-overlay'"
    (when (and ov (overlay-buffer ov))
      (overlay-put ov 'display
                   (propertize markdown-fold-citation-display-string
                               'face 'markdown-fold-citation-face))
      (overlay-put ov 'face 'markdown-fold-citation-face)))

  (defun markdown-fold--show-overlay (ov)
    "Remove display properties from overlay OV to reveal the citation.

This temporarily shows the original citation text while keeping
the overlay in place. The `markdown-fold-citation-unfolded-face'
is applied to indicate the special state.

Arguments:
  OV - An overlay created by `markdown-fold--make-overlay'"
    (when (and ov (overlay-buffer ov))
      (overlay-put ov 'display nil)
      (overlay-put ov 'face 'markdown-fold-citation-unfolded-face)))

  (defun markdown-fold--citation-overlay-at-point ()
    "Return the citation fold overlay at point, or nil if none exists.

Searches through all overlays at the current point position and
returns the first one with category `markdown-fold-citation'.

Returns:
  An overlay object or nil."
    (seq-find (lambda (ov)
                (eq (overlay-get ov 'category) 'markdown-fold-citation))
              (overlays-at (point))))

  (defun markdown-fold--citation-overlays-in-region (start end)
    "Return all citation fold overlays between START and END.

Arguments:
  START - Beginning of region to search
  END   - End of region to search

Returns:
  A list of overlay objects (may be empty)."
    (seq-filter (lambda (ov)
                  (eq (overlay-get ov 'category) 'markdown-fold-citation))
                (overlays-in start end)))

  (defun markdown-fold-citation-at-point ()
    "Fold the citation at point, if any.

Searches for a Pandoc-style citation ([@...]) at or around point
and creates a fold overlay for it. If a fold overlay already
exists at point, this function does nothing.

Returns:
  The created overlay, or nil if no citation was found or
  if one was already folded."
    (interactive)
    (unless (markdown-fold--citation-overlay-at-point)
      (save-excursion
        (let ((orig-point (point))
              start end)
          ;; Search backward for opening bracket
          (when (or (looking-at markdown-fold--citation-regexp)
                    (and (re-search-backward "\\[" (line-beginning-position) t)
                         (looking-at markdown-fold--citation-regexp)
                         (<= (match-beginning 0) orig-point)
                         (>= (match-end 0) orig-point)))
            (setq start (match-beginning 0)
                  end (match-end 0))
            (let ((ov (markdown-fold--make-overlay start end)))
              (markdown-fold--hide-overlay ov)
              ov))))))

  (defun markdown-fold-citations-region (start end)
    "Fold all citations in the region from START to END.

Searches for all Pandoc-style citations within the specified
region and creates fold overlays for each one. Existing fold
overlays in the region are preserved (not duplicated).

When called interactively, operates on the active region.

Arguments:
  START - Beginning of region to fold
  END   - End of region to fold

Returns:
  The number of citations folded."
    (interactive "r")
    (let ((count 0))
      ;; First, remove existing overlays to avoid duplicates
      (markdown-fold-citations-clearout-region start end)
      (save-excursion
        (goto-char start)
        (while (re-search-forward markdown-fold--citation-regexp end t)
          (let ((ov (markdown-fold--make-overlay (match-beginning 0)
                                                  (match-end 0))))
            (markdown-fold--hide-overlay ov)
            (setq count (1+ count)))))
      (when (called-interactively-p 'interactive)
        (message "Folded %d citation%s" count (if (= count 1) "" "s")))
      count))

  (defun markdown-fold-citations-buffer ()
    "Fold all citations in the current buffer.

This is a convenience function that calls `markdown-fold-citations-region'
on the entire buffer. Any existing fold overlays are first removed
to ensure a clean state.

Returns:
  The number of citations folded."
    (interactive)
    (markdown-fold-citations-region (point-min) (point-max)))

  (defun markdown-fold-citations-clearout-region (start end)
    "Remove all citation fold overlays in the region from START to END.

This restores the original display of all citations in the
specified region by deleting their fold overlays.

Arguments:
  START - Beginning of region to unfold
  END   - End of region to unfold

Returns:
  The number of overlays removed."
    (interactive "r")
    (let ((overlays (markdown-fold--citation-overlays-in-region start end))
          (count 0))
      (dolist (ov overlays)
        (delete-overlay ov)
        (setq count (1+ count)))
      (when (called-interactively-p 'interactive)
        (message "Unfolded %d citation%s" count (if (= count 1) "" "s")))
      count))

  (defun markdown-fold-citations-clearout-buffer ()
    "Remove all citation fold overlays in the current buffer.

This is a convenience function that calls
`markdown-fold-citations-clearout-region' on the entire buffer,
restoring all citations to their original display.

Returns:
  The number of overlays removed."
    (interactive)
    (markdown-fold-citations-clearout-region (point-min) (point-max)))

  (defun markdown-fold-citation-toggle-at-point ()
    "Toggle the fold state of the citation at point.

If point is on a folded citation, unfold it (remove the overlay).
If point is on an unfolded citation, fold it.
If point is not on a citation, do nothing.

Returns:
  Non-nil if a toggle action was performed."
    (interactive)
    (let ((ov (markdown-fold--citation-overlay-at-point)))
      (if ov
          (progn
            (delete-overlay ov)
            (message "Citation unfolded")
            t)
        (when (markdown-fold-citation-at-point)
          (message "Citation folded")
          t))))

  (defun markdown-fold-citations-dwim ()
    "Do What I Mean for citation folding.

Smart command that performs different actions based on context:

1. If point is on a folded citation: unfold it
2. If point is on an unfolded citation: fold it
3. If region is active: fold all citations in region
4. Otherwise: fold all citations in buffer

This provides a single convenient keybinding for all common
folding operations."
    (interactive)
    (cond
     ;; If on a citation overlay, toggle it
     ((markdown-fold--citation-overlay-at-point)
      (markdown-fold-citation-toggle-at-point))
     ;; If on an unfolded citation, fold it
     ((save-excursion
        (or (looking-at markdown-fold--citation-regexp)
            (and (re-search-backward "\\[" (line-beginning-position) t)
                 (looking-at markdown-fold--citation-regexp)
                 (<= (match-beginning 0) (point))
                 (>= (match-end 0) (point)))))
      (markdown-fold-citation-at-point)
      (message "Citation folded"))
     ;; If region active, fold region
     ((use-region-p)
      (markdown-fold-citations-region (region-beginning) (region-end)))
     ;; Otherwise fold buffer
     (t
      (markdown-fold-citations-buffer))))

  (defun markdown-fold--post-command-hook ()
    "Hook function for auto-revealing folded citations.

This function runs after each command and implements the
auto-reveal behavior controlled by `markdown-fold-auto-reveal'.
When point moves into a folded citation, the citation is
temporarily revealed. When point moves out, it folds again.

The function tracks revealed overlays in `markdown-fold--open-spots'
to properly manage the fold state across multiple windows.

This design is adapted from AUCTeX's `TeX-fold-post-command'."
    (when markdown-fold-auto-reveal
      (condition-case err
          (let* ((current-window (selected-window))
                 ;; Partition spots into current window vs others
                 (partition (seq-group-by
                             (lambda (spot)
                               (eq (car spot) current-window))
                             markdown-fold--open-spots))
                 (current-spots (cdr (assq t partition)))
                 (other-spots (cdr (assq nil partition)))
                 (old-overlays (mapcar #'cdr current-spots))
                 (new-open-spots other-spots))
            ;; Check for overlays at point that should be revealed
            (dolist (ov (overlays-at (point)))
              (when (eq (overlay-get ov 'category) 'markdown-fold-citation)
                ;; Reveal this overlay
                (markdown-fold--show-overlay ov)
                (push (cons current-window ov) new-open-spots)
                (setq old-overlays (delq ov old-overlays))))
            ;; Re-hide overlays that point has left
            (dolist (ov old-overlays)
              (when (and (overlay-buffer ov)
                         (not (and (>= (point) (overlay-start ov))
                                   (<= (point) (overlay-end ov)))))
                (markdown-fold--hide-overlay ov)))
            (setq markdown-fold--open-spots new-open-spots))
        (error (message "markdown-fold: %s" err)))))

  ;; Install the post-command hook for auto-reveal in markdown buffers
  (add-hook 'markdown-mode-hook
            (lambda ()
              (add-hook 'post-command-hook
                        #'markdown-fold--post-command-hook nil t)))

  ;; Also enable in gfm-mode (GitHub Flavored Markdown)
  (add-hook 'gfm-mode-hook
            (lambda ()
              (add-hook 'post-command-hook
                        #'markdown-fold--post-command-hook nil t))))

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
  (if (or (null line-spacing) (< line-spacing 0.2))
      (setq line-spacing 0.5)
    (setq line-spacing 0.1))
  (redraw-frame (selected-frame)))


;; end my-setup-writing
(provide 'my-setup-writing)

