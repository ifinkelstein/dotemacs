;;; keybindings.el --- summary -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein


;;; Commentary:

;; Keybindings for my Emacs. This has its disadvantages (e.g. separating
;; functions or packages from keybindings) but it also makes it the place to go
;; to deal with all keybindings. I use `bind-key' for setting bindings, which
;; comes with `use-package'. All Emacs keybindings are under the prefix
;; specified by `my-prefix'.

;;; Code:
(message "Setting up keybindings...")

;;;; Global keybindings
;; buffer and tab bar navigation
(global-set-key (kbd "s-[") 'my-previous-user-buffer)
(global-set-key (kbd "s-]") 'my-next-user-buffer)
(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)

(keymap-global-unset "s-k")
(keymap-global-set "s-K" 'my-kill-this-buffer)


;;;; Bind Key
;; Note that bind-key comes with use-package
(use-package bind-key
  :ensure nil
  :config
  (setq bind-key-describe-special-forms nil))


;;;; Meow for Modal Editing
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-motion-overwrite-define-key
   '("s-[" . my-previous-user-buffer)
   '("s-]" . my-next-user-buffer)
   '("s-{" . tab-bar-switch-to-prev-tab)
   '("s-}" . tab-bar-switch-to-next-tab)
   '("n" . meow-next)
   '("e" . meow-prev))

  ;; Set leader This isn't the sanctioned way to do this, but it seems to be the
  ;; only way to get `leader' to properly display keys from
  ;; `meow-leader-define-key' and my personal keymap in `my+leader-map' I think
  ;; the preferred way is via (setq meow-keypad-leader-dispatch "...") but
  ;; that doesn't work as i want it to
  (add-to-list 'meow-keymap-alist (cons 'leader my+leader-map))

  ;; Set INSERT state for a few major modes. The meow documentation appears to be outdated;
  ;; the variable meow-mode-state-list can also start in insert state
  ;;https://github.com/meow-edit/meow/issues/260
  (add-to-list 'meow-mode-state-list '(org-msg-edit-mode . insert))
  (add-to-list 'meow-mode-state-list '(org-capture-mode . insert))
  (add-to-list 'meow-mode-state-list '(mu4e-compose-mode . insert))
  (add-to-list 'meow-mode-state-list '(mu4e-main-mode . insert))
  (add-to-list 'meow-mode-state-list '(mu4e-view-mode . motion))
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))

  ;; Keypad prefixes hijack personal keybinds so disable them
  ;; See https://github.com/meow-edit/meow/issues/206
  (setq meow-keypad-meta-prefix nil
        meow-keypad-ctrl-meta-prefix nil
        meow-keypad-literal-prefix nil
        meow-keypad-start-keys nil)

  (meow-leader-define-key
   ;; bindings for high frequency commands
   '("?" . consult-apropos)
   ;; high frequency keybindings
   '(")" . "C-)")
   '("}" . "C-}")
   '("." . "M-.")
   '("[" . cpm/previous-user-buffer)
   '("]" . cpm/next-user-buffer)
   '("{" . tab-bar-switch-to-prev-tab)
   '("}" . tab-bar-switch-to-next-tab)
   '("TAB" . cpm/tab-bar-select-tab-dwim)
   '("SPC" . execute-extended-command)
   '(";" . comment-line)
   '(":" . my+comment-wrap-keys)
   '("/" . meow-keypad-describe-key)
   '("=" . hl-line-mode)
   '("'" . embark-act)
   '("\"" . embark-dwim)
   '("a" . my-open-agenda-in-workspace)
   '("A" . consult-org-agenda)
   '("b" . my+buffer-keys)
   ;; '("c" . my+comment-wrap-keys)
   '("c" . org-capture)
   '("C" . my+config-keys)
   ;; '("d" . dired-jump)
   '("d" . dired-recent-open)
   '("D" . dired-jump-other-window)
   '("e" . my+eval-keys)
   '("E" . restart-emacs-start-new-emacs)
   '("f" . my+file-keys)
   '("F" . my+flycheck-keys)
   '("i" . cpm/find-files-setup-config-directory)
   '("I" . cpm/search-setup-config-files)
   ;; '("J" . crux-top-join-line)
   '("j" . avy-goto-word-1)
   '("J" . avy-goto-char-timer)
   '("k" . consult-yank-from-kill-ring)
   '("l" . vertico-repeat)
   '("L" . consult-locate)
   '("m" . my-messaging-keys)
   '("M" . my+mail-keys)
   '("n" . my+notes-keys)
   '("N" . consult-notes-search-all)
   `("p" . ,project-prefix-map)
   '("q" . my+quit-keys)
   '("r" . consult-register)
   '("R" . consult-recent-file)
   '("s" . my+search-keys)
   '("S" . cpm/search-in-input-dir)
   '("t" . my-org-capture-todo)
   '("T" . my+toggle-keys)
   '("u" . my+user-keys)
   '("v" . my+vc-keys)
   '("V" . multi-vterm-dedicated-toggle)
   '("w" . my+window-keys)
   '("W" . my+workspace-keys)
   '("y" . yas-minor-mode-map)
   '("z" . my-org-capture-todo))


  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("(" . backward-sentence)
   '(")" . forward-sentence)
   '("{" . backward-paragraph)
   '("}" . forward-paragraph)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-mark-word)
   '("H" . meow-mark-symbol)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("m" . meow-left)
   '("M" . meow-left-expand)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("&" . meow-query-replace-regexp)
   '("%" . meow-query-replace)
   '("=" . meow-grab)
   '("s-[" . my-previous-user-buffer)
   '("s-]" . my-next-user-buffer)
   '("s-{" . tab-bar-switch-to-prev-tab)
   '("s-}" . tab-bar-switch-to-next-tab)
   '("<escape>" . meow-cancel-selection)))

;;;; Personal Keybindings Prefix
(defcustom my-prefix "C-c C-SPC"
  "Prefix for all personal keybinds."
  :type 'string
  :group 'my-emacs)

;;;; Personal Leader Key

(defcustom my+leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> key, for use with modal keybindings."
  :type 'string
  :group 'my-emacs)

;; Use my-prefix as leader namespace
(bind-keys :prefix-map my+leader-map
           :prefix my-prefix)


;;;; Meow
;; Note load Meow before loading personal keybindings, otherwise some might get clobbered
(use-package meow
  :config
  ;; set colors in bespoke theme
  (setq meow-use-dynamic-face-color nil)
  (setq meow-use-cursor-position-hack t)
  ;; Make sure delete char means delete char
  ;; see https://github.com/meow-edit/meow/issues/112
  (setq meow--kbd-delete-char "<deletechar>")
  (setq meow-use-clipboard t)
  (setq meow-goto-line-function 'consult-goto-line)
  (meow-thing-register 'angle '(regexp "<" ">") '(regexp "<" ">"))
  (add-to-list 'meow-char-thing-table '(?a . angle))
  (add-to-list 'meow-char-thing-table (cons ?{ 'paragraph))
  (add-to-list 'meow-char-thing-table (cons ?} 'paragraph))
  (meow-setup)
  (meow-global-mode 1))


;;;; Personal Keybindings by Group
;;;;; Buffer Keys
(bind-keys :prefix-map my+buffer-keys
           :prefix (concat my-prefix " b")
           ("a" . ibuffer)
           ("b" . consult-buffer)
           ("c" . my-copy-whole-buffer-to-clipboard )
           ("d" . kill-buffer-and-window             )
           ("E" . erase-buffer                       )
           ("f" . reveal-in-osx-finder               )
           ("i" . consult-imenu                      )
           ("j" . my-jump-in-buffer                 )
           ("k" . my-kill-this-buffer               )
           ("K" . crux-kill-other-buffers            )
           ("m" . consult-global-mark                )
           ("n" . my-create-new-buffer              )
           ("N" . my-new-buffer-new-frame           )
           ("o" .  clone-indirect-buffer-other-window)
           ("p" . consult-project-buffer             )
           ("r" . revert-buffer                      )
           ("R" . crux-rename-buffer-and-file        )
           ("s" . consult-buffer-other-window        )
           ("S" . hydra-spelling/body                )
           ("t" . tab-bar-new-tab                    )
           ("[" . my-previous-user-buffer           )
           ("]" . my-next-user-buffer               )
           ("{" . tab-bar-switch-to-prev-tab         )
           ("}" . tab-bar-switch-to-next-tab         )
           ("<backtab>" . crux-switch-to-previous-buffer))


;;;;; Comment Keybindings
(bind-keys :prefix-map my+comment-wrap-keys
           :prefix (concat my-prefix " c")
           ("c" . comment-dwim)
           ("d" . crux-duplicate-and-comment-current-line-or-region)
           ("l" . comment-line)
           ("o" . org-block-wrap)
           ("y" . my-yaml-wrap))


;;;;; Config Keybindings
;; FIXME: fix goto functions to make them more generic
;; FIXME: fix kill-and-archive so that it creates an archive file
(bind-keys :prefix-map my+config-keys
           :prefix (concat my-prefix " C")
           ("a" . my-setup-kill-and-archive-region     )
           ("c" . my-goto-custom.el                    )
           ("d" . my-goto-emacs-dir                    )
           ("e" . my-goto-early-init.el                )
           ("f" . my-find-emacs-file                   )
           ("k" . my-byte-compile-dotemacs             )
           ("K" . my-delete-byte-compiled-files        )
           ("l" . my-load-config                       )
           ("i" . my-goto-init.el                      )
           ("I" . my-load-init-file                    )
           ("o" . my-goto-org-files                    )
           ("s" . my-search-emacs-files               ))

;;;;; Eval Keybindings
(bind-keys :prefix-map my+eval-keys
           :prefix (concat my-prefix " e")
           ("b"  . eval-buffer )
           ("c"  . my-eval-current-form)
           ("e"  . eval-last-sexp)
           ("f"  . eval-defun)
           ("r"  . eval-last-region))

;;;;; File Keybindings
(bind-keys :prefix-map my+file-keys
           :prefix (concat my-prefix " f")
           ("b" . consult-bookmark                 )
           ("f" . find-file                        )
           ("l" . consult-locate                   )
           ("o" . crux-open-with                   )
           ("s" . save-buffer                      )
           ("r" . consult-recent-file              )
           ("y" . my-show-and-copy-buffer-filename))


;;;;; Mail Keybindings
(bind-keys :prefix-map my+mail-keys
           :prefix (concat my-prefix " m")
           ("a" . mu4e-view-save-attachment-multi)
           ("c" . mu4e-compose-new           )
           ("e" . my-email-to-kill-ring    )
           ("i" . my-go-to-mail-inbox       )
           ("k" . mu4e-kill-update-mail      )
           ("m" . my-open-email-in-workspace)
           ("s" . mu4e-update-mail-and-index )
           ("S" . my-swiftbar-email-update  )
           ("u" . my-go-to-mail-unread      ))

;;;;; Notes
;; General notes
(bind-keys :prefix-map my+notes-keys
           :prefix (concat my-prefix " n")
           ("a"  .  org-roam-alias-add      )
           ("A"  .  consult-notes           )
           ("b"  .  org-roam-buffer-toggle  )
           ("C"  .  citar-open-notes        )
           ("c"  .  org-roam-capture        )
           ("i"  .  org-roam-node-insert    )
           ;;TODO: Define function below
           ("F"  .  bms/org-roam-rg-search  )
           ("f"  .  org-roam-node-find      )
           ("g"  .  org-roam-dailies-capture-tomorrow)
           ("G"  .  org-roam-graph          )
           ("i"  .  org-roam-node-insert    )
           ("j"  .  org-roam-dailies-capture-today)
           ("n"  .  consult-notes           )
           ("N"  .  org-roam--new-file-named)
           ("o"  .  org-id-get-create       )
           ("r"  .  citar-open-notes        )
           ("R"  .  consult-notes-org-roam-cited)
           ("s"  .  consult-notes-search-in-all-notes)
           ("t"  .  org-roam-dailies-goto-today)
           ("y"  .  org-roam-dailies-capture-yesterday))

;; Biblio notes
(bind-keys :prefix-map my+bib-keys
           :prefix (concat my-prefix " n b")
           ("a"  .  citar-denote-add-citekey)
           ("c"  .  citar-create-note)
           ("o"  .  citar-open-notes)
           ("O"  .  citar-denote-dwim))

;;;;; Org-mode keybindings
(bind-keys :prefix-map ijf+org-keys
           :prefix (concat my-prefix " o")
           ("a" . org-attach-attach)
           ("A" . my-org-archive-done-tasks)
           ("c" . org-copy-subtree)
           ("C" . org-clock-cancel)
           ("e" . org-set-effort)
           ("f" . (lambda () (interactive)
                    (grab-mac-link-dwim 'firefox))) ;; re-define to avoid an anon function
           ("g" . org-clock-goto)
           ("G" . my-goto-gtd.org)
           ("j" . org-goto-interactive)
           ("h" . org-timeblock)
           ("i" . org-mru-clock-in)
           ("I" . my-goto-inbox.org)
           ("k" . org-cut-subtree)
           ("l" . org-store-link)
           ("m" . org-menu)
           ("n" . my-narrow-or-widen-dwim)
           ("N" . org-mru-clock-show-narrowed)
           ("o" . org-clock-out)
           ("p" . org-pomodoro)
           ("q" . org-set-tags-command)
           ("R" . org-mru-clock-select-recent-task)
           ("s" . my-consult-org-ql-agenda-jump) ;; defined in org-ql, needs consult
           ("S" . my-org-reschedule) ;;TODO: fix this bit so we get an intelligent search
           ;; ("S" . org-ql-search)
           ("w" . org-refile)
           ("z" . org-cite-insert))


;;;;; Quit Keybindings
(bind-keys :prefix-map my+quit-keys
           :prefix (concat my-prefix " q")
           ("d" . my-kill-emacs-capture-daemon)
           ("q" . save-buffers-kill-emacs      )
           ("Q" . my-kill-all-emacsen         )
           ("r" . restart-emacs                ))

;;;;; Spelling Keybindings
(bind-keys :prefix-map my+spelling-keys
           :prefix (concat my-prefix " S")
           ("b" . consult-flyspell          )
           ("h" . hydra-spelling/body       )
           ("n" . flyspell-correct-next     )
           ("p" . flyspell-correct-previous ))

;;;;; Search Keybindings
(bind-keys :prefix-map my+search-keys
           :prefix (concat my-prefix " s")
           ("a" . consult-org-agenda           )
           ("b" . consult-multi-occur          )
           ;; search current buffer's directory
           ("d" . consult-ripgrep              )
           ;; search with directory input
           ("D" . my-search-in-input-dir      )
           ("f" . consult-line                 )
           ("F" . affe-find                        )
           ("H" . (lambda () (interactive)
                    (affe-find "~")) )
           ("h" . consult-org-heading          )
           ("j" . my-forward-or-backward-sexp )
           ("k" . consult-yank-pop             )
           ("l" . selectrum-repeat             )
           ("n" . consult-notes-search-all     )
           ("r" . vr/query-replace             )
           ("R" . substitute-target-in-buffer  )
           ("s" . consult-line                 )
           ;; search for next spelling error
           ("S" . my-flyspell-ispell-goto-next-error)
           ("t" . my-hydra-todo/body          )
           ("W" . (lambda () (interactive)
                    (affe-find "~/Work")) )
           ("." . consult-line-symbol-at-point ))

;;;;; Toggle Keybindings
(bind-keys :prefix-map my+toggle-keys
           :prefix (concat my-prefix " T")
           ("b" . buffer-line-mode            )
           ("g" . git-gutter-mode             )
           ("h" . hl-line-mode                )
           ("H" . hidden-mode-line-mode       )
           ("e" . toggle-indicate-empty-lines )
           ("E" . eldoc-mode                  )
           ("F" . flymake-mode                )
           ("l" . my-toggle-line-spacing      )
           ("m" . my-toggle-display-markup    )
           ("n" . display-line-numbers-mode   )
           ("N" . org-numbers-overlay-mode    )
           ("o" . imenu-list-smart-toggle     )
           ("O" . olivetti-mode               )
           ("p" . puni-global-mode            )
           ("P" . show-paren-mode             )
           ("r" . rainbow-identifiers-mode    )
           ("s" . flyspell-mode               )
           ("S" . flyspell-correct-wrapper    )
           ("t" . toggle-dark-light-theme     )
           ("T" . my-load-theme              )
           ("w" . writeroom-mode              )
           ("z" . zone                        ))

;;;;; User Keybindings
;; This keymap is for user-specific keybindings.
(bind-keys :prefix-map my+user-keys
           :prefix (concat my-prefix " u"))

;;;;; Version Control (Git) Keybindings
(bind-keys :prefix-map  my+vc-keys
           :prefix (concat my-prefix " g")
           ("b" .  magit-blame                 )
           ("c" .  magit-commit                )
           ("d" .  magit-diff                  )
           ("h" .  hydra-git-gutter/body       )
           ("l" .  magit-log                   )
           ;; show history of selected region
           ("L" .  magit-log-buffer-file       )
           ("n" .  git-gutter:next-hunk        )
           ("p" .  git-gutter:previous-hunk    )
           ;; quick commit file
           ("q" .  vc-next-action              )
           ("r" .  magit-reflog                )
           ("s" .  magit-status                ))

;;;;; Window Keybindings
(bind-keys :prefix-map my+window-keys
           :prefix (concat my-prefix " w")
           ("a" .  ace-window                      )
           ("b" .  balance-windows )
           ("f" .  my-toggle-window-split         )
           ("c" .  delete-window                   )
           ("d" .  delete-window                   )
           ("h" .  my-split-window-below-and-focus)
           ("H" .  split-window-below              )
           ("m" .  delete-other-windows            )
           ("o" .  my-other-window                )
           ("r" .  my-rotate-windows              )
           ("R" .  my-rotate-windows-backward     )
           ("t" .  tear-off-window                 )
           ("u" .  winner-undo                     )
           ("U" .  winner-redo                     )
           ("v" .  my-split-window-right-and-focus)
           ("V" .  split-window-right              )
           ("w" .  ace-window                      )
           ("x" .  my-window-exchange-buffer      )
           ("-" .  split-window-below              )
           ("_" .  my-split-window-below-and-focus))

;;;;; Workspace Keybindings
(bind-keys :prefix-map my+workspace-keys
           :prefix (concat my-prefix " W")
           ("b"  .  tabspaces-switch-to-buffer)
           ("c"  .  tabspaces-clear-buffers)
           ("d"  .  tabspaces-close-workspace)
           ("k"  .  tabspaces-kill-buffers-close-workspace)
           ("o"  .  tabspaces-open-or-create-project-and-workspace)
           ("p"  .  tabspaces-project-switch-project-open-file)
           ("r"  .  tabspaces-remove-current-buffer)
           ("R"  .  tabspaces-remove-selected-buffer)
           ("s"  .  tabspaces-switch-or-create-workspace))

;;;; Which Key
(use-package which-key
  :defer 1
  :diminish ""
  :config
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; Set the time delay (in seconds) for the which-key popup to appear.
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay .75)
  (setq which-key-idle-secondary-delay 0.05)
  ;; use widow
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-max-height 0.5)
  (setq which-key-allow-imprecise-window-fit nil)
  (setq which-key-side-window-location 'top)
  ;; use minibuffer
  ;; (which-key-setup-minibuffer)
  ;; separator
  (setq which-key-separator " → ")
  (which-key-mode))

;;;; Hydras

;;;; Ediff Hydra
;; From the hydra wiki https://github.com/abo-abo/hydra/wiki/Emacs#ediff

(with-eval-after-load 'ediff
  (with-eval-after-load 'hydra
    (defhydra hydra-ediff (:color blue :hint nil)
      "
      ^Buffers           Files           VC                     Ediff regions
      ----------------------------------------------------------------------
      _b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
      _B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
      _c_urrent file
      "
      ("b" ediff-buffers)
      ("B" ediff-buffers3)
      ("=" ediff-files)
      ("f" ediff-files)
      ("F" ediff-files3)
      ("c" ediff-current-file)
      ("r" ediff-revision)
      ("l" ediff-regions-linewise)
      ("w" ediff-regions-wordwise))))
;; esc quits

;;;;; Transpose hydra
;; From the hydra wiki https://github.com/abo-abo/hydra/wiki/Emacs#transpose

(with-eval-after-load 'hydra
  (bind-key (concat my-prefix " .")
            (defhydra hydra-transpose (:color red)
              "Transpose"
              ("c" transpose-chars "characters")
              ("w" transpose-words "words")
              ("o" org-transpose-words "Org mode words")
              ("l" transpose-lines "lines")
              ("s" transpose-sentences "sentences")
              ("e" org-transpose-element "Org mode elements")
              ("p" transpose-paragraphs "paragraphs")
              ("t" org-table-transpose-table-at-point "Org mode table")
              ("q" nil "cancel" :color blue))))

;;;;; Hydra Rectangle
(with-eval-after-load 'hydra
  (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                             :color pink
                             :hint nil
                             :post (deactivate-mark))
    "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
    ("k" rectangle-previous-line)
    ("j" rectangle-next-line)
    ("h" rectangle-backward-char)
    ("l" rectangle-forward-char)
    ("d" kill-rectangle)                    ;; C-x r k
    ("y" yank-rectangle)                    ;; C-x r y
    ("w" copy-rectangle-as-kill)            ;; C-x r M-w
    ("o" open-rectangle)                    ;; C-x r o
    ("t" string-rectangle)                  ;; C-x r t
    ("c" clear-rectangle)                   ;; C-x r c
    ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
    ("N" rectangle-number-lines)            ;; C-x r N
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)))
    ("u" undo nil)
    ("g" nil)))

;; TODO: Add org and markdown keybindings
;;; End keybindings
(provide 'my-setup-keybindings)

;;; keybindings.el ends here
