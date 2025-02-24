;; my-setup-notes.el  -*- lexical-binding: t -*-

;; This library provides settings for using org-roam

(message "Setting up notes...")

;; * Org Roam
(use-package org-roam
  :defer 10
  :after org
  ;; other meow-specific bindings are in keybindings.el
  ;; :bind (("C-c n l" . org-roam-buffer-toggle)
  ;;        ("C-c n f" . org-roam-node-find)
  ;;        ("C-c n i" . org-roam-node-insert)
  ;;        ("C-c n c" . org-roam-capture)
  ;;        ("C-c n o" . org-id-get-create)
  ;;        ("C-c n a" . org-roam-alias-add)

  ;;        ;; Dailies
  ;;        ("C-c n j" . org-roam-dailies-capture-today)
  ;;        ("C-c n y" . org-roam-dailies-capture-yesterday)
  ;;        ("C-c n t" . org-roam-dailies-goto-today)
  ;;        ("C-c n g" . org-roam-dailies-capture-tomorrow)

  ;;        :map org-mode-map
  ;;        ("C-M-i" . completion-at-point))
  :custom
  ;; Configure dirs
  (org-roam-directory "/Users/ilya/Work/org-roam/")
  (org-roam-db-location (concat org-roam-directory "org-roam.db"))
  (org-roam-completion-everywhere t)
  :config
  ;; Org Roam Templating
  ;; see https://org-roam.readthedocs.io/en/latest/templating/
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n \n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n")
           :immediate-finish t ;; this bypasses the org-roam capture C-c C-c interface to insert notes rapidly during composition
           :unnarrowed t)))
  (setq org-roam-dailies-directory "daily/")

  ;; add an entry with a clock report to each daily file so I can see where my time goes
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+TITLE: %<%Y-%m-%d>\n \n#+CREATED: %U\n#+LAST_MODIFIED: %U
#+OPTIONS: toc:nil author:nil date:nil title:nil
#+LATEX_CLASS: article
\n\n

* Agenda

* Summary Clock Table :noexport:
#+BEGIN: clocktable :maxlevel 2 :emphasize nil :scope agenda :block %<%Y-%m-%d> :fileskip0 t\n\n#+END:\n" ))))

  ;; exclude headings with org-attach tags from indexing by org-roam
  ;; https://www.orgroam.com/manual.html#What-to-cache
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags)))))

  ;; update time stamp after editing org-roam note files
  ;; https://org-roam.discourse.group/t/update-a-field-last-modified-at-save/321/19
  (add-hook 'org-roam-mode-hook (lambda ()
                                  (setq-local time-stamp-active t
                                              time-stamp-start "#\\+EDITED:[ \t]*"
                                              time-stamp-end "$"
                                              time-stamp-format "\[%04y-%02m-%02d %3a %02H:%02M\]")
                                  (add-hook 'before-save-hook 'time-stamp nil 'local)))

  (org-roam-setup))

;; ** Consult Notes
;; Adapted from https://github.com/minad/consult/wiki/hrm-notes
(use-package consult-notes
  :after org-roam
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config
  (consult-notes-org-roam-mode 1)
  (consult-notes-org-headings-mode)
  (setq consult-notes-file-dir-sources '(("Name"  ?k  "~/Work/org-roam/"))) ;; Set notes dir(s), see below

  (defun consult-notes-open-dired (cand)
    "Open notes directory dired with point on file CAND."
    (interactive "fNote: ")
    ;; dired-jump is in dired-x.el but is moved to dired in Emacs 28
    (dired-jump nil cand))

  (defun consult-notes-marked (cand)
    "Open a notes file CAND in Marked 2.
Marked 2 is a mac app that renders markdown."
    (interactive "fNote: ")
    (call-process-shell-command (format "open -a \"Marked 2\" \"%s\"" (expand-file-name cand))))

  (defun consult-notes-grep (cand)
    "Run grep in directory of notes file CAND."
    (interactive "fNote: ")
    (consult-grep (file-name-directory cand)))

  ;; (defvar-keymap consult-notes-map
  ;;   :doc "Keymap for Embark notes actions."
  ;;   :parent embark-file-map
  ;;   "d" #'consult-notes-dired
  ;;   "g" #'consult-notes-grep
  ;;   "m" #'consult-notes-marked)

  ;; (add-to-list 'embark-keymap-alist `(,consult-notes-category . consult-notes-map))

  ;; make embark-export use dired for notes
  (setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired))

;; Provide Notes
(provide 'my-setup-notes)
