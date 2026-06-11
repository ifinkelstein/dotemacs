;; my-setup-notes.el  -*- lexical-binding: t -*-

;; This library provides settings for using org-roam

(message "Setting up notes...")

;;* Org Roam
(use-package org-roam
  :defer 10
  :custom
  ;; Configure dirs
  (org-roam-directory "~/Work/org-roam/")
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
                              ":PROPERTIES:\n:ID:       %<%Y%m%dT%H%M%S.000000>\n:END:\n#+TITLE: %<%Y-%m-%d>\n \n#+CREATED: %U\n#+LAST_MODIFIED: %U
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

  ;; Update #+LAST_MODIFIED timestamp on save for org-roam notes
  (defun my-org-roam-update-timestamp ()
    "Update #+LAST_MODIFIED timestamp when saving an org-roam file."
    (when (and (derived-mode-p 'org-mode)
               (buffer-file-name)
               (string-prefix-p (expand-file-name org-roam-directory)
                                (expand-file-name (buffer-file-name))))
      (setq-local time-stamp-start "#\\+LAST_MODIFIED:[ \t]*"
                  time-stamp-end "$"
                  time-stamp-format "[%Y-%m-%d %a %H:%M]")
      (time-stamp)))

  (add-hook 'before-save-hook #'my-org-roam-update-timestamp)

  (org-roam-db-autosync-mode))

;;** Consult Notes
;; Adapted from https://github.com/minad/consult/wiki/hrm-notes
(use-package consult-notes
  :defer 10
  :after org-roam
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config
  (consult-notes-org-roam-mode 1)
  (consult-notes-org-headings-mode)

  ;; make embark-export use dired for notes
  (with-eval-after-load 'embark
    (setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired)))

;;* Org Contacts
(defvar my-org-contacts-file (concat org-directory "contacts/contacts.org")
  "Path to the org-contacts file.")

(use-package org-contacts
  :after org org-roam
  :custom
  (org-contacts-files (list my-org-contacts-file))
  ;; NOTE: org-contacts can aggressively override mu4e's native contact
  ;; completion in compose buffers. Set to nil to use mu4e's own completion
  ;; (which draws from your mail history) instead.
  ;; (org-contacts-enable-completion nil)
  :config
  (add-to-list 'org-capture-templates
               '("c" "Contact" entry (file (lambda () (car org-contacts-files)))
                 "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE:
:ADDRESS:
:BIRTHDAY:
:NOTE:
:END:")))

;; Provide Notes
(provide 'my-setup-notes)
