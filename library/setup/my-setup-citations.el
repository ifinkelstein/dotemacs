;;; my-setup-citations.el --- summary -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein

;;; Commentary:

;; Packages for academic citation. For discussion of what these various parts
;; mean see https://kristofferbalintona.me/posts/202206141852/ and
;; https://blog.tecosaur.com/tmio/2021-07-31-citations.html
;; https://www.reddit.com/r/emacs/comments/134s5be/integrate_zotero_pdf_notes_with_org_roam/

;;; Code

;;;; Citation Variables
(defcustom my-bibliography nil "User bibliography for citation."
  :group 'my-emacs
  :tag "my-emacs User Bibliography")
(defcustom my-bib-notes nil "User citation notes directory."
  :group 'my-emacs
  :tag "my-emacs Citation Notes Directory")
(defcustom my-citar-note nil "Template for use with citar notes."
  :group 'my-emacs
  :tag "my-Emacs Citar Notes Template")

(setopt my-bibliography "/Users/ilya/Work/01-09 meta/citations/lab.bib")
(setopt my-bib-notes "/Users/ilya/Work/01-09 meta/citations")

;;;; Org-Cite
;; Eventually this should be a full replacement for org-ref
(use-package oc
  :ensure nil
  :after org
  :config
  (setq org-cite-global-bibliography `(,my-bibliography))
  (setq org-cite-export-processors
        '((beamer csl)
          (latex csl)
          (t csl))))

;; Use csl
(use-package oc-csl
  :ensure nil
  :after oc
  :init
  ;; make sure to download csl
  ;; https://citationstyles.org
  ;; https://github.com/citation-style-language
  ;; repos for styles & locales
  (setq org-cite-csl-styles-dir "/Users/ilya/Work/01-09 meta/citations/styles")
  ;; (setq org-cite-csl-locales-dir "~/.local/share/csl/locales")
  )

;;;; Citeproc
(use-package citeproc
  :after (oc oc-csl))

;;;; Citar
(use-package citar
  :commands (citar-open-beref
             citar-open-notes
             citar-insert-citation)
  :bind (:map citar-map
         ("b" .  #'citar-open-beref))
  :custom
  ;; Use with org citation
  (org-cite-global-bibliography `(,my-bibliography))
  (citar-bibliography `(,my-bibliography))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :config
  ;; use embark with at-point
  (setq citar-at-point-function 'embark-act)
  ;; add beref entry for bookends
  (setq citar-additional-fields '("doi" "url"))
  (setq citar-templates
        `((main . " ${=key= id:15} ${title:48}")
          (note . "Notes on ${author editor:%etal}, ${title}")
          (suffix . "${author editor:30}  ${=type=:12}  ${=beref=:12} ${tags keywords:*}")
          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
          (note . ,my-citar-note)))
  (when (display-graphic-p)
    (setq citar-symbols
          `((file ,(all-the-icons-octicon "file-pdf"      :face 'error) . " ")
            (note ,(all-the-icons-octicon "file-text"     :face 'warning) . " ")
            (link ,(all-the-icons-octicon "link-external" :face 'org-link) . " "))))
  ;; edit notes
  (setq citar-notes-paths `(,my-bib-notes)))

(use-package citar-embark
  :config
  (citar-embark-mode t))


;;;; org-roam-bibtex
;; https://github.com/org-roam/org-roam-bibtex/issues/178
;; zotero integration
(use-package org-roam-bibtex
  :after org-roam)


(provide 'my-setup-citations)
;;; my-setup-citations.el ends here
