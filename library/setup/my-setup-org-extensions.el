;;; my-setup-org-extensions.el --- Org-mode settings -*- lexical-binding: t -*-

;; Author: Ilya Finkelstein

;;; Commentary:

;; Basic setup for org-mode

;;; Code:
;; - org roam setup is in org-roam
;; - org citations setup is in citation
(message "Setting up org-extensions...")
;;; Org Appearance
;;;; Org-Appear (Show Markup/Pretty Entities)
;; show markup at point -- this should be part of org!
(use-package org-appear
  :after org
  :commands (org-appear-mode)
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t)
  :hook (org-mode . org-appear-mode))


;;;; Org Modern (Display properties, bullets, etc)
;; A nicer set of default display options
(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-hide-stars 'leading)
  (org-modern-todo nil)
  (org-modern-tag t)
  ;; Customize this per your font
  (org-modern-label-border .25)
  ;; Note that these stars allow differentiation of levels
  ;; "①" "②" "③" "④" "⑤" "⑥" "⑦"
  (org-modern-star ["⦶" "⦷" "⦹" "⊕" "⍟" "⊛" "⏣" "❂"]))

;;; Org Autolist (Smart Lists)
;; Better list behavior
(use-package org-autolist
  ;; :straight (:type git :host github :repo "calvinwyoung/org-autolist")
  :hook (org-mode . org-autolist-mode))

;;; Org Babel
;; Avoid `org-babel-do-load-languages' since it does an eager require.
(use-package ob-python
  :ensure nil
  :defer t
  :commands (org-babel-execute:python)
  :config
  (progn
    (setq org-babel-python-command "python3"))) ;Default to python 3.x

(use-package ob-ditaa
  :ensure nil
  :defer t
  :config
  (progn
    ;; http://pages.sachachua.com/.emacs.d/Sacha.html
    (setq org-ditaa-jar-path (expand-file-name
                              "ditaa.jar"
                              (concat user-emacs-directory "software/")))))

(use-package ob-plantuml
  :ensure nil
  :defer t
  :config
  (progn
    (setq org-plantuml-jar-path (expand-file-name
                                 "plantuml.jar"
                                 (concat user-emacs-directory "software/")))

    (defun lem-advice-org-babel-execute:plantuml (orig-fun &rest args)
      "Force `shell-file-name' to be bash as the \">\" operator is used for redirection.

If this forcing is not done, and if `shell-file-name' is tcsh,
\">\" does not work.  When trying to overwrite files, we get a
\"File exists\" error, and \">!\" would need to be used instead.

Instead it's simpler to use bash."
      (let ((shell-file-name (executable-find "bash")))
        (apply orig-fun args)))
    (advice-add 'org-babel-execute:plantuml :around #'lem-advice-org-babel-execute:plantuml)))

(use-package ob-shell
  :ensure nil
  :defer t
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))

(use-package ob-lisp
  :ensure nil
  :defer t
  :commands (org-babel-execute:lisp))

(use-package ob-latex
  :ensure nil
  :defer t
  :commands
  (org-babel-execute:latex))

;;; Org Babel Tangle
(use-package ob-tangle
  :ensure nil
  :defer t
  :config
  (progn
    ;; Trailing whitespace management
    ;; Delete trailing whitespace in tangled buffer and save it.
    (add-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
    (add-hook 'org-babel-post-tangle-hook #'save-buffer :append)))

;;; Org-Download
;; Drag and drop images to Emacs org-mode. Courtesy of abo-abo.
;; https://github.com/abo-abo/org-download.
(use-package org-download
  :commands (org-download-delete org-download-clipboard org-download-rename-at-point org-download-yank org-download-screenshot org-download-image)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir (concat org-roam-directory "org-pictures/"))
  (org-download-image-latex-width 500)
  (org-download-screenshot-method "screencapture")
  (org-download-timestamp "%Y-%m-%d_%H-%M-%S_")
  :hook (dired-mode. org-download-enable))

;;; Org Export Extensions
;;;; Ox-Pandoc
;; Export w/pandoc
(use-package ox-pandoc
  :if (executable-find "pandoc")
  :after ox
  :custom
  (org-pandoc-command (executable-find "pandoc"))
  (org-pandoc-options '((standalone .  t)))
  (org-pandoc-options-for-docx '((standalone . nil)))
  ;; (org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  (org-pandoc-format-extensions '(org+smart)))

;;;; Ox-Pandoc Export Menu Options
;; Set pandoc export options
(setq org-pandoc-menu-entry
      '(
        ;;(?0 "to jats." org-pandoc-export-to-jats)
        ;;(?0 "to jats and open." org-pandoc-export-to-jats-and-open)
        ;;(?  "as jats." org-pandoc-export-as-jats)
        ;;(?1 "to epub2 and open." org-pandoc-export-to-epub2-and-open)
        ;;(?! "to epub2." org-pandoc-export-to-epub2)
        ;;(?2 "to tei." org-pandoc-export-to-tei)
        ;;(?2 "to tei and open." org-pandoc-export-to-tei-and-open)
        ;;(?" "as tei." org-pandoc-export-as-tei)
        ;;(?3 "to markdown_mmd." org-pandoc-export-to-markdown_mmd)
        ;;(?3 "to markdown_mmd and open." org-pandoc-export-to-markdown_mmd-and-open)
        ;;(?# "as markdown_mmd." org-pandoc-export-as-markdown_mmd)
        ;;(?4 "to html5." org-pandoc-export-to-html5)
        (?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
        (?$ "as html5." org-pandoc-export-as-html5)
        (?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
        (?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
        ;;(?6 "to markdown_phpextra." org-pandoc-export-to-markdown_phpextra)
        ;;(?6 "to markdown_phpextra and open." org-pandoc-export-to-markdown_phpextra-and-open)
        ;;(?& "as markdown_phpextra." org-pandoc-export-as-markdown_phpextra)
        ;;(?7 "to markdown_strict." org-pandoc-export-to-markdown_strict)
        ;;(?7 "to markdown_strict and open." org-pandoc-export-to-markdown_strict-and-open)
        ;;(?' "as markdown_strict." org-pandoc-export-as-markdown_strict)
        ;; (?8 "to opendocument." org-pandoc-export-to-opendocument)
        ;; (?8 "to opendocument and open." org-pandoc-export-to-opendocument-and-open)
        ;; (?( "as opendocument." org-pandoc-export-as-opendocument)
        ;; (?8 "to opml." org-pandoc-export-to-opml)
        ;; (?9 "to opml and open." org-pandoc-export-to-opml-and-open)
        ;; (?* "as opml." org-pandoc-export-as-opml)
        ;;(?: "to rst." org-pandoc-export-to-rst)
        ;;(?: "to rst and open." org-pandoc-export-to-rst-and-open)
        ;;(?* "as rst." org-pandoc-export-as-rst)
        ;;(?< "to slideous." org-pandoc-export-to-slideous)
        ;; (?< "to slideous and open." org-pandoc-export-to-slideous-and-open)
        ;; (?, "as slideous." org-pandoc-export-as-slideous)
        ;; (?= "to ms-pdf and open." org-pandoc-export-to-ms-pdf-and-open)
        ;; (?- "to ms-pdf." org-pandoc-export-to-ms-pdf)
        ;;(?> "to textile." org-pandoc-export-to-textile)
        ;;(?> "to textile and open." org-pandoc-export-to-textile-and-open)
        ;;(?. "as textile." org-pandoc-export-as-textile)
        ;;(?a "to asciidoc." org-pandoc-export-to-asciidoc)
        ;;(?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
        ;;(?A "as asciidoc." org-pandoc-export-as-asciidoc)
        ;; (?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
        ;; (?B "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
        ;; (?c "to context-pdf and open." org-pandoc-export-to-context-pdf-and-open)
        ;; (?C "to context-pdf." org-pandoc-export-to-context-pdf)
        ;;(?d "to docbook5." org-pandoc-export-to-docbook5)
        ;; (?d "to docbook5 and open." org-pandoc-export-to-docbook5-and-open)
        ;; (?D "as docbook5." org-pandoc-export-as-docbook5)
        ;; (?e "to epub3 and open." org-pandoc-export-to-epub3-and-open)
        ;; (?E "to epub3." org-pandoc-export-to-epub3)
        ;;(?f "to fb2." org-pandoc-export-to-fb2)
        ;;(?f "to fb2 and open." org-pandoc-export-to-fb2-and-open)
        ;;(?F "as fb2." org-pandoc-export-as-fb2)
        ;;(?g "to gfm." org-pandoc-export-to-gfm)
        ;; (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
        ;; (?G "as gfm." org-pandoc-export-as-gfm)
        ;;(?h "to html4." org-pandoc-export-to-html4)
        (?h "to html4 and open." org-pandoc-export-to-html4-and-open)
        (?H "as html4." org-pandoc-export-as-html4)
        ;;(?i "to icml." org-pandoc-export-to-icml)
        ;; (?i "to icml and open." org-pandoc-export-to-icml-and-open)
        ;; (?I "as icml." org-pandoc-export-as-icml)
        ;;(?j "to json." org-pandoc-export-to-json)
        (?j "to json and open." org-pandoc-export-to-json-and-open)
        (?J "as json." org-pandoc-export-as-json)
        ;; (?k "to markdown." org-pandoc-export-to-markdown)
        (?k "to markdown and open." org-pandoc-export-to-markdown-and-open)
        (?K "as markdown." org-pandoc-export-as-markdown)
        (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
        (?L "to latex-pdf." org-pandoc-export-to-latex-pdf)
        ;;(?m "to man." org-pandoc-export-to-man)
        ;; (?m "to man and open." org-pandoc-export-to-man-and-open)
        ;; (?M "as man." org-pandoc-export-as-man)
        ;;(?n "to native." org-pandoc-export-to-native)
        ;; (?n "to native and open." org-pandoc-export-to-native-and-open)
        ;; (?N "as native." org-pandoc-export-as-native)
        (?o "to odt and open." org-pandoc-export-to-odt-and-open)
        (?O "to odt." org-pandoc-export-to-odt)
        (?p "to pptx and open." org-pandoc-export-to-pptx-and-open)
        (?P "to pptx." org-pandoc-export-to-pptx)
        ;;(?q "to commonmark." org-pandoc-export-to-commonmark)
        ;;(?q "to commonmark and open." org-pandoc-export-to-commonmark-and-open)
        ;;(?Q "as commonmark." org-pandoc-export-as-commonmark)
        ;;(?r "to rtf." org-pandoc-export-to-rtf)
        ;; (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
        ;; (?R "as rtf." org-pandoc-export-as-rtf)
        ;;(?s "to s5." org-pandoc-export-to-s5)
        ;;(?s "to s5 and open." org-pandoc-export-to-s5-and-open)
        ;;(?S "as s5." org-pandoc-export-as-s5)
        ;;(?t "to texinfo." org-pandoc-export-to-texinfo)
        ;;(?t "to texinfo and open." org-pandoc-export-to-texinfo-and-open)
        ;;(?T "as texinfo." org-pandoc-export-as-texinfo)
        ;;(?u "to dokuwiki." org-pandoc-export-to-dokuwiki)
        ;; (?u "to dokuwiki and open." org-pandoc-export-to-dokuwiki-and-open)
        ;; (?U "as dokuwiki." org-pandoc-export-as-dokuwiki)
        ;; (?v "to revealjs." org-pandoc-export-to-revealjs)
        ;; (?v "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
        ;; (?V "as revealjs." org-pandoc-export-as-revealjs)
        ;;(?w "to mediawiki." org-pandoc-export-to-mediawiki)
        ;; (?w "to mediawiki and open." org-pandoc-export-to-mediawiki-and-open)
        ;; (?W "as mediawiki." org-pandoc-export-as-mediawiki)
        (?x "to docx and open." org-pandoc-export-to-docx-and-open)
        (?X "to docx." org-pandoc-export-to-docx)
        ;;(?y "to slidy." org-pandoc-export-to-slidy)
        ;; (?y "to slidy and open." org-pandoc-export-to-slidy-and-open)
        ;; (?Y "as slidy." org-pandoc-export-as-slidy)
        ;;(?z "to dzslides." org-pandoc-export-to-dzslides)
        ;; (?z "to dzslides and open." org-pandoc-export-to-dzslides-and-open)
        ;; (?Z "as dzslides." org-pandoc-export-as-dzslides)
        ;;(?{ "to muse." org-pandoc-export-to-muse)
        ;;(?{ "to muse and open." org-pandoc-export-to-muse-and-open)
        ;;(?[ "as muse." org-pandoc-export-as-muse)
        ;;(?} "to zimwiki." org-pandoc-export-to-zimwiki)
        ;;(?} "to zimwiki and open." org-pandoc-export-to-zimwiki-and-open)
        ;;(?] "as zimwiki." org-pandoc-export-as-zimwiki)
        ;;(?~ "to haddock." org-pandoc-export-to-haddock)
        ;;(?~ "to haddock and open." org-pandoc-export-to-haddock-and-open)
        ;;(?^ "as haddock." org-pandoc-export-as-haddock)
        ))



;;; Org Html Conversion
(use-package htmlize
  :commands (htmlize-buffer))

;;; org-mru-clock for better clocking into and out of tasks
;; TODO: in the future, integrate with org-pomodoro
(use-package org-mru-clock
  :after (org embark vertico org-pomodoro)
  ;; :bind* (("C-c C-x i" . org-mru-clock-in)
  ;;         ("C-c C-x C-j" . org-mru-clock-select-recent-task))
  :custom
  (org-mru-clock-how-many 100)
  :config
  (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))

;;; Org Pomodoro

;; Helps with time tracking
(use-package org-pomodoro
  :commands (org-pomodoro ijf-org-clock-time-xbar)
  :after org
  :defer 5 ;; load after ~5 idle seconds.
  :config
  ;; integrate org-pomodoro with the xbar so I can see timer minutes remaining at all times
  ;; ref: https://colekillian.com/posts/org-pomodoro-and-polybar/
  (setq org-pomodoro-audio-player "/usr/bin/afplay")
  (setq org-pomodoro-start-sound "/Users/ilya/.config/custom-sounds/water-drip.mp3")
  (setq org-pomodoro-killed-sound "/Users/ilya/.config/custom-sounds/water-drip.mp3")
  (setq org-pomodoro-finished-sound "/Users/ilya/.config/custom-sounds/water-drip.mp3")
  (setq org-pomodoro-short-break-sound "/Users/ilya/.config/custom-sounds/water-drip.mp3")
  (setq org-pomodoro-long-break-sound "/Users/ilya/.config/custom-sounds/water-drip.mp3")
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (setq org-pomodoro-length 45)
  )

(defun ijf-org-clock-time-xbar ()
  "If org-pomodoro is running, return the pomodoro state and time.
If org-clock is running without a pomodoro timer, return the
minutes on the current clock period. NOTE: I explicitly return
just the clock-in on the current period; not the entire
clock-time If nothing is clocked in, indicate that with a 'No
Clock' message.

Usage: I use this with xbar to display the current clock in the
menu bar on MacOS. So the message is truncated to fit on screen.
Also, server-mode must be enabled. If the server gets confused,
use 'server-force-delete' and 'server-mode' to restart."
  (cond ((org-pomodoro-active-p)
         (cl-case org-pomodoro-state
           (:pomodoro
            (format "🍅%d m: %s" (/ (org-pomodoro-remaining-seconds) 60) (string-limit org-clock-heading 20)))
           (:short-break
            (format "🍅Short: %d m" (/ (org-pomodoro-remaining-seconds) 60)))
           (:long-break
            (format "🍅Long: %d m" (/ (org-pomodoro-remaining-seconds) 60)))
           (:overtime
            (format "🍅Overtime! %d m" (/ (org-pomodoro-remaining-seconds) 60)))))
        ((and (org-clocking-p) (not (org-pomodoro-active-p)))
         (format "⏳%d m: %s" (/ (org-time-convert-to-integer (time-since org-clock-start-time)) 60) (string-limit org-clock-heading 20)))
        (t
         (message "No clock"))
        ))
;;; org-extra-emphasis
;; easier highlighting in org
(use-package org-extra-emphasis
  :vc (:fetcher github :repo QiangF/org-extra-emphasis))

;;; org-ql
;; search org files with a query language org-ql
(use-package org-ql
  :after (org) ;; for org agenda searches
  :demand t)

;;;; org-ql associated functions
(defun my-org-agenda-next ()
  "Show the agenda block, followed by NEXT tasks in every project.
The NEXT tasks are sorted by priority."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (or (ts-active :on today)
              (deadline auto)
              (scheduled :to today))
          (not (done)))
    :title "My Agenda View"
    ;; The `org-super-agenda-groups' setting is used automatically when set, or it
    ;; may be overriden by specifying it here:
    :super-groups '((:name "bla"
                     :tag "writing")
                    (:todo ("NEXT" "TODO")
                     :order 7)
                    (:name "Personal"
                     :habit t
                     :tag "personal"
                     :order 3)
                    (:todo "WAITING"
                     :order 6)
                    (:priority "A" :order 1)
                    (:priority "B" :order 2)

                    (:priority "C" :order 2))))

;; inspiration for the functions below from here:
;; https://sachachua.com/blog/2024/01/using-consult-and-org-ql-to-search-my-org-mode-agenda-files-and-sort-the-results-to-prioritize-heading-matches/
(defun my-consult-org-ql-agenda-jump ()
  "Search agenda files with preview."
  (interactive)
  (require 'org-ql-search) ;; this will hopefully avoid loading errors

  (let* ((marker (consult--read
                  (consult--dynamic-collection
                   #'my-consult-org-ql-agenda-match)
                  :state (consult--jump-state)
                  :category 'consult-org-heading
                  :prompt "Heading: "
                  :sort nil
                  :lookup #'consult--lookup-candidate))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    ;; based on org-agenda-switch-to
    (unless buffer (user-error "Trying to switch to non-existent buffer"))
    (pop-to-buffer-same-window buffer)
    (goto-char pos)
    (when (derived-mode-p 'org-mode)
      (org-fold-show-context 'agenda)
      (run-hooks 'org-agenda-after-show-hook))))

(defun my-consult-org-ql-agenda-format (o)
  (propertize
   (org-ql-view--format-element o)
   'consult--candidate (org-element-property :org-hd-marker o)))

(defun my-consult-org-ql-agenda-match (string)
  "Return candidates that match STRING.
Sort heading matches first, followed by other matches.
Within those groups, sort by date and priority."
  (let* ((query (org-ql--query-string-to-sexp string))
         (sort '(date reverse priority))
         (heading-query (-tree-map (lambda (x) (if (eq x 'rifle) 'heading x)) query))
         (matched-heading
          (mapcar #'my-consult-org-ql-agenda-format
                  (org-ql-select 'org-agenda-files heading-query
                    :action 'element-with-markers
                    :sort sort)))
         (all-matches
          (mapcar #'my-consult-org-ql-agenda-format
                  (org-ql-select 'org-agenda-files query
                    :action 'element-with-markers
                    :sort sort))))
    (append
     matched-heading
     (seq-difference all-matches matched-heading))))

;;; org-sticky-header
(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :custom
  (org-sticky-header-full-path 'full))

;;; org-super-agenda
;; useful for folding elements in org-agenda
;; I can use outline-mode for this, but this already exists and is ready to go
;; https://www.reddit.com/r/orgmode/comments/1070bi2/a_little_elisp_to_kill_lines_in_the_agenda_like/
(use-package origami
  :hook (org-agenda-mode . origami-mode)
  :bind (:map org-super-agenda-header-map
         ("<tab>" . origami-toggle-node)))

(use-package org-super-agenda
  :after org
  :config
  ;; (setq org-super-agenda-groups nil)
  (org-super-agenda-mode))
(setq org-agenda-custom-commands
      (quote (("z" "Super View"
               ((agenda "" ((org-agenda-span 'day)
                            (org-super-agenda-groups
                             '((:name "Today"
                                :time-grid t
                                :date today
                                :deadline today
                                :deadline past
                                :scheduled today
                                :discard (:anything t)
                                :order 1))) ))
                ;; use org-super-agenda to organize all agenda items in a bucket
                (alltodo "" ((org-use-tag-inheritance t)
                             (org-agenda-dim-blocked-tasks nil) ;; speeds up agenda generation
                             (org-agenda-show-inherited-tags 'always) ;; makes sure org-super-agenda can search tags
                             (org-agenda-overriding-header "")
                             (org-super-agenda-groups
                              '((:name "Refile"
                                 :tag "inbox"
                                 :order 10)
                                (:name "Lab and Administrative"
                                 :and (:category "Lab"
                                       :todo "NEXT")
                                 :and (:category "Lab"
                                       :todo "TODO")
                                 :and (:category "Lab"
                                       :todo "WAITING")
                                 :order 20)

                                (:name "Grants"
                                 :and (:category "Grants"
                                       :todo "NEXT")
                                 :and (:category "Grants"
                                       :todo "TODO")
                                 :and (:category "Grants"
                                       :todo "WAITING")
                                 :order 30)

                                (:name "Projects"
                                 :and (:category "Project"
                                       :todo "NEXT")
                                 :and (:category "Project"
                                       :todo "TODO")
                                 :and (:category "Project"
                                       :todo "WAITING")
                                 :order 40)

                                ;; chores--only next or TODO
                                (:name "Chores"
                                 :and (:category "Chores"
                                       :todo "NEXT")
                                 :and (:category "Chores"
                                       :todo "TODO")
                                 :and (:category "Chores"
                                       :todo "WAITING")
                                 :order 50)

                                (:name "Career"
                                 :and (:category "Career"
                                       :todo "NEXT")
                                 :and (:category "Career"
                                       :todo "TODO")
                                 :and (:category "Career"
                                       :todo "WAITING")
                                 :order 60)

                                (:name "Family"
                                 :and (:category "Family"
                                       :todo "NEXT")
                                 :and (:category "Family"
                                       :todo "TODO")
                                 :and (:category "Family"
                                       :todo "WAITING")
                                 :order 70)

                                (:name "Teaching"
                                 :and (:category "Teach"
                                       :todo "NEXT")
                                 :and (:category "Teach"
                                       :todo "TODO")
                                 :and (:category "Teach"
                                       :todo "WAITING")
                                 :order 80)

                                (:name "Collaborations"
                                 :and (:category "collab"
                                       :todo "NEXT")
                                 :and (:category "collab"
                                       :todo "TODO")
                                 :and (:category "collab"
                                       :todo "WAITING")
                                 :order 95)
                                ))))))
              ("a" "Agenda"
               ((agenda "" ((org-agenda-span 2)))
                (alltodo ""
                         ((org-agenda-overriding-header "")
                          (org-agenda-show-inherited-tags )
                          (org-super-agenda-groups
                           '((:name "Inbox, unscheduled"
                              :and (:scheduled nil)
                              :order 1)
                             (:name "Important, unscheduled"
                              :and (:priority "A"
                                    :scheduled nil)
                              :order 2)

                             (:name "Project-related, unscheduled"
                              :and (:tag "project" :date nil :todo ("STARTED" "WAITING" "TODO"))
                              :order 3)
                             (:name "Waiting"
                              :and (:todo "WAITING"
                                    :scheduled nil)
                              :order 4)
                             (:discard (:todo "SOMEDAY"
                                        :category "cooking"
                                        :date t))
                             (:name "Unscheduled"
                              :scheduled nil
                              :order 5)
                             (:discard (:anything t))
                             )
                           )))
                ;; (tags-todo "TODO=\"TODO\"-project-cooking-routine-errands-shopping-video-evilplans"
                ;;            ((org-agenda-skip-function 'my-org-agenda-skip-scheduled)
                ;;             (org-agenda-prefix-format "%-6e ")
                ;;             (org-agenda-overriding-header "Unscheduled TODO entries: ")
                ;;             (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))))
                ))
              ;; show all emacs tasks, with the top-most entry as NEXT, then TODO, then WAITING
              ("e" "Emacs" tags "emacs"
               ((org-super-agenda-groups
                 '((:name "Next Action"
                    :todo "NEXT"
                    :order 1)
                   (:name "TODOs"
                    :todo "TODO"
                    :order 2)))))
              ("i" "Inbox" alltodo ""
               ((org-agenda-files '("~/sync/orgzly/Inbox.org" "~/sync/orgzly/computer-inbox.org"))))

              ;; quick tasks, with NEXT first, but limited to five entries
              ;; then show all todo ENTRIES
              ;; finally, show everything else
              ("q" "Quick tasks" alltodo ""
               ((org-agenda-overriding-header "")
                (org-super-agenda-groups
                 '((:name "Next Action"
                    :take (5 (:and (:todo "NEXT"
                                    :effort< "0:30")))
                    :order 1)
                   (:name "TODOs"
                    :and (:todo "TODO"
                          :effort< "0:30")
                    :order 2)
                   (:name "Everything else"
                    :and (:not (:todo "TODO")
                          :effort< "0:30")
                    :discard (:anything t)
                    :order 3)))))
              ("0" "Unestimated tasks" tags-todo "EFFORT=\"\"")
              ("d" "Timeline for today" ((agenda "" ))
               ((org-agenda-ndays 1)
                (org-agenda-show-log t)
                (org-agenda-log-mode-items '(clock closed))
                (org-agenda-clockreport-mode t)
                (org-agenda-entry-types '())))
              ("." "Waiting for" todo "WAITING")
              )))


;;; org-timeblock for scheduling todos
(use-package org-timeblock)

;;; org-transclusion
(use-package org-transclusion
  :after (org org-roam)
  :config
  ;; The frige is likely overwritten by org-modern, per org-transclusion manual
  ;; (set-face-attribute
  ;;  'org-transclusion-fringe nil
  ;;  :foreground "green"
  ;;  :background "green")

  (set-face-attribute
   'org-transclusion nil
   :background "#f5f5dc")

  (set-face-attribute
   'org-transclusion-source nil
   :background "#f5f5dc"))

;;; Org-Web-Tools
;; Download snapshots of websites straight into an org file
(use-package org-web-tools)

;;; org-pandoc-import
;; never leave org again!
;; on-the-fly convert to org and back
;; (use-package org-pandoc-import
;;   :vc (org-pandoc-import :url "https://github.com/tecosaur/org-pandoc-import"
;;                          :lisp-dir ("preprocessors/" "filters/"))
;;   :config
;;   ;; automatically convert various files to org mode when visiting them, say in Dired.
;;   ;; NOTE: may not be what I want. Try it out and see.
;;   (require 'org-pandoc-import-transient)
;;   (org-pandoc-import-transient-mode) )



;;; TOC-Org
;; export TOCs in org and markdown files for GitHub
(use-package toc-org
  :hook ((org-mode . toc-org-mode)
         (markdown-mode . toc-org-mode)))
;;; org-clock-convenience
;; additional clocking functions for agenda view
(use-package org-clock-convenience
  :disabled TODO:not sure this works or is worth it
  :after (org)
  :commands (org-clock-convenience-goto-ts
             org-clock-convenience-goto-last-clockout
             org-clock-convenience-timestamp-up
             org-clock-convenience-timestamp-down)
  :bind (:map org-agenda-mode-map
   	     ("<S-up>" . org-clock-convenience-timestamp-up)
   	     ("<S-down>" . org-clock-convenience-timestamp-down)))
;;; valign
;; package for nice vertical table alignment with variable width fonts
;; note: performance suffers for multiple tables or when there are >100 entries in a single table
;; I decided to turn this off in general, and toggle in on as buffer-local in some org files
(use-package valign
  ;; :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar t))

;;; org-bookmark-heading
;;Use the standard Emacs bookmark commands, C-x r m, etc, to mark org headings
(use-package org-bookmark-heading)

;;; Provide Org Extensions
(provide 'my-setup-org-extensions)
;;; my-setup-org-extensions.el ends here
