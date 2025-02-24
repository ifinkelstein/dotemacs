;;; my-setup-reading.el  -*- lexical-binding: t -*-

(message "Setting up reading...")
;;* Elfeed
(use-package elfeed
  :commands (elfeed-load-db-and-open)
  :bind (:map elfeed-search-mode-map
              ("<" . beginning-of-buffer)
              (">" . end-of-buffer)
              ("a" . elfeed-show-all)
              ("B" . elfeed-search-browse-background-url)
              ("d" . elfeed-show-daily)
              ("e" . elfeed-show-emacs)
              ("p" . elfeed-play-in-external-player)
              ("q" . elfeed-save-db-and-bury)
              ("R" . elfeed-mark-all-as-read)
              ("U" . elfeed-update)
              :map elfeed-show-mode-map
              ("f" . link-hint-open-link))
  :custom
  (elfeed-db-directory (concat my-cache-dir ".elfeed"))
  (elfeed-enclosure-default-dir (expand-file-name "~/Downloads"))

  :config
  ;; Make sure the database is created
  (when (not (file-exists-p (concat elfeed-db-directory "/data")))
    (elfeed-update))
  ;; save the elfeed DB when emacs is idle for two minutes
  (run-with-idle-timer 120 1
                       'elfeed-db-save)

  ;; Useful functions for elfeed
  (defun elfeed-show-all ()
    "Show all feeds within the last two months."
    (interactive)
    (elfeed-search-set-filter "@2-months-ago"))

  (defun elfeed-mark-all-as-read ()
    "Mark all feeds in buffer as read."
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (defun elfeed-play-in-external-player ()
    "Play with mpv."
    (interactive)
    (elfeed-search-yank)
    (interactive)
    (play-with-mpv (substring-no-properties (car kill-ring))))

  ;; Functions to support syncing .elfeed between machines makes sure elfeed
  ;; reads index from disk before launching
  (defun elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening."
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  ;; Write to disk when quitting
  (defun elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer."
    (interactive)
    (elfeed-db-save)
    (quit-window))

  ;; https://xenodium.com/open-emacs-elfeed-links-in-background/
  (defun elfeed-search-browse-background-url ()
    "Open current `elfeed' entry (or region entries) in browser without losing focus."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (mapc (lambda (entry)
              (cl-assert (memq system-type '(darwin)) t "open command is macOS only")
              (start-process (concat "open " (elfeed-entry-link entry))
                             nil "open" "--background" (elfeed-entry-link entry))
              (elfeed-untag entry 'unread)
              (elfeed-search-update-entry entry))
            entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line)))))

;;** Elfeed-Org
;; manage RSS feeds using an org file
(use-package elfeed-org
  :after (elfeed)
  :config
  (setq rmh-elfeed-org-files '("/Users/ilya/.config/.emacs/var/elfeed.org")) 
  (elfeed-org))

;;** Elfeed Goodies
;; vertical split screen browsing and other enhancements
(use-package elfeed-goodies
  :after (elfeed)
  :custom-face
  ;;elfeed faces
  ;; (powerline-active1 ((t (:background unspecified :inherit lambda-aqua))))
  ;; (powerline-active2 ((t (:background unspecified :inherit lambda-aqua))))
  ;; ( ((t (:foreground unspecified :background unspecified :inherit lambda-green))))
  :config
  (setq elfeed-goodies/entry-pane-position 'bottom) ;; split with content on the bottom like mu4e
  (setq elfeed-goodies/entry-pane-size 0.5)
  (elfeed-goodies/setup))

;;* calibre integration
;; for ebook support
(use-package calibredb
  :config
  (setq calibredb-root-dir "~/Documents/eBooks")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Documents/eBooks"))))

;;* nov.el
;; for reading EPUBs in emacs
;; check out xwidget-based nov.el integration at some point
;; https://www.reddit.com/r/emacs/comments/v2fjec/a_new_epub_reader_in_emacs_novxwidgetel/
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-hook 'nov-mode-hook 'olivetti-mode))

;;* end my-setup-reading
(provide 'my-setup-reading)
