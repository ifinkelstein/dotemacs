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
              ("p" . my-elfeed-play-mpv)
              ("q" . elfeed-save-db-and-bury)
              ("R" . elfeed-mark-all-as-read)
              ("U" . elfeed-update)
              :map elfeed-show-mode-map
              ("f" . link-hint-open-link))
  :custom
  (elfeed-db-directory (concat my-cache-dir ".elfeed"))
  (elfeed-enclosure-default-dir (expand-file-name "~/Downloads"))

  :config
  ;; save the elfeed DB when emacs is idle for two minutes
  (defvar my-elfeed-db-save-timer nil
    "Idle timer that periodically saves the elfeed database.")
  (unless my-elfeed-db-save-timer
    (setq my-elfeed-db-save-timer
          (run-with-idle-timer 120 t #'elfeed-db-save)))

  ;; Useful functions for elfeed
  (defun elfeed-show-all ()
    "Show all feeds within the last two months."
    (interactive)
    (elfeed-search-set-filter "@2-months-ago"))

  (defun elfeed-mark-all-as-read ()
    "Mark all feeds in buffer as read."
    (interactive)
    (let ((elfeed-search-remain-on-entry t))
      (save-excursion
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (elfeed-search-untag-all-unread)))
    (deactivate-mark))

  (defun my-elfeed-play-mpv ()
    "Play the link of the selected elfeed entry in mpv."
    (interactive)
    (when-let* ((entry (car (elfeed-search-selected)))
                (url (elfeed-entry-link entry)))
      (start-process "mpv" nil "mpv" url)))

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

;;** Elfeed Feeds
;; Add feeds directly via elfeed-feeds
(setq elfeed-feeds
      '(;; Emacs
        ("http://www.reddit.com/r/emacs/new/.rss?sort=new" emacs reddit)
        ("https://planet.emacslife.com/atom.xml" emacs)
        ("https://oneofus.la/have-emacs-will-hack/feed.xml" emacs)
        ("http://oremacs.com/atom.xml" emacs)
        ("http://pragmaticemacs.com/feed/" emacs)
        ("https://www.reddit.com/r/orgmode.rss" emacs reddit)
        ("https://blog.tecosaur.com/tmio/rss.xml" emacs)
        ;; Reddit
        ("https://www.reddit.com/r/usenet.rss" reddit)
        ;; ML
        ("https://simonwillison.net/atom/everything/" ml)
        ;; Literature
        ("https://kill-the-newsletter.com/feeds/9v1p9tzxifsim9cb.xml" lit)
        ;; Biotech
        ("http://www.reddit.com/r/biotech/new/.rss?sort=new" biotech reddit)
        ;; Running
        ("https://www.reddit.com/r/advancedrunning.rss" run reddit)
        ;; Science
        ("https://www.science.org/blogs/pipeline/feed" science)
        ;; Writing
        ("https://www.helensword.com/helen-sword-blog?format=rss" writing)
        ("https://kill-the-newsletter.com/feeds/mcc8ko4l1zrjw998.xml" writing)
        ;; News
        ("https://hnrss.org/frontpage" news)
        ;; Comics
        ("https://xkcd.com/rss.xml" comics)
        ("https://www.smbc-comics.com/rss.php" comics)
        ("https://theoatmeal.com/feed/rss" comics)
        ;; Academics
        ("https://writepublishthrive.blogspot.com/feeds/posts/default" academic)
        ))

;;** Elfeed entry window placement
;; Show the elfeed entry buffer in a bottom window (~60% height), like mu4e.
(add-to-list 'display-buffer-alist
             '("\\*elfeed-entry\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.6)))

;;* end my-setup-reading
(provide 'my-setup-reading)
