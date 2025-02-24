;;; my-setup-media.el -*- lexical-binding: t -*-

(message "Setting up media packages...")
;;* emms (emacs multimedia system)
;; http://blog.binchen.org/posts/how-to-use-emms-effectively/
;; https://www.gnu.org/software/emms/manual/
(defvar my-emms-playlist-filter-keyword "mozart|bach"
  "Keyword to filter tracks in emms playlist.
Space in the keyword matches any characters.
 \"|\" means OR operator in regexp.")

(use-package emms
  :custom
  (emms-source-beets-database "/Users/ilya/Media/Music/musiclibrary.db")
  (emms-player-list '(emms-player-mpv
                      emms-player-vlc))
  (emms-playlist-buffer-name "*Music*") ;; name of EMMS buffer
  (emms-source-file-default-directory "~/Media/Music/")

  ;; todo: fix this so that mpv can change the volume
  ;; (emms-volume-change-function 'emms-volume-mpd-change)

  ;; covers
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (emms-browser-thumbnail-small-size 100)
  (emms-browser-thumbnail-medium-size 200)

  :config
  (emms-all)

  ;; mpv integration
  ;; https://www.reddit.com/r/emacs/comments/syop1h/control_emmsmpv_volume/
  (defvar emms-player-mpv-volume 100)

  (defun emms-player-mpv-get-volume ()
    "Sets `emms-player-mpv-volume' to the current volume value
and sends a message of the current volume status."
    (emms-player-mpv-cmd '(get_property volume)
                         #'(lambda (vol err)
                             (unless err
                               (let ((vol (truncate vol)))
                                 (setq emms-player-mpv-volume vol)
                                 (message "Music volume: %s%%"
                                          vol))))))

  (defun emms-player-mpv-raise-volume (&optional amount)
    (interactive)
    (let* ((amount (or amount 10))
           (new-volume (+ emms-player-mpv-volume amount)))
      (if (> new-volume 100)
          (emms-player-mpv-cmd '(set_property volume 100))
        (emms-player-mpv-cmd `(add volume ,amount))))
    (emms-player-mpv-get-volume))

  (defun emms-player-mpv-lower-volume (&optional amount)
    (interactive)
    (emms-player-mpv-cmd `(add volume ,(- (or amount '10))))
    (emms-player-mpv-get-volume))


  ;; history
  (emms-history-load)

  ;; beets integration
  (emms-add-beets)

  ;; `emms-info-native' supports mp3,flac,ogg and requires NO CLI tools
  (unless (memq 'emms-info-native emms-info-functions)
    (require 'emms-info-native)
    (push 'emms-info-native emms-info-functions))

  ;; extract track info when loading the playlist
  (push 'emms-info-initialize-track emms-track-initialize-functions))

;;** EMMS helpers
;; transient to control EMMS
;; https://tech.toryanderson.com/2023/11/29/transient-for-convenience-with-emms/
(transient-define-prefix my-transient-emms ()
  "EMMS music"
  :transient-non-suffix 'transient--do-quit-one
  ["EMMS"
   ["Controls"
    ("p" "⏯ Play/Pause" emms-pause)
    ("s" "⏹ Stop" emms-stop)
    ("S" "⏲ Seek to time" emms-seek-to)
    ("n" "⏭ Next" emms-next)
    ("B" "⏮ Back (Previous)" emms-previous)
    ("b" "⏪ Back rewind" emms-seek-backward :transient transient--do-stay) ;; I want the transient to stay open on just these commands, so I can easily repeat them
    ("f" "⏩ Fast-Forward" emms-seek-forward :transient transient--do-stay)]
   ["Playlist"
    ("N" "Cue Next" emms-cue-previous)
    ("P" "Cue Previous" emms-cue-previous)
    ("r" "🔀 play Random" emms-random)
    ("R" "🔀 toggle shuffle" emms-toggle-random-playlist)
    ]
   ["Global/External"
    ("d" "📂 emms Dired" emms-play-dired)
    ;; ("u" "Music dir" tsa/jump-to-music) ;; invokes a bookmark, which in turn hops to my bookmarked music directory
    ("m" "Modeline" emms-mode-line-mode)
    ("M" "current info" emms-show)
    ("e" "emms" emms)]
   ])

;;* listen.el
;; broken by newer transient
(use-package listen
  :vc (:url "https://github.com/alphapapa/listen.el")
  :custom
  (listen-directory "/Users/ilya/Media/Music"))

;;* ready-player for dired integration
(use-package ready-player
  :config
  (ready-player-mode +1))

;;* Provide my-setup-media.el
(provide 'my-setup-media)
;;* my-setup-media.el ends here
