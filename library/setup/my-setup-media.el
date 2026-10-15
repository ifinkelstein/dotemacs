;;; my-setup-media.el -*- lexical-binding: t -*-

(message "Setting up media packages...")

;; Needed at compile time so `transient-define-prefix' is recognized as a
;; macro; otherwise the byte-compiler treats it as a function call and the
;; prefix name is miscompiled into a variable reference (void-variable at load).
(require 'transient)
;;* emms (emacs multimedia system)
;; http://blog.binchen.org/posts/how-to-use-emms-effectively/
;; https://www.gnu.org/software/emms/manual/
(use-package emms
  :defer t

  :custom
  (emms-player-list '(emms-player-mpv
                      emms-player-vlc))
  (emms-playlist-buffer-name "*Music*") ;; name of EMMS buffer
  (emms-source-file-default-directory "~/Media/Music/")
  ;; emms-volume-raise/lower talk to mpv over its IPC socket
  (emms-volume-change-function #'emms-volume-mpv-change)

  ;; covers
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (emms-browser-thumbnail-small-size 100)
  (emms-browser-thumbnail-medium-size 200)

  :config
  ;; also sets up emms-info-native and per-track info initialization
  (emms-all)

  ;; history
  (emms-history-load))

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
    ("N" "Cue Next" emms-cue-next)
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
   ]
  (interactive)
  ;; emms is deferred; its commands must be defined before transient-setup
  ;; checks the suffixes.
  (require 'emms)
  (transient-setup 'my-transient-emms))

;; Bind globally to the locally-defined transient (which loads emms lazily),
;; instead of via `use-package' :bind, which would autoload the command from
;; the emms feature and warn that it is defined multiple times.
(keymap-global-set "M-M" #'my-transient-emms)

;;* Provide my-setup-media.el
(provide 'my-setup-media)
;;* my-setup-media.el ends here
