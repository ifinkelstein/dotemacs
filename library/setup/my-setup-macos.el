;;; my-setup-macos.el -*- lexical-binding: t -*-

(message "Loading MacOS settings...")

;;* Opening files in preview
(use-package image-mode
  :ensure nil
  ;; open images with "S" in image-mode
  :bind (:map image-mode-map
              ("S" . xah-open-in-external-app)))

;;* General Settings

;; Delete to Trash
;; macOS ships /usr/bin/trash; system-move-file-to-trash defun below wins over trash-directory.
(setq delete-by-moving-to-trash t)
(defun system-move-file-to-trash (path)
  "Move file at PATH to the macOS Trash according to `move-file-to-trash' convention.

Relies on the command-line utility 'trash' to be installed.
Get it from:  <http://hasseg.org/trash/>"
  (shell-command (concat "trash -v " (shell-quote-argument path))
                 nil
                 "*Trash Error Buffer*"))

;;* Frames & Fullscreen
;; (Do not) make new frames when opening a new file with Emacs unless on scratch buffer
(setq ns-pop-up-frames nil)

;; Fullscreen (disable for non-space full screen)
;; Using fullscreen with "notched" M1 macs requires non-native.
(setq ns-use-native-fullscreen nil)

;;* Clipboard
;; Saving whatever's in the current (system) clipboard before
;; replacing it with the Emacs' text.
(setq save-interprogram-paste-before-kill t)

;; Fix for non-ascii characters
;; see https://gist.github.com/the-kenny/267162#gistcomment-2883522
(setenv "LANG" "en_US.UTF-8")

;;* Keybindings
;; Set modifier keys
(setq mac-option-modifier 'meta) ;; Bind meta to ALT
(setq mac-command-modifier 'super) ;; Bind apple/command to super if you want
(setq mac-function-modifier 'hyper) ;; Bind function key to hyper if you want
(setq mac-right-option-modifier 'none) ;; unbind right key for accented input

;; Make forward delete work
(global-set-key (kbd "<H-backspace>") 'delete-forward-char)

;; Keybindings
(global-set-key (kbd "s-q") 'my-delete-frame-or-quit)
(global-set-key (kbd "s-v") 'yank)
(with-eval-after-load 'meow
  (global-set-key (kbd "s-c") 'meow-clipboard-save))
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "s-W") 'delete-frame)
(global-set-key (kbd "s-N") 'make-frame)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-Z") 'undo-redo)
(keymap-global-set "s-s" #'save-buffer)

;;* Reveal in Finder
(use-package reveal-in-osx-finder
  :commands (reveal-in-osx-finder))

;;* Get mac links from safari
(use-package grab-mac-link
  :commands (grab-mac-link grab-mac-link-dwim))

;;* Homebrew
;; simple wrapper for homebrew
(use-package homebrew
  :vc (:url "https://github.com/jdormit/homebrew.el")
  :commands
  (homebrew-install homebrew-upgrade homebrew-update homebrew-edit homebrew-info homebrew-package-info))

;;* Security Keychain
;; See https://www.reddit.com/r/emacs/comments/ew75ib/emacs_mu4e_and_mbsync_setup_for_fastmail_on_macos/fg23tcj?utm_source=share&utm_medium=web2x&context=3
(when sys-mac
  (with-eval-after-load 'auth-source
    (add-to-list 'auth-sources 'macos-keychain-internet)
    (add-to-list 'auth-sources 'macos-keychain-generic)))

;;* Provide my-setup-macos
(provide 'my-setup-macos)
;;; my-setup-macos.el ends here
