;;; frames.el --- summary -*- lexical-binding: t -*-

;; Author:

;;; Commentary:

;; Setup for frames. Note that some basic settings are placed in early-init for
;; better startup.

;;; Code:
(message "Setting up frames settings...")

;;;; Frame defaults
(use-package frame
  :ensure nil
  :config
  ;; Make a clean & minimalist frame
  ;; To modify initial frame set `initial-frame-alist`
  (setq-default default-frame-alist
                (append (list
                         '(frame-title-format . nil)
                         '(internal-border-width . 18)
                         '(tool-bar-lines . 0)
                         '(vertical-scroll-bars . nil)
                         '(horizontal-scroll-bars . nil))))
  ;; Resize pixel-wise to avoid gaps
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t)

  ;; Don't show icon in frame
  (setq-default ns-use-proxy-icon nil))

;;;; (Re)Center Frames
(defun my-frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.5) (left . 0.5)))))

;; un/comment this hook if you want frames recentered
(add-hook 'after-make-frame-functions #'my-frame-recenter)


;;;; Fix titlebar titling colors
;; see also https://github.com/d12frosted/homebrew-emacs-plus/issues/55
(use-package ns-auto-titlebar
  :commands ns-auto-titlebar-mode
  :if (eq system-type 'darwin)
  :init (ns-auto-titlebar-mode))

;;;; Run commands in a popup frame
;; ref: https://protesilaos.com/codelog/2024-09-19-emacs-command-popup-frame-emacsclient/
;; better: https://localauthor.github.io/posts/popup-frames.html

(defun my-window-popup-frame-delete (&rest _)
  "Kill selected frame if it has parameter `popup-frame'."
  (when (frame-parameter nil 'popup-frame))
  (delete-frame))

(defmacro my-window-popup-frame-define (command title &optional delete-frame)
  "Define interactive function to call COMMAND in frame with TITLE."
  `(defun ,(intern (format "popup-frame-%s" command)) ()
     (interactive)
     (let* ((display-buffer-alist '(("")
                                    (display-buffer-full-frame)))
            (frame (make-frame
                    '((title . ,title)
                      (window-system . ns)
                      (popup-frame . t)))))
       ;; (message display-buffer-alist)
       (select-frame frame)
       (switch-to-buffer " popup-frame-hidden-buffer")
       (condition-case nil
           (progn
             (call-interactively ',command)
             (delete-other-windows))
         (error (delete-frame frame)))
       (when ,delete-frame
         (sit-for 0.5)
         (delete-frame frame)))))

;; (declare-function org-capture "org-capture" (&optional goto keys))
;; (defvar org-capture-after-finalize-hook)

(my-window-popup-frame-define org-capture "capture-popup")
(add-hook 'org-capture-after-finalize-hook #'my-window-popup-frame-delete)

(my-window-popup-frame-define org-mru-clock-in "large-popup")
(add-hook 'org-clock-in-hook #'my-window-popup-frame-delete)

;; (my-window-popup-frame-define password-store-copy "minimal-popup" 'delete-frame)

;; (popup-frame-define citar-open-files "minimal-popup" 'delete-frame)

;; NOTE: The emacsclient call depends on the daemon or `server-mode' (I use the latter)

;; (use-package server
;;   :ensure nil
;;   :defer 1
;;   :config
;;   (unless (server-running-p)
;;     (server-start)))

;; The emacsclient calls that need ot be bound to system-wide keys
;; emacsclient -e '(my-window-popup-org-capture)'
;; emacsclient -e '(my-window-popup-tmr)'


(provide 'my-setup-frames)
;;; frames.el ends here
