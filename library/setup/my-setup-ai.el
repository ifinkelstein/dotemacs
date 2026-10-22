;; my-setup-ai.el -*- lexical-binding: t -*-
(message "Setting up AI packages...")


;;* whisper local audio to text in Emacs
;; Pick the microphone with `rk/select-default-audio-device' (AI menu D).
(use-package whisper
  :commands (whisper-run whisper-file)
  :bind ("s-R" . whisper-run)
  :custom
  ;; Transcribe with a resident Parakeet TDT v3 server (parakeet-mlx on
  ;; Metal), run by launchd from library/bin/parakeet-server.py.  It speaks
  ;; the whisper.cpp /inference protocol, so whisper.el's `remote' mode talks
  ;; to it directly.  The model stays loaded (~1.4 GB), so a dictation takes
  ;; ~0.3 s instead of ~3 s for a fresh mlx_whisper process.
  (whisper-server-mode 'remote)
  (whisper-server-baseurl "http://127.0.0.1:8642")
  (whisper-install-whispercpp nil)
  ;; move cursor to the end of the inserted text
  (whisper-return-cursor-to-start nil)
  :config
  (setq whisper--ffmpeg-input-device ":0")

  (add-hook 'whisper-before-transcription-hook #'my-whisper-check-server)

  ;; Route dictation into ghostel terminal buffers through the subprocess
  ;; (bracketed paste) instead of `insert', which fails on the renderer-owned,
  ;; read-only ghostel buffer.  Mirrors whisper's built-in vterm/eat handling.
  (defun my-whisper-insert-text-ghostel (orig text)
    "Around-advice for `whisper--insert-text': paste into ghostel buffers."
    (if (derived-mode-p 'ghostel-mode)
        (ghostel-paste-string text)
      (funcall orig text)))
  (advice-add 'whisper--insert-text :around #'my-whisper-insert-text-ghostel)

  ;; Raw transcription log.  Every transcription is appended verbatim under a
  ;; timestamped subheading in two sibling sections: `* Raw' (pristine
  ;; reference) and `* Corrected' (an identical duplicate to hand-edit later).
  ;; Read the Corrected section periodically and fix mis-transcriptions there.
  ;; NOTE: this logging is temporary scaffolding — once `my-whisper-replacements'
  ;; (or better correction heuristics) cover the common errors, remove the log
  ;; and its hook.
  (defvar my-whisper-log-file
    (expand-file-name "whisper-log.org" my-var-dir)
    "Org file accumulating raw and to-be-corrected whisper transcriptions.")

  (defun my-whisper--ensure-log ()
    "Create `my-whisper-log-file' with its section scaffold if absent."
    (unless (file-exists-p my-whisper-log-file)
      (with-temp-file my-whisper-log-file
        (insert "#+title: Whisper transcription log\n\n* Raw\n* Corrected\n"))))

  (defun my-whisper--append-under (heading text stamp)
    "Append a STAMP subheading containing TEXT under top-level HEADING."
    (goto-char (point-min))
    (re-search-forward (format "^\\* %s$" (regexp-quote heading)))
    (let ((end (if (re-search-forward "^\\* " nil t)
                   (match-beginning 0)
                 (point-max))))
      (goto-char end)
      (insert (format "** %s\n%s\n" stamp text))))

  (defun my-whisper-log-org ()
    "Append the raw transcription to `my-whisper-log-file'.
Runs in whisper's output buffer via `whisper-after-transcription-hook'."
    (let ((text (string-trim (buffer-string)))
          (stamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (unless (string-empty-p text)
        (my-whisper--ensure-log)
        (with-current-buffer (find-file-noselect my-whisper-log-file)
          (save-excursion
            (my-whisper--append-under "Raw" text stamp)
            (my-whisper--append-under "Corrected" text stamp))
          (save-buffer)))))

  (add-hook 'whisper-after-transcription-hook #'my-whisper-log-org)

  ;; Deterministic fixups for recurrent whisper mis-transcriptions.  Each
  ;; REGEXP (whisper's wrong output) is rewritten to the intended term.  Runs
  ;; on the output buffer *after* `my-whisper-log-org' so the raw log keeps the
  ;; original for review while the inserted text is corrected.  Grow this list
  ;; as new errors surface in the Corrected section of `my-whisper-log-file'.
  (defvar my-whisper-replacements
    '(("\\bMinecraft\\b"      . "BindCraft")   ; protein binder design tool
      ("\\bBB copy\\b"        . "pbcopy")       ; macOS clipboard command
      ("\\bKlaus\\b"          . "Claus")        ; collaborator Claus (Wilke)
      ("\\bChong\\b"          . "Chung"))       ; collaborator's name
    "Alist of (REGEXP . REPLACEMENT) fixing recurrent whisper errors.")

  (defun my-whisper-fix-vocabulary ()
    "Apply `my-whisper-replacements' to the whisper output buffer."
    (dolist (pair my-whisper-replacements)
      (goto-char (point-min))
      (while (re-search-forward (car pair) nil t)
        (replace-match (cdr pair) t))))

  (add-hook 'whisper-after-transcription-hook #'my-whisper-fix-vocabulary t))

(defvar whisper--ffmpeg-input-device)

(defun my-whisper-check-server ()
  "Signal an error before recording if the Parakeet server isn't up.
Kickstarts its launchd job so the next attempt can succeed."
  (condition-case nil
      (delete-process
       (open-network-stream "parakeet-check" nil "127.0.0.1" 8642))
    (file-error
     (call-process "launchctl" nil 0 nil "kickstart" "-k"
                   (format "gui/%d/com.ilya.parakeet-server" (user-uid)))
     (user-error "Parakeet server was down; restarting it, retry s-R in ~10 s"))))

(defun rk/select-default-audio-device (&optional device-name)
  "Select the ffmpeg audio input device used by whisper.el.
Prompt among the avfoundation audio devices unless DEVICE-NAME is given."
  (interactive)
  ;; Load whisper first so its :config default (":0") can't override this.
  (require 'whisper)
  (let* ((audio-devices (cadr (rk/get-ffmpeg-device)))
         (name (or device-name
                   (completing-read "Select audio device: "
                                    (mapcar #'cdr audio-devices) nil t)))
         (index (or (car (rassoc name audio-devices))
                    (user-error "No audio device named %s" name))))
    (setq whisper--ffmpeg-input-device (format ":%s" index))))

(defun rk/get-ffmpeg-device ()
  "Gets the list of devices available to ffmpeg.
The output of the ffmpeg command is pretty messy, e.g.
  [AVFoundation indev @ 0x7f867f004580] AVFoundation video devices:
  [AVFoundation indev @ 0x7f867f004580] [0] FaceTime HD Camera (Built-in)
  [AVFoundation indev @ 0x7f867f004580] AVFoundation audio devices:
  [AVFoundation indev @ 0x7f867f004580] [0] Cam Link 4K
  [AVFoundation indev @ 0x7f867f004580] [1] MacBook Pro Microphone
so we need to parse it to get the list of devices.
The return value contains two lists, one for video devices and one for audio devices.
Each list contains a list of cons cells, where the car is the device number and the cdr is the device name."
  (unless (string-equal system-type "darwin")
    (error "This function is currently only supported on macOS"))

  (let ((lines (string-split (shell-command-to-string "ffmpeg -list_devices true -f avfoundation -i dummy || true") "\n")))
    (cl-loop with at-video-devices = nil
             with at-audio-devices = nil
             with video-devices = nil
             with audio-devices = nil
             for line in lines
             when (string-match "AVFoundation video devices:" line)
             do (setq at-video-devices t
                      at-audio-devices nil)
             when (string-match "AVFoundation audio devices:" line)
             do (setq at-audio-devices t
                      at-video-devices nil)
             when (and at-video-devices
                       (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
             do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) video-devices)
             when (and at-audio-devices
                       (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
             do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) audio-devices)
             finally return (list (nreverse video-devices) (nreverse audio-devices)))))


;;* ghostel + Meow insert state
;; Selecting an interactive ghostel window (or opening a coding agent there)
;; should always leave Meow in insert state, even if `<escape>' was pressed
;; earlier in that buffer to reach the `<SPC>' leader map.
(defvar ghostel--input-mode)
(declare-function meow-insert-mode-p "meow-util" ())
(declare-function meow--disable-current-state "meow-util" ())
(declare-function meow--switch-state "meow-util" (state &optional no-hook))
(declare-function meow-update-display "meow-util" ())

(defun my-ghostel-ensure-meow-insert (&rest _)
  "Ensure the selected window's buffer is in `meow-insert-mode' when in an interactive ghostel terminal."
  (with-selected-window (selected-window)
    (when (and (derived-mode-p 'ghostel-mode)
               (bound-and-true-p meow-mode)
               (not (memq ghostel--input-mode '(copy emacs))))
      (unless (meow-insert-mode-p)
        (meow--disable-current-state)
        (meow--switch-state 'insert))
      (meow-update-display))))

(with-eval-after-load 'ghostel
  (add-hook 'window-selection-change-functions #'my-ghostel-ensure-meow-insert))

;;* claude-code
;; Deferred via :commands.  inheritenv comes in as a claude-code dependency.
(use-package claude-code
  :commands (claude-code claude-code-transient claude-code-start-in-directory)
  :config
  (setq claude-code-terminal-backend 'ghostel)
  (setq claude-code-toggle-auto-select t)
  (advice-add 'claude-code--start :after #'my-ghostel-ensure-meow-insert)
  (advice-add 'claude-code-toggle :after #'my-ghostel-ensure-meow-insert)

  (defun my-claude-code-insert-org-link ()
    "Insert the top stored org link into the Claude Code buffer, no prompts."
    (interactive)
    (if-let* ((entry (car org-stored-links)))
        (let* ((link (car entry))
               (desc (or (cadr entry) link))
               (link-text (format "[[%s][%s]]" link desc)))
          (claude-code--term-send-string claude-code-terminal-backend link-text))
      (message "No stored links")))

  (defun my-claude-code-insert-timestamp ()
    "Prompt for an org timestamp and send it to the terminal."
    (interactive)
    (let ((ts (format "<%s>" (org-read-date))))
      (claude-code--term-send-string claude-code-terminal-backend ts)))

  (with-eval-after-load 'ghostel
    (define-key ghostel-mode-map (kbd "C-c C-l") #'my-claude-code-insert-org-link)
    (define-key ghostel-mode-map (kbd "C-c .") #'my-claude-code-insert-timestamp)
    (define-key ghostel-mode-map (kbd "s-V") #'yank))

  (add-hook 'claude-code-start-hook
            (lambda ()
              (local-set-key (kbd "M-o") #'claude-code-transient)))

  (claude-code-mode))

;;* agy (Google Antigravity CLI)
;; Own package, modeled on claude-code.el.  Run `agy-install-hooks' once so
;; agy's Stop hook tells Emacs when a turn ends.
(use-package agy
  :load-path "~/projects/elisp/agy"
  ;; `agy-bookmark-handler' restores sessions saved in bookmarks and activities.
  :commands (agy agy-transient agy-resume agy-start-in-directory agy-bookmark-handler)
  :config
  (setq agy-terminal-backend 'ghostel)
  (setq agy-toggle-auto-select t)
  (advice-add 'agy--start :after #'my-ghostel-ensure-meow-insert)
  (advice-add 'agy-toggle :after #'my-ghostel-ensure-meow-insert)
  (add-hook 'agy-start-hook
            (lambda () (local-set-key (kbd "M-o") #'agy-transient))))

;;* codex (OpenAI Codex CLI in a ghostel terminal)
;; Minimal launcher modeled on how claude-code.el starts its terminal:
;; `ghostel-exec' execs PROGRAM directly in a fresh ghostel buffer, wrapped
;; in `inheritenv' so the subprocess inherits the buffer's process
;; environment (same pattern as `claude-code--term-make' for the ghostel
;; backend).  No claude-code dependency — this is a standalone command.
(defcustom my-codex-program "codex"
  "Executable used to start the OpenAI Codex CLI."
  :type 'string)

(defun my-codex (&optional arg)
  "Launch Codex in a new ghostel terminal.
The working directory is the directory of the file the current buffer
visits, or `default-directory' otherwise (Dired, a scratch buffer, etc.).
With prefix ARG, prompt for the directory instead.

A fresh, uniquely-named buffer is created on each call, so several Codex
sessions can run at once (in different windows or tabs)."
  (interactive "P")
  (require 'ghostel)
  (require 'inheritenv)
  (let* ((dir (cond (arg (read-directory-name "Codex directory: "))
                    (buffer-file-name (file-name-directory buffer-file-name))
                    (t default-directory)))
         (default-directory (expand-file-name dir))
         (buffer (generate-new-buffer
                  (format "*codex: %s*"
                          (abbreviate-file-name
                           (directory-file-name default-directory))))))
    ;; Display before exec: `ghostel-exec' sizes the terminal to the window
    ;; showing BUFFER (else falls back to 80x24).
    (pop-to-buffer buffer)
    (inheritenv (ghostel-exec buffer my-codex-program))
    (my-ghostel-ensure-meow-insert)
    buffer))

;;* pilish (pi coding agent)
(use-package md-ts-mode :ensure t :defer t)
(use-package markdown-table-wrap :ensure t :defer t)

(use-package pilish
  ;; Upstream checkout (dnouri/pi-coding-agent); the package was renamed
  ;; pi-coding-agent -> pilish in v3.0.0.
  :load-path "~/projects/elisp/pilish"

  :bind (:map pilish-input-mode-map
         ("<s-return>" . pilish-send)
         ("C-c C-l" . org-insert-link-global)
         ("C-." . jinx-correct))

  :commands (pilish)

  :custom
  (pilish-input-window-height 10)
  (pilish-tool-preview-lines 10)
  (pilish-bash-preview-lines 5)
  (pilish-context-warning-threshold 70)
  (pilish-context-error-threshold 90)
  (pilish-visit-file-other-window t))

;;* provide
(provide 'my-setup-ai)
;; my-setup-ai.el ends here
