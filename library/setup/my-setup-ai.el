;; my-setup-ai.el -*- lexical-binding: t -*-
(message "Setting up AI packages...")


;;* whisper local audio to text in Emacs

;; example usage:
;; (rk/find-device-matching "FaceTime" :video)
;; (rk/find-device-matching "Macbook Pro Microphone" :audio)
;; (rk/select-default-audio-device)
(use-package whisper
  :commands (whisper-run whisper-file rk/select-default-audio-device)
  :bind ("s-R" . whisper-run)
  :custom
  (whisper-insert-text-at-point t)
  (whisper-install-directory my-var-dir)
  ;; Use mlx_whisper (Apple Metal) as the inference engine instead of the
  ;; bundled whisper.cpp.  nil here routes `whisper-run' through the
  ;; "user-provided inference engine" branch, skipping the whisper.cpp
  ;; install/build and ggml model checks.  `whisper-model' is unused now;
  ;; the model is `my-whisper-mlx-model' below.
  (whisper-install-whispercpp nil)
  (whisper-language "en")
  (whisper-translate nil)
  (whisper--ffmpeg-input-device-name "Macbook Pro Microphone")
  ;; (whisper--ffmpeg-input-device (rk/find-device-matching whisper--ffmpeg-input-device-name :audio))
  (whisper--ffmpeg-input-format "avfoundation")
  ;; move cursor to the end of the inserted text
  (whisper-return-cursor-to-start nil)
  :config
  (setq rk/default-audio-device 0)
  (setq whisper--ffmpeg-input-device ":0")

  (defvar my-whisper-mlx-model "mlx-community/whisper-large-v3-turbo"
    "Hugging Face repo id (or local path) of the mlx_whisper model.
`whisper-large-v3-turbo' is fast and fine for clean mic input.  Switch to
\"mlx-community/whisper-large-v3-mlx\" (full large-v3) for more accuracy
and extra resistance to hallucination loops, at the cost of speed.")

  (defvar my-whisper-mlx-extra-flags
    '("--condition-on-previous-text" "False"   ; main loop fix: don't feed one
                                               ; window's output forward as the
                                               ; next window's prompt
      "--word-timestamps" "True"               ; required by the next flag
      "--hallucination-silence-threshold" "2") ; skip silent gaps >2s when a
                                               ; hallucination is suspected
    "Extra mlx_whisper flags that suppress the silence/hallucination loop.")

  ;; Override the package's whisper.cpp command builder.  mlx_whisper only
  ;; writes output files (no clean-text-to-stdout mode), so run it and then
  ;; `cat' the resulting .txt, which is what whisper.el reads from stdout.
  (defun whisper-command (input-file)
    "Transcribe INPUT-FILE locally with mlx_whisper.
See `my-whisper-mlx-model' and `my-whisper-mlx-extra-flags' to tune."
    (let* ((dir (file-name-directory input-file))
           (base (file-name-base input-file))
           (txt (expand-file-name (concat base ".txt") dir))
           (args (append
                  (list (or (executable-find "mlx_whisper") "mlx_whisper")
                        input-file
                        "--model" my-whisper-mlx-model
                        "--output-dir" dir
                        "--output-name" base
                        "--output-format" "txt"
                        "--verbose" "False"
                        "--task" (if whisper-translate "translate" "transcribe"))
                  (unless (string-equal whisper-language "auto")
                    (list "--language" whisper-language))
                  my-whisper-mlx-extra-flags)))
      (list "sh" "-c"
            (concat (mapconcat #'shell-quote-argument args " ")
                    " && cat " (shell-quote-argument txt)))))

  (defun my-whisper-mlx-model-installed-p ()
    "Return non-nil if `my-whisper-mlx-model' is available locally.
True for an explicit local directory path, or a populated Hugging Face
cache entry for the repo id."
    (or (file-directory-p my-whisper-mlx-model)
        (let* ((hf-home (or (getenv "HF_HOME")
                            (expand-file-name "~/.cache/huggingface")))
               (snapshots (expand-file-name
                           (format "hub/models--%s/snapshots"
                                   (replace-regexp-in-string "/" "--" my-whisper-mlx-model))
                           hf-home)))
          (and (file-directory-p snapshots)
               (directory-files snapshots nil "\\`[^.]" t)))))

  ;; Mirror whisper.el's "model isn't available, download now?" prompt for the
  ;; mlx_whisper model, so the first run doesn't silently stall on a large
  ;; Hugging Face download.
  (defun my-whisper-mlx-ensure-model ()
    "Offer to download `my-whisper-mlx-model' if it isn't installed yet.
Downloads via uv: \"uvx --from huggingface_hub hf download REPO\"."
    (unless (my-whisper-mlx-model-installed-p)
      (if (yes-or-no-p
           (format "mlx_whisper model %S isn't installed, download now? "
                   my-whisper-mlx-model))
          (let* ((uvx (or (executable-find "uvx")
                          (error "uvx not found; install uv")))
                 (cmd (format "%s --from huggingface_hub hf download %s"
                              (shell-quote-argument uvx)
                              (shell-quote-argument my-whisper-mlx-model))))
            (compile cmd)
            (error "Downloading %s; re-run whisper (s-R) when it finishes"
                   my-whisper-mlx-model))
        (error "mlx_whisper model %S not installed" my-whisper-mlx-model))))

  (add-hook 'whisper-before-transcription-hook #'my-whisper-mlx-ensure-model)

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

(defcustom rk/default-audio-device nil
  "The default audio device to use for whisper.el and other audio processes."
  :type 'natnum)

(defun rk/find-device-matching (string type)
  "Get the devices from `rk/get-ffmpeg-device' and look for a device
matching `STRING'. `TYPE' can be :video or :audio."
  (let* ((devices (rk/get-ffmpeg-device))
         (device-list (if (eq type :video)
                          (car devices)
                        (cadr devices))))
    (message "Available %s devices: %s"
             (if (eq type :video) "video" "audio")
             (mapconcat (lambda (d) (format "[%s] %s" (car d) (cdr d)))
                        device-list ", "))
    (cl-loop for device in device-list
             when (string-match-p string (cdr device))
             return (car device))))

(defun rk/select-default-audio-device (&optional device-name)
  "Interactively select an audio device to use for whisper.el and other audio processes.
If `DEVICE-NAME' is provided, it will be used instead of prompting the user."
  (interactive)
  (let* ((audio-devices (cadr (rk/get-ffmpeg-device)))
         (indexes (mapcar #'car audio-devices))
         (names (mapcar #'cdr audio-devices))
         (name (or device-name (completing-read "Select audio device: " names nil t))))
    (setq rk/default-audio-device (rk/find-device-matching name :audio))
    (when (boundp 'whisper--ffmpeg-input-device)
      (setq whisper--ffmpeg-input-device (format ":%s" rk/default-audio-device)))))

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


;;* claude-code
;; Deferred via :commands.  inheritenv comes in as a claude-code dependency.
(use-package claude-code
  :commands (claude-code claude-code-transient claude-code-start-in-directory)
  :config
  (setq claude-code-terminal-backend 'ghostel)

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
