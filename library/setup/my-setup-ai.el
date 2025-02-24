;; my-setup-ai.el -*- lexical-binding: t -*-
(message "Setting up AI packages...")

;;* GPTel
(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :rev :newest :branch "master")
  :commands (gptel my-gptel-process-message gptel-request)
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-expert-commands t) ;; turn on additional 'expert' commands

  ;; gptel helper functions

  (defun my-gptel-replace-context ()
    "Clear all current gptel context. Use gptel-add to add context from a dired buffer"
    (interactive)
    (gptel-context-remove-all)
    (gptel-context-add))

  ;; useful quick LLM calls
  (defun my-gptel-reply-to-message (prompt)
    "Prompt the user for input and sends it along with the current buffer's contents to GPT-4 for a reply.
PROMPT is the input provided by the user. The model 'gpt-4-1106-preview' is used for generating a professional and concise response, which is then saved to a new file in '~/Downloads'."
    (interactive "sPrompt (v1-v5; e1-e5): ")

    ;; (setq gptel-model 'gemini-exp-1121)
    (message (concat "Sending query using " gptel-model))
    (gptel-request
        (concat "\nRespond using the guidance below as a busy professor. I will specify a few command line-style instructions at the start of my guidance, separated by a ';'. The level of verbosity, {v}, is on a five point scale with 'v1' being very short but polite and 'v5' being very verbose, very polite but not obsequeous. I will use 'v1' to 'v5' notation for verbosity at the start of the guidance below. The use of emojis, {e}, is on a five point scale with e1 means ZERO emojis and e5 means many emojis. Use emojis from the following set: {🦄, 🌈, ☺️, 🙏, ✨, 🌞, 🎉} to highlight positive emotions. This message IS VERY IMPORTANT TO ME so reply AS BEST AS YOU CAN and I will be GRATEFUL AND HAPPY. If you do not do a great job and follow the verbosity tag, I WILL LOSE MY JOB AND MAY DIE. If I do not specify anything, assume 'v1' and 'e1'. I do not include a subject line."
                "\n: Guidance: "
                prompt
                "\n If this is an email, sign off with 'Kind Regards, \nIlya' Respond to the message below:"
                (buffer-string))
      :buffer (current-buffer)
      :system   "You are a helpful executive assistant. Use short, concise, professional, positive tone."
      :callback
      (lambda (response info)
        (if (not response)
            (message "gptel-quick failed with message: %s" (plist-get info :status))
          (let ((filename (concat "~/Downloads/" (format-time-string "%Y-%m-%d_%H-%M-%S") ".org")))
            (switch-to-buffer (generate-new-buffer filename))
            (insert response)
            (kill-new response) ;; save response to kill ring
            (write-file filename))))))


  (defun my-gptel-rewrite-dictation (start end &optional _arg)
    "Use an LLM to rewrite the dictation.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark."
    (interactive (if current-prefix-arg
		             (list (point-min) (point-max) current-prefix-arg)
		           (list (region-beginning) (region-end) nil)))

    (let ((gptel-model "Claude:claude-3-haiku-20240307") ;; earlier model: gpt-4-0125-preview
          (dictation (buffer-substring-no-properties start end)))
      (message (concat "Sending query using " gptel-model))
      (gptel-request
          (concat
           "You are an expert editor specializing in refining voice-dictated text. Your task is to:
             1. Fix grammar and spelling errors
             2. Add proper punctuation and sentence structure
             3. Break run-on sentences into clear, distinct thoughts
             4. Preserve the original meaning and key words
             5. Make minimal changes - only what's necessary for clarity
             6. Maintain the original voice and style

             Rules:
             - Don't add new information or change the meaning
             - Keep the same vocabulary level
             - Preserve technical terms exactly as given
             - Only split sentences when necessary for readability
             - Keep contractions and informal language if present
             - Only return the re-written text. NO EXPLANATIONS OR OTHER TEXT.

             Please edit the following voice-dictated text:"
           dictation)
        :buffer (current-buffer)
        :system   "You are a helpful executive assistant."
        :context (cons (set-marker (make-marker) start)
                       (set-marker (make-marker) end))
        :callback
        (lambda (response info)
          (if (not response)
              (message "LLM response failed with: %s" (plist-get info :status))
            (let* ((bounds (plist-get info :context))
                   (beg (car bounds))
                   (end (cdr bounds))
                   (buf (plist-get info :buffer)))
              (with-current-buffer buf
                (save-excursion
                  (goto-char beg)
                  (kill-region beg end)
                  (insert response)
                  (set-marker beg nil)
                  (set-marker end nil)
                  (message "Rewrote. Original saved to kill-ring."))))))))) ;; my-gptel-rewrite-dictation
  ) ;; gptel use-package

;;** GPTel helper functions
(defun my-remove-triple-ticks (str)
  "Remove all lines that begin with ``` in str, preserving other whitespace."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "^```.*" nil t)
      (replace-match ""))
    (buffer-string)))


;;** GPTEl extending packages
(use-package gptel-aibo
  :vc (:url "https://github.com/dolmens/gptel-aibo" :rev :newest :branch "master")
  :after (gptel)
  :config
  (define-key gptel-aibo-mode-map
              (kbd "C-c /") #'gptel-aibo-apply-last-suggestions))
;;* whisper local audio to text in Emacs
;; example usage:
;; (rk/find-device-matching "FaceTime" :video)
;; (rk/find-device-matching "Macbook Pro Microphone" :audio)
;; (rk/select-default-audio-device)

(use-package whisper
  :vc (:url "https://github.com/natrys/whisper.el" )
  :commands (whisper-run whisper-file rk/select-default-audio-device)
  :bind ("s-R" . whisper-run)
  :config
  (setq whisper-install-directory my-var-dir)
  (setq whisper-model "base")
  (setq whisper-language "en")
  (setq whisper-translate nil)
  (setq whisper--ffmpeg-input-device-name "Macbook Pro Microphone")
  (setq whisper--ffmpeg-input-device (rk/find-device-matching whisper--ffmpeg-input-device-name :audio))
  (setq whisper--ffmpeg-input-format "avfoundation"))

(defcustom rk/default-audio-device nil
  "The default audio device to use for whisper.el and outher audio processes."
  :type 'string)

(defun rk/find-device-matching (string type)
  "Get the devices from `rk/get-ffmpeg-device' and look for a device
matching `STRING'. `TYPE' can be :video or :audio."
  (let* ((devices (rk/get-ffmpeg-device))
         (device-list (if (eq type :video)
                          (car devices)
                        (cadr devices))))
    (cl-loop for device in device-list
             when (string-match-p string (cdr device))
             return (car device))))

(defun rk/find-device-string-matching-number (number type)
  "Get the device string from `rk/get-ffmpeg-device' with device
matching `NUMBER'. `TYPE' can be :video or :audio. Return nil if
'NUMBER' isn't a valid device."
  (let* ((devices (rk/get-ffmpeg-device))
         (device-list (if (eq type :video)
                          (car devices)
                        (cadr devices))))
    ;; (print device-list)
    ;; (print (nth number device-list))
    (when (<= number (length device-list))
      (alist-get number device-list))))

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

;;* provide
(provide 'my-setup-ai)
;; my-setup-ai.el ends here
