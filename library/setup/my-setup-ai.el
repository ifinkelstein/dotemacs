;;; my-setup-ai.el ---Setup for packages that interact with AI/ML models   -*- lexical-binding: t -*-
;;; Commentary:

;; Setting up AI modules

;;; Code:
(message "Setting up AI packages...")

;;;; GPTel
(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel--debug nil) ;; turn to "t" if I need to see the error message
  (gptel-expert-commands t) ;; turn on additional 'expert' commands
  :config
  ;; change the transient menu to reduce strokes for model switching
  ;; (transient-suffix-put 'gptel-menu (kbd "-m") :key "l")

  ;; useful directives/persona


  ;; gptel helper functions

  (defun my-convert-pdf-to-org-and-open ()
    "If current buffer is a PDF, convert it to plain text using pdftotext and open it with .org extension.
The goal is to use this with GPTel to query PDF files via an LLM."q
    (interactive)
    (cond ((derived-mode-p 'pdf-view-mode) ;; Replace pdf-view-mode with doc-view-mode if necessary
           (let* ((pdf-file (buffer-file-name))
                  (base-name (file-name-sans-extension pdf-file))
                  (org-file (concat base-name ".org"))
                  (command (format "pdftotext '%s' '%s'"
                                   (shell-quote-argument pdf-file)
                                   (shell-quote-argument org-file))))
             (unless (executable-find "pdftotext")
               (error "The pdftotext command is not available"))
             (if (and pdf-file (file-exists-p pdf-file))
                 (progn
                   ;; Execute pdftotext command
                   (message "Converting '%s' to text..." pdf-file)
                   (shell-command command)
                   (when (file-exists-p org-file)
                     (message "Conversion successful. Opening '%s'..." org-file)
                     (find-file org-file)
                     (goto-char (point-min))
                     (insert ":PROPERTIES: \n"
                             ":GPTEL_MODEL: gpt-4-1106-preview\n"
                             ":GPTEL_BACKEND: ChatGPT\n"
                             ":END:\n"
                             "* Summarizes the top ten findings of the research paper\n"
                             "** Manuscript text:\n")
                     (goto-char (point-max))))
               (error "No file associated with this buffer or file doesn't exist")))
           (t
            (error "Not in a PDF buffer!")))))

  ;; useful quick LLM calls
  (defun my-gptel-reply-to-message (prompt)
    "Prompt the user for input and sends it along with the current buffer's contents to GPT-4 for a reply.
PROMPT is the input provided by the user. The model 'gpt-4-1106-preview' is used for generating a professional and concise response, which is then saved to a new file in '~/Downloads'."
    (interactive "sPrompt (v1-v5; e1-e5): ")
    (setq gptel-model "gpt-4-1106-preview")
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

  (defun my-gptel-ical-from-message ()
    " Create an iCalendar (RFC 5545) file from a buffer, such as a mu4e thread.

This function prompts the user to save a message as an iCalendar (RFC 5545)
file. It extracts the necessary details, such as the meeting date, time, and
location, from the message and generates an iCalendar file compliant with RFC
5545 format. It assumes default values for missing information, such as assuming
a central time zone (Chicago) and the current year if not explicitly specified.
The function also includes the option to save the file and open it with the
system's default application."

    (interactive)
    (setq gptel-model "gpt-4-0125-preview")
    (message (concat "Sending query using " gptel-model))
    (gptel-request
     (concat "The text below is about scheduling a meeting. Summarize the following text in an RFC5545 compliant text response with the meeting date, time, and location. Do not add any additional characters to the top or last line of the response. Again, the response should be able to be parsed by any RFC5545 compliant calendar software. If the meeting is via zoom, include a zoom link only when it was mentioned. Include a time zone. If no time zone is defined, assume central time (Chicago). If no year is defined, assume this year. The host or organizer should be Ilya Finkelstein with the email ilya@finkelsteinlab.org. In the notes field, include a concise synopsis of the topic of the meeting. Limit the synopsis to three sentences, maximum. Start the synopsis with [Generated by ChatGPT on "
             (format-time-string "%Y-%m-%d") "]\n"
             "TEXT TO SUMMARIZE: "
             (buffer-string)
             )
     :buffer (current-buffer)
     :system   "You are a helpful executive assistant. Reply only in an iCalendar (RFC 5545)-compatible file"
     :callback
     (lambda (response info)
       (if (not response)
           (message "gptel-quick failed with message: %s" (plist-get info :status))
         (let ((filename (concat "~/Downloads/" (format-time-string "%Y-%m-%d_%H-%M-%S") ".ics")))
           (switch-to-buffer (generate-new-buffer filename))
           (insert response)
           (write-file filename)
           (when (y-or-n-p "Save to calendar? ")
             (let ((program (cdr (assoc system-type '((darwin . "open") (windows-nt . "start") (gnu/linux . "xdg-open"))))))
               (when program
                 (shell-command (concat program " " filename))))))))))

  (defun gptel-rewrite-and-replace-dictation (bounds &optional directive)
    (interactive
     (list
      (cond
       ((use-region-p) (cons (region-beginning) (region-end)))
       ((derived-mode-p 'text-mode)
        (list (bounds-of-thing-at-point 'sentence)))
       (t (cons (line-beginning-position) (line-end-position))))
      (and current-prefix-arg
           (read-string "ChatGPT Directive: "
                        "You are a prose editor. Rewrite my prompt more professionally."))))
    (gptel-request
     (buffer-substring-no-properties (car bounds) (cdr bounds)) ;the prompt
     :system (or directive "This text is an audio transcript. Re-write to repair grammar, spelling, abbreviations, and other common dictation errors. Remove repeating sentences. Make sure to keep as close to the original as possible in your final output.")
     :buffer (current-buffer)
     :context (cons (set-marker (make-marker) (car bounds))
                    (set-marker (make-marker) (cdr bounds)))
     :callback
     (lambda (response info)
       (if (not response)
           (message "ChatGPT response failed with: %s" (plist-get info :status))
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
               (message "Rewrote line. Original line saved to kill-ring."))))))))
  ) ;; gptel use-package

;;;; whisper local audio to text in Emacs
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; audio-to-text
;; whisper
;; reference to set up whisper and org-ai https://gist.github.com/rksm/04be012be07671cd5e1dc6ec5b077e34
;; example usage:
;; (rk/find-device-matching "FaceTime" :video)
;; (rk/find-device-matching "Macbook Pro Microphone" :audio)
;; (rk/select-default-audio-device)

(use-package whisper
  :vc (:fetcher github :repo natrys/whisper.el)
  :bind ("s-R" . whisper-run)
  :custom
  (whisper-install-directory my-var-dir)
  (whisper-model "base")
  (whisper-language "en")
  (whisper-translate nil)
  (setq whisper--ffmpeg-input-device ":0")
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
;;; Provide AI
(provide 'my-setup-ai)
;;; ai.el ends here
