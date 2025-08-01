;;; my-setup-shell.el -*- lexical-binding: t -*-

(message "Setting up the shells...")

;;** dwim-shell-command
;; convenient commands for working with the CLI
(use-package dwim-shell-command
  :defer 30 ;; wait to load, but I want the dwim commands available
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  :config

  (defun dwim-shell-split-cue-to-flac ()
    "Convert all selectede cue files to FLAC. 

This function uses xld to convert marked cue files to FLAC

The function operates on marked files in Dired mode or the current file when
called interactively. Original files are preserved.
"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert cue to FLAC"
     "xld -f flac --profile 'ilya' -c '<<fne>>.cue' <<f>>"
     :utils "xld"))
  
  (defun dwim-shell-command-convert-movie-mp4 ()
    "Convert and compress video files to MP4 format using H.265 codec.

This function uses FFmpeg to convert marked video files to MP4 format with
H.265 (libx265) encoding for optimal compression. The conversion uses:
- CRF (Constant Rate Factor) of 25 for good quality/size balance
- 'slow' preset for better compression efficiency at the cost of encoding time
- Output files are named with '_compressed' suffix

The function operates on marked files in Dired mode or the current file when
called interactively. Original files are preserved.

Requires FFmpeg to be installed and available in PATH.

Example output: 'movie.avi' becomes 'movie_compressed.mp4'"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Compresses MP4s with libx265 using slow preset to bring down size"
     "ffmpeg -i '<<f>>' -c:v libx265 -crf 25 -preset slow '<<fne>>_compressed.mp4'"
     :utils "ffmpeg"))

  ;; Based on
  ;; https://apps.bram85.nl/git/bram/gists/src/commit/31ac3363da925daafa2420b7f96c67612ca28241/gists/dwim-0x0-upload.el
  (defun dwim-shell-commands-upload-to-0x0 ()
    "Upload the marked files to 0x0.st"
    (interactive)
    (dwim-shell-command-on-marked-files
     "0x0 upload"
     "curl -Ffile=@<<f>> -Fsecret= https://0x0.st"
     :utils "curl"
     :post-process-template
     ;; Insert the single quotes at the appropriate place according to
     ;; 0x0.st example online:
     ;; curl -F'file=@yourfile.png' -Fsecret= https://0x0.st
     ;;
     ;; The placement of these single quotes confuse the escaping
     ;; mechanisms of dwim-shell-command, as it considers @ as the
     ;; opening 'quote' as it appears right in front of <<f>>.
     (lambda (template path)
       (string-replace "-Ffile" "-F'file"
                       (string-replace path (concat path "'") template)))
     :on-completion
     (lambda (buffer process)
       (if (= (process-exit-status process) 0)
           (with-current-buffer buffer
             (let ((url (car (last (split-string (string-trim (buffer-string)) "\n")))))
               (eww url)
               (kill-new url)
               (message "Copied: %s" (current-kill 0)))
             (kill-buffer buffer))
         (switch-to-buffer buffer)))))

  (defun dwim-shell-commands-video-to-h265 ()
    "Convert videos to h.265 format. Useful for compressing."
    (interactive)
    (dwim-shell-command-on-marked-files
     "video to h.265 compressed format"
     "ffmpeg -i '<<f>>' -c:v libx265 -crf 28 -c:a aac -b:a 128k '<<fne>>.mp4'"
     :utils "ffmpeg"))
  
  (defun dwim-shell-commands-docx-to-org ()
    "Convert docx(s)) to org."
    (interactive)
    (dwim-shell-command-on-marked-files
     "docx to org"
     "pandoc --extract-media=. --from=docx --to=org '<<f>>' > '<<fne>>.org'"
     :extensions "docx"
     :utils "pandoc"))

  (defun dwim-shell-commands-docx-to-pdf ()
    "Convert docx(s) to pdf (via latex)."
    (interactive)
    (dwim-shell-command-on-marked-files
     "docx to pdf (via latex)"
     "pandoc -t latex '<<f>>' -o '<<fne>>.pdf'"
     :extensions "docx" ;; brew install mactex
     :utils "pdflatex"))

  (defun dwim-shell-commands-xls-to-csv ()
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert" "ssconvert '<<f>>' '<<fne>>.csv'"
     :utils "ssconvert"))

  (defun dwim-shell-commands-unzip ()
    "Unzip all marked archives (of any kind) using `atool'."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Unzip" "atool --extract --explain '<<f>>'"
     :utils "atool"))

  (defun dwim-shell-commands-zip ()
    "Zip all marked files into archive.zip."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Zip" (if (eq 1 (seq-length (dwim-shell-command--files)))
               "zip -r '<<fne>>.<<e>>' '<<f>>'"
             "zip -r '<<archive.zip(u)>>' '<<*>>'")
     :utils "zip"))

  (defun dwim-shell-command-print ()
    "Convert selected files to PDF (pandoc: org, md; soffice: others)
Spool to default printer using lp with double-sided printing on."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert to PDF via soffice and print file(s) to default printer."
     "if [[ '<<e>>' == 'org' || '<<e>>' == 'md' ]]; then
    pandoc '<<f>>' -o '<<fne>>.pdf'
  elif [[ '<<e>>' == 'pdf' ]]; then
  else
    soffice --headless --convert-to pdf '<<f>>'
  fi
    lp -o sides=two-sided-long-edge '<<fne>>.pdf'
"
     :utils '("lp" "soffice" "pandoc")))

  (defun dwim-shell-command-ai-to-pdf ()
    "Convert AI files to PDF."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert AI to PDF"
     "gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=\"<<fne>>.pdf\" \"<<f>>\""
     :utils "gs"))

  (defun dwim-shell-command-split-FLAC ()
    "Split a FLAC album file into tracks using xld and a CUE file in the same folder."
    (interactive)

    (dwim-shell-command-on-marked-files
     "Split FLAC album file into tracks"
     "xld -c \"<<fne>>.cue\" -f FLAC \"<<f>>\""
     :utils "xld"))

  (defun dwim-shell-command-reduce-PDF-size ()
    "Reduce PDF file size; images downsampled to 150 dpi.

If this doesn't work, then the file can be reduced further in Adobe Acrobat."
    (interactive)
    ;; https://askubuntu.com/questions/113544/how-can-i-reduce-the-file-size-of-a-scanned-pdf-file
    (dwim-shell-command-on-marked-files
     "Reduce PDF file size"
     "  gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile=\"<<fne>>.reduced.pdf\" \"<<f>>\""
     :utils "gs"))

  (defun dwim-shell-commands-converst-to-docx ()
    "Convert file(s) to docx using custom template."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert to DOCX"
     "pandoc --reference-doc '/Users/ilya/Work/80-89 other writing/80 letter-templates/generic-word-template.docx' -i '<<f>>' -o '<<fne>>.docx'"
     :utils "pandoc"))

  (defun dwim-shell-commands-pdf-to-txt ()
    "Convert pdf to txt."
    (interactive)
    (dwim-shell-command-on-marked-files
     "pdf to txt"
     "pdftotext -layout '<<f>>' '<<fne>>.txt'"
     :utils "pdftotext"))

  (defun dwim-shell-commands-files-combined-size ()
    "Get files combined file size."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Get files combined file size"
     "du -csh '<<*>>'"
     :utils "du"
     :on-completion (lambda (buffer _process)
                      (with-current-buffer buffer
                        (message "Total size: %s"
                                 (progn
                                   (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
                                   (match-string 1))))
                      (kill-buffer buffer))))

  (defun dwim-shell-command-rename-with-date ()
    "Rename the file with the current date between the filename and extension"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Rename with date"
     (concat "mv \"<<f>>\" \"<<fne>>." (format-time-string "%Y%m%d") ".<<e>>\" ")
     :utils "mv"))
  ) ;;dwim-shell-command
;;* eshell and helpers

;;** eat

(use-package eat
  :ensure t
  ;; :custom
  ;; (eat-term-name "xterm")
  :custom-face
  (ansi-color-bright-blue ((t (:foreground "#00afff" :background "#00afff"))))
  :config
  (eat-eshell-mode)
  (setq eshell-visual-commands '())
  (eat-eshell-visual-command-mode))

;;* vterm and helpers
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :commands vterm-toggle)

;; Managing multiple vterm buffers
(use-package multi-vterm
  :after (vterm))

;;* Tramp
;; An easy way to manage files over ssh/scp
(use-package tramp
  :ensure nil
  :defer 1
  :config
  (setq tramp-persistency-file-name (concat my-cache-dir "tramp")
        ;; the most reliable tramp setup I have found (used at work every day...)
        tramp-default-method "ssh"
        tramp-copy-size-limit nil
        tramp-use-ssh-controlmaster-options nil))

;; I recommend the following ~/.ssh/config settings be used with the tramp settings in this cfg:
;; Host *
;; ForwardAgent yes
;; AddKeysToAgent yes
;; ControlMaster auto
;; ControlPath ~/.ssh/master-%r@%h:%p
;; ControlPersist yes
;; ServerAliveInterval 10
;; ServerAliveCountMax 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-setup-shell)
