;;; custom.el  -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0f220ea77c6355c411508e71225680ecb3e308b4858ef6c8326089d9ea94b86f"
     "1b623b81f373d49bcf057315fe404b30c500c3b5a387cf86c699d83f2f5763f4"
     "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563"
     "5ec088e25ddfcfe37b6ae7712c9cb37fd283ea5df7ac609d007cafa27dab6c64" default))
 '(package-selected-packages
   '(ace-window activities affe ag aggressive-indent aidermacs avy-menu avy-zap
                bufler cape casual claude-code consult-dir consult-notes
                consult-reftex consult-todo corfu cperl-mode crux csharp-mode
                ctrlf deadgrep dictionary diff-hl dired-hacks dired-narrow
                dired-preview dired-ranger dired-recent diredfl dogears
                dwim-shell-command easy-kill-extras eat editorconfig
                elfeed-goodies elfeed-org elisp-def elixir-ts-mode
                emacs-everywhere emacs-slack embark-consult embrace emms
                exec-path-from-shell expreg faceup fancy-dabbrev fix-word fzf
                ghostel goggles goto-chg gptel gptel-aibo gptel-prompts
                grab-mac-link helpful highlight-defined highlight-quoted
                homebrew hungry-delete imenu-list inheritenv javelin jinx
                json-snatcher keyfreq kind-icon lambda-line lambda-themes
                less-css-mode link-hint lua-mode marginalia markdown-table-wrap
                markdown-ts-mode mcp md-ts-mode meow mixed-pitch move-text
                mu4e-column-faces mu4e-query mwim nerd-icons-completion
                nerd-icons-corfu nerd-icons-dired nov olivetti org org-appear
                org-autolist org-bookmark-heading org-contacts org-download
                org-modern org-mru-clock org-msg org-pomodoro org-ql org-roam
                org-sticky-header org-timeblock org-transclusion org-web-tools
                origami ox-pandoc pdf-tools peg phscroll popper popwin prism
                puni ragmacs rainbow-delimiters reveal-in-osx-finder
                revert-buffer-all rg semext slack substitute svg-tag-mode
                tabspaces taxy-magit-section timeout tramp trashed ultra-scroll
                vdiff-magit verilog-mode vertico visual-regexp-steroids
                wallpaper web-server which-key whisper yasnippet-snippets))
 '(package-vc-selected-packages
   '((phscroll :vc-backend Git :url "https://github.com/misohena/phscroll")
     (lambda-line :url "https://codeberg.org/Lambda-Emacs/lambda-line" :branch
                  "main")
     (homebrew :url "https://github.com/jdormit/homebrew.el")
     (emacs-slack :url "https://github.com/emacs-slack/emacs-slack")
     (semext :url "https://github.com/ahyatt/semext/" :branch "master")
     (ragmacs :url "https://github.com/positron-solutions/ragmacs" :branch
              "master")
     (gptel-prompts :url "https://github.com/jwiegley/gptel-prompts" :branch
                    "master")
     (mu4e-query :url "https://github.com/mickeynp/mu4e-query")
     (consult-reftex :url "https://github.com/karthink/consult-reftex" :branch
                     "main")
     (dired-hacks :url "https://github.com/Fuco1/dired-hacks")
     (lambda-themes :url "https://github.com/Lambda-Emacs/lambda-themes" :branch
                    "main")))
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook
           (lambda nil
             (let
                 ((stamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
                  (case-fold-search t))
               (save-excursion
                 (goto-char (point-min))
                 (unless (re-search-forward "^#\\+CREATED:" nil t)
                   (goto-char (point-min))
                   (if (re-search-forward "^#\\+DATE:.*$" nil t) (end-of-line)
                     (goto-char (point-min)))
                   (insert "\12#+CREATED: " stamp))
                 (goto-char (point-min))
                 (if (re-search-forward "^#\\+LAST_MODIFIED:.*$" nil t)
                     (replace-match (concat "#+LAST_MODIFIED: " stamp) t t)
                   (if (re-search-forward "^#\\+CREATED:.*$" nil t)
                       (end-of-line)
                     (goto-char (point-min)))
                   (insert "\12#+LAST_MODIFIED: " stamp)))))
           nil t)
     (org-archive-location . "../archive.org::* Archived from job-targets.org")
     (org-archive-location . "archive.org::* Archived from archive.org")
     (org-archive-save-context-info time file olpath category todo itags)
     (org-archive-location . "archive.org::* Archived from next-steps.org")
     (line-spacing . 0.3) (reftex-default-bibliography "bibliography.bib")
     (line-spacing . 0.5) (olivetti-mode . -1)
     (org-duration-format quote (("h" . t) (special . 2))) (eval valign-mode t)
     (eval and (fboundp 'gptel-mode) (gptel-mode 1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
