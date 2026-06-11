;;; custom.el  -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1b623b81f373d49bcf057315fe404b30c500c3b5a387cf86c699d83f2f5763f4" default))
 '(package-selected-packages
   '(avy-zap claude-code claude-code-ide consult-reftex gptel gptel-aibo
             gptel-prompts highlight-sexp inheritenv jinx mcp mu4e-query
             ob-applescript org-autolist org-modern org-msg org-timeblock
             ragmacs ultra-scroll whisper))
 '(package-vc-selected-packages
   '((phscroll :vc-backend Git :url "https://github.com/misohena/phscroll")
     (lambda-line :url "https://github.com/Lambda-Emacs/lambda-line" :branch
                  "main")
     (homebrew :url "https://github.com/jdormit/homebrew.el")
     (emacs-slack :url "https://github.com/emacs-slack/emacs-slack")
     (claude-code-ide :url "https://github.com/manzaltu/claude-code-ide.el")
     (semext :url "https://github.com/ahyatt/semext/" :branch "master")
     (ragmacs :url "https://github.com/positron-solutions/ragmacs" :branch
              "master")
     (gptel-prompts :url "https://github.com/jwiegley/gptel-prompts" :branch
                    "master")
     (mu4e-query :url "https://github.com/mickeynp/mu4e-query")
     (consult-reftex :url "https://github.com/karthink/consult-reftex" :branch
                     "main")
     (dired-hacks :url "https://github.com/Fuco1/dired-hacks")
     (highlight-sexp :url "https://github.com/daimrod/highlight-sexp")
     (lambda-themes :url "https://github.com/Lambda-Emacs/lambda-themes" :branch
                    "main")))
 '(safe-local-variable-values
   '((line-spacing . 0.3) (reftex-default-bibliography "bibliography.bib")
     (line-spacing . 0.5) (olivetti-mode . -1)
     (org-duration-format quote (("h" . t) (special . 2))) (eval valign-mode t)
     (eval and (fboundp 'gptel-mode) (gptel-mode 1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
