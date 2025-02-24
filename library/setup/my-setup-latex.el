;; my-setup-latex.el  -*- lexical-binding: t -*-

;; TODO: clean this up later -- just need it working now
(message "Setting up LaTeX settings...")
;;* Latex Packages
;; Basic settings
;; inspiration: https://jwiegley.github.io/use-package/keywords/#defer-demand
(use-package latex
  :ensure auctex
  ;; :demand ;; load everything immediately. otherwise LaTeX mode doesn't get recognized. maybe?
  :mode ("\\.tex\\'" . LaTeX-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode
                        LaTeX-environment TeX-insert-macro
                        LaTeX-narrow-to-environment
                        TeX-fold-dwim
                        TeX-fold-buffer)
  
  :hook ((latex-mode . LaTeX-mode) ;; absurd that this needs to be added
         (LaTeX-mode . variable-pitch-mode)
         (LaTeX-mode . LaTeX-preview-setup)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . outline-minor-mode) ;; clobbers TAB expansion of yas-snippets
         (LaTeX-mode . electric-pair-mode)
         (LaTeX-mode . olivetti-mode)
         (LaTeX-mode . hl-todo-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . (lambda ()
                         (TeX-fold-mode 1))) ;; enable hiding various things
         (LaTeX-mode . electric-indent-local-mode)) ;; trying to see if I like this mode
  :mode ("\\.tex\\'" . latex-mode)
  ;; introduce dummy variables to silence compile and other warnings
  ;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-latex.el
  :defines (TeX-auto-save
            TeX-parse-self
            TeX-electric-escape
            TeX-PDF-mode
            TeX-source-correlate-method
            TeX-newline-function
            TeX-view-program-list
            TeX-view-program-selection
            TeX-mode-map)
  :bind (:map LaTeX-mode-map
              ("C-M-u" . LaTeX-backward-up-list)
              ("C-M-e" . LaTeX-forward-environment)
              ("C-M-a" . LaTeX-backward-environment)
              ("M-RET" . LaTeX-insert-item)
              ("s-u" . (lambda () (interactive) (yas-expand-snippet (yas-lookup-snippet "underline"))))
              ("s-b" . (lambda () (interactive) (yas-expand-snippet (yas-lookup-snippet "textbf"))))
              ("s-i" . (lambda () (interactive) (yas-expand-snippet (yas-lookup-snippet "emph"))))
              ("s-h" . (lambda () (interactive) (yas-expand-snippet (yas-lookup-snippet "highlight"))))
              ("s-$" . (lambda () (interactive) (yas-expand-snippet (yas-lookup-snippet "inline math"))))
              ("s-d" . my-TeX-delete-current-macro)
              ("M-i" . my-tab-to-tab-stop)
              ("M-I" . my-tab-to-tab-stop-back)
              ("C-)" . puni-slurp-forward)
              ("C-(" . puni-slurp-backward)
              ("M-," . embark-act)
              ("M-." . embark-dwim))

  :custom
  ;; https://emacs.stackexchange.com/questions/3083/how-to-indent-items-in-latex-auctex-itemize-environments
  (LaTeX-indent-level 4) ;; set reasonable indentation for lists
  (LaTeX-item-indent -2) ;; set reasonable indentation for lists

  (TeX-error-overview-open-after-TeX-run nil) ; do not open the error overview automatically after running TeX.

  (TeX-parse-self t) ;; this should auto-detect when biber is needed for C-c C-a
  (TeX-electric-escape nil) ; if true, offer auto-completion when I type /
  ;; for navigation menu
  (reftex-toc-split-windows-fraction 0.35)
  (reftex-toc-split-windows-horizontally t)

  ;; disable reftex from prompting for how to cite
  (reftex-ref-macro-prompt nil)

  ;; fold blocks between comments using outline-minor-mode in TeX-mode
  (TeX-outline-extra
   '(("%%" 1)
     ("%%%" 2)
     ("%%%%" 3)
     ("%%%%%" 4)
     ("%chapter" 1)
     ("%section" 2)
     ("%subsection" 3)
     ("%subsubsection" 4)
     ("%paragraph" 5)))

  ;; outline-minor-mode settings
  (outline-minor-mode-cycle t)

  ;; add font locking to the headers
  (font-lock-add-keywords
   'latex-mode
   '(("^%\\(chapter\\|\\(sub\\|subsub\\)?section\\|paragraph\\)"
      0 'font-lock-keyword-face t)
     ("^%chapter{\\(.*\\)}"       1 'font-latex-sectioning-1-face t)
     ("^%section{\\(.*\\)}"       1 'font-latex-sectioning-2-face t)
     ("^%subsection{\\(.*\\)}"    1 'font-latex-sectioning-3-face t)
     ("^%subsubsection{\\(.*\\)}" 1 'font-latex-sectioning-4-face t)
     ("^%paragraph{\\(.*\\)}"     1 'font-latex-sectioning-5-face t)))

  ;; click on a PDF to see the TeX source
  (TeX-source-correlate-mode t)
  ;; (setq-default TeX-source-correlate-start-server t)

  (TeX-newline-function 'reindent-then-newline-and-indent)

  ;; this sets the TeX engine to luatex for new plugins
  ;; this seems to work by using PdfLatex engine
  (TeX-engine 'luatex)

  (TeX-auto-save t) ;; save style info w/ buffer (?)
  (TeX-save-query nil)
  (TeX-PDF-mode t)
  (TeX-master nil)
  :config
  ;; add additional useful macro commands
  (TeX-add-symbols '("texttt" t)) ;; typewriter mode macro with single input



  (TeX-source-correlate-mode) ;; show where the errors are
  (advice-add 'TeX-view :around #'my-widen-first) ; fixes bug in TeX-view
  (put 'LaTeX-narrow-to-environment 'disabled nil) ;; disable warning when using this function
  (add-to-list 'TeX-file-extensions "tex\\.~[0-9a-f]+~") ;; for backup files too

   ;;;; Helper functions
  ;; source: https://emacs.stackexchange.com/questions/6045/how-to-delete-a-latex-macro-while-preserving-its-text-content/7997#7997
  ;; more useful than C-c C-f C-d
  (defun my-TeX-delete-current-macro (&optional arg)
    "Remove the current macro.
With an optional argument ARG, delete just the ARG-th macro
starting from the innermost."
    (interactive "*p")
    (let (macro end)
      (when
          (dotimes (i arg macro)
            (goto-char (TeX-find-macro-start))
            (setq macro (TeX-current-macro)
                  end (TeX-find-macro-end))
            ;; If we need to look for an outer macro we have to "exit" from the
            ;; current one.
            (backward-char))
        ;; Return to the beginning of the macro to be deleted.
        (forward-char)
        (re-search-forward
         (concat (regexp-quote TeX-esc) macro "\\(?:\\[[^]]*\\]\\)?"
                 TeX-grop "\\(\\(.\\|\n\\)*\\)")
         end t)
        (replace-match "\\1")
        ;; Delete the closing brace.
        (delete-backward-char 1))))

  ;; citation for the two functions below:https://www.reddit.com/r/emacs/comments/5f99nv/help_with_auctex_how_to_delete_an_environment/
  (defun my-LaTeX-delete-macro ()
    "Remove current macro and return `t'.  If no macro at point,
return `nil'."
    (interactive)
    (when (TeX-current-macro)
      (let ((bounds (TeX-find-macro-boundaries))
            (brace  (save-excursion
                      (goto-char (1- (TeX-find-macro-end)))
                      (TeX-find-opening-brace))))
        (delete-region (1- (cdr bounds)) (cdr bounds))
        (delete-region (car bounds) (1+ brace)))
      t))

  (defun my-LaTeX-delete-environment ()
    (interactive)
    (when (LaTeX-current-environment)
      (save-excursion
        (let* ((begin-start (save-excursion
                              (LaTeX-find-matching-begin)
                              (point)))
               (begin-end (save-excursion
                            (goto-char begin-start)
                            (search-forward-regexp "begin{.*?}")))
               (end-end (save-excursion
                          (LaTeX-find-matching-end)
                          (point)))
               (end-start (save-excursion
                            (goto-char end-end)
                            (1- (search-backward-regexp "\\end")))))
          ;; delete end first since if we delete begin first it shifts the
          ;; location of end
          (delete-region end-start end-end)
          (delete-region begin-start begin-end)))))

  ;; view generated PDF with `pdf-tools'.
  (unless (assoc "PDF Tools" TeX-view-program-list)
    (add-to-list 'TeX-view-program-list
                 '("PDF Tools" TeX-pdf-tools-sync-view)))
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "PDF Tools"))) ;;auctex use-package
;;** reftex
(use-package reftex
  :after auctex
  :commands (turn-on-reftex reftex-citation reftex-reference reftex-toc)
  :config
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-insert-label-flags '("sf" "sfte"))
  ;; (setq reftex-ref-style-default-list '("Default" "AMSMath" "Cleveref"))
  (setq reftex-use-multiple-selection-buffers t))

;;** latex-change-env
(use-package latex-change-env
  :after latex
  ;; :bind (:map LaTeX-mode-map ("C-c r" . latex-change-env))
  )


;;** consult-reftex
(use-package consult-reftex
  :vc (:url "https://github.com/karthink/consult-reftex" :branch "main" :rev :newest)
  :after (reftex consult embark)
  :bind (:map reftex-mode-map
              ("C-c )"   . consult-reftex-insert-reference)
              ("C-c M-." . consult-reftex-goto-label)
              :map org-mode-map
              ("C-c (" . consult-reftex-goto-label)
              ("C-c )"   . consult-reftex-insert-reference))
  :config
  (setq consult-reftex-preview-function
        #'consult-reftex-make-window-preview
        consult-reftex-preferred-style-order
        '("\\eqref" "\\ref"))
  (consult-customize consult-reftex-insert-reference
                     :preview-key (list :debounce 0.3 'any)))
;;** bibtex
(use-package bibtex
  :after auctex
  :defer t
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))

(with-eval-after-load 'font-latex
  (set-face-attribute 'font-latex-sedate-face nil :inherit 'fixed-pitch)
  (set-face-attribute 'font-latex-math-face nil :inherit 'fixed-pitch))

;;** ox-latex
;; https://jakebox.github.io/youtube/org_latex_video.html
(with-eval-after-load 'ox-latex
  (setq org-latex-compiler "lualatex") ;; change org-latex output. Also check org-latex-to-pdf-process
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;;* Prettify latex
;; https://github.com/karthink/.emacs.d/blob/master/lisp/pretty-latex.el
(add-hook 'prettify-symbols-mode-hook
          (defun prettify-symbols-latex-symbols ()
            "List of pretty symbols for latex-mode"
            (interactive)
            (setq-local prettify-symbols-alist '(("$" . 183)
                                                 ("\\alpha" . 945)
                                                 ("\\beta" . 946)
                                                 ("\\gamma" . 947)
                                                 ("\\delta" . 948)
                                                 ("\\epsilon" . 1013)
                                                 ("\\zeta" . 950)
                                                 ("\\eta" . 951)
                                                 ("\\theta" . 952)
                                                 ("\\iota" . 953)
                                                 ("\\kappa" . 954)
                                                 ("\\lambda" . 955)
                                                 ("\\mu" . 956)
                                                 ("\\nu" . 957)
                                                 ("\\xi" . 958)
                                                 ("\\pi" . 960)
                                                 ("\\rho" . 961)
                                                 ("\\sigma" . 963)
                                                 ("\\tau" . 964)
                                                 ("\\upsilon" . 965)
                                                 ("\\phi" . 981)
                                                 ("\\chi" . 967)
                                                 ("\\psi" . 968)
                                                 ("\\omega" . 969)
                                                 ("\\Gamma" . 915)
                                                 ("\\Delta" . 916)
                                                 ("\\Lambda" . 923)
                                                 ("\\Phi" . 934)
                                                 ("\\Pi" . 928)
                                                 ("\\Psi" . 936)
                                                 ("\\Sigma" . 931)
                                                 ("\\Theta" . 920)
                                                 ("\\Upsilon" . 933)
                                                 ("\\Xi" . 926)
                                                 ("\\Omega" . 937)
                                                 ("\\Box" . 9633)
                                                 ("\\Bumpeq" . 8782)
                                                 ("\\Cap" . 8914)
                                                 ("\\Cup" . 8915)
                                                 ("\\Diamond" . 9671)
                                                 ("\\Downarrow" . 8659)
                                                 ("\\H{o}" . 337)
                                                 ("\\Im" . 8465)
                                                 ("\\Join" . 8904)
                                                 ("\\Leftarrow" . 8656)
                                                 ("\\Leftrightarrow" . 8660)
                                                 ("\\Ll" . 8920)
                                                 ("\\Lleftarrow" . 8666)
                                                 ("\\Longleftarrow" . 8656)
                                                 ("\\Longleftrightarrow" . 8660)
                                                 ("\\Longrightarrow" . 8658)
                                                 ("\\Lsh" . 8624)
                                                 ("\\Re" . 8476)
                                                 ("\\Rightarrow" . 8658)
                                                 ("\\Rrightarrow" . 8667)
                                                 ("\\Rsh" . 8625)
                                                 ("\\Subset" . 8912)
                                                 ("\\Supset" . 8913)
                                                 ("\\Uparrow" . 8657)
                                                 ("\\Updownarrow" . 8661)
                                                 ("\\Vdash" . 8873)
                                                 ("\\Vert" . 8214)
                                                 ("\\Vvdash" . 8874)
                                                 ("\\aleph" . 8501)
                                                 ("\\amalg" . 8720)
                                                 ("\\angle" . 8736)
                                                 ("\\approx" . 8776)
                                                 ("\\approxeq" . 8778)
                                                 ("\\ast" . 8727)
                                                 ("\\asymp" . 8781)
                                                 ("\\backcong" . 8780)
                                                 ("\\backepsilon" . 8717)
                                                 ("\\backprime" . 8245)
                                                 ("\\backsim" . 8765)
                                                 ("\\backsimeq" . 8909)
                                                 ("\\backslash" . 92)
                                                 ("\\barwedge" . 8892)
                                                 ("\\because" . 8757)
                                                 ("\\beth" . 8502)
                                                 ("\\between" . 8812)
                                                 ("\\bigcap" . 8898)
                                                 ("\\bigcirc" . 9711)
                                                 ("\\bigcup" . 8899)
                                                 ("\\bigstar" . 9733)
                                                 ("\\bigtriangledown" . 9661)
                                                 ("\\bigtriangleup" . 9651)
                                                 ("\\bigvee" . 8897)
                                                 ("\\bigwedge" . 8896)
                                                 ("\\blacklozenge" . 10022)
                                                 ("\\blacksquare" . 9642)
                                                 ("\\blacktriangle" . 9652)
                                                 ("\\blacktriangledown" . 9662)
                                                 ("\\blacktriangleleft" . 9666)
                                                 ("\\blacktriangleright" . 9656)
                                                 ("\\bot" . 8869)
                                                 ("\\bowtie" . 8904)
                                                 ("\\boxminus" . 8863)
                                                 ("\\boxplus" . 8862)
                                                 ("\\boxtimes" . 8864)
                                                 ("\\bullet" . 8226)
                                                 ("\\bumpeq" . 8783)
                                                 ("\\cap" . 8745)
                                                 ("\\cdots" . 8943)
                                                 ("\\centerdot" . 183)
                                                 ("\\checkmark" . 10003)
                                                 ("\\chi" . 967)
                                                 ("\\cdot" . 8901)
                                                 ("\\cdots" . 8943)
                                                 ("\\circ" . 8728)
                                                 ("\\circeq" . 8791)
                                                 ("\\circlearrowleft" . 8634)
                                                 ("\\circlearrowright" . 8635)
                                                 ("\\circledR" . 174)
                                                 ("\\circledS" . 9416)
                                                 ("\\circledast" . 8859)
                                                 ("\\circledcirc" . 8858)
                                                 ("\\circleddash" . 8861)
                                                 ("\\clubsuit" . 9827)
                                                 ("\\coloneq" . 8788)
                                                 ("\\complement" . 8705)
                                                 ("\\cong" . 8773)
                                                 ("\\coprod" . 8720)
                                                 ("\\cup" . 8746)
                                                 ("\\curlyeqprec" . 8926)
                                                 ("\\curlyeqsucc" . 8927)
                                                 ("\\curlypreceq" . 8828)
                                                 ("\\curlyvee" . 8910)
                                                 ("\\curlywedge" . 8911)
                                                 ("\\curvearrowleft" . 8630)
                                                 ("\\curvearrowright" . 8631)
                                                 ("\\dag" . 8224)
                                                 ("\\dagger" . 8224)
                                                 ("\\daleth" . 8504)
                                                 ("\\dashv" . 8867)
                                                 ("\\ddag" . 8225)
                                                 ("\\ddagger" . 8225)
                                                 ("\\ddots" . 8945)
                                                 ("\\diamond" . 8900)
                                                 ("\\diamondsuit" . 9826)
                                                 ("\\divideontimes" . 8903)
                                                 ("\\doteq" . 8784)
                                                 ("\\doteqdot" . 8785)
                                                 ("\\dotplus" . 8724)
                                                 ("\\dotsquare" . 8865)
                                                 ("\\downarrow" . 8595)
                                                 ("\\downdownarrows" . 8650)
                                                 ("\\downleftharpoon" . 8643)
                                                 ("\\downrightharpoon" . 8642)
                                                 ("\\ell" . 8467)
                                                 ("\\emptyset" . 8709)
                                                 ("\\eqcirc" . 8790)
                                                 ("\\eqcolon" . 8789)
                                                 ("\\eqslantgtr" . 8925)
                                                 ("\\eqslantless" . 8924)
                                                 ("\\equiv" . 8801)
                                                 ("\\exists" . 8707)
                                                 ("\\fallingdotseq" . 8786)
                                                 ("\\flat" . 9837)
                                                 ("\\forall" . 8704)
                                                 ("\\frown" . 8994)
                                                 ("\\ge" . 8805)
                                                 ("\\geq" . 8805)
                                                 ("\\geqq" . 8807)
                                                 ("\\geqslant" . 8805)
                                                 ("\\gets" . 8592)
                                                 ("\\gg" . 8811)
                                                 ("\\ggg" . 8921)
                                                 ("\\gimel" . 8503)
                                                 ("\\gnapprox" . 8935)
                                                 ("\\gneq" . 8809)
                                                 ("\\gneqq" . 8809)
                                                 ("\\gnsim" . 8935)
                                                 ("\\gtrapprox" . 8819)
                                                 ("\\gtrdot" . 8919)
                                                 ("\\gtreqless" . 8923)
                                                 ("\\gtreqqless" . 8923)
                                                 ("\\gtrless" . 8823)
                                                 ("\\gtrsim" . 8819)
                                                 ("\\gvertneqq" . 8809)
                                                 ("\\hbar" . 8463)
                                                 ("\\heartsuit" . 9829)
                                                 ("\\hookleftarrow" . 8617)
                                                 ("\\hookrightarrow" . 8618)
                                                 ("\\iff" . 8660)
                                                 ("\\imath" . 305)
                                                 ("\\in" . 8712)
                                                 ("\\infty" . 8734)
                                                 ("\\int" . 8747)
                                                 ("\\intercal" . 8890)
                                                 ("\\langle" . 10216)
                                                 ("\\lbrace" . 123)
                                                 ("\\lbrack" . 91)
                                                 ("\\lceil" . 8968)
                                                 ("\\ldots" . 8230)
                                                 ("\\le" . 8804)
                                                 ("\\leadsto" . 8605)
                                                 ("\\leftarrow" . 8592)
                                                 ("\\leftarrowtail" . 8610)
                                                 ("\\leftharpoondown" . 8637)
                                                 ("\\leftharpoonup" . 8636)
                                                 ("\\leftleftarrows" . 8647)
                                                 ("\\leftrightarrow" . 8596)
                                                 ("\\leftrightarrows" . 8646)
                                                 ("\\leftrightharpoons" . 8651)
                                                 ("\\leftrightsquigarrow" . 8621)
                                                 ("\\leftthreetimes" . 8907)
                                                 ("\\leq" . 8804)
                                                 ("\\leqq" . 8806)
                                                 ("\\leqslant" . 8804)
                                                 ("\\lessapprox" . 8818)
                                                 ("\\lessdot" . 8918)
                                                 ("\\lesseqgtr" . 8922)
                                                 ("\\lesseqqgtr" . 8922)
                                                 ("\\lessgtr" . 8822)
                                                 ("\\lesssim" . 8818)
                                                 ("\\lfloor" . 8970)
                                                 ("\\lhd" . 9665)
                                                 ("\\rhd" . 9655)
                                                 ("\\ll" . 8810)
                                                 ("\\llcorner" . 8990)
                                                 ("\\lnapprox" . 8934)
                                                 ("\\lneq" . 8808)
                                                 ("\\lneqq" . 8808)
                                                 ("\\lnsim" . 8934)
                                                 ("\\longleftarrow" . 8592)
                                                 ("\\longleftrightarrow" . 8596)
                                                 ("\\longmapsto" . 8614)
                                                 ("\\longrightarrow" . 8594)
                                                 ("\\looparrowleft" . 8619)
                                                 ("\\looparrowright" . 8620)
                                                 ("\\lozenge" . 10023)
                                                 ("\\lq" . 8216)
                                                 ("\\lrcorner" . 8991)
                                                 ("\\ltimes" . 8905)
                                                 ("\\lvertneqq" . 8808)
                                                 ("\\maltese" . 10016)
                                                 ("\\mapsto" . 8614)
                                                 ("\\measuredangle" . 8737)
                                                 ("\\mho" . 8487)
                                                 ("\\mid" . 8739)
                                                 ("\\models" . 8871)
                                                 ("\\mp" . 8723)
                                                 ("\\multimap" . 8888)
                                                 ("\\nLeftarrow" . 8653)
                                                 ("\\nLeftrightarrow" . 8654)
                                                 ("\\nRightarrow" . 8655)
                                                 ("\\nVDash" . 8879)
                                                 ("\\nVdash" . 8878)
                                                 ("\\nabla" . 8711)
                                                 ("\\napprox" . 8777)
                                                 ("\\natural" . 9838)
                                                 ("\\ncong" . 8775)
                                                 ("\\ne" . 8800)
                                                 ("\\nearrow" . 8599)
                                                 ("\\neg" . 172)
                                                 ("\\neq" . 8800)
                                                 ("\\nequiv" . 8802)
                                                 ("\\newline" . 8232)
                                                 ("\\nexists" . 8708)
                                                 ("\\ngeq" . 8817)
                                                 ("\\ngeqq" . 8817)
                                                 ("\\ngeqslant" . 8817)
                                                 ("\\ngtr" . 8815)
                                                 ("\\ni" . 8715)
                                                 ("\\nleftarrow" . 8602)
                                                 ("\\nleftrightarrow" . 8622)
                                                 ("\\nleq" . 8816)
                                                 ("\\nleqq" . 8816)
                                                 ("\\nleqslant" . 8816)
                                                 ("\\nless" . 8814)
                                                 ("\\nmid" . 8740)
                                                 ("\\notin" . 8713)
                                                 ("\\nparallel" . 8742)
                                                 ("\\nprec" . 8832)
                                                 ("\\npreceq" . 8928)
                                                 ("\\nrightarrow" . 8603)
                                                 ("\\nshortmid" . 8740)
                                                 ("\\nshortparallel" . 8742)
                                                 ("\\nsim" . 8769)
                                                 ("\\nsimeq" . 8772)
                                                 ("\\nsubset" . 8836)
                                                 ("\\nsubseteq" . 8840)
                                                 ("\\nsubseteqq" . 8840)
                                                 ("\\nsucc" . 8833)
                                                 ("\\nsucceq" . 8929)
                                                 ("\\nsupset" . 8837)
                                                 ("\\nsupseteq" . 8841)
                                                 ("\\nsupseteqq" . 8841)
                                                 ("\\ntriangleleft" . 8938)
                                                 ("\\ntrianglelefteq" . 8940)
                                                 ("\\ntriangleright" . 8939)
                                                 ("\\ntrianglerighteq" . 8941)
                                                 ("\\nvDash" . 8877)
                                                 ("\\nvdash" . 8876)
                                                 ("\\nwarrow" . 8598)
                                                 ("\\odot" . 8857)
                                                 ("\\oint" . 8750)
                                                 ("\\ominus" . 8854)
                                                 ("\\oplus" . 8853)
                                                 ("\\oslash" . 8856)
                                                 ("\\otimes" . 8855)
                                                 ("\\par" . 8233)
                                                 ("\\parallel" . 8741)
                                                 ("\\partial" . 8706)
                                                 ("\\perp" . 8869)
                                                 ("\\pitchfork" . 8916)
                                                 ("\\prec" . 8826)
                                                 ("\\precapprox" . 8830)
                                                 ("\\preceq" . 8828)
                                                 ("\\precnapprox" . 8936)
                                                 ("\\precnsim" . 8936)
                                                 ("\\precsim" . 8830)
                                                 ("\\prime" . 8242)
                                                 ("\\prod" . 8719)
                                                 ("\\propto" . 8733)
                                                 ("\\qed" . 8718)
                                                 ("\\qquad" . 10722)
                                                 ("\\quad" . 9251)
                                                 ("\\rangle" . 10217)
                                                 ("\\rbrace" . 125)
                                                 ("\\rbrack" . 93)
                                                 ("\\rceil" . 8969)
                                                 ("\\rfloor" . 8971)
                                                 ("\\rightarrow" . 8594)
                                                 ("\\rightarrowtail" . 8611)
                                                 ("\\rightharpoondown" . 8641)
                                                 ("\\rightharpoonup" . 8640)
                                                 ("\\rightleftarrows" . 8644)
                                                 ("\\rightleftharpoons" . 8652)
                                                 ("\\rightrightarrows" . 8649)
                                                 ("\\rightthreetimes" . 8908)
                                                 ("\\risingdotseq" . 8787)
                                                 ("\\rtimes" . 8906)
                                                 ("\\times" . 215)
                                                 ("\\sbs" . 65128)
                                                 ("\\searrow" . 8600)
                                                 ("\\setminus" . 8726)
                                                 ("\\sharp" . 9839)
                                                 ("\\shortmid" . 8739)
                                                 ("\\shortparallel" . 8741)
                                                 ("\\sim" . 8764)
                                                 ("\\simeq" . 8771)
                                                 ("\\smallamalg" . 8720)
                                                 ("\\smallsetminus" . 8726)
                                                 ("\\smallsmile" . 8995)
                                                 ("\\smile" . 8995)
                                                 ("\\spadesuit" . 9824)
                                                 ("\\sphericalangle" . 8738)
                                                 ("\\sqcap" . 8851)
                                                 ("\\sqcup" . 8852)
                                                 ("\\sqsubset" . 8847)
                                                 ("\\sqsubseteq" . 8849)
                                                 ("\\sqsupset" . 8848)
                                                 ("\\sqsupseteq" . 8850)
                                                 ("\\square" . 9633)
                                                 ("\\squigarrowright" . 8669)
                                                 ("\\star" . 8902)
                                                 ("\\straightphi" . 966)
                                                 ("\\subset" . 8834)
                                                 ("\\subseteq" . 8838)
                                                 ("\\subseteqq" . 8838)
                                                 ("\\subsetneq" . 8842)
                                                 ("\\subsetneqq" . 8842)
                                                 ("\\succ" . 8827)
                                                 ("\\succapprox" . 8831)
                                                 ("\\succcurlyeq" . 8829)
                                                 ("\\succeq" . 8829)
                                                 ("\\succnapprox" . 8937)
                                                 ("\\succnsim" . 8937)
                                                 ("\\succsim" . 8831)
                                                 ("\\sum" . 8721)
                                                 ("\\supset" . 8835)
                                                 ("\\supseteq" . 8839)
                                                 ("\\supseteqq" . 8839)
                                                 ("\\supsetneq" . 8843)
                                                 ("\\supsetneqq" . 8843)
                                                 ("\\surd" . 8730)
                                                 ("\\swarrow" . 8601)
                                                 ("\\therefore" . 8756)
                                                 ("\\thickapprox" . 8776)
                                                 ("\\thicksim" . 8764)
                                                 ("\\to" . 8594)
                                                 ("\\top" . 8868)
                                                 ("\\triangle" . 9653)
                                                 ("\\triangledown" . 9663)
                                                 ("\\triangleleft" . 9667)
                                                 ("\\trianglelefteq" . 8884)
                                                 ("\\triangleq" . 8796)
                                                 ("\\triangleright" . 9657)
                                                 ("\\trianglerighteq" . 8885)
                                                 ("\\twoheadleftarrow" . 8606)
                                                 ("\\twoheadrightarrow" . 8608)
                                                 ("\\ulcorner" . 8988)
                                                 ("\\uparrow" . 8593)
                                                 ("\\updownarrow" . 8597)
                                                 ("\\upleftharpoon" . 8639)
                                                 ("\\uplus" . 8846)
                                                 ("\\uprightharpoon" . 8638)
                                                 ("\\upuparrows" . 8648)
                                                 ("\\urcorner" . 8989)
                                                 ("\\u{i}" . 301)
                                                 ("\\vDash" . 8872)
                                                 ("\\varepsilon" . 949)
                                                 ("\\varphi" . 966)
                                                 ("\\varprime" . 8242)
                                                 ("\\varpropto" . 8733)
                                                 ("\\varrho" . 1009)
                                                 ("\\varsigma" 962)
                                                 ("\\vartriangleleft" . 8882)
                                                 ("\\vartriangleright" . 8883)
                                                 ("\\vdash" . 8866)
                                                 ("\\vdots" . 8942)
                                                 ("\\vee" . 8744)
                                                 ("\\veebar" . 8891)
                                                 ("\\vert" . 124)
                                                 ("\\wedge" . 8743)
                                                 ("\\wp" . 8472)
                                                 ("\\wr" . 8768)
                                                 ("\\Bbb{N}" . 8469)
                                                 ("\\Bbb{P}" . 8473)
                                                 ("\\Bbb{Q}" . 8474)
                                                 ("\\Bbb{R}" . 8477)
                                                 ("\\Bbb{Z}" . 8484)
                                                 ("--" . 8211)
                                                 ("---" . 8212)
                                                 ("\\ordfeminine" . 170)
                                                 ("\\ordmasculine" . 186)
                                                 ("\\lambdabar" . 411)
                                                 ("\\celsius" . 8451)
                                                 ("\\textmu" . 181)
                                                 ("\\textfractionsolidus" . 8260)
                                                 ("\\textbigcircle" . 8413)
                                                 ("\\textmusicalnote" . 9834)
                                                 ("\\textdied" . 10013)
                                                 ("\\textcolonmonetary" . 8353)
                                                 ("\\textwon" . 8361)
                                                 ("\\textnaira" . 8358)
                                                 ("\\textpeso" . 8369)
                                                 ("\\textlira" . 8356)
                                                 ("\\textrecipe" . 8478)
                                                 ("\\textinterrobang" . 8253)
                                                 ("\\textpertenthousand" . 8241)
                                                 ("\\textbaht" . 3647)
                                                 ("\\textnumero" . 8470)
                                                 ("\\textdiscount" . 8274)
                                                 ("\\textestimated" . 8494)
                                                 ("\\textopenbullet" . 9702)
                                                 ("\\textlquill" . 8261)
                                                 ("\\textrquill" . 8262)
                                                 ("\\textcircledP" . 8471)
                                                 ("\\textreferencemark" . 8251)))))


;;* Helpful functions
;;** bibtool is a CLI for cleaning up bib files
;; https://www.reddit.com/r/emacs/comments/1hlwpr0/weekly_tips_tricks_c_thread_20241225_week_52/
(defun my-bibtool-current-file ()
  "Run bibtool on the current buffer's file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (let ((default-directory (file-name-directory file))
              (base-file (file-name-nondirectory file)))
          (shell-command (concat "bibtool " base-file " -o " base-file)))
      (message "Not visiting a bibtex file!"))))
;;** control how reftex toc shows up
(add-to-list 'display-buffer-alist
             '("^\\*toc\\*" imenu-list-display-buffer))

(defun my-LaTeX-mark-inside-environment ()
  "Like `LaTeX-mark-environment' but marks the inside of the environment.
Skips past [] and {} arguments to the environment.
Adapted by the er/mark-LaTeX-inside-environment function"
  (interactive)
  (LaTeX-mark-environment)
  (when (looking-at "\\\\begin{")
    (forward-sexp 2)
    ;; Assume these are arguments
    (while (looking-at "[ \t\n]*[{[]")
      (forward-sexp 1))
    ;; Go to next line if there is nothing interesting on this one
    (skip-syntax-forward " ") ;; newlines are ">" i.e. end comment
    (when (looking-at "%\\|$")
      (forward-line))
    ;; Clean up the end portion
    (exchange-point-and-mark)
    (backward-sexp 2)
    (skip-syntax-backward " ")
    (exchange-point-and-mark)))

;; ref:
;; https://github.com/malb/emacs.d/blob/master/malb.org#latex-2
;; TODO: test this function in my workflow
(defun my-latex-bib-jump ()
  "If point is on a citation, jump to the bibtex file, otherwise open refex menu."
  (interactive)
  (xref-push-marker-stack)
  (let ((current (point)))
    (ignore-errors (org-ref-latex-jump-to-bibtex))
    (if (eq current (point))
        (reftex-goto-label))))
;;* provide my-setup-latex
(provide 'my-setup-latex)
;;* end my-setup-latex
