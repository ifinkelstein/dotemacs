;;; my-setup-writing-test.el --- ERT tests for sentence motion and killing  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `my-forward-to-sentence-end', `my-kill-sentence-dwim',
;; and `my-sentence-setup' across `text-mode', `org-mode', and `LaTeX-mode'.

;;; Code:

(require 'ert)

(defun my-test--kill-sentence-in (mode text)
  "Insert TEXT into a temp buffer in MODE, put point at `|', and kill sentence.
Return the resulting buffer string with `|' marking final point."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (goto-char (point-min))
    (unless (search-forward "|" nil t)
      (error "Test input must contain `|' cursor marker: %S" text))
    (delete-char -1)
    (my-kill-sentence-dwim)
    (concat (buffer-substring-no-properties (point-min) (point))
            "|"
            (buffer-substring-no-properties (point) (point-max)))))

(ert-deftest my-kill-sentence-dwim-mid-sentence-basic ()
  "Test killing from mid-sentence preserves terminal punctuation."
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | a test.")
                 "This is|."))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is |a test.")
                 "This is|."))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is| a test.")
                 "This is|."))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | a test.  Next sentence.")
                 "This is|.  Next sentence.")))

(ert-deftest my-kill-sentence-dwim-parenthetical-before-terminator ()
  "Test that parentheticals and delimiters before terminal punctuation are killed."
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | a test (bla).")
                 "This is|."))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | a test (what happens to parens)?")
                 "This is|?"))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | a test (etc.).")
                 "This is|."))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | a test (really!).")
                 "This is|."))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | a test [1, 2].")
                 "This is|."))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | a \"test\".")
                 "This is|."))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | 50%.")
                 "This is|.")))

(ert-deftest my-kill-sentence-dwim-org-markup ()
  "Test killing mid-sentence with Org emphasis and LaTeX fragments."
  ;; Markup closed before the period is killed in full.
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | *bold*.")
                 "This is|."))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | ~code~.")
                 "This is|."))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | $x + 1$.")
                 "This is|."))
  ;; Markup wrapping the whole sentence keeps its closing marker after the period.
  (should (equal (my-test--kill-sentence-in #'org-mode "*This is | a test.*")
                 "*This is|.*"))
  (should (equal (my-test--kill-sentence-in #'org-mode "/This is | a test./")
                 "/This is|./"))
  (should (equal (my-test--kill-sentence-in #'org-mode "\\emph{This is | a test.}")
                 "\\emph{This is|.}"))
  (should (equal (my-test--kill-sentence-in #'org-mode "*This is | a test.* Next.")
                 "*This is|.* Next.")))

(ert-deftest my-kill-sentence-dwim-enclosing-delimiters ()
  "Test killing inside a parenthesized or quoted sentence keeps the outer closer."
  (should (equal (my-test--kill-sentence-in #'org-mode "(This is | a test.)")
                 "(This is|.)"))
  (should (equal (my-test--kill-sentence-in #'org-mode "\"This is | a test.\"")
                 "\"This is|.\""))
  (should (equal (my-test--kill-sentence-in #'text-mode "(This is | a test (bla).)")
                 "(This is|.)")))

(ert-deftest my-kill-sentence-dwim-multi-terminator-and-unpunctuated ()
  "Test multi-character terminators and unpunctuated sentence ends."
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | a test...")
                 "This is|..."))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | a test?!")
                 "This is|?!"))
  (should (equal (my-test--kill-sentence-in #'org-mode "This is | a test")
                 "This is|")))

(ert-deftest my-kill-sentence-dwim-beginning-of-sentence ()
  "Test killing from the beginning of a sentence removes punctuation too."
  (should (equal (my-test--kill-sentence-in #'org-mode "|This is a test (bla). Next sentence.")
                 "|Next sentence."))
  (should (equal (my-test--kill-sentence-in #'org-mode "First. |This is a test (bla). Next.")
                 "First. |Next.")))

(provide 'my-setup-writing-test)
;;; my-setup-writing-test.el ends here
