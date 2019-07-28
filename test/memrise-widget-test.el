;;; memrise-widget-test.el --- Tests for memrise.el widgets -*- lexical-binding: t; -*-

(require 'memrise-session)
(require 'memrise-session-objects)
(require 'memrise-widget)

(ert-deftest memrise:test-multi-choice-widget ()
  (with-memrise-test-session
      (let* ((prompt (memrise-session-test-prompt))
             (test (memrise-session-test
                    :kind "multiple_choice"
                    :prompt prompt
                    :answer "qwe"
                    :choices '("asd" "fgh" "jkl")
                    :correct '("qwe")))
             (widget (memrise/multiple-choice-widget test 4))
             (buffer-text (buffer-string)))
        (widget-setup)
        (should (memrise:contains-all buffer-text "asd" "fgh" "jkl" "qwe")))))

;;; memrise-widget-test.el ends here
