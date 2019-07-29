;;; memrise-widget-test.el --- Tests for memrise.el widgets -*- lexical-binding: t; -*-

(require 'memrise-session)
(require 'memrise-session-objects)
(require 'memrise-widget)

(require 'el-mock)

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
        (should (memrise:contains-all buffer-text "asd" "fgh" "jkl" "qwe"))
        (with-mock
          (mock (memrise--request-send-answer * * * * * *) :times 1)
          (mock (memrise/widget-run-hooks * *) :times 1)
          (mock (run-at-time * * * * *) :times 1)
          (memrise:press "a")))))

;;; memrise-widget-test.el ends here
