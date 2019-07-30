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
             (widget (memrise-multiple-choice-widget test 4))
             (buffer-text (buffer-string)))
        (widget-setup)
        (should (memrise:contains-all buffer-text "asd" "fgh" "jkl" "qwe"))
        (with-mock
          (memrise:mock-submit)
          (memrise:press "a")))))

(ert-deftest memrise:test-reversed-multi-choice-widget ()
  (with-memrise-test-session
      (with-mock
        (mock (memrise-play-audio "qwe.mp3") :times 1)
        (memrise:mock-submit)
        (let* ((prompt (memrise-session-test-prompt :audio "qwe.mp3"))
               (test (memrise-session-test
                      :kind "reversed_multiple_choice"
                      :prompt prompt
                      :answer "qwe"
                      :choices '("asd" "fgh" "jkl")
                      :correct '("qwe")))
               (widget (memrise-reversed-multiple-choice-widget test 4))
               (buffer-text (buffer-string)))
          (widget-setup)
          (should (memrise:contains-all buffer-text "asd" "fgh" "jkl" "qwe"))
          (memrise:press "a")))))

(ert-deftest memrise:test-typing-widget ()
  (with-memrise-test-session
      (let* ((prompt (memrise-session-test-prompt))
             (test (memrise-session-test
                    :kind "typing"
                    :prompt prompt
                    :answer "qwe"
                    :choices '("☀" "☁" "☂")
                    :correct '("qwe")))
             (widget (memrise-typing-widget test))
             (buffer-text (buffer-string)))
        (widget-setup)
        (should (memrise:contains-all buffer-text "☀" "☁" "☂"))
        (with-mock
          (memrise:mock-submit)
          (memrise:press "asdfg TAB asd TAB asdfg TAB d TAB")
          (should (equal (widget-value widget) "asdfg☀☁☂asdfg☂"))
          (memrise:press "C-m")
          (should (equal (memrise--widget-get-answer widget)
                         "asdfg☀☁☂asdfg☂"))))))

(ert-deftest memrise:test-audio-choice-widget ()
  (with-memrise-test-session
      (let* ((prompt (memrise-session-test-prompt))
             (test (memrise-session-test
                    :kind "audio_multiple_choice"
                    :prompt prompt
                    :answer "qwe.mp3"
                    :choices '("asd.mp3" "fgh.mp3" "jkl.mp3")
                    :correct '("qwe.mp3")))
             (widget (memrise-audio-multiple-choice-widget test 4))
             (buffer-text (substring-no-properties (buffer-string))))
        (widget-setup)
        (should (not (memrise:contains-any
                      buffer-text "qwe.mp3" "asd.mp3" "fgh.mp3" "jkl.mp3")))
        (cl-letf* ((played-audios nil)
                   ((symbol-function 'memrise-play-audio)
                    (memrise:test-mock
                     (lambda (audio) (add-to-list 'played-audios audio)))))
          (with-mock
            (memrise:mock-submit)
            (memrise:press "a s d f C-m")
            (should (equal '("asd.mp3" "fgh.mp3" "jkl.mp3" "qwe.mp3")
                           (sort played-audios #'string-lessp))))))))

(ert-deftest memrise:test-tapping-widget-test ()
  (with-memrise-test-session
      (cl-letf* ((prompt (memrise-session-test-prompt))
                 (test (memrise-session-test
                        :kind "tapping"
                        :prompt prompt
                        :answer "qwe rty"
                        :choices '("asd" "fgh" "jkl" "zxc")
                        :correct '(("qwe" "rty"))))
                 ((symbol-function 'memrise-shuffle-list)
                  (memrise:test-mock #'identity))
                 (widget (memrise-tapping-widget test))
                 ;; don't shuffle the list of choices
                 (buffer-text (buffer-string)))
        (widget-setup)
        (should (memrise:contains-all
                 buffer-text "qwe" "rty" "asd" "fgh" "jkl" "zxc"))
        (with-mock
          (memrise:mock-submit)
          (memrise:press "a")
          (should (equal (memrise--widget-get-answer widget)
                         '("qwe")))
          (memrise:press "f")
          (should (equal (memrise--widget-get-answer widget)
                         '("qwe" "fgh")))
          (memrise:press "a")
          (should (equal (memrise--widget-get-answer widget)
                         '("qwe" "fgh" "qwe")))
          (memrise:press "C-a C-d")
          (should (equal (memrise--widget-get-answer widget)
                         '("fgh" "qwe")))
          (memrise:press "C-e <backspace>")
          (should (equal (memrise--widget-get-answer widget)
                         '("fgh")))
          (memrise:press "a")
          (should (equal (memrise--widget-get-answer widget)
                         '("fgh" "qwe")))
          (memrise:press "C-3 C-b s d")
          (should (equal (memrise--widget-get-answer widget)
                         '("fgh" "qwe" "rty" "asd")))
          (should (equal (widget-value widget)
                         "fgh qwe rty asd "))
          (memrise:press "M-b M-b f")
          (should (equal (memrise--widget-get-answer widget)
                         '("fgh" "qwe" "fgh" "rty" "asd")))
          (memrise:press "C-m")))))

;;; memrise-widget-test.el ends here
