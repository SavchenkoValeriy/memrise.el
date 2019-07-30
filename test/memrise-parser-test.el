;;; memrise-parser-test.el --- Tests for memrise.el parsers -*- lexical-binding: t; -*-

(require 'memrise-session-objects)
(require 'memrise-session-parser)

(ert-deftest memrise:parse-learnable-test ()
  (cl-letf* ((json (memrise:test-load-json "examples/learnable.json"))
             ((symbol-function 'memrise-download)
              (memrise:test-mock))
             (result (cdr (memrise-parse-session-learnable (car json)))))
    (should (= (oref result id)
               65550811332866))
    (should (string= (oref result text)
                     "was macht er sehr gern?"))
    (should (string= (oref result translation)
                     "what does he love to do?"))
    (should (string= (oref result literal-translation)
                     "what makes he very with-pleasure?"))
    (should (equal (oref result audio)
                   (memrise:mocked "audio" "12475.mp3")))))

(ert-deftest memrise:parse-session-task-sentinel-test ()
  (let* ((json (memrise:test-load-json "examples/task_sentinel.json"))
         (result (jeison-read memrise-session-task json)))
    (should (= (oref result learnable-id)
               65550801568002))
    (should (string= (oref result template)
                     "sentinel"))
    (should (= (oref result learn-level) 2))))

(ert-deftest memrise:parse-session-task-presentation-test ()
  (let* ((json (memrise:test-load-json "examples/task_presentation.json"))
         (result (jeison-read memrise-session-task json)))
    (should (= (oref result learnable-id)
               65550801240322))
    (should (string= (oref result template)
                     "presentation"))
    (should (not (oref result learn-level)))))

(ert-deftest memrise:parse-session-test-typing ()
  (let* ((json (memrise:test-load-json "examples/test_typing.json"))
         (result (cdr (memrise-parse-session-test (car json)))))
    (should (string= (oref result kind)
                     "typing"))
    (should (string= (oref result answer)
                     "der Notfall"))
    (should (equal (oref result correct)
                   '("der Notfall" "der notfall")))
    (should (equal (oref result choices)
                   '("ä" "é" "ö" "ü" "ß")))))

(ert-deftest memrise:parse-session-test-prompt-test ()
  (cl-letf* ((json (memrise:test-load-json "examples/test_prompt.json"))
             ((symbol-function 'memrise-process-media)
              (memrise:test-mock))
             (result (jeison-read memrise-session-test-prompt json)))
    (should (string= (oref result text)
                     "the emergency"))
    (should (string= (oref result audio)
                     (memrise:mocked "audio" "normal.mp3")))))

(ert-deftest memrise:parse-audio-multiple-choice-test ()
  (cl-letf* ((json (memrise:test-load-json
                    "examples/audio_multiple_choice.json"))
             ((symbol-function 'memrise-download)
              (memrise:test-mock))
             (result (cdr (memrise-parse-session-test (car json)))))
    (should (string= (oref result kind)
                     "audio_multiple_choice"))
    (should (equal (oref result correct)
                   (list (memrise:mocked "audio" "12475.mp3"))))
    (should (equal (oref result answer)
                   (memrise:mocked "audio" "12475.mp3")))
    (should (equal (oref result choices)
                   (mapcar (lambda (file) (memrise:mocked "audio" file))
                           '("13056.mp3"
                             "13699.mp3"
                             "13833.mp3"
                             "13195.mp3"
                             "12814.mp3"
                             "14296.mp3"
                             "13845.mp3"
                             "12822.mp3"
                             "12185.mp3"
                             "12698.mp3"
                             "12192.mp3"
                             "13606.mp3"))))))

;;; memrise-parser-test.el ends here
