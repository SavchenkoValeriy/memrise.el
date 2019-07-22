;;; memrise-parser-tests.el --- Tests for JSON parsers -*- lexical-binding: t; -*-

(require 'memrise-session-objects)
(require 'memrise-learnable-parser)
(require 'memrise-task-parser)
(require 'memrise-test-parser)

(ert-deftest memrise-parse-learnable-test ()
  (let* ((json (memrise-test-load-json "assets/learnable.json"))
         (result (cdr (memrise/parse-session-learnable json))))
    (should (= (oref result id)
               65550801240322))
    (should (string= (oref result text)
                     "der Notfall"))
    (should (string= (oref result translation)
                     "the emergency"))
    (should (not (oref result audio)))))

(ert-deftest memrise-parse-session-task-sentinel-test ()
  (let* ((json (memrise-test-load-json "assets/task_sentinel.json"))
         (result (memrise/parse-session-task json)))
    (should (= (oref result learnable-id)
               65550801568002))
    (should (string= (oref result template)
                     "sentinel"))
    (should (= (oref result learn-level)
               2))))

(ert-deftest memrise-parse-session-task-presentation ()
  (let* ((json (memrise-test-load-json "assets/task_presentation.json"))
         (result (memrise/parse-session-task json)))
    (should (= (oref result learnable-id)
               65550801240322))
    (should (string= (oref result template)
                     "presentation"))
    (should (not (oref result learn-level)))))

(ert-deftest memrise-parse-session-test-typing ()
  (cl-letf* ((json (memrise-test-load-json "assets/test_typing.json"))
             ((symbol-function 'memrise/parse-session-test-prompt)
              (memrise-test-mock))
             (result (cdr (memrise/parse-session-test (car json)))))
    (should (string= (memrise/session-test-kind result)
                     "typing"))
    (should (string= (memrise/session-test-correct result)
                     "der Notfall"))
    (should (equal (memrise/session-test-accepted result)
                   '("der Notfall" "der notfall")))
    (should (equal (memrise/session-test-choices result)
                   '("ä" "é" "ö" "ü" "ß")))
    (should (string= (memrise/session-test-prompt result)
                     (memrise-mocked "fake prompt")))))

(ert-deftest memrise-parse-session-test-prompt-test ()
  (cl-letf* ((json (memrise-test-load-json "assets/test_prompt.json"))
             ((symbol-function 'memrise/parse-session-audio)
              (memrise-test-mock))
             (result (memrise/parse-session-test-prompt json)))
    (should (string= (memrise/session-test-prompt-text result)
                     "the emergency"))
    (should (string= (memrise/session-test-prompt-audio result)
                     (memrise-mocked "fake audio")))))

(ert-deftest memrise-parse-prompt-audio-test ()
  (cl-letf* ((json (memrise-test-load-json "assets/test_prompt_audio.json"))
             ((symbol-function 'memrise/process-media)
              (memrise-test-mock))
             (result (memrise/parse-session-audio json)))
    (should (string= result (memrise-mocked "audio" '("normal.mp3"))))))

(ert-deftest memrise-audio-multiple-choice-test ()
  (cl-letf* ((json (memrise-test-load-json
                    "assets/audio_multiple_choice.json"))
             ((symbol-function 'memrise/parse-session-test-prompt)
              (memrise-test-mock))
             ((symbol-function 'memrise/process-media)
              (memrise-test-mock))
             (result (cdr (memrise/parse-session-test (car json)))))
    (should (string= (memrise/session-test-kind result)
                     "audio_multiple_choice"))
    (should (string= (memrise/session-test-correct result)
                     (memrise-mocked "audio" '("12475.mp3"))))
    (should (equal (memrise/session-test-accepted result)
                   (memrise-mocked "audio" '("12475.mp3"))))
    (should (string= (memrise/session-test-prompt result)
                     (memrise-mocked "fake prompt")))
    (should (equal (memrise/session-test-choices result)
                   (memrise-mocked "audio" '("13056.mp3"
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

(defun memrise-test-mock ()
  (lambda (&rest x) (apply #'memrise-mocked x)))

(defun memrise-mocked (&rest args)
  (format "mock-%S" args))

(defun memrise-test-load-json (json-path)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents json-path)
    (json-read-from-string (buffer-string))))
