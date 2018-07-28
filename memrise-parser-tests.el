;;; memrise-parser-tests.el --- Tests for JSON parsers -*- lexical-binding: t; -*-

(require 'memrise-session-objects)
(require 'memrise-learnable-parser)
(require 'memrise-task-parser)

(ert-deftest memrise-parse-learnable-test ()
  (let* ((json (memrise-test-load-json "assets/learnable.json"))
         (result (cdr (memrise/parse-session-learnable json))))
    (should (= (memrise/session-learnable-id result)
               65550801240322))
    (should (string= (memrise/session-learnable-text result)
                     "der Notfall"))
    (should (string= (memrise/session-learnable-translation result)
                     "the emergency"))
    (should (not (memrise/session-learnable-audio result)))))

(ert-deftest memrise-parse-session-task-sentinel ()
  (let* ((json (memrise-test-load-json "assets/task_sentinel.json"))
         (result (memrise/parse-session-task json)))
    (should (= (memrise/session-task-learnable-id result)
               65550801568002))
    (should (string= (memrise/session-task-template result)
                     "sentinel"))
    (should (= (memrise/session-task-learn-level result)
               2))))

(ert-deftest memrise-parse-session-task-presentation ()
  (let* ((json (memrise-test-load-json "assets/task_presentation.json"))
         (result (memrise/parse-session-task json)))
    (should (= (memrise/session-task-learnable-id result)
               65550801240322))
    (should (string= (memrise/session-task-template result)
                     "presentation"))
    (should (not (memrise/session-task-learn-level result)))))


(defun memrise-test-load-json (json-path)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents json-path)
    (json-read-from-string (buffer-string))))
