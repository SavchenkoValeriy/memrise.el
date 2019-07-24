;;; memrise-session-parser.el --- A set of functions to parse session JSON -*- lexical-binding: t; -*-

(require 'memrise-task-parser)
(require 'memrise-learnable-parser)
(require 'memrise-test-parser)

(defun memrise/parse-session (json)
  (jeison-read memrise-session json))

(defun memrise/parse-session-learnables (json)
  (mapcar 'memrise/parse-session-learnable json))

(defun memrise/parse-session-tests (json)
  (mapcar 'memrise/parse-session-learnable-tests json))

(defun memrise/parse-session-learnable-tests (json)
  (let* ((id (string-to-number (symbol-name (car json))))
         (body (cdr (cdr json)))
         (tests (mapcar #'memrise/parse-session-test body)))
    `(,id . ,tests)))

(provide 'memrise-session-parser)
;;; memrise-session-parser.el ends here
