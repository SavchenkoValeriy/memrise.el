;;; memrise-task-parser.el --- A set of functions to parse task JSON -*- lexical-binding: t; -*-

(defun memrise/parse-session-task (json)
  (jeison-read memrise-session-task json))

(provide 'memrise-task-parser)
