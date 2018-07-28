;;; memrise-task-parser.el --- A set of functions to parse task JSON -*- lexical-binding: t; -*-

(defun memrise/parse-session-task (json)
  (let ((id (string-to-number (assoc-default 'learnable_id json)))
        (template (assoc-default 'template json))
        (learn-level (assoc-default 'learn_session_level json)))
    (make-memrise/session-task
     :learnable-id id
     :template template
     :learn-level learn-level)))

(provide 'memrise-task-parser)
