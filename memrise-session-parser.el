;;; memrise-session-parser.el --- A set of functions to parse session JSON -*- lexical-binding: t; -*-

(require 'memrise-task-parser)
(require 'memrise-learnable-parser)
(require 'memrise-test-parser)

(defun memrise/parse-session (json)
  (let* ((name (memrise/parse-session-course-name json))
         (title (memrise/parse-session-title json))
         (source (memrise/parse-session-source json))
         (target (memrise/parse-session-target json))
         (tasks (memrise/parse-session-tasks json))
         (tests (memrise/parse-session-tests json))
         (learnables (memrise/parse-session-learnables json)))
    (make-memrise/session
     :course-name name
     :title title
     :source source
     :target target
     :tasks tasks
     :tests tests
     :learnables learnables)))

(defun memrise/parse-session-course-name (json)
  (let* ((session (assoc-default 'session json))
         (course (assoc-default 'course session))
         (name (assoc-default 'name course)))
    name))

(defun memrise/parse-session-title (json)
  (let* ((session (assoc-default 'session json))
         (level (assoc-default 'level session))
         (title (assoc-default 'title level)))
    title))

(defun memrise/parse-session-source (json)
  (let* ((session (assoc-default 'session json))
         (course (assoc-default 'course session))
         (source (assoc-default 'source course)))
    (assoc-default 'name source)))

(defun memrise/parse-session-target (json)
  (let* ((session (assoc-default 'session json))
         (course (assoc-default 'course session))
         (target (assoc-default 'target course)))
    (assoc-default 'name target)))

(defun memrise/parse-session-tasks (json)
  (mapcar 'memrise/parse-session-task
          (assoc-default 'boxes json)))

(defun memrise/parse-session-learnables (json)
  (mapcar 'memrise/parse-session-learnable
          (assoc-default 'learnables json)))

(defun memrise/parse-session-tests (json)
  (mapcar 'memrise/parse-session-learnable-tests
          (assoc-default 'screens json)))

(defun memrise/parse-session-learnable-tests (json)
  (let* ((id (string-to-number (symbol-name (car json))))
         (body (cdr json))
         (tests (mapcar #'memrise/parse-session-test body)))
    `(,id . ,tests)))

(provide 'memrise-session-parser)
;;; memrise-session-parser.el ends here
