;;; test-helper.el --- Helpers for memrise-test.el -*- lexical-binding: t; -*-

(defun memrise:test-load-json (json-path)
  "Return `JSON-PATH''s file content."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name
      json-path
      (or ert-runner-test-path
          default-directory)))
    (json-read-from-string (buffer-string))))

(defun memrise:test-mock ()
  (lambda (&rest x) (apply #'memrise:mocked x)))

(defun memrise:mocked (&rest args)
  (format "mock-%S" args))

;;; test-helper.el ends here
