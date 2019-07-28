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

(defmacro with-memrise-test-session (&rest body)
  (declare (indent 1) (debug t))
  `(with-current-buffer (memrise/session-buffer)
     (let ((inhibit-read-only t))
       (kill-all-local-variables)
       (erase-buffer)
       (memrise-session-mode)
       (make-local-variable 'session)
       (make-local-variable 'learnable)
       (setq session (memrise-session :title "Test course"
                                      :source "A"
                                      :target "B"))
       (setq learnable (memrise-session-learnable))
       ,@body)))

(defun memrise:contains-all (text &rest needles)
  "Return true if all `NEEDLES' are in the `TEXT'."
  (--every-p (s-contains-p it text) needles))

;;; test-helper.el ends here
