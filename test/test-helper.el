;;; test-helper.el --- Helpers for memrise-test.el -*- lexical-binding: t; -*-

(defun memrise:test-load-json (json-path)
  "Return `JSON-PATH''s file content."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name
      json-path
      (if (boundp 'ert-runner-test-path)
          ert-runner-test-path
        default-directory)))
    (json-read-from-string (buffer-string))))

(defun memrise:test-mock (&optional fake-fun)
  "Return a mocking function.

It would use `FAKE-FUN' if given and `memrise:mocked otherwise'."
  (lambda (&rest x) (apply (or fake-fun #'memrise:mocked) x)))

(defun memrise:mocked (&rest args)
  "Return stringified `ARGS' of a mocked function."
  (format "mock-%S" args))

(defmacro with-memrise-test-session (&rest body)
  "Start, switch to a test Memrise session and execute `BODY'."
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
                                      :target "B"
                                      :course-id 42))
       (setq learnable (memrise-session-learnable))
       (switch-to-buffer (memrise/session-buffer))
       ,@body
       (kill-buffer (memrise/session-buffer)))))

(defun memrise:contains-all (text &rest needles)
  "Return true if all `NEEDLES' are in the `TEXT'."
  (--every-p (s-contains-p it text) needles))

(defun memrise:contains-any (text &rest needles)
  "Return true if any `NEEDLES' are in the `TEXT'."
  (--some-p (s-contains-p it text) needles))

(defun memrise:press (key)
  "Simulate pressing `KEY'."
  (execute-kbd-macro (read-kbd-macro key)))

(defmacro memrise:mock-submit ()
  "Mock widget submit functions."
  `(progn
     (mock (memrise--request-send-answer * * * * * *) :times 1)
     (mock (memrise/widget-run-hooks * *) :times 1)
     (mock (memrise--proceed-to-the-next-test *) :times 1)))

;;; test-helper.el ends here
