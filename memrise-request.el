(use-package request :ensure t)

(require 'cl)

(setq memrise/url "https://www.memrise.com")
(setq memrise/login-url "https://www.memrise.com/login/")
(setq memrise/next-home-url "https://www.memrise.com/login/?next=/home/")
(setq memrise/dashboard-url "http://www.memrise.com/ajax/courses/dashboard/")
(setq memrise/session-url "https://www.memrise.com/ajax/session/")

(defun memrise/cookie ()
  "Returns memrise session cookie"
  (request-cookie-alist "www.memrise.com" "/"))

(defun memrise/get-csrf-token ()
  "Returns CSRF token for memrise.com"
  (assoc-default "csrftoken" (memrise/cookie)))

(defun memrise/get-session-id ()
  "Returns memrise session ID"
  (assoc-default "sessionid" (memrise/cookie)))

(defun memrise/to-buffer (buffer)
  (interactive)
  `(lambda () (let ((oldbuf (current-buffer)))
          (with-current-buffer (get-buffer-create ,buffer)
            (barf-if-buffer-read-only)
            (erase-buffer)
            (save-excursion
              (insert-buffer-substring oldbuf))))))

(defun memrise/debug-result ()
  (funcall (memrise/to-buffer "result")))

(setq request-log-level `debug)

(setq request-backend `curl)

(defun memrise/request-home ()
  (request
   "https://www.memrise.com/home/"
   :parser (memrise/to-buffer "home.html")))

(defun memrise/request-dashboard (callback)
  (lexical-let ((inner callback))
    (request
     memrise/dashboard-url
     :type "GET"
     :params '(("courses_filter" . "most_recent"))
     :parser 'json-read
     :success (cl-function (lambda (&key data &allow-other-keys)
                             (funcall inner data))))))

(defun memrise/request-session (course-id type callback)
  (lexical-let ((inner callback))
    (request
     memrise/session-url
     :type "GET"
     :params `(("course_id" . ,course-id)
               ("session_slug" . ,type)
               ("_" . ,(memrise/get-session-id)))
     :parser 'json-read
     :success (cl-function (lambda (&key data &allow-other-keys)
                             (funcall inner data))))))

(provide 'memrise-request)
