(require 'memrise-utils)
(require 'request)
(require 'cl)

(defconst memrise/url "https://www.memrise.com")
(defconst memrise/home-url "https://www.memrise.com/home/")
(defconst memrise/login-url "https://www.memrise.com/login/")
(defconst memrise/next-home-url "https://www.memrise.com/login/?next=/home/")
(defconst memrise/dashboard-url "https://www.memrise.com/ajax/courses/dashboard/")
(defconst memrise/session-url "https://www.memrise.com/ajax/session/")
(defconst memrise/register-url "https://www.memrise.com/ajax/learning/register")

(defun memrise/cookie ()
  "Return memrise session cookie"
  (request-cookie-alist "www.memrise.com" "/" t))

(defun memrise/init-cookie ()
  "Request one of memrise.com pages to init cookie"
  (request memrise/home-url :sync t))

(defun memrise/get-csrf-token ()
  "Return CSRF token for memrise.com"
  (assoc-default "csrftoken" (memrise/cookie)))

(defun memrise/get-session-id ()
  "Return memrise session ID"
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

(defun memrise/debug-connection ()
  (setq request-log-level `debug)
  (setq request-message-level `debug))

(defun memrise/login (username password)
  "Login to memrise account using `username' and `password'"
  (interactive
   (let ((username (read-string "Username: "))
         (password (read-passwd "Password: ")))
         (list username password)))
  (unless (memrise/cookie)
    (memrise/init-cookie))
  (request
   memrise/next-home-url
   :data `(("csrfmiddlewaretoken" . ,(memrise/get-csrf-token))
           ("password" . ,password)
           ("username" . ,username))
   :headers `(("Referer" . ,memrise/login-url))
   :sync t
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (message "Success")))
   :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                         (message "Got error: %S" error-thrown)))))

(defun memrise/request-dashboard (callback)
  (lexical-let ((inner callback))
    (request
     memrise/dashboard-url
     :type "GET"
     :params '(("courses_filter" . "most_recent"))
     :parser #'json-read
     :success (cl-function (lambda (&key data &allow-other-keys)
                             (funcall inner data)))
     :status-code `((403 . ,(memrise/make-argument-ignoring-lambda
                             (-partial #'memrise/login-and-retry
                                       #'memrise/request-dashboard
                                       inner)))))))

(defun memrise/request-session (course-id type callback)
  (lexical-let ((inner callback))
    (request
     memrise/session-url
     :type "GET"
     :params `(("course_id" . ,course-id)
               ("session_slug" . ,type)
               ("_" . ,(memrise/get-session-id)))
     :parser #'memrise--json-read
     :success (cl-function (lambda (&key data &allow-other-keys)
                             (funcall inner data))))))

(defun memrise--request-send-answer (learnable
                                     course-id
                                     template
                                     given-answer
                                     points
                                     time-spent)
  (request
   memrise/register-url
   :type "POST"
   :data `(("learnable_id" . ,(oref learnable id))
           ("box_template" . ,template)
           ("score" . ,(if (= points 0) 0 1))
           ("points" . ,points)
           ("course_id" . ,course-id)
           ("time_spent" . ,time-spent)
           ("fully_grow" . ,nil)
           ("learning_element" . ,(oref learnable text))
           ("given_answer" . ,given-answer)
           ("definition_element" . ,(oref learnable translation)))
   :encoding 'utf-8
   :headers `(("referer" . ,memrise/home-url)
              ("x-csrftoken" . ,(memrise/get-csrf-token))
              ("origin" . ,memrise/url)
              ("authority" . "www.memrise.com")
              ("x-requested-with" . "XMLHttpRequest"))))

(defun memrise/login-and-retry (func &rest args)
  (call-interactively 'memrise/login)
  (apply func args))

(defun memrise--json-read ()
  (let ((json-array-type 'list))
    (json-read)))

(provide 'memrise-request)
