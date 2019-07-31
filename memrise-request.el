;;; memrise-request.el --- Memrise requests -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019  Valeriy Savchenko

;; Author: Valeriy Savchenko <sinmipt@gmail.com>

;; This file is NOT part of GNU Emacs.

;; memrise.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; memrise.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with memrise.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This source file contains all the functions and pieces of data related
;; to Memrise requests.  Requests we use to login, receive data and send
;; answers.

;;; Code:

(require 'memrise-utils)
(require 'request)
(require 'cl-lib)

(defconst memrise-url "https://www.memrise.com")
(defconst memrise-home-url "https://www.memrise.com/home/")
(defconst memrise-login-url "https://www.memrise.com/login/")
(defconst memrise-next-home-url "https://www.memrise.com/login/?next=/home/")
(defconst memrise-dashboard-url "https://www.memrise.com/ajax/courses/dashboard/")
(defconst memrise-session-url "https://www.memrise.com/ajax/session/")
(defconst memrise-register-url "https://www.memrise.com/ajax/learning/register")

(defcustom memrise-sync-requests
  nil
  "Perform Memrise requests synchronously."
  :type 'boolean
  :group 'memrise)
(defcustom memrise-save-received-data
  nil
  "Save raw data received from Memrise into a special buffer."
  :type 'boolean
  :group 'memrise)

(defun memrise-cookie ()
  "Return memrise session cookie."
  (request-cookie-alist "www.memrise.com" "/" t))

(defun memrise-init-cookie ()
  "Request one of memrise.com pages to init cookie."
  (request memrise-home-url :sync t))

(defun memrise-get-csrf-token ()
  "Return CSRF token for memrise.com."
  (assoc-default "csrftoken" (memrise-cookie)))

(defun memrise-get-session-id ()
  "Return memrise session ID."
  (assoc-default "sessionid" (memrise-cookie)))

(defun memrise-debug-connection ()
  "Turn on debug prints from sent requests."
  (setq request-log-level `debug)
  (setq request-message-level `debug))

;;;###autoload
(defun memrise-login (username password)
  "Login to memrise account using `USERNAME' and `PASSWORD'."
  (interactive
   (let ((username (read-string "Username: "))
         (password (read-passwd "Password: ")))
     (list username password)))
  (unless (memrise-cookie)
    (memrise-init-cookie))
  (request
   memrise-next-home-url
   :data `(("csrfmiddlewaretoken" . ,(memrise-get-csrf-token))
           ("password" . ,password)
           ("username" . ,username))
   :headers `(("Referer" . ,memrise-login-url))
   ;; memrise-login is always sync!
   :sync t
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (message "Success")))
   :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                         (message "Got error: %S" error-thrown)))))

(defun memrise-request-dashboard (callback)
  "Send a request for the user's dashboard.

After response is received, call `CALLBACK' with received data."
  (request
   memrise-dashboard-url
   :type "GET"
   :params '(("courses_filter" . "most_recent"))
   :parser #'memrise--json-read
   :sync memrise-sync-requests
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (funcall callback data)))
   ;; on 403 we should login and try to send the same
   ;; request all over again
   :status-code `((403 . ,(memrise-make-argument-ignoring-lambda
                           (-partial #'memrise--login-and-apply
                                     #'memrise-request-dashboard
                                     callback))))))

(defun memrise-request-session (course-id type callback)
  "Send a request for a session of `TYPE' type for the `COURSE-ID'.

After response is received, call `CALLBACK' with received data."
  (request
   memrise-session-url
   :type "GET"
   :params `(("course_id" . ,course-id)
             ("session_slug" . ,type)
             ("_" . ,(memrise-get-session-id)))
   :parser #'memrise--json-read
   :sync memrise-sync-requests
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (funcall callback data)))))

(defun memrise--request-send-answer (learnable
                                     course-id
                                     template
                                     given-answer
                                     points
                                     time-spent)
  "Send request with the results of the test.

`LEARNABLE' is the subject of the test.
`COURSE-ID' is an ID of the current course.
`TEMPLATE' is the kind of the taken test.
`GIVEN-ANSWER' is the answer given by the user.
`POINTS' is a number of points the user should get for the test.
`TIME-SPENT' is the amount of time spent answering."
  (request
   memrise-register-url
   :type "POST"
   :data `(("learnable_id" . ,(oref learnable id))
           ("box_template" . ,template)
           ;; score is 0 if the user answered incorrectly
           ("score" . ,(if (= points 0) 0 1))
           ("points" . ,points)
           ("course_id" . ,course-id)
           ("time_spent" . ,time-spent)
           ;; to be fair, I didn't figure out this parameter yet
           ("fully_grow" . ,nil)
           ;; text representation of the learned thing
           ("learning_element" . ,(oref learnable text))
           ("given_answer" . ,given-answer)
           ("definition_element" . ,(oref learnable translation)))
   ;; answer and other parts of the request are very likely to have UTF
   :encoding 'utf-8
   :headers `(("referer" . ,memrise-home-url)
              ("x-csrftoken" . ,(memrise-get-csrf-token))
              ("origin" . ,memrise-url)
              ("authority" . "www.memrise.com")
              ("x-requested-with" . "XMLHttpRequest"))))

(defun memrise--login-and-apply (func &rest args)
  "Login to Memrise and apply `FUNC' to `ARGS'."
  (call-interactively 'memrise-login)
  (apply func args))

(defun memrise--json-read ()
  "Read JSON with a little bit of extra flavoring."
  (let ((json-array-type 'list))
    (when memrise-save-received-data
      (memrise--to-buffer "*memrise-result*"))
    (json-read)))

(provide 'memrise-request)
;;; memrise-request.el ends here
