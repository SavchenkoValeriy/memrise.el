(use-package request :ensure t)

(setq memrise/url "https://www.memrise.com")
(setq memrise/login-url "https://www.memrise.com/login/")
(setq memrise/next-home-url "https://www.memrise.com/login/?next=/home/")
(setq memrise/dasboard-url "http://www.memrise.com/ajax/courses/dashboard/")

(request memrise/login-url)

(defstruct memrise/course name description number-of-things learned to-review difficult)

(defvar memrise/json-to-field '((name 1)
                                (description 2)
                                (num_things 3)
                                (learned 4)
                                (review 5)
                                (difficult 6)))

(defun memrise/parse-course (course-alist)
  "Converts given alist object into a 'memrise/course' object"
  (let ((result (make-memrise/course)))
    (mapc (lambda (pair)
              (let ((field (car pair))
                    (index (cadr pair)))
                (aset result index (assoc-default field course-alist))))
          memrise/json-to-field)
    result))

(defun memrise/parse-courses (courses-list)
  "Returns a list of 'memrise/course' corresponding to the given list"
  (mapcar 'memrise/parse-course (assoc-default 'courses courses-list)))

(defun memrise/get-csrf-token ()
  "Returns CSRF token for memrise.com"
  (assoc-default "csrftoken" (request-cookie-alist "www.memrise.com" "/")))

(memrise/get-csrf-token)

(memrise/parse-courses memrise/test)

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

(request "https://www.memrise.com/home/" :parser (memrise/to-buffer "home.html"))

(request
 "https://www.memrise.com/api/category/learning/"
 :type "GET"
 :params '(("with_num_ready_to_water" . "true"))
 :parser (memrise/to-buffer "response.json"))

(request
 memrise/dasboard-url
 :type "GET"
 :params '(("courses_filter" . "most_recent"))
 :parser 'json-read
 :success (cl-function
           (lambda (&key data &allow-other-keys)
             (message "I got: %S" (memrise/parse-courses data)))))
