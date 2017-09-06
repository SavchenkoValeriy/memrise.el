(use-package request :ensure t)

(require 'cl)

(setq memrise/url "https://www.memrise.com")
(setq memrise/login-url "https://www.memrise.com/login/")
(setq memrise/next-home-url "https://www.memrise.com/login/?next=/home/")
(setq memrise/dasboard-url "http://www.memrise.com/ajax/courses/dashboard/")

(defun memrise/get-csrf-token ()
  "Returns CSRF token for memrise.com"
  (assoc-default "csrftoken" (request-cookie-alist "www.memrise.com" "/")))

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

(defun memrise/home ()
  (request
   "https://www.memrise.com/home/"
   :parser (memrise/to-buffer "home.html")))

(defun memrise/request-dashboard (callback)
  (lexical-let ((inner callback))
  (request
   memrise/dasboard-url
   :type "GET"
   :params '(("courses_filter" . "most_recent"))
   :parser 'json-read
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (funcall inner data))))))

(provide 'memrise-request)
