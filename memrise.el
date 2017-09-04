(use-package request :ensure t)

(setq memrise/url "https://www.memrise.com")
(setq memrise/login-url "https://www.memrise.com/login/")
(setq memrise/next-home-url "https://www.memrise.com/login/?next=/home/")
(setq memrise/dasboard-url "http://www.memrise.com/ajax/courses/dashboard/")

(request memrise/login-url)

(defun memrise/get-csrf-token ()
  "Returns CSRF token for memrise.com"
  (cdr (assoc "csrftoken" (request-cookie-alist "www.memrise.com" "/"))))

(memrise/get-csrf-token)

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
 :parser 'json-read)
