(require 'memrise-request)
(require 'memrise-session)
(require 'memrise-ui)
(require 'all-the-icons)
(require 'jeison)

(defvar memrise-mode-map
  (let ((map (make-keymap)))
    (define-key map "n" 'memrise/dashboard-course-forward)
    (define-key map "p" 'memrise/dashboard-course-backward)
    (define-key map "l" 'memrise/dashboard-course-learn)
    (define-key map "r" 'memrise/dashboard-course-review)
    (define-key map "g" 'memrise/dashboard)
    map)
  "Keymap for a memrise dashboard buffer")

(define-derived-mode memrise-mode special-mode "Memrise")

(defun memrise/dashboard-buffer ()
  (get-buffer-create "Memrise"))

;;;###autoload
(defun memrise/dashboard ()
  (interactive)
  (lexical-let ((buffer (memrise/dashboard-buffer)))
    (memrise/request-dashboard
     (lambda (data)
       (with-current-buffer buffer
         (let ((inhibit-read-only t))
           (kill-all-local-variables)
           (erase-buffer)
           (memrise-mode)
           (make-local-variable 'courses)
           (setq courses (memrise/parse-courses data))
           (memrise/display-courses courses buffer)
           (switch-to-buffer buffer)))))))

;; Course structs
(jeison-defclass memrise-course nil
  ((name :documentation "name (Swedish 3)")
   (description :documentation "description (couple of sentences)")
   (number-of-things :path num_things :documentation "overall number of things to learn")
   (learned :documentation "number of things already learned")
   (to-review :path review :documentation "number of things to review/water")
   (difficult :documentation "number of difficult words")
   (id :documentation "memrise ID of the course")
   (start :documentation "dashboard position where course widget starts")
   (end :documentation "dashboard position where course widget ends")
   ))

(defun memrise/courses ()
  (with-current-buffer (memrise/dashboard-buffer)
    courses))

(defun memrise/current-course ()
  (get-text-property (point-marker) 'course))

(defun memrise/next-course (course courses)
  (let ((next-courses (cdr (member course (memrise/courses)))))
    (if next-courses
        (car next-courses)
      (car courses))))

(defun memrise/dashboard-course-learn (course)
  "Starts 'learning new words' session for the given course"
  (interactive (list (memrise/current-course)))
  (let ((all (oref course number-of-things))
        (learned (oref course learned)))
    (if (eq (- all learned) 0)
        (message "Nothing's left to learn in the course. Did you mean 'review'?")
      (memrise/start-learn-session (oref course id)))))

(defun memrise/dashboard-course-review (course)
  "Starts review/water session for the given course"
  (interactive (list (memrise/current-course)))
  (let ((to-review (oref course to-review)))
    (if (eq to-review 0)
        (message "Nothing to review in the course. Did you mean 'learn'?")
      (memrise/start-review-session (oref course id)))))

(defun memrise/dashboard-course-forward (course)
  "Moves cursor to a next course on the dashboard"
  (interactive (list (memrise/current-course)))
  (let ((dest (memrise/next-course course courses)))
    (goto-char (oref dest start))))

(defun memrise/dashboard-course-backward (course)
  "Moves cursor to a previous course on the dashboard"
  (interactive (list (memrise/current-course)))
  (let ((dest (memrise/next-course course (reverse courses))))
    (goto-char (oref dest start))))

(defun memrise/display-courses (courses buffer)
  (with-current-buffer buffer
    (mapc 'memrise/display-course courses)))

(defcustom memrise/dashboard-format
  "${name}    ${learned}/${all}  ${rev-icon}${review} ${diff-icon}${difficult}\n${description}"
  "Format string to display courses on Memrise dashboard.

{name}        - course name ('Swedish 3')
{learned}     - number of things that the user learned in the course
{all}         - overall number of things in the course
{rev-icon}    - icon to use for words that require review/water
{review}      - number of things to review/water in the course
{diff-icon}   - icon to use for difficult words
{difficult}   - number of words marked as 'difficult' in the course
{description} - course description"
  :group 'memrise)

(defcustom memrise/review-icon
  (all-the-icons-faicon "tint" :v-adjust 0.0)
  "Icon to use for words that require review/water"
  :group 'memrise)

(defcustom memrise/difficult-icon
  (all-the-icons-faicon "bolt" :v-adjust 0.0)
  "Icon to use for words marked as 'difficult'"
  :group 'memrise)

(defun memrise/insert-course (text course)
  (oset course start (point-marker))
  (insert (propertize text 'course course))
  (oset course end (point-marker)))

(defun memrise/display-course (course)
  (let* ((format-objects `((name . ,(oref course name))
                           (learned . ,(oref course learned))
                           (all . ,(oref course number-of-things))
                           (rev-icon . ,memrise/review-icon)
                           (review . ,(oref course to-review))
                           (diff-icon . ,memrise/difficult-icon)
                           (difficult . ,(oref course difficult))
                           (description . ,(oref course description)))))
    (memrise/insert-course (memrise/format-elements-with-faces memrise/dashboard-format format-objects memrise/dashboard-faces) course)
    (insert "\n\n")))

(defun memrise/parse-courses (courses-list)
  "Returns a list of 'memrise/course' corresponding to the given list"
  (jeison-read '(list-of memrise-course) courses-list 'courses))

(provide 'memrise-dashboard)
