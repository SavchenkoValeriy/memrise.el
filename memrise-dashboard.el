(require 'memrise-request)
(require 'memrise-session)
(require 'memrise-ui)

(defvar memrise-mode-map
  (let ((map (make-keymap)))
    (define-key map "n" 'memrise/dashboard-course-forward)
    (define-key map "p" 'memrise/dashboard-course-backward)
    (define-key map "l" 'memrise/dashboard-course-learn)
    (define-key map "r" 'memrise/dashboard-course-review)
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
(defstruct memrise/course
  name             ;; name (Swedish 3)
  description      ;; description (couple of sentences)
  number-of-things ;; overall number of things to learn
  learned          ;; number of things already learned
  to-review        ;; number of things to review/water
  difficult        ;; number of difficult words
  id               ;; memrise ID of the course
  start            ;; dashboard position where course widget starts
  end              ;; dashboard position where course widget ends
  )

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
  (let ((all (memrise/course-number-of-things course))
        (learned (memrise/course-learned course)))
    (if (eq (- all learned) 0)
        (message "Nothing's left to learn in the course. Did you mean 'review'?")
      (memrise/start-learn-session (memrise/course-id course)))))

(defun memrise/dashboard-course-review (course)
  "Starts review/water session for the given course"
  (interactive (list (memrise/current-course)))
  (let ((to-review (memrise/course-to-review course)))
    (if (eq to-review 0)
        (message "Nothing to review in the course. Did you mean 'learn'?")
      (memrise/start-review-session (memrise/course-id course)))))

(defun memrise/dashboard-course-forward (course)
  "Moves cursor to a next course on the dashboard"
  (interactive (list (memrise/current-course)))
  (let ((dest (memrise/next-course course courses)))
    (goto-char (memrise/course-start dest))))

(defun memrise/dashboard-course-backward (course)
  "Moves cursor to a previous course on the dashboard"
  (interactive (list (memrise/current-course)))
  (let ((dest (memrise/next-course course (reverse courses))))
    (goto-char (memrise/course-start dest))))

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
  (setf (memrise/course-start course) (point-marker))
  (insert (propertize text 'course course))
  (setf (memrise/course-end course) (point-marker)))

(defun memrise/display-course (course)
  (let* ((format-objects `((name . ,(memrise/course-name course))
                           (learned . ,(memrise/course-learned course))
                           (all . ,(memrise/course-number-of-things course))
                           (rev-icon . ,memrise/review-icon)
                           (review . ,(memrise/course-to-review course))
                           (diff-icon . ,memrise/difficult-icon)
                           (difficult . ,(memrise/course-difficult course))
                           (description . ,(memrise/course-description course)))))
    (memrise/insert-course (memrise/format-elements-with-faces memrise/dashboard-format format-objects memrise/dashboard-faces) course)
    (insert "\n\n")))

;; it's some sort of a map from JSON field name to a memrise/course's field index
(defvar memrise/json-to-field '((name 1)
                                (description 2)
                                (num_things 3)
                                (learned 4)
                                (review 5)
                                (difficult 6)
                                (id 7)))

;; Course JSON parsers
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

(provide 'memrise-dashboard)


(defvar test '((courses . [((name . "German 3") (description . "Find your way around, talk about the future, learn some German expressions that will impress everyone you meet.") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/img/400sqf/from/uploads/course_photos/16054981000161215161109.jpg") (num_things . 641) (audio_mode . t) (url . "https://www.memrise.com/course/1180560/german-3/") (id . 1180560) (category (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/language_photos/german.png")) (goal (goal . 1500) (points . 975) (course_id . 1180560) (streak . 0)) (learned . 0) (review . 0) (ignored . 0) (ltm . 0) (difficult . 0) (percent_complete . 0) (pro_chats . []) (grammar_chats . [((title . "Asking questions") (mission_id . 487)) ((title . "Denial (not the river)") (mission_id . 711)) ((title . "Denial Volume 2") (mission_id . 716)) ((title . "Asking questions") (mission_id . 487)) ((title . "Denial (not the river)") (mission_id . 711)) ((title . "Denial Volume 2") (mission_id . 716))])) ((name . "German 2") (description . "Build your basic vocab. Learn to count, go shopping with confidence, learn a bunch of colloquial expressions that’ll make people laugh.") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/img/400sqf/from/uploads/course_photos/16054981000161215161055.jpg") (num_things . 348) (audio_mode . t) (url . "https://www.memrise.com/course/1180559/german-2/") (id . 1180559) (category (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/language_photos/german.png")) (goal (goal . 6000) (points . 6184) (course_id . 1180559) (streak . 1)) (learned . 348) (review . 112) (ignored . 0) (ltm . 236) (difficult . 4) (percent_complete . 100) (pro_chats . []) (grammar_chats . [((title . "Asking questions") (mission_id . 487)) ((title . "Denial (not the river)") (mission_id . 711)) ((title . "Denial Volume 2") (mission_id . 716)) ((title . "Asking questions") (mission_id . 487)) ((title . "Denial (not the river)") (mission_id . 711)) ((title . "Denial Volume 2") (mission_id . 716))])) ((name . "Swedish 1") (description . "Introduce yourself, get around, and learn a bunch of useful colloquial expressions to make people smile") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/img/400sqf/from/uploads/course_photos/14005276000160822140032.jpg") (num_things . 220) (audio_mode . t) (url . "https://www.memrise.com/course/1179762/swedish-1/") (id . 1179762) (category (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/language_photos/DemoFlags-18_copy.png")) (goal (goal . 1500) (points . 2373) (course_id . 1179762) (streak . 2)) (learned . 188) (review . 23) (ignored . 0) (ltm . 165) (difficult . 9) (percent_complete . 85) (pro_chats . [((title . "Who are you?") (mission_id . 179)) ((title . "Who are you?") (mission_id . 179))]) (grammar_chats . [])) ((name . "German 1") (description . "Introduce yourself, get around, and learn a bunch of useful colloquial German expressions to make people smile") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/img/400sqf/from/uploads/course_photos/16054981000161215161042.jpg") (num_things . 197) (audio_mode . t) (url . "https://www.memrise.com/course/1180558/german-1/") (id . 1180558) (category (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/language_photos/german.png")) (goal (goal . 6000) (points . 0) (course_id . 1180558) (streak . 0)) (learned . 197) (review . 197) (ignored . 0) (ltm . 0) (difficult . 10) (percent_complete . 100) (pro_chats . []) (grammar_chats . [((title . "Asking questions") (mission_id . 487)) ((title . "Denial (not the river)") (mission_id . 711)) ((title . "Denial Volume 2") (mission_id . 716)) ((title . "Asking questions") (mission_id . 487)) ((title . "Denial (not the river)") (mission_id . 711)) ((title . "Denial Volume 2") (mission_id . 716))]))]) (to_review_total . 0) (has_more_courses . :json-false)))
