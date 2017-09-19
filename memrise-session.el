;;; memrise-session.el --- Memrise session, from parsing to execution  -*- lexical-binding: t; -*-

(require 'memrise-request)
(require 'memrise-widget)
(require 'dash)
(require 'emms)
(require 's)

(setq memrise-session-mode-map
      (copy-keymap widget-keymap))

(define-derived-mode memrise-session-mode fundamental-mode "Memrise-session")

(add-hook 'memrise-session-mode 'memrise/turn-off-completions)

(defun memrise/turn-off-completions ()
  "Turn off completion minor modes during session.
Completion doesn't really help a learning process."
  (memrise/turn-off-minor-mode 'company-mode)
  (memrise/turn-off-minor-mode 'auto-complete-mode))

(defun memrise/turn-off-minor-mode (mode)
  "Turn off given minor `mode' if it's active."
  (when (bound-and-true-p mode)
    (funcall mode nil)))

(defun memrise/session-buffer ()
  (get-buffer-create "*session*"))

(defun memrise/start-learn-session (course-id)
  "Starts a new memrise learn session"
  (memrise/start-session course-id "learn"))

(defun memrise/start-review-session (course-id)
  "Starts a new memrise review/water session"
  (memrise/start-session course-id "classic_review"))

(defun memrise/start-session (course-id type)
  "Starts a new memrise session"
  (lexical-let ((buffer (memrise/session-buffer)))
    (with-current-buffer buffer
;;      (memrise/request-session
;;       course-id
;;       type
;;       'memrise/start-session-internal)
      (memrise/start-session-internal session-test)
      (switch-to-buffer buffer))))

(defun memrise/start-session-internal (json)
  (with-current-buffer (memrise/session-buffer)
    (let ((inhibit-read-only t))
      (kill-all-local-variables)
      (erase-buffer)
      (memrise-session-mode)
      (make-local-variable 'session)
      (setq session (memrise/parse-session json))
      (memrise/display-session))))

(defun memrise/display-session ()
  (make-local-variable 'main-widget)
  (make-local-variable 'next-task)
  (widget-insert (memrise/session-course-name session))
  (widget-insert "\n")
  (widget-insert (memrise/session-title session))
  (widget-insert "\n\n")
  (memrise/display-tasks (memrise/session-tasks session)))

(defun memrise/display-tasks (tasks)
  (lexical-let* ((task (car tasks))
                 (learnable (assoc-default (memrise/session-task-learnable-id task)
                                           (memrise/session-learnables session))))
    (setq next-task (-partial 'memrise/display-next-task-internal tasks))
    (setq main-widget nil)
    (setq main-widget
          (if (string= (memrise/session-task-template task)
                       "presentation")
              (memrise/presentation learnable)
            (memrise/pick-and-display-test learnable
                                           (memrise/session-task-learn-level task))))
    (widget-setup)))

(defun memrise/pick-and-display-test (learnable level)
  (let ((tests (memrise/session-learnable-tests learnable))
        (number-of-choices (memrise/decide-number-of-choices level)))
    (memrise/display-test (memrise/pick-test tests level)
                          number-of-choices)))

(defvar memrise/minimal-number-of-choices 4)
(defvar memrise/average-number-of-choices 6)
(defvar memrise/maximal-number-of-choices 8)

(defun memrise/decide-number-of-choices (level)
  (memrise/icase level
    `((1 . 2) ,memrise/minimal-number-of-choices)
    `((3 . 4) ,memrise/average-number-of-choices)
    `((5 . 6) ,memrise/maximal-number-of-choices)
    ;; words for review have `level' == `nil'
    `(nil     ,memrise/maximal-number-of-choices)))

(defun memrise/pick-test (tests level)
  "According to the given `level' picks one of the `tests'"
  (if (eq level 1)
      (assoc-default "multiple_choice")
    (cdr (memrise/random-element tests))))

(defun memrise/display-test (test number)
  (pcase (memrise/session-test-kind test)
    ("multiple_choice"          (memrise/multiple-choice-widget test number))
    ("reversed_multiple_choice" (memrise/reversed-multiple-choice-widget test number))
    ("audio_multiple_choice"    (memrise/audio-multiple-choice-widget
                                 test memrise/minimal-number-of-choices))
    ("typing"                   (memrise/typing-widget test))))

(defun memrise/icase (value &rest args)
  (let* ((head (car args))
         (range (car head))
         (result (cadr head)))
    (cond
     ((not head) nil) ;; the list of args is over
     ;; if value \in range -> return corresponding result
     ((memrise/icase-in-range-p value range) result)
     ;; try other arguments
     (t (apply 'memrise/icase value (cdr args))))))

(defun memrise/icase-in-range-p (value range)
  (cond
   ((-cons-pair? range) (and value ;; value can be `nil'
                             (>= value (car range))
                             (<= value (cdr range))))
   ;; if range is not actually a range, simply compare values
   (t (eq value range))))

(defun memrise/display-next-task ()
  (interactive)
  (funcall next-task))

(defun memrise/display-next-task-internal (tasks)
  (if main-widget
      (widget-delete main-widget))
  (memrise/reset-session-bindings)
  (memrise/display-tasks (cdr tasks)))

(defun memrise/reset-session-bindings ()
  (setq memrise-session-mode-map
        (copy-keymap widget-keymap))
  (use-local-map memrise-session-mode-map))

(defstruct memrise/session
  course-name ;; "Russian 2"
  title       ;; "Fuel Your Vocab: Places"
  source      ;; in what language are we learning
  target      ;; what are we learning
  tasks       ;; learning tasks
  learnables  ;; actual things that can be learned
  )

(defstruct memrise/session-task
  learnable-id ;; id of a learnable thing to train in this task
  template     ;; kind of a task (presentation or sentinel)
  learn-level  ;; learn level (nil or 1-6)
  )

(defstruct memrise/session-learnable
  id                  ;; unique ID to identify the thing
  text                ;; text representation
  translation         ;; translation of a text representation
  audio               ;; audio representation
  literal-translation ;; can be nil
  tests               ;; tests that are possible to be taken
  )

(defstruct memrise/session-test
  kind     ;; one of '("multiple_choice"
           ;;          "reversed_multiple_choice"
           ;;          "audio_multiple_choice"
           ;;          "typing"
           ;;          "tapping")
  prompt   ;; information for a test title
  correct  ;; correct answer
  choices  ;; other choices (do not include correct)
  accepted ;; other accepted answers
  )

(defstruct memrise/session-test-prompt
  text  ;; text to show
  audio ;; audio to play (can be nil)
  )

(defvar memrise/video-quality 'medium
  "Memrise video quality, one of '(low medium high)")

(defcustom memrise/material-storage-directory
  (concat (file-name-as-directory user-emacs-directory) "memrise")
  "Directory to store data related to request.el."
  :type 'directory
  :group 'memrise)

;; JSON parsing section
(defun memrise/parse-session (json)
  (let* ((name (memrise/parse-session-course-name json))
         (title (memrise/parse-session-title json))
         (source (memrise/parse-session-source json))
         (target (memrise/parse-session-target json))
         (tasks (memrise/parse-session-tasks json))
         (learnables (memrise/parse-session-learnables json)))
    (make-memrise/session
     :course-name name
     :title title
     :source source
     :target target
     :tasks tasks
     :learnables learnables)))

(defun memrise/parse-session-course-name (json)
  (let* ((session (assoc-default 'session json))
         (course (assoc-default 'course session))
         (name (assoc-default 'name course)))
    name))

(defun memrise/parse-session-title (json)
  (let* ((session (assoc-default 'session json))
         (level (assoc-default 'level session))
         (title (assoc-default 'title level)))
    title))

(defun memrise/parse-session-source (json)
  (let* ((session (assoc-default 'session json))
         (course (assoc-default 'course session))
         (source (assoc-default 'source course)))
    (assoc-default 'name source)))

(defun memrise/parse-session-target (json)
  (let* ((session (assoc-default 'session json))
         (course (assoc-default 'course session))
         (target (assoc-default 'target course)))
    (assoc-default 'name target)))

(defun memrise/parse-session-tasks (json)
  (mapcar 'memrise/parse-session-task (assoc-default 'boxes json)))

(defun memrise/parse-session-task (json)
  (let ((id (string-to-number (assoc-default 'learnable_id json)))
        (template (assoc-default 'template json))
        (learn-level (assoc-default 'learn_session_level json)))
    (make-memrise/session-task
     :learnable-id id
     :template template
     :learn-level learn-level)))

(defun memrise/parse-session-learnables (json)
  (mapcar 'memrise/parse-session-learnable
          (assoc-default 'learnables json)))

(defun memrise/parse-session-learnable (json)
  (let* ((id (string-to-number
              (assoc-default 'learnable_id json)))
         (text (assoc-default 'value
                              (assoc-default 'item json)))
         (translation (assoc-default 'value
                                     (assoc-default 'definition json)))
         (audio (memrise/parse-session-learnable-audio json))
         (literal-translation (memrise/parse-session-learnable-literal-translation json))
         (tests (memrise/parse-session-tests json)))
    `(,id . ,(make-memrise/session-learnable
              :id id
              :text text
              :translation translation
              :audio audio
              :literal-translation literal-translation
              :tests tests))))

(defun memrise/parse-session-learnable-audio (json)
  (memrise/process-audio
   (memrise/parse-column-value json "Audio")))

(defun memrise/parse-session-learnable-literal-translation (json)
  (memrise/parse-column-value json "Literal translation"))

(defun memrise/parse-column-value (json label)
  (let ((column (find-if (lambda (x)
                           (string= (assoc-default 'label x)
                                    label))
                         (assoc-default 'columns json))))
    (assoc-default 'value column)))

(defun memrise/parse-session-tests (json)
  (mapcar 'memrise/parse-session-test
          (assoc-default 'tests json)))

(defun memrise/parse-session-test (json)
  (let* ((kind (symbol-name (car json))) ;; kind would be a string
         (body (cdr json))
         (prompt (memrise/parse-session-test-prompt body))
         (correct (assoc-default 'correct body))
         (choices (memrise/vector-to-list
                   (assoc-default 'choices body)))
         (accepted (memrise/vector-to-list
                    (assoc-default 'accepted body))))
    ;; if test is an audio test we should download all audios
    (when (s-contains? "audio" kind)
      (setq correct (memrise/process-audio correct))
      (setq choices (memrise/process-audio choices)))
    `(,kind . ,(make-memrise/session-test
                :kind kind
                :prompt prompt
                :correct correct
                :choices choices
                :accepted accepted))))

(defun memrise/parse-session-test-prompt (json)
  (let* ((body (assoc-default 'prompt json))
         (text (assoc-default 'text body))
         (audio (memrise/process-audio
                 (assoc-default 'audio body))))
   (make-memrise/session-test-prompt
    :text text
    :audio audio)))

(defun memrise/process-audio (vector-or-list)
  (let ((result (memrise/download-audios
                 (if (vectorp vector-or-list)
                     (memrise/vector-to-list vector-or-list)
                   vector-or-list))))
    (if (eq (length result) 1)
        (car result)
      result)))

(defun memrise/download-audios (urls)
  (mapcar 'memrise/download-audio urls))

(defun memrise/download-audio (url)
  (let* ((audio-dir (file-name-as-directory "audio"))
         (file (concat
                audio-dir
                (memrise/get-audio-id url)
                "."
                (memrise/get-audio-extension url))))
    (memrise/download url file)))

(defun memrise/get-audio-id (url)
  ;; example: url == .../11967/1.mp3
  (file-name-nondirectory        ;; 11967
   (directory-file-name          ;; .../11967
    (file-name-directory url)))) ;; .../11967/

(defun memrise/get-audio-extension (url)
  (file-name-extension url))

(defun memrise/download-video (json)
  (let* ((url (assoc-default memrise/video-quality json))
         (video-dir (file-name-as-directory "video"))
         (file (concat video-dir (memrise/get-video-file-name url))))
    (memrise/download url file)))

(defun memrise/get-video-file-name (url)
  (file-name-nondirectory url))

(defun memrise/download (what where)
  "Download file from location `what' and puts it by location `where'"
  (let* ((where (concat
                 (file-name-as-directory
                  memrise/material-storage-directory)
                 where))
         (dest-dir (file-name-directory where)))
    (if (file-exists-p where)
        ;; re-use previously downloaded one
        where
      (if (not (file-exists-p dest-dir))
          ;; recursively make directories for downloading
          (make-directory dest-dir t))
      (url-copy-file what where)
      where)))

(defun memrise/vector-to-list (vector)
  (append vector nil))

(defun memrise/integer-for-id (symbol)
  (string-to-number (symbol-name symbol)))

(defun memrise/id-for-integer (integer)
  (intern (number-to-string integer)))

(defun memrise/test ()
  (interactive)
  (message "%S" (memrise/parse-session session-test)))

(setq session-test '((initial_session_points . 604679) (session (slug . "learn") (is_audio_enabled . t) (course_id . 1180560) (course (id . 1180560) (name . "German 3") (slug . "german-3") (url . "/course/1180560/german-3/") (description . "Find your way around, talk about the future, learn some German expressions that will impress everyone you meet.") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/course_photos/16054981000161215161109.jpg") (photo_small . "https://d2rhekw5qr4gcj.cloudfront.net/img/100sqf/from/uploads/course_photos/16054981000161215161109.jpg") (photo_large . "https://d2rhekw5qr4gcj.cloudfront.net/img/400sqf/from/uploads/course_photos/16054981000161215161109.jpg") (num_things . 641) (num_levels . 43) (num_learners . 9178) (source (id . 963) (slug . "english-us") (name . "English (US)") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/category_photos/us_flag.png") (parent_id . 578) (index . 0)) (target (id . 4) (slug . "german-2") (name . "German") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/language_photos/german.png") (parent_id . 879) (index . 1048))) (category (id . 4) (slug . "german-2") (name . "German") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/language_photos/german.png") (parent_id . 879) (index . 1048)) (goal_points . 1621) (goal . 1500) (streak . 1) (level_id . 4568860) (level (id . 4568860) (index . 3) (course_id . 1180560) (title . "Fuel Your Vocab: Places") (kind . 1) (url . "/course/1180560/german-3/3/") (column_a . 1) (column_b . 2) (pool_id . 100080019))) (boxes . [((learnable_id . "65550792982786") (template . "presentation")) ((learnable_id . "65550793048322") (template . "presentation")) ((learnable_id . "65550793113858") (template . "presentation")) ((learnable_id . "65550793179394") (template . "presentation")) ((learnable_id . "65550793113858") (template . "sentinel") (learn_session_level . 3)) ((learnable_id . "65550793244930") (template . "presentation")) ((learnable_id . "65550793310466") (template . "presentation")) ((learnable_id . "65550793113858") (template . "sentinel") (learn_session_level . 4)) ((learnable_id . "65550793244930") (template . "sentinel") (learn_session_level . 3)) ((learnable_id . "65550793113858") (template . "sentinel") (learn_session_level . 5)) ((learnable_id . "65550793310466") (template . "sentinel") (learn_session_level . 3)) ((learnable_id . "65550793048322") (template . "sentinel") (learn_session_level . 5)) ((learnable_id . "65550793244930") (template . "sentinel") (learn_session_level . 4)) ((learnable_id . "65550793048322") (template . "sentinel") (learn_session_level . 6)) ((learnable_id . "65550793179394") (template . "sentinel") (learn_session_level . 4)) ((learnable_id . "65550793310466") (template . "sentinel") (learn_session_level . 4)) ((learnable_id . "65550793113858") (template . "sentinel") (learn_session_level . 6)) ((learnable_id . "65550792982786") (template . "sentinel") (learn_session_level . 5)) ((learnable_id . "65550793179394") (template . "sentinel") (learn_session_level . 5)) ((learnable_id . "65550792982786") (template . "sentinel") (learn_session_level . 6)) ((learnable_id . "65550793244930") (template . "sentinel") (learn_session_level . 5)) ((learnable_id . "65550793179394") (template . "sentinel") (learn_session_level . 6)) ((learnable_id . "65550793244930") (template . "sentinel") (learn_session_level . 6)) ((learnable_id . "65550793310466") (template . "sentinel") (learn_session_level . 5)) ((learnable_id . "65550793310466") (template . "sentinel") (learn_session_level . 6))]) (learnables . [((learnable_id . "65550792982786") (thing_id . 1000225723) (item (kind . "text") (value . "die Karte") (label . "German") (always_show . :json-false) (alternatives . #1=[]) (classes . #1#)) (definition (kind . "text") (value . "the map; the card") (label . "English (US)") (always_show . :json-false) (alternatives . #1#) (classes . #1#)) (tests (multiple_choice (prompt (text . "the map; the card") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12356/1.mp3"])) (correct . "die Karte") (choices . ["die Kartoffel" "die Karriere" "die Kamera" "die Katze" "die Katastrophe" "die Kampagne" "die Kuh" "die Kirche" "die Komödie" "die Kunst" "die Kneipe" "die Kosten"]) (accepted . #1#)) (reversed_multiple_choice (prompt (text . "die Karte") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12356/1.mp3"])) (correct . "the map; the card") (choices . ["the potato" "the career" "the camera" "the cat" "the disaster; the catastrophe" "the campaign" "the cow" "the church" "the comedy" "the art" "the bar; the pub" "the costs"]) (accepted . #1#)) (audio_multiple_choice (prompt (text . "the map; the card")) (correct . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12356/1.mp3"]) (choices . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/11851/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13102/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14104/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12237/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14311/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14335/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12239/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12560/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13086/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12598/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12352/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14020/1.mp3"]) (accepted . #1#)) (typing (prompt (text . "the map; the card") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12356/1.mp3"])) (correct . "die Karte") (choices . "irdtKace") (accepted . ["die karte"]))) (columns . [((kind . "audio") (value . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12356/1.mp3"]) (label . "Audio") (always_show . :json-false) (alternatives . #1#) (classes . #1#))]) (attributes . #1#) (audios . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12356/1.mp3"]) (show_at_tests_attributes . #1#) (show_at_tests_info) (is_typing_strict . :json-false) (is_right_to_left . :json-false)) ((learnable_id . "65550793048322") (thing_id . 1000225724) (item (kind . "text") (value . "der Reiseführer") (label . "German") (always_show . :json-false) (alternatives . #1#) (classes . #1#)) (definition (kind . "text") (value . "the guide (book)") (label . "English (US)") (always_show . :json-false) (alternatives . #1#) (classes . #1#)) (tests (multiple_choice (prompt (text . "the guide (book)") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12357/1.mp3"])) (correct . "der Reiseführer") (choices . ["der Reisepass" "der Flug" "das reicht!" "der Reifen" "der Rock" "der Roman" "der Regenschirm" "der Regenwald" "der Rucksack" "der Rat" "die Reihe" "der Reis"]) (accepted . #1#)) (reversed_multiple_choice (prompt (text . "der Reiseführer") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12357/1.mp3"])) (correct . "the guide (book)") (choices . ["the passport" "the flight" "that's enough!" "the tire" "the skirt" "the novel" "the umbrella" "the rainforest" "the backpack" "the advice; the piece of advice" "the range (of products); the row" "the rice"]) (accepted . #1#)) (audio_multiple_choice (prompt (text . "the guide (book)")) (correct . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12357/1.mp3"]) (choices . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12679/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12808/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12171/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14222/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12185/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13852/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12201/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13360/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13362/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13621/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14273/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/11848/1.mp3"]) (accepted . #1#)) (typing (prompt (text . "the guide (book)") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12357/1.mp3"])) (correct . "der Reiseführer") (choices . "erfüpsihdR") (accepted . ["der reiseführer" "der reisefuhrer"]))) (columns . [((kind . "audio") (value . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12357/1.mp3"]) (label . "Audio") (always_show . :json-false) (alternatives . #1#) (classes . #1#))]) (attributes . #1#) (audios . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12357/1.mp3"]) (show_at_tests_attributes . #1#) (show_at_tests_info) (is_typing_strict . :json-false) (is_right_to_left . :json-false)) ((learnable_id . "65550793113858") (thing_id . 1000225725) (item (kind . "text") (value . "breit") (label . "German") (always_show . :json-false) (alternatives . #1#) (classes . #1#)) (definition (kind . "text") (value . "wide; broad") (label . "English (US)") (always_show . :json-false) (alternatives . #1#) (classes . #1#)) (tests (multiple_choice (prompt (text . "wide; broad") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14289/1.mp3"])) (correct . "breit") (choices . ["brennen" "braun" "brauchen" "blau" "bald" "Boxen" "bitte" "beide" "blond" "bauen" "beste"]) (accepted . #1#)) (reversed_multiple_choice (prompt (text . "breit") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14289/1.mp3"])) (correct . "wide; broad") (choices . ["to burn (up)" "brown" "to need" "blue" "soon" "boxing" "please" "both" "blonde" "to build" "best"]) (accepted . #1#)) (audio_multiple_choice (prompt (text . "wide; broad")) (correct . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14289/1.mp3"]) (choices . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14242/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12158/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/11992/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12150/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12895/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13079/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/11801/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14503/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12466/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13270/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13148/1.mp3"]) (accepted . #1#)) (typing (prompt (text . "wide; broad") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14289/1.mp3"])) (correct . "breit") (choices . "srxtedbi") (accepted . ["breit"]))) (columns . [((kind . "audio") (value . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14289/1.mp3"]) (label . "Audio") (always_show . :json-false) (alternatives . #1#) (classes . #1#))]) (attributes . #1#) (audios . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14289/1.mp3"]) (show_at_tests_attributes . #1#) (show_at_tests_info) (is_typing_strict . :json-false) (is_right_to_left . :json-false)) ((learnable_id . "65550793179394") (thing_id . 1000225726) (item (kind . "text") (value . "der Weg") (label . "German") (always_show . :json-false) (alternatives . #1#) (classes . #1#)) (definition (kind . "text") (value . "the way") (label . "English (US)") (always_show . :json-false) (alternatives . #1#) (classes . #1#)) (tests (multiple_choice (prompt (text . "the way") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14077/1.mp3"])) (correct . "der Weg") (choices . ["der Sinn" "der Wasserfall" "der Flug" "der Wochentag" "der Witz" "der See" "der Weißwein" "die Welt" "der Plan" "der Mann" "weit weg" "der Wald"]) (accepted . #1#)) (reversed_multiple_choice (prompt (text . "der Weg") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14077/1.mp3"])) (correct . "the way") (choices . ["the sense" "the waterfall" "the flight" "the day of the week" "the joke" "the lake" "the white wine" "the world" "the plan" "the man; the husband" "far away" "the forest"]) (accepted . #1#)) (audio_multiple_choice (prompt (text . "the way")) (correct . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14077/1.mp3"]) (choices . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13056/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13700/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12808/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12938/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13195/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12814/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12376/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12867/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12822/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12121/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12570/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13359/1.mp3"]) (accepted . #1#)) (typing (prompt (text . "the way") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14077/1.mp3"])) (correct . "der Weg") (choices . "ldesSgWr") (accepted . ["der weg"]))) (columns . [((kind . "audio") (value . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14077/1.mp3"]) (label . "Audio") (always_show . :json-false) (alternatives . #1#) (classes . #1#))]) (attributes . #1#) (audios . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14077/1.mp3"]) (show_at_tests_attributes . #1#) (show_at_tests_info) (is_typing_strict . :json-false) (is_right_to_left . :json-false)) ((learnable_id . "65550793244930") (thing_id . 1000225727) (item (kind . "text") (value . "sich verlaufen") (label . "German") (always_show . :json-false) (alternatives . #1#) (classes . #1#)) (definition (kind . "text") (value . "to get (oneself) lost") (label . "English (US)") (always_show . :json-false) (alternatives . #1#) (classes . #1#)) (tests (multiple_choice (prompt (text . "to get (oneself) lost") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12358/1.mp3"])) (correct . "sich verlaufen") (choices . ["wir sind" "sie sind" "Sie sind" "sie ist" "verstehst du?" "sich übergeben" "sich interessieren" "Zeit verbringen" "sich verspäten" "sich entspannen" "sich betrinken" "das Verbrechen"]) (accepted . #1#)) (reversed_multiple_choice (prompt (text . "sich verlaufen") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12358/1.mp3"])) (correct . "to get (oneself) lost") (choices . ["we're" "they're" "you're (formal)" "she's; it's" "do you understand?" "to vomit (oneself)" "to interest (oneself)" "to spend time" "to be late; to be delayed" "to relax (oneself)" "to get (oneself) drunk" "the crime"]) (accepted . #1#)) (audio_multiple_choice (prompt (text . "to get (oneself) lost")) (correct . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12358/1.mp3"]) (choices . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/11937/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/11939/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/11940/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/11977/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12003/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12492/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12615/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12766/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12817/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12970/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12971/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13041/1.mp3"]) (accepted . #1#)) (typing (prompt (text . "to get (oneself) lost") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12358/1.mp3"])) (correct . "sich verlaufen") (choices . "bulvcarenfhsi") (accepted . ["sich verlaufen"]))) (columns . [((kind . "audio") (value . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12358/1.mp3"]) (label . "Audio") (always_show . :json-false) (alternatives . #1#) (classes . #1#))]) (attributes . #1#) (audios . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12358/1.mp3"]) (show_at_tests_attributes . #1#) (show_at_tests_info) (is_typing_strict . :json-false) (is_right_to_left . :json-false)) ((learnable_id . "65550793310466") (thing_id . 1000225728) (item (kind . "text") (value . "durcheinander") (label . "German") (always_show . :json-false) (alternatives . #1#) (classes . #1#)) (definition (kind . "text") (value . "confused") (label . "English (US)") (always_show . :json-false) (alternatives . #1#) (classes . #1#)) (tests (multiple_choice (prompt (text . "confused") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12359/1.mp3"])) (correct . "durcheinander") (choices . ["durch" "durchmachen" "du" "dumm" "dunkel" "duschen" "dunkelrot" "drei" "dir" "dort" "das" "dünn"]) (accepted . #1#)) (reversed_multiple_choice (prompt (text . "durcheinander") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12359/1.mp3"])) (correct . "confused") (choices . ["through" "to go through" "you (singular informal)" "stupid" "dark" "to take a shower" "dark red" "three; 3" "you; to you (singular informal)" "there" "this; that; the" "thin"]) (accepted . #1#)) (audio_multiple_choice (prompt (text . "confused")) (correct . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12359/1.mp3"]) (choices . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/13439/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/14171/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/11819/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12257/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12161/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12960/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12163/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/11915/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/11812/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12080/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12092/1.mp3" "https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/11967/1.mp3"]) (accepted . #1#)) (typing (prompt (text . "confused") (audio . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12359/1.mp3"])) (correct . "durcheinander") (choices . "uniarehcdo") (accepted . ["durcheinander"]))) (columns . [((kind . "audio") (value . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12359/1.mp3"]) (label . "Audio") (always_show . :json-false) (alternatives . #1#) (classes . #1#)) ((kind . "text") (value . "through-an-other") (label . "Literal translation") (always_show . t) (alternatives . #1#) (classes . #1#))]) (attributes . #1#) (audios . ["https://d2rhekw5qr4gcj.cloudfront.net/uploads/babylon/8/audios-v3/12359/1.mp3"]) (show_at_tests_attributes . #1#) (show_at_tests_info (kind . "text") (value . "through-an-other") (label . "Literal translation") (always_show . t) (alternatives . #1#) (classes . #1#)) (is_typing_strict . :json-false) (is_right_to_left . :json-false))]) (thingusers . [((thing_id . 1000225723) (user_id . 26440507) (column_a . 1) (column_b . 2) (ignored . :json-false) (last_date . "2017-09-11T13:59:35Z") (created_date . "2017-09-11T13:55:30Z") (next_date . "2017-09-11T13:55:31Z") (interval . 0.01) (growth_level . 4) (current_streak . 4) (mem_id) (starred . :json-false) (correct . 4) (attempts . 5) (total_streak . 4) (is_difficult . :json-false) (learnable_id . "65550792982786")) ((thing_id . 1000225724) (user_id . 26440507) (column_a . 1) (column_b . 2) (ignored . :json-false) (last_date . "2017-09-11T13:59:05Z") (created_date . "2017-09-11T13:55:42Z") (next_date . "2017-09-11T13:55:43Z") (interval . 0.01) (growth_level . 4) (current_streak . 3) (mem_id) (starred . :json-false) (correct . 4) (attempts . 5) (total_streak . 3) (is_difficult . :json-false) (learnable_id . "65550793048322")) ((thing_id . 1000225725) (user_id . 26440507) (column_a . 1) (column_b . 2) (ignored . :json-false) (last_date . "2017-09-11T13:58:34Z") (created_date . "2017-09-11T13:56:24Z") (next_date . "2017-09-11T13:56:25Z") (interval . 0.01) (growth_level . 2) (current_streak . 2) (mem_id) (starred . :json-false) (correct . 2) (attempts . 2) (total_streak . 2) (is_difficult . :json-false) (learnable_id . "65550793113858")) ((thing_id . 1000225726) (user_id . 26440507) (column_a . 1) (column_b . 2) (ignored . :json-false) (last_date . "2017-09-11T13:58:40Z") (created_date . "2017-09-11T13:56:28Z") (next_date . "2017-09-11T13:56:29Z") (interval . 0.01) (growth_level . 3) (current_streak . 3) (mem_id) (starred . :json-false) (correct . 3) (attempts . 3) (total_streak . 3) (is_difficult . :json-false) (learnable_id . "65550793179394")) ((thing_id . 1000225727) (user_id . 26440507) (column_a . 1) (column_b . 2) (ignored . :json-false) (last_date . "2017-09-11T13:58:28Z") (created_date . "2017-09-11T13:58:11Z") (next_date . "2017-09-11T13:58:11Z") (interval . 0.01) (growth_level . 2) (current_streak . 2) (mem_id) (starred . :json-false) (correct . 2) (attempts . 2) (total_streak . 2) (is_difficult . :json-false) (learnable_id . "65550793244930")) ((thing_id . 1000225728) (user_id . 26440507) (column_a . 1) (column_b . 2) (ignored . :json-false) (last_date . "2017-09-11T13:59:00Z") (created_date . "2017-09-11T13:58:47Z") (next_date . "2017-09-11T13:58:47Z") (interval . 0.01) (growth_level . 2) (current_streak . 2) (mem_id) (starred . :json-false) (correct . 2) (attempts . 2) (total_streak . 2) (is_difficult . :json-false) (learnable_id . "65550793310466"))]) (things_to_courses) (can_curate . :json-false)))

(provide 'memrise-session)
