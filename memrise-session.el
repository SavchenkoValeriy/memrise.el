(require 'memrise-request)

(define-derived-mode memrise-session-mode special-mode "Memrise-session")

(defun memrise/session-buffer ()
  (get-buffer-create "*session*"))

(defun memrise/start-session (course-id type)
  "Starts a new memrise session"
  (lexical-let ((buffer (memrise/session-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (kill-all-local-variables)
        (erase-buffer)
        (memrise-session-mode)
        (make-local-variable 'things-to-learn)
        (memrise/request-session
         course-id
         type
         (lambda (data)
           (with-current-buffer buffer
             (message "%S" data))))
        (switch-to-buffer buffer)))))

(defun memrise/start-learn-session (course-id)
  "Starts a new memrise learn session"
  (memrise/start-session course-id "learn"))

(defun memrise/start-review-session (course-id)
  "Starts a new memrise review/water session"
  (memrise/start-session course-id "classic_review"))

(defstruct memrise/session
  course-name
  title
  helper
  things
  tasks
  )

(defstruct memrise/helper
  source
  target
  (audio "Audio")
  (video "Videos")
  (literal-translation "Literal translation")
  pools
  )

(defstruct memrise/session-thing
  id
  text
  translation
  literal-translation
  audio
  video
  text-options
  translation-options
  audio-options
  )

(defstruct memrise/session-task
  thing-id
  kind
  column-a
  column-b
  learn-level
  )

(defstruct memrise/session-pool
  id
  columns
  )

(defstruct memrise/session-pool-column
  id
  kind
  keyboard
  )

(defvar memrise/video-quality 'medium
  "Memrise video quality, one of '(low medium high)")

(defvar memrise/material-storage-url "https://d2rhekw5qr4gcj.cloudfront.net/"
  "Memrise web-site used to download all audio/video materials")

(defcustom memrise/material-storage-directory
  (concat (file-name-as-directory user-emacs-directory) "memrise")
  "Directory to store data related to request.el."
  :type 'directory)

;; JSON parsing section
(defun memrise/parse-session (json)
  (let* ((name (memrise/parse-session-course-name json))
         (title (memrise/parse-session-title json))
         (helper (memrise/parse-session-helper json))
         (things (memrise/parse-session-things json helper))
         (tasks (memrise/parse-session-tasks json)))
    (make-memrise/session
     :course-name name
     :title title
     :helper helper
     :things things
     :tasks tasks)))

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

(defun memrise/parse-session-things (json pools)
  (mapcar
   `(lambda (thing-json) (memrise/parse-session-thing thing-json pools))
   (assoc-default 'things json)))

(defun memrise/parse-session-thing (json generic-helper)
  (let* ((id (memrise/integer-for-id (car json)))
         (columns (assoc-default 'columns json))
         (pool-id (assoc-default 'pool_id json))
         (helper (memrise/get-specific-helper generic-helper pool-id))
         (texts (memrise/parse-session-thing-texts columns helper))
         (translations (memrise/parse-session-thing-translations columns helper))
         (audios (memrise/parse-session-thing-audios columns helper))
         (literal-translation (memrise/parse-session-thing-literal-translation columns helper))
         (video (memrise/parse-session-thing-video columns helper)))
    `(,id . ,(make-memrise/session-thing
              :id id
              :text (car texts)
              :translation (car translations)
              :literal-translation literal-translation
              :audio (car audios)
              :video video
              :text-options (cdr texts)
              :translation-options (cdr translations)
              :audio-options (cdr audios)))))

(defun memrise/get-specific-helper (helper pool-id)
  "Returns modified version of a helper specific to the given pool-id"
  (let ((result (copy-memrise/helper helper))
        (pools (memrise/helper-pools helper)))
    (setf (memrise/helper-pools result) `(,(assoc-default pool-id pools)))
    result))

(defun memrise/parse-session-thing-texts (json helper)
  (memrise/parse-session-thing-column json helper 'memrise/session-helper-text-column))

(defun memrise/parse-session-thing-translations (json helper)
  (memrise/parse-session-thing-column json helper 'memrise/session-helper-translation-column))

(defun memrise/parse-session-thing-audios (json helper)
  (let* ((audio-jsons (memrise/parse-session-thing-column json
                                                          helper
                                                          'memrise/session-helper-audio-column))
         (audio (car audio-jsons)))
    (if (vectorp audio)
        (setq audio-jsons (append `(,(aref audio 0)) (cdr audio-jsons))))
    (memrise/download-audios audio-jsons)))

(defun memrise/download-audios (json)
  (mapcar 'memrise/download-audio json))

(defun memrise/download-audio (json)
  (let* ((relative-url (assoc-default 'url json))
         (url (concat memrise/material-storage-url relative-url))
         (audio-dir (file-name-as-directory "audio"))
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

(defun memrise/parse-session-thing-literal-translation (json helper)
  (car (memrise/parse-session-thing-column json helper 'memrise/session-helper-literal-translation-column)))

(defun memrise/parse-session-thing-video (json helper)
  (let ((video (car (memrise/parse-session-thing-column
                     json
                     helper
                     'memrise/session-helper-video-column))))
    (if (> (length video) 0)
        (progn
          (setq video (aref video 0))
          (if video
              (memrise/download-video video)
            nil))
      nil)))

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

(defun memrise/parse-session-thing-column (json pool column-id-getter)
  (let* ((column-id (funcall column-id-getter pool))
         (column (assoc-default column-id json))
         (val (assoc-default 'val column))
         (choices (append (assoc-default 'choices column) nil)))
    (add-to-list 'choices val)))

;; hard-coded for now
(defun memrise/session-helper-text-column (helper)
  (memrise/session-helper-column-for helper (memrise/helper-target helper)))
(defun memrise/session-helper-translation-column (helper)
  (memrise/session-helper-column-for helper (memrise/helper-source helper)))
(defun memrise/session-helper-audio-column (helper)
  (memrise/session-helper-column-for helper (memrise/helper-audio helper)))
(defun memrise/session-helper-literal-translation-column (helper)
  (memrise/session-helper-column-for helper (memrise/helper-audio helper)))
(defun memrise/session-helper-video-column (helper)
  (memrise/session-helper-column-for helper (memrise/helper-video helper)))

(defun memrise/session-helper-column-for (helper name)
  (lexical-let ((pool (car (memrise/helper-pools helper)))
                (goal name))
    (memrise/id-for-integer (car (find-if (memrise/session-pool-column-predicate name)
                                          (memrise/session-pool-columns pool))))))

(defun memrise/session-pool-column-predicate (name)
  `(lambda (pair)
    (let* ((column (cdr pair))
           (kind (memrise/session-pool-column-kind column)))
      (string= kind name))))

(defun memrise/parse-session-tasks (json)
  (mapcar 'memrise/parse-session-task (assoc-default 'boxes json)))

(defun memrise/parse-session-task (json)
  (let ((id (assoc-default 'thing_id json))
        (kind (assoc-default 'template json))
        (column-a (assoc-default 'column_a json))
        (column-b (assoc-default 'column_b json))
        (learn-level (assoc-default 'learn_session_level json)))
    (make-memrise/session-task
     :thing-id id
     :kind kind
     :column-a column-a
     :column-b column-b
     :learn-level learn-level)))

(defun memrise/parse-session-helper (json)
  (let ((source (memrise/parse-session-helper-source json))
        (target (memrise/parse-session-helper-target json))
        (pools (memrise/parse-session-pools json)))
    (make-memrise/helper
     :source source
     :target target
     :pools pools)))

(defun memrise/parse-session-helper-source (json)
  (let* ((session (assoc-default 'session json))
         (course (assoc-default 'course session))
         (source (assoc-default 'source course)))
    (assoc-default 'name source)))

(defun memrise/parse-session-helper-target (json)
  (let* ((session (assoc-default 'session json))
         (course (assoc-default 'course session))
         (target (assoc-default 'target course)))
    (assoc-default 'name target)))

(defun memrise/parse-session-pools (json)
  (mapcar
   'memrise/parse-session-pool
   (assoc-default 'pools json)))

(defun memrise/parse-session-pool (json)
  (let ((id (memrise/integer-for-id (car json)))
        (columns (memrise/parse-session-pool-columns
                  (assoc-default 'columns (cdr json)))))
    `(,id . ,(make-memrise/session-pool :id id :columns columns))))

(defun memrise/parse-session-pool-columns (json)
  (mapcar
   'memrise/parse-session-pool-column
   json))

(defun memrise/parse-session-pool-column (json)
  (let* ((id (memrise/integer-for-id (car json)))
         (contents (cdr json))
         (kind (assoc-default 'label json))
         (keyboard (assoc-default 'keyboard json)))
    `(,id . ,(make-memrise/session-pool-column
              :id id
              :kind kind
              :keyboard keyboard))))

(defun memrise/integer-for-id (symbol)
  (string-to-number (symbol-name symbol)))

(defun memrise/id-for-integer (integer)
  (intern (number-to-string integer)))

(defun memrise/test ()
  (interactive)
  (message "%S" (memrise/parse-session session-test)))

(defvar session-test '((initial_session_points . 592468) (session (slug . "learn") (is_audio_enabled . t) (is_video_enabled . t) (course_id . 1180560) (course (id . 1180560) (name . "German 3") (slug . "german-3") (url . "/course/1180560/german-3/") (description . "Find your way around, talk about the future, learn some German expressions that will impress everyone you meet.") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/course_photos/16054981000161215161109.jpg") (photo_small . "https://d2rhekw5qr4gcj.cloudfront.net/img/100sqf/from/uploads/course_photos/16054981000161215161109.jpg") (photo_large . "https://d2rhekw5qr4gcj.cloudfront.net/img/400sqf/from/uploads/course_photos/16054981000161215161109.jpg") (num_things . 641) (num_levels . 43) (num_learners . 8991) (source (id . 963) (slug . "english-us") (name . "English (US)") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/category_photos/us_flag.png") (parent_id . 578) (index . 0)) (target (id . 4) (slug . "german-2") (name . "German") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/language_photos/german.png") (parent_id . 879) (index . 1048)) (creator (id . 2224242) (email . "") (username . "Memrise") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/profiles/Memrise_170424_1700_15.png") (photo_small . "https://d2rhekw5qr4gcj.cloudfront.net/img/100sqf/from/uploads/profiles/Memrise_170424_1700_15.png") (photo_large . "https://d2rhekw5qr4gcj.cloudfront.net/img/400sqf/from/uploads/profiles/Memrise_170424_1700_15.png") (is_authenticated . t) (is_staff . t) (url . "/user/Memrise/") (num_followers . 20155) (num_following . 8) (num_things_flowered . 0))) (category (id . 4) (slug . "german-2") (name . "German") (photo . "https://d2rhekw5qr4gcj.cloudfront.net/uploads/language_photos/german.png") (parent_id . 879) (index . 1048)) (goal_points . 1530) (goal . 1500) (streak . 2) (level_id . 4568860) (level (id . 4568860) (index . 3) (course_id . 1180560) (title . "Fuel Your Vocab: Places") (kind . 1) (url . "/course/1180560/german-3/3/") (column_a . 1) (column_b . 2) (pool_id . 100080019))) (things (\1000225723 (id . 1000225723) (pool_id . 100080019) (columns (\1 (alts . []) (val . "die Karte") (choices . ["die Kartoffel" "die Karriere" "die Kamera" "die Katze" "die Katastrophe" "die Kampagne" "die Kuh" "die Kirche" "die Komödie" "die Kunst" "die Kneipe" "die Kosten"]) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["die karte"]) (tapping . [["die" "karte"]]))) (\2 (alts . []) (val . "the map; the card") (choices . ["the potato" "the career" "the camera" "the cat" "the disaster; the catastrophe" "the campaign" "the cow" "the church" "the comedy" "the art" "the bar; the pub" "the costs"]) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["the map the card" "the map" "the card" "the map the card" "the card the map"]) (tapping . [["the" "map" "the" "card"] ["the card" "the map"]]))) (\3 (alts . []) (val . [((id . 1) (url . "uploads/babylon/8/audios-v3/12356/1.mp3"))]) (choices . [((id . 1) (url . "uploads/babylon/8/audios-v3/11851/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13102/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/14104/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12237/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/14311/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/14335/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12239/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12560/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13086/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12598/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12352/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/14020/1.mp3"))]) (kind . "audio") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . [])) (\4 (alts . []) (val . "") (choices . []) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . []) (tapping . []))) (\5 (alts . []) (val . []) (choices . []) (kind . "video") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []))) (attributes)) (\1000225724 (id . 1000225724) (pool_id . 100080019) (columns (\1 (alts . []) (val . "der Reiseführer") (choices . ["der Reisepass" "der Flug" "das reicht!" "der Reifen" "der Rock" "der Roman" "der Regenschirm" "der Regenwald" "der Rucksack" "der Rat" "die Reihe" "der Reis"]) (kind . "text") (accepted . ["der Reisefuhrer"]) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["der reiseführer" "der reisefuhrer"]) (tapping . [["der" "reiseführer"] ["der" "reisefuhrer"]]))) (\2 (alts . []) (val . "the guide (book)") (choices . ["the passport" "the flight" "that's enough!" "the tire" "the skirt" "the novel" "the umbrella" "the rainforest" "the backpack" "the advice; the piece of advice" "the range (of products); the row" "the rice"]) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["the guide" "the guide" "book" "the guide book"]) (tapping . [["the" "guide"]]))) (\3 (alts . []) (val . [((id . 1) (url . "uploads/babylon/8/audios-v3/12357/1.mp3"))]) (choices . [((id . 1) (url . "uploads/babylon/8/audios-v3/12679/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12808/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12171/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/14222/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12185/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13852/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12201/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13360/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13362/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13621/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/14273/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/11848/1.mp3"))]) (kind . "audio") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . [])) (\4 (alts . []) (val . "") (choices . []) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . []) (tapping . []))) (\5 (alts . []) (val . []) (choices . []) (kind . "video") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []))) (attributes)) (\1000225725 (id . 1000225725) (pool_id . 100080019) (columns (\1 (alts . []) (val . "breit") (choices . ["brennen" "braun" "brauchen" "blau" "bald" "Boxen" "bitte" "beide" "blond" "bauen" "beste"]) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["breit"]) (tapping . [["breit"]]))) (\2 (alts . []) (val . "wide; broad") (choices . ["to burn (up)" "brown" "to need" "blue" "soon" "boxing" "please" "both" "blonde" "to build" "best"]) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["wide broad" "wide" "broad" "wide broad" "broad wide"]) (tapping . [["wide" "broad"] ["broad" "wide"]]))) (\3 (alts . []) (val . [((id . 1) (url . "uploads/babylon/8/audios-v3/14289/1.mp3"))]) (choices . [((id . 1) (url . "uploads/babylon/8/audios-v3/14242/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12158/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/11992/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12150/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12895/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13079/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/11801/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/14503/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12466/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13270/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13148/1.mp3"))]) (kind . "audio") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . [])) (\4 (alts . []) (val . "") (choices . []) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . []) (tapping . []))) (\5 (alts . []) (val . []) (choices . []) (kind . "video") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []))) (attributes)) (\1000225726 (id . 1000225726) (pool_id . 100080019) (columns (\1 (alts . []) (val . "der Weg") (choices . ["der Sinn" "der Wasserfall" "der Flug" "der Wochentag" "der Witz" "der See" "der Weißwein" "die Welt" "der Plan" "der Mann" "weit weg" "der Wald"]) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["der weg"]) (tapping . [["der" "weg"]]))) (\2 (alts . []) (val . "the way") (choices . ["the sense" "the waterfall" "the flight" "the day of the week" "the joke" "the lake" "the white wine" "the world" "the plan" "the man; the husband" "far away" "the forest"]) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["the way"]) (tapping . [["the" "way"]]))) (\3 (alts . []) (val . [((id . 1) (url . "uploads/babylon/8/audios-v3/14077/1.mp3"))]) (choices . [((id . 1) (url . "uploads/babylon/8/audios-v3/13056/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13700/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12808/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12938/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13195/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12814/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12376/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12867/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12822/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12121/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12570/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13359/1.mp3"))]) (kind . "audio") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . [])) (\4 (alts . []) (val . "") (choices . []) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . []) (tapping . []))) (\5 (alts . []) (val . []) (choices . []) (kind . "video") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []))) (attributes)) (\1000225727 (id . 1000225727) (pool_id . 100080019) (columns (\1 (alts . []) (val . "sich verlaufen") (choices . ["wir sind" "sie sind" "Sie sind" "sie ist" "verstehst du?" "sich übergeben" "sich interessieren" "Zeit verbringen" "sich verspäten" "sich entspannen" "sich betrinken" "das Verbrechen"]) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["sich verlaufen"]) (tapping . [["sich" "verlaufen"]]))) (\2 (alts . []) (val . "to get (oneself) lost") (choices . ["we're" "they're" "you're (formal)" "she's; it's" "do you understand?" "to vomit (oneself)" "to interest (oneself)" "to spend time" "to be late; to be delayed" "to relax (oneself)" "to get (oneself) drunk" "the crime"]) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["to get lost" "to get lost" "oneself" "to get oneself lost"]) (tapping . [["to" "get" "lost"]]))) (\3 (alts . []) (val . [((id . 1) (url . "uploads/babylon/8/audios-v3/12358/1.mp3"))]) (choices . [((id . 1) (url . "uploads/babylon/8/audios-v3/11937/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/11939/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/11940/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/11977/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12003/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12492/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12615/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12766/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12817/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12970/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12971/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/13041/1.mp3"))]) (kind . "audio") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . [])) (\4 (alts . []) (val . "") (choices . []) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . []) (tapping . []))) (\5 (alts . []) (val . []) (choices . []) (kind . "video") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []))) (attributes)) (\1000225728 (id . 1000225728) (pool_id . 100080019) (columns (\1 (alts . []) (val . "durcheinander") (choices . ["durch" "durchmachen" "du" "dumm" "dunkel" "duschen" "dunkelrot" "drei" "dir" "dort" "das" "dünn"]) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["durcheinander"]) (tapping . [["durcheinander"]]))) (\2 (alts . []) (val . "confused") (choices . ["through" "to go through" "you (singular informal)" "stupid" "dark" "to take a shower" "dark red" "three; 3" "you; to you (singular informal)" "there" "this; that; the" "thin"]) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["confused"]) (tapping . [["confused"]]))) (\3 (alts . []) (val . [((id . 1) (url . "uploads/babylon/8/audios-v3/12359/1.mp3"))]) (choices . [((id . 1) (url . "uploads/babylon/8/audios-v3/13439/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/14171/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/11819/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12257/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12161/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12960/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12163/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/11915/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/11812/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12080/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/12092/1.mp3")) ((id . 1) (url . "uploads/babylon/8/audios-v3/11967/1.mp3"))]) (kind . "audio") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . [])) (\4 (alts . []) (val . "through-an-other") (choices . []) (kind . "text") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []) (possible_answers (typing . ["throughanother"]) (tapping . [["throughanother"]]))) (\5 (alts . []) (val . []) (choices . []) (kind . "video") (accepted . []) (typing_corrects) (tapping_choices (corrects . []) (distractors . [])) (segments . []))) (attributes))) (boxes . [((thing_id . 1000225723) (column_a . 1) (column_b . 2) (template . "presentation")) ((thing_id . 1000225724) (column_a . 1) (column_b . 2) (template . "presentation")) ((thing_id . 1000225723) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 1)) ((thing_id . 1000225724) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 1)) ((thing_id . 1000225725) (column_a . 1) (column_b . 2) (template . "presentation")) ((thing_id . 1000225723) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 2)) ((thing_id . 1000225726) (column_a . 1) (column_b . 2) (template . "presentation")) ((thing_id . 1000225725) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 1)) ((thing_id . 1000225723) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 3)) ((thing_id . 1000225724) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 2)) ((thing_id . 1000225726) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 1)) ((thing_id . 1000225727) (column_a . 1) (column_b . 2) (template . "presentation")) ((thing_id . 1000225728) (column_a . 1) (column_b . 2) (template . "presentation")) ((thing_id . 1000225726) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 2)) ((thing_id . 1000225723) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 4)) ((thing_id . 1000225727) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 1)) ((thing_id . 1000225725) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 2)) ((thing_id . 1000225728) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 1)) ((thing_id . 1000225724) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 3)) ((thing_id . 1000225725) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 3)) ((thing_id . 1000225724) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 4)) ((thing_id . 1000225727) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 2)) ((thing_id . 1000225726) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 3)) ((thing_id . 1000225727) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 3)) ((thing_id . 1000225724) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 5)) ((thing_id . 1000225728) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 2)) ((thing_id . 1000225726) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 4)) ((thing_id . 1000225727) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 4)) ((thing_id . 1000225728) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 3)) ((thing_id . 1000225723) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 5)) ((thing_id . 1000225725) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 4)) ((thing_id . 1000225724) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 6)) ((thing_id . 1000225727) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 5)) ((thing_id . 1000225726) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 5)) ((thing_id . 1000225728) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 4)) ((thing_id . 1000225725) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 5)) ((thing_id . 1000225723) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 6)) ((thing_id . 1000225727) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 6)) ((thing_id . 1000225725) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 6)) ((thing_id . 1000225726) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 6)) ((thing_id . 1000225728) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 5)) ((thing_id . 1000225728) (column_a . 1) (column_b . 2) (template . "sentinel") (learn_session_level . 6))]) (pools (\100080019 (id . 100080019) (name . "German for English (US) speakers - Words") (columns (\1 (kind . "text") (label . "German") (typing_disabled . :json-false) (typing_strict . :json-false) (show_after_tests . :json-false) (always_show . :json-false) (keyboard . "äéöüß") (tapping_disabled . :json-false) (classes . []) (is_babylon_data . t)) (\2 (kind . "text") (label . "English (US)") (typing_disabled . :json-false) (typing_strict . :json-false) (show_after_tests . :json-false) (always_show . :json-false) (keyboard . "") (tapping_disabled . :json-false) (classes . []) (is_babylon_data . :json-false)) (\3 (kind . "audio") (label . "Audio") (typing_disabled . :json-false) (typing_strict . :json-false) (show_after_tests . :json-false) (always_show . :json-false) (keyboard . "") (tapping_disabled . :json-false) (classes . []) (is_babylon_data . :json-false)) (\4 (kind . "text") (label . "Literal translation") (typing_disabled . :json-false) (typing_strict . :json-false) (show_after_tests . :json-false) (always_show . t) (keyboard . "") (tapping_disabled . :json-false) (classes . []) (is_babylon_data . :json-false)) (\5 (kind . "video") (label . "Videos") (typing_disabled . :json-false) (typing_strict . :json-false) (show_after_tests . :json-false) (always_show . :json-false) (keyboard . "") (tapping_disabled . :json-false) (classes . []) (is_babylon_data . :json-false))) (attributes))) (thingusers . []) (things_to_courses) (can_curate . :json-false)))

(provide 'memrise-session)
