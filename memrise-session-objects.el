;;; memrise-session-objects.el --- Session structures -*- lexical-binding: t; -*-

(defstruct memrise/session
  course-name ;; "Russian 2"
  title       ;; "Fuel Your Vocab: Places"
  source      ;; in what language are we learning
  target      ;; what are we learning
  tasks       ;; learning tasks
  tests       ;; tests that are possible to be taken
  learnables  ;; actual things that can be learned
  )

(jeison-defclass memrise-session-task nil
  ((learnable-id :path ((string-to-number learnable_id))
                 :documentation "id of a learnable thing to train in this task")
   (template :documentation "kind of a task (presentation or sentinel)")
   (learn-level :path learn_session_level
                :documentation "learn level (nil or 1-6)")))

(jeison-defclass memrise-session-learnable nil
  ((id :path ((string-to-number learnable_id))
       :documentation "a unique ID to identify the thing")
   (text :path (item value) :documentation "text representation")
   (translation :path (definition value)
                :documentation "translation of a text representation")
   (audio :path ((memrise/parse-session-learnable-audio nil))
          :documentation "audio representation")
   (literal-translation :path ((memrise/parse-session-learnable-literal-translation nil))
                        :documentation "can be nil")))

(defstruct memrise/session-test
  kind     ;; one of '("multiple_choice"
  ;;                   "reversed_multiple_choice"
  ;;                   "audio_multiple_choice"
  ;;                   "typing"
  ;;                   "tapping")
  prompt   ;; information for a test title
  correct  ;; correct answer
  choices  ;; other choices (do not include correct)
  accepted ;; other accepted answers
  )

(defstruct memrise/session-test-prompt
  text  ;; text to show
  audio ;; audio to play (can be nil)
  video ;; video to show (can be nil)
  )

(provide 'memrise-session-objects)
