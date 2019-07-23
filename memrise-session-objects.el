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
  ((id :type number :path ((string-to-number learnable_id))
       :documentation "a unique ID to identify the thing")
   (text :initform "" :path (item value) :documentation "text representation")
   (translation :initform "" :path (definition value)
                :documentation "translation of a text representation")
   (audio :path ((memrise/parse-session-learnable-audio nil))
          :documentation "audio representation")
   (literal-translation
    :initform nil
    :path ((memrise/parse-session-learnable-literal-translation nil))
    :documentation "can be nil")))

(jeison-defclass memrise-session-test nil
  ;; we prefer kind to be a string not a symbol
  ((kind :path ((cdr nil) template)
         :documentation "one of '(\"multiple_choice\"
\"reversed_multiple_choice\"
\"audio_multiple_choice\"
\"typing\"
\"tapping\")")
   (prompt :path ((cdr nil) (memrise/parse-session-test-prompt prompt))
           :documentation "information for a test title")
   (correct :path ((cdr nil) answer value)
            :documentation "correct answer")
   (choices :type (list-of t) :path ((cdr nil) choices)
            :documentation "other choices (do not include correct)")
   (accepted :type (list-of t) :path ((cdr nil) correct)
             :documentation "other accepted answers")))

(jeison-defclass memrise-session-test-prompt nil
  ((text :initform "" :path (text value) :documentation "Text to show")
   (audio :initform "" :path (memrise/parse-session-audio audio)
          :documentation "Audio to play (can be nil)")
   (video :initform "" :documentation "Video to show (can be nil)")))

(provide 'memrise-session-objects)
