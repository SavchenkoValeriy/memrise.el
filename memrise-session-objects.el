;;; memrise-session-objects.el --- Session structures -*- lexical-binding: t; -*-

(require 'jeison)

(jeison-defclass memrise-session nil
  ((course-name :path (session course name)
                :documentation "Name of the course (e.g. Russian 2)")
   (course-id :path (session course_id)
              :documentation "Internal ID of the course")
   (title :path (session level title)
          :documentation "Fuel Your Vocab: Places")
   (source :path (session course source name)
           :documentation "in what language are we learning")
   (target :path (session course target name)
           :documentation "what are we learning")
   (tasks :type (list-of memrise-session-task) :path boxes
          :documentation "learning tasks")
   (tests :path ((memrise/parse-session-tests screens))
          :documentation "tests that are possible to be taken")
   (learnables :path ((memrise/parse-session-learnables learnables))
               :documentation "actual things that can be learned")))

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
  ((kind :path template
         :documentation "one of '(\"multiple_choice\"
\"reversed_multiple_choice\"
\"audio_multiple_choice\"
\"typing\"
\"tapping\")")
   (prompt :type memrise-session-test-prompt
           :documentation "information for a test title")
   (answer :path (answer value)
           :documentation "correct answer")
   (choices :type (list-of t)
            :documentation "other choices (do not include correct)")
   (correct :type (list-of t) :path correct
            :documentation "accepted answers")))

(jeison-defclass memrise-session-test-prompt nil
  ((text :initform "" :path (text value) :documentation "Text to show")
   (audio :initform nil :path (audio (memrise/download-normal-pace-audio value))
          :documentation "Audio to play (can be nil)")
   (video :initform nil :documentation "Video to show (can be nil)")))

(provide 'memrise-session-objects)
