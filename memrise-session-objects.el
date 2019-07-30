;;; memrise-session-objects.el --- Session structures -*- lexical-binding: t; -*-

(require 'jeison)

(jeison-defclass memrise-session nil
  ((course-name :path (session course name)
                :documentation "Name of the course (e.g. Russian 2)")
   (course-id :path (session course_id) :initarg :course-id
              :documentation "Internal ID of the course")
   (title :path (session level title) :initarg :title
          :documentation "Fuel Your Vocab: Places")
   (source :path (session course source name) :initarg :source
           :documentation "In what language are we learning")
   (target :path (session course target name) :initarg :target
           :documentation "What are we learning")
   (tasks :type (list-of memrise-session-task) :path boxes
          :documentation "Learning tasks")
   (tests :path ((memrise-parse-session-tests screens))
          :documentation "All possible tests")
   (learnables :path ((memrise-parse-session-learnables screens))
               :documentation "Actual things that can be learned")))

(jeison-defclass memrise-session-task nil
  ((learnable-id :path ((string-to-number learnable_id))
                 :documentation "id of a learnable thing to train in this task")
   (template :documentation "kind of a task (presentation or sentinel)")
   (learn-level :path learn_session_level
                :documentation "learn level (nil or 1-6)")))

(jeison-defclass memrise-session-learnable nil
  ((id :documentation "a unique ID to identify the thing")
   (text :initform "" :path (item value)
         :documentation "Text representation of a learnable object")
   (translation :initform "" :path (definition value)
                :documentation "Translation of a learnable object")
   (audio :path ((memrise-download-normal-pace-audio (audio value)))
          :documentation "Audio representation")
   (literal-translation
    :initform nil
    :path ((memrise-parse-session-learnable-literal-translation visible_info))
    :documentation "Literal translation (can be nil)")))

(jeison-defclass memrise-session-test nil
  ;; we prefer kind to be a string not a symbol
  ((kind :path template :initarg :kind
         :documentation "one of '(\"multiple_choice\"
\"reversed_multiple_choice\"
\"audio_multiple_choice\"
\"typing\"
\"tapping\")")
   (prompt :type memrise-session-test-prompt :initarg :prompt
           :documentation "Information for a test title")
   (answer :path (answer value) :initarg :answer
           :documentation "Correct answer")
   (choices :type (list-of t) :initarg :choices
            :documentation "Other choices (do not include correct)")
   (correct :type (list-of t) :path correct :initarg :correct
            :documentation "A list of accepted answers")))

(jeison-defclass memrise-session-test-prompt nil
  ((text :initform "" :path (text value) :initarg :text
         :documentation "Text to show")
   (audio :initform nil :path (audio (memrise-download-normal-pace-audio value))
          :initarg :audio :documentation "Audio to play (can be nil)")
   (video :initform nil :initarg :video
          :documentation "Video to show (can be nil)")))

(provide 'memrise-session-objects)
;;; memrise-session-objects.el ends here
