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
  )

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
