;;; memrise-test-parser.el --- A set of functions to parse test JSON -*- lexical-binding: t; -*-

(require 'memrise-media)

(defun memrise/parse-session-test (json)
  (let* ((kind (symbol-name (car json))) ;; kind would be a string
         (body (cdr json))
         (prompt (memrise/parse-session-test-prompt
                  (assoc-default 'prompt body)))
         (correct-json (assoc-default 'answer body))
         (correct (assoc-default 'value correct-json))
         (choices (memrise/vector-to-list
                   (assoc-default 'choices body)))
         (accepted (memrise/vector-to-list
                    (assoc-default 'correct body))))
    ;; if test is an audio test we should download all audios
    (when (s-contains? "audio" kind)
      (setq correct (memrise/download-normal-pace-audio correct))
      (setq choices (memrise/process-media "audio" choices))
      (setq accepted (memrise/process-media "audio" accepted)))
    `(,kind . ,(make-memrise/session-test
                :kind kind
                :prompt prompt
                :correct correct
                :choices choices
                :accepted accepted))))

(defun memrise/parse-session-test-prompt (json)
  (let* ((text-json (assoc-default 'text json))
         (text (assoc-default 'value text-json))
         (audio (memrise/parse-session-audio
                 (assoc-default 'audio json)))
         (video (memrise/process-media
                 "video"
                 (assoc-default 'video json))))
    (make-memrise/session-test-prompt
     :text text
     :audio audio
     :video video)))

(defun memrise/parse-session-audio (json)
  (let* ((value (assoc-default 'value json)))
    (memrise/download-normal-pace-audio value)))

(defun memrise/download-normal-pace-audio (json)
  (let* ((audios (elt json 0))
         (audio (assoc-default 'normal audios)))
    (when audio
      (memrise/process-media "audio" (list audio)))))

(provide 'memrise-test-parser)
