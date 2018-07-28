;;; memrise-test-parser.el --- A set of functions to parse test JSON -*- lexical-binding: t; -*-

(require 'memrise-media)

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
      (setq correct (memrise/process-media "audio" correct))
      (setq choices (memrise/process-media "audio" choices)))
    `(,kind . ,(make-memrise/session-test
                :kind kind
                :prompt prompt
                :correct correct
                :choices choices
                :accepted accepted))))

(defun memrise/parse-session-test-prompt (json)
  (let* ((body (assoc-default 'prompt json))
         (text (assoc-default 'text body))
         (audio (memrise/parse-session-audio
                 (assoc-default 'audio body)))
         (video (memrise/process-media
                 "video"
                 (assoc-default 'video body))))
    (make-memrise/session-test-prompt
     :text text
     :audio audio
     :video video)))

(defun memrise/parse-session-audio (json)
  (let* ((value (assoc-default 'value json))
         (audios (elt value 0))
         (audio (assoc-default 'normal audios)))
    (when audio
      (memrise/process-media "audio" (list audio)))))

(provide 'memrise-test-parser)
