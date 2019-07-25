;;; memrise-test-parser.el --- A set of functions to parse test JSON -*- lexical-binding: t; -*-

(require 'memrise-media)

(defun memrise/parse-session-test (json)
  (let* ((result (jeison-read memrise-session-test json))
         (kind (oref result kind)))
    ;; if test is an audio test we should download all audios
    (when (s-contains? "audio" kind)
      (oset result correct (memrise/download-normal-pace-audio
                            (oref result correct)))
      (oset result choices (memrise/process-media "audio"
                                                  (oref result choices)))
      (oset result accepted (memrise/process-media "audio"
                                                   (oref result accepted))))
    (cons kind result)))

(defun memrise/download-normal-pace-audio (json)
  (let ((audio (jeison-read t json '(0 normal))))
    (when audio
      (memrise/process-media "audio" audio))))

(provide 'memrise-test-parser)
