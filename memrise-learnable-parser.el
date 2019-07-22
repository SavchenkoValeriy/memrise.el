;;; memrise-learnable-parser.el --- A set of functions to parse learnable JSON -*- lexical-binding: t; -*-

(require 'memrise-media)

(defun memrise/parse-session-learnable (json)
  (let* ((result (jeison-read memrise-session-learnable json)))
    (cons (oref result id) result)))

(defun memrise/parse-session-learnable-audio (json)
  (memrise/process-media
   "audio"
   (memrise/parse-column-value json "Audio")))

(defun memrise/parse-session-learnable-literal-translation (json)
  (memrise/parse-column-value json "Literal translation"))

(defun memrise/parse-column-value (json label)
  (let ((column (find-if (lambda (x)
                           (string= (assoc-default 'label x)
                                    label))
                         (assoc-default 'columns json))))
    (assoc-default 'value column)))

(provide 'memrise-learnable-parser)
