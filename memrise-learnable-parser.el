;;; memrise-learnable-parser.el --- A set of functions to parse learnable JSON -*- lexical-binding: t; -*-

(require 'memrise-media)

(defun memrise/parse-session-learnable (json)
  (let* ((id (string-to-number
              (assoc-default 'learnable_id json)))
         (text (assoc-default 'value
                              (assoc-default 'item json)))
         (translation (assoc-default 'value
                                     (assoc-default 'definition json)))
         (audio (memrise/parse-session-learnable-audio json))
         (literal-translation (memrise/parse-session-learnable-literal-translation json)))
    `(,id . ,(make-memrise/session-learnable
              :id id
              :text text
              :translation translation
              :audio audio
              :literal-translation literal-translation))))

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
