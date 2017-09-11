;;; memrise-widget.el --- Collection of memrise widgets  -*- lexical-binding: t; -*-

(require 'widget)
(require 'wid-edit)

(defvar memrise/presentation-format
  "${text}\n${translation}\n${literal-translation}"
  "Template for presentation of a new thing")

(defvar memrise/multiple-choice-format
  "${translation}\nSelect the correct ${target} for the ${source} above:\n"
  "Template for multiple-choice widget")

(defvar memrise/inverted-multiple-choice-format
  "${text}\nSelect the correct ${source} for the ${target} above:\n"
  "Template for inverted-multiple-choice widget")

(defvar memrise/audio-multiple-choice-format
  "${translation}\nChoose the correct ${target} for the ${source}\n"
  "Template for audio-multiple-choice widget")

(defvar memrise/typing-format
  "${translation}\nType the ${target} for the ${source} above:\n"
  "Template for typing widget")

(defun memrise/session-format (thing)
  (let ((helper (memrise/session-helper session)))
      `((text . ,(memrise/session-thing-text thing))
        (translation . ,(memrise/session-thing-translation thing))
        (literal-translation . ,(memrise/session-thing-literal-translation thing))
        (source . ,(memrise/helper-source helper))
        (target . ,(memrise/helper-target helper)))))

(defun memrise/session-itemize-choices (choices)
  (mapcar (lambda (x) `(radio-button :off "[ ]" :button-prefix "[a]" :format "%t\n" ,x)) choices))

(defun memrise/create-inverted-multiple-choice-widget (thing)
  (let* ((text (memrise/format-elements-with-faces
                memrise/inverted-multiple-choice-format
                (memrise/session-format thing)
                memrise/session-faces))
         (result (apply #'widget-create 'radio-button-choice
                        :format (concat text "%v")
                        :notify (lambda (widget &rest i)
                                  (message "%S" (widget-value widget)))
                        (memrise/session-itemize-choices
                         (memrise/session-thing-translation-options thing)))))
    (mapc (lambda (x) (message "%S" x)) (widget-get result :children))
    result))

(provide 'memrise-widget)
