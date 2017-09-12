;;; memrise-widget.el --- Collection of memrise widgets  -*- lexical-binding: t; -*-

(require 'widget)
(require 'wid-edit)
(require 'dash)

(defcustom memrise/radio-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;)
  "Default keys for picking answers during memrise session"
  :type '(repeat :tag "Keys" (character :tag "char")))

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

(defun memrise/create-inverted-multiple-choice-widget (thing)
  (lexical-let* ((text (memrise/format-elements-with-faces
                        memrise/inverted-multiple-choice-format
                        (memrise/session-format thing)
                        memrise/session-faces))
                 (choices (memrise/translation-choices thing 4))
                 (result (apply #'widget-create 'radio-button-choice
                                :format (concat text "%v")
                                :notify (lambda (widget &rest i)
                                          (message "%S" (widget-value widget)))
                                (memrise/session-itemize-choices
                                 choices)
                                )))
    (memrise/assign-buttons-keybindings (widget-get result :buttons)
                                        memrise/radio-keys)
    result))

(defun memrise/translation-choices (thing number)
  "Picks a `number' of translation choices for a quiz."
  (memrise/construct-choices (memrise/session-thing-translation thing)
                             (memrise/session-thing-translation-options thing)
                             number))

(defun memrise/construct-choices (correct incorrect number)
  "For given choices construct a randomized list of size `number'.
The result is guaranteed to have `correct' element in it."
  (let* ((filtered-incorrect (-take (1- number)
                                    (memrise/shuffle-list incorrect))))
    (memrise/shuffle-list (cons correct filtered-incorrect))))

(defun memrise/shuffle-list (list)
  "Shuffles the given `list'"
  (sort list (lambda (a b) (eq (random 2) 1))))

(defun memrise/session-format (thing)
  (let ((helper (memrise/session-helper session)))
    `((text . ,(memrise/session-thing-text thing))
      (translation . ,(memrise/session-thing-translation thing))
      (literal-translation . ,(memrise/session-thing-literal-translation thing))
      (source . ,(memrise/helper-source helper))
      (target . ,(memrise/helper-target helper)))))

(defun memrise/session-itemize-choices (choices)
  (mapcar (lambda (x) `(item ,x)) choices))

(defun memrise/assign-buttons-keybindings (buttons bindings)
  (mapc (lambda (args) (apply 'memrise/assign-button-keybinding args))
        (-zip buttons bindings (number-sequence 1 (length buttons)))))

(defun memrise/assign-button-keybinding (button keybinding index)
  "Changes radio `button' shape and `keybinding', colors it with a rainbow color `index'"
  (lexical-let ((button button))
    (widget-put button
                :button-prefix
                (propertize (format "[%c] " keybinding)
                            'face
                            (rainbow-delimiters-default-pick-face index t nil)))
    ;;to show the new button shape, we need to redraw it
    (memrise/redraw-widget button)
    (local-set-key (char-to-string keybinding)
                   (lambda () (interactive) (widget-apply button :action)))))

(defun memrise/redraw-widget (widget)
  (widget-value-set widget (widget-value widget)))

(provide 'memrise-widget)
