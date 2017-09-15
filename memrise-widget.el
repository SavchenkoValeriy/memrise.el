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
  "${prompt}\nSelect the correct ${target} for the ${source} above:\n"
  "Template for multiple-choice widget")

(defvar memrise/reversed-multiple-choice-format
  "${prompt}\nSelect the correct ${source} for the ${target} above:\n"
  "Template for inverted-multiple-choice widget")

(defvar memrise/audio-multiple-choice-format
  "${prompt}\nChoose the correct ${target} for the ${source}\n"
  "Template for audio-multiple-choice widget")

(defvar memrise/typing-format
  "${prompt}\nType the ${target} for the ${source} above:\n"
  "Template for typing widget")

(defun memrise/presentation (learnable)
  (let ((play-presentation (memrise/make-interactive
                            (-partial 'memrise/play-audio
                                      (memrise/session-learnable-audio learnable)))))
    (local-set-key (kbd "C-m") 'memrise/display-next-task)
    (local-set-key (kbd "C-r") play-presentation)
    (funcall play-presentation)
    (widget-create 'item
                   :format (memrise/format-widget
                            memrise/presentation-format
                            learnable))))

(defun memrise/multiple-choice-widget (test-for-widget format number)
  (lexical-let* ((text (memrise/format-widget format test-for-widget))
                 (choices (memrise/widget-choices test-for-widget number))
                 (result (apply #'widget-create 'radio-button-choice
                                :format (concat text "%v")
                                :notify (lambda (widget &rest i)
                                          (message "%S" (widget-value widget)))
                                (memrise/session-itemize-choices
                                 choices)
                                )))
    (make-local-variable 'test)
    (setq test test-for-widget)
    (memrise/assign-buttons-keybindings (widget-get result :buttons)
                                        memrise/radio-keys)
    result))

(defun memrise/play-audio (audio)
  "Plays given `audio' file"
  (emms-play-file audio))

(defun memrise/make-interactive (fun)
  "Returns interactive version of the given `fun'"
  (lambda () (interactive) (funcall fun)))

(defun memrise/format-widget (format test-or-learnable)
  (memrise/format-elements-with-faces format
                                      (memrise/session-format test-or-learnable)
                                      memrise/session-faces))

(defun memrise/widget-choices (test number)
  "Picks a `number' of translation choices for a quiz."
  (memrise/construct-choices (memrise/session-test-correct test)
                             (memrise/session-test-choices test)
                             number))

(defun memrise/construct-choices (correct incorrect number)
  "For given choices construct a randomized list of size `number'.
The result is guaranteed to have `correct' element in it."
  (let* ((filtered-incorrect (-take (1- number)
                                    (memrise/shuffle-list incorrect))))
    (memrise/shuffle-list (cons correct filtered-incorrect))))

(defun memrise/shuffle-list (list)
  "Shuffles the given `list'"
  (sort (copy-list list) (lambda (a b) (eq (random 2) 1))))

(defun memrise/session-format (test-or-learnable)
  (let ((prompt (make-memrise/session-test-prompt))
        (learnable (make-memrise/session-learnable)))
    (when (memrise/session-test-p test-or-learnable)
      (setq prompt (memrise/session-test-prompt test-or-learnable)))
    (when (memrise/session-learnable-p test-or-learnable)
      (setq learnable test-or-learnable))
    `((text . ,(memrise/session-learnable-text learnable))
      (translation . ,(memrise/session-learnable-translation learnable))
      (literal-translation . ,(or (memrise/session-learnable-literal-translation learnable) ""))
      (prompt . ,(memrise/session-test-prompt-text prompt))
      (source . ,(memrise/session-source session))
      (target . ,(memrise/session-target session)))))

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
