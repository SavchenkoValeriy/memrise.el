;;; memrise-widget.el --- Collection of memrise widgets  -*- lexical-binding: t; -*-

(require 'memrise-utils)
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

(defcustom memrise/radio-on
  (all-the-icons-faicon "dot-circle-o" :v-adjust 0.0)
  "Symbol to show for chosen radio button"
  :type '(string)
  :group 'memrise)

(defcustom memrise/radio-off
  (all-the-icons-faicon "circle-o" :v-adjust 0.0)
  "Symbol to show for not chosen radio button"
  :type '(string)
  :group 'memrise)

(defcustom memrise/audio-radio-on
  (all-the-icons-faicon "play-circle" :v-adjust 0.0)
  "Symbol to show for chosen audio radio button"
  :type '(string)
  :group 'memrise)

(defcustom memrise/audio-radio-off
  memrise/radio-off
  "Symbol to show for not chosen audio radio button"
  :type '(string)
  :group 'memrise)

(defun memrise/presentation (learnable)
  (let ((play-presentation (memrise/make-interactive
                            (-partial 'memrise/play-audio
                                      (memrise/session-learnable-audio learnable))))
        (widget (widget-create 'item
                               :format (memrise/format-widget
                                        memrise/presentation-format
                                        learnable))))
    (local-set-key (kbd "C-m") (memrise/make-interactive (-partial 'memrise/display-next-task widget)))
    (local-set-key (kbd "C-r") play-presentation)
    (funcall play-presentation)
    widget))

(defun memrise/multiple-choice-widget (test number)
  (widget-create 'memrise/choice-widget
                 :test test
                 :prefix-format memrise/multiple-choice-format
                 :requires-audio 'after
                 :size number))

(defun memrise/reversed-multiple-choice-widget (test number)
  (widget-create 'memrise/choice-widget
                 :test test
                 :prefix-format memrise/reversed-multiple-choice-format
                 :requires-audio 'before
                 :size number))

(defun memrise/audio-multiple-choice-widget (test number)
  (widget-create 'memrise/choice-widget
                 :test test
                 :prefix-format memrise/audio-multiple-choice-format
                 :on memrise/audio-radio-on
                 :off memrise/audio-radio-off
                 :requires-audio nil
                 :notify (memrise/make-widget-callback
                          'memrise/audio-multiple-choice-widget-play)
                 :instant-submit nil
                 :labels (number-sequence 1 8)))

(define-widget 'memrise/choice-widget 'radio-button-choice
  "Multiple choice widget for memrise tests."
  :test nil
  :prefix-format ""
  :requires-audio nil ; one of `(before after ,nil)
  :create 'memrise/choice-widget-create
  :assign-keys t
  :on memrise/radio-on
  :off memrise/radio-off
  :size 4
  :submit 'memrise/choice-widget-submit-answer
  :on-submit-hook nil
  :instant-submit t
  :labels '()
  )

(defun memrise/choice-widget-create (widget)
  (let* ((test           (widget-get widget :test))
         (prefix-format  (widget-get widget :prefix-format))
         (requires-audio (widget-get widget :requires-audio))
         (assign-keys    (widget-get widget :assign-keys))
         (on             (widget-get widget :on))
         (off            (widget-get widget :off))
         (size           (widget-get widget :size))
         (submit         (widget-get widget :submit))
         (on-submit-hook (widget-get widget :on-submit-hook))
         (instant-submit (widget-get widget :instant-submit))
         (labels         (widget-get widget :labels))
         (audio          (memrise/session-test-prompt-audio
                          (memrise/session-test-prompt test)))
         (text           (memrise/format-widget prefix-format test))
         (choices        (memrise/widget-choices test size)))
    (widget-put widget :button-args
                `(:on ,on :off ,off :on-glyph nil :off-glyph nil))
    (widget-put widget :format (concat text "%v"))
    (widget-put widget :args (memrise/session-itemize-choices choices))
    (if instant-submit
        (widget-put widget :notify (lambda (widget &rest event) (funcall submit widget)))
      (local-set-key (kbd "C-m") (memrise/make-interactive (-partial submit widget))))
    (widget-default-create widget)
    (when assign-keys
      (memrise/assign-buttons-keybindings (widget-get widget :buttons)
                                          memrise/radio-keys))
    (when labels
      (memrise/assign-labels (widget-get widget :children) labels))
    (memrise/widget-setup-audio widget)))

(defun memrise/choice-widget-submit-answer (widget)
  (let ((correct (memrise/get-correct-answer widget))
        (given (widget-value widget)))
    (if (not given)
        (message "Please, give an answer first!")
      (if (string= correct given)
          (message "Correct!")
        (message "Oops, correct answer is \"%s\"" correct))
      (mapc (lambda (x) (widget-apply x :deactivate))
            (widget-get widget :buttons))
      (memrise/widget-run-hooks widget (widget-get widget :on-submit-hook))
      (memrise/reset-session-bindings)
      (run-at-time "0.5 sec" nil 'memrise/display-next-task widget))))

(defun memrise/widget-run-hooks (widget hooks)
  (lexical-let ((arg widget))
      (mapc (lambda (hook) (funcall hook arg)) hooks)))

(defun memrise/typing-widget (test)
  (widget-create 'memrise/text-input-widget
                 :test test
                 :prefix-format memrise/typing-format
                 :instant-submit t
                 :requires-audio 'after))

(define-widget 'memrise/text-input-widget 'editable-field
  "Text input widget for memrise tests."
  :test nil
  :prefix-format ""
  :create 'memrise/text-input-widget-create
  :requires-audio nil
  :instant-submit nil
  :assign-keys t
  :input-method 'default
  :on-submit-hook nil
  :submit 'memrise/choice-widget-submit-answer)

(defun memrise/text-input-widget-create (widget)
  (lexical-let* ((test           (widget-get widget :test))
                 (prefix-format  (widget-get widget :prefix-format))
                 (requires-audio (widget-get widget :requires-audio))
                 (assign-keys    (widget-get widget :assign-keys))
                 (submit         (widget-get widget :submit))
                 (instant-submit (widget-get widget :instant-submit))
                 (audio          (memrise/session-test-prompt-audio
                                  (memrise/session-test-prompt test)))
                 (text           (memrise/format-widget prefix-format test)))
    (widget-put widget :format (concat text "%v"))
    (widget-put widget :notify (lambda (widget &rest _)
                                 (when (string= (widget-value widget)
                                                (memrise/get-correct-answer widget))
                                   (funcall submit widget))))
    (widget-default-create widget)
    (use-local-map widget-field-keymap)
    (local-set-key (kbd "C-m") (memrise/make-interactive submit widget))
    (memrise/widget-setup-audio widget)))

(defun memrise/get-correct-answer (widget)
  (memrise/session-test-correct (widget-get widget :test)))

(defun memrise/widget-setup-audio (widget)
  (let* ((test           (widget-get widget :test))
         (requires-audio (widget-get widget :requires-audio))
         (on-submit-hook (widget-get widget :on-submit-hook))
         (audio          (memrise/session-test-prompt-audio
                          (memrise/session-test-prompt test)))
         (play (-partial 'memrise/play-audio audio)))
    (cond ((eq requires-audio 'before)
           (funcall play)
           (local-set-key (kbd "C-r") (memrise/make-interactive play)))
          ((eq requires-audio 'after)
           (widget-put widget :on-submit-hook
                       (cons (memrise/make-argument-ignoring-lambda play)
                             on-submit-hook))))))

(defun memrise/audio-multiple-choice-widget-play (widget)
  (memrise/play-audio (widget-value widget)))

(defun memrise/play-audio (audio)
  "Play given `audio' file"
  (emms-play-file audio))

(defun memrise/format-widget (format test-or-learnable)
  (memrise/format-elements-with-faces format
                                      (memrise/session-format test-or-learnable)
                                      memrise/session-faces))

(defun memrise/widget-choices (test number)
  "Pick a `number' of translation choices for a quiz."
  (memrise/construct-choices (memrise/session-test-correct test)
                             (memrise/session-test-choices test)
                             number))

(defun memrise/construct-choices (correct incorrect number)
  "For given choices construct a randomized list of size `number'.
The result is guaranteed to have `correct' element in it."
  (let* ((filtered-incorrect (memrise/random-sublist incorrect
                                                     (1- number))))
    (memrise/shuffle-list (cons correct filtered-incorrect))))

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
  (mapcar (lambda (x) `(item :value ,x)) choices))

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

(defun memrise/assign-labels (items labels)
  "Show `labels' instead of `items'' values"
  (mapc (lambda (pair) (memrise/assign-label (car pair) (cdr pair)))
        (-zip items labels)))

(defun memrise/assign-label (item label)
  "Show `label' instead of `item''s values"
  (widget-put item :format (format "%S\n" label))
  (memrise/redraw-widget item))

(defun memrise/redraw-widget (widget)
  (widget-value-set widget (widget-value widget)))

(provide 'memrise-widget)
