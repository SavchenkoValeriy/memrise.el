;;; memrise-widget.el --- Collection of memrise widgets -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'memrise-utils)
(require 'widget)
(require 'wid-edit)
(require 'dash)

(defcustom memrise/radio-keys '([?a] [?s] [?d] [?f] [?g] [?h] [?j] [?k] [?l] [?\;]
                                [?z] [?x] [?c] [?v] [?b] [?n] [?m] [?,] [?.] [?/]
                                [?q] [?w] [?e] [?r] [?t] [?y] [?u] [?i] [?o] [?p])
  "Default keys for picking answers during memrise session."
  :type '(repeat :tag "Keys" (key-sequence :tag "key"))
  :group 'memrise)

(defcustom memrise/input-mode-key [?\C-i]
  "Key to turn on/off memrise button input mode."
  :type '(key-sequence :tag "key")
  :group 'memrise)

(defvar memrise/presentation-format
  "${text}\n${translation}\n${literal-translation}"
  "Template for presentation of a new thing.")

(defvar memrise/multiple-choice-format
  "${prompt}\nSelect the correct ${target} for the ${source} above:\n"
  "Template for multiple-choice widget.")

(defvar memrise/reversed-multiple-choice-format
  "${prompt}\nSelect the correct ${source} for the ${target} above:\n"
  "Template for inverted-multiple-choice widget.")

(defvar memrise/audio-multiple-choice-format
  "${prompt}\nChoose the correct ${target} for the ${source}\n"
  "Template for audio-multiple-choice widget.")

(defvar memrise/typing-format
  "${prompt}\nType the ${target} for the ${source} above:\n"
  "Template for typing widget.")

(defvar memrise/tapping-format
  "${prompt}\nArrange the ${target} to translate the ${source} above:\n"
  "Template for tapping widget.")

(defcustom memrise/radio-on
  (all-the-icons-faicon "dot-circle-o" :v-adjust 0.0)
  "Symbol to show for chosen radio button."
  :type '(string)
  :group 'memrise)

(defcustom memrise/radio-off
  (all-the-icons-faicon "circle-o" :v-adjust 0.0)
  "Symbol to show for not chosen radio button."
  :type '(string)
  :group 'memrise)

(defcustom memrise/audio-radio-on
  (all-the-icons-faicon "play-circle" :v-adjust 0.0)
  "Symbol to show for chosen audio radio button."
  :type '(string)
  :group 'memrise)

(defcustom memrise/audio-radio-off
  memrise/radio-off
  "Symbol to show for not chosen audio radio button."
  :type '(string)
  :group 'memrise)

(defun memrise/presentation (learnable)
  "Present a new `LEARNABLE' entity."
  (widget-create 'memrise/presentation-widget
                 :learnable learnable))

(define-widget 'memrise/presentation-widget 'item
  "Widget for presentation of a new word."
  :learnable nil
  :prefix-format memrise/presentation-format
  :requires-audio 'before
  :get-audio #'memrise/get-audio-from-test
  :create #'memrise/presentation-widget-create
  )

(defun memrise/presentation-widget-create (widget)
  "Create a memrise/presentation-widget from `WIDGET'."
  (let* ((learnable      (widget-get widget :learnable))
         (prefix-format  (widget-get widget :prefix-format))
         (text           (memrise/format-widget prefix-format learnable)))
    (widget-put widget :format text)
    (local-set-key (kbd "C-m") (memrise/make-interactive (-partial 'memrise/display-next-task widget)))
    (widget-default-create widget)
    (memrise/widget-setup-audio widget)))

(defun memrise/get-audio-from-learnable (widget)
  "Extracts audio file from `WIDGET's learnable."
  (memrise/session-learnable-audio (widget-get widget :learnable)))

(defun memrise/multiple-choice-widget (test number)
  "Create mutliple choice `TEST'.

\"multiple choice\" stands for choosing one answer in a target
language, for a given translation in a source language.
`TEST' is a memrise/session-test of a 'multiple-choice' kind.
`NUMBER' is a number of choices to show."
  (widget-create 'memrise/choice-widget
                 :test test
                 :prefix-format memrise/multiple-choice-format
                 :requires-audio 'after
                 :size number))

(defun memrise/reversed-multiple-choice-widget (test number)
  "Create reversed mutliple choice `TEST'.

\"reverse multiple choice\" stands for choosing one answer in a
source language, for a given translation in a target language.
`TEST' is a memrise/session-test of a 'reversed-multiple-choice' kind.
`NUMBER' is a number of choices to show."
  (widget-create 'memrise/choice-widget
                 :test test
                 :prefix-format memrise/reversed-multiple-choice-format
                 :requires-audio 'before
                 :size number))

(defun memrise/audio-multiple-choice-widget (test number)
  "Create audio mutliple choice `TEST'.

\"audio multiple choice\" stands for choosing correct audio
with a translation of a given word in a source language.
`TEST' is a memrise/session-test of a 'audio-multiple-choice' kind.
`NUMBER' is a number of choices to show."
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
  :get-audio #'memrise/get-audio-from-test
  :create #'memrise/choice-widget-create
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
  "Create a memrise/choice-widget from `WIDGET'."
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
         (audio          (oref (oref test prompt) audio))
         (text           (memrise/format-widget prefix-format test))
         (choices        (memrise/widget-choices test size)))
    ;; all child buttons should have the same shape using
    ;; give :on and :off values.
    ;; as long as we don't use glyphs for buttons, define this
    ;; explicitly to reduce creation time
    (widget-put widget :button-args
                `(:on ,on :off ,off :on-glyph nil :off-glyph nil))
    (widget-put widget :format (concat text "%v"))
    (widget-put widget :args (memrise/session-itemize-choices choices))
    (if instant-submit
        (widget-put widget :notify (memrise/make-widget-callback submit))
      (local-set-key (kbd "C-m") (memrise/make-interactive (-partial submit widget))))
    (widget-default-create widget)
    (when assign-keys
      (memrise/assign-buttons-keybindings (widget-get widget :buttons)
                                          memrise/radio-keys))
    (when labels
      (memrise/assign-labels labels (widget-get widget :children)))
    (memrise/widget-setup-audio widget)))

(defun memrise/choice-widget-submit-answer (widget)
  "Read the value of `WIDGET' and submit it as the answer."
  (let ((correct (memrise/get-correct-answer widget))
        (given (widget-value widget)))
    (if (not given)
        (message "Please, give an answer first!")
      (if (string= correct given)
          (message "Correct!")
        (message "Oops, correct answer is \"%s\"" correct))
      ;; to abandon user from picking other choices after
      ;; submitting her answer, turn off buttons...
      (mapc (lambda (x) (widget-apply x :deactivate))
            (widget-get widget :buttons))
      ;; ...and disable key-bindings
      (memrise/reset-session-bindings)
      (memrise/widget-run-hooks (widget-get widget :on-submit-hook) widget)
      (run-at-time "0.5 sec"
                   nil
                   #'memrise/call-after-all-audio-is-finished
                   #'memrise/display-next-task
                   widget))))

(defun memrise/call-after-all-audio-is-finished (func &rest args)
  "Call `FUNC' with `ARGS' after all audio is over."
  (lexical-let ((callback (-partial #'apply func args)))
    (defun memrise/audio-hook ()
      (remove-hook 'emms-player-finished-hook
                   'memrise/audio-hook)
      ;; running callback synchroneously mess up with emms
      (run-at-time "0.0 sec" nil callback))
    (if (not emms-player-playing-p)
        ;; nothing is playing - ready to call
        (funcall callback)
      ;; use emms callbacks to call it after player has finished
      (add-hook 'emms-player-finished-hook
                'memrise/audio-hook))))

(defun memrise/get-audio-from-test (widget)
  "Extracts audio file from `WIDGET's test."
  (let ((test (widget-get widget :test)))
    (oref (oref test prompt) audio)))

(defun memrise/widget-run-hooks (hooks widget)
  "Run `HOOKS' with `WIDGET' as their argument."
  (lexical-let ((arg widget))
    (mapc (lambda (hook) (funcall hook arg)) hooks)))

(defun memrise/typing-widget (test)
  "Create typing `TEST'."
  (widget-create 'memrise/text-input-widget
                 :test test
                 :prefix-format memrise/typing-format
                 :instant-submit t
                 :requires-audio 'after
                 :get-audio #'memrise/get-audio-from-test))

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

(define-minor-mode memrise/input-mode
  "Memrise mode to press push buttons for input."
  nil nil
  (make-keymap))

(define-widget 'memrise/pick-button 'item
  "Button to pick parts of input"
  :button-prefix ""
  :button-suffix ""
  :parent nil
  :format "%[%v%]\n"
  :action 'memrise/pick-button-insert-value)

(defun memrise/text-input-widget-create (widget)
  "Create a memrise/text-input-widget from `WIDGET'."
  (lexical-let* ((test           (widget-get widget :test))
                 (prefix-format  (widget-get widget :prefix-format))
                 (assign-keys    (widget-get widget :assign-keys))
                 (submit         (widget-get widget :submit))
                 (instant-submit (widget-get widget :instant-submit))
                 (input-method   (widget-get widget :input-method))
                 (text           (memrise/format-widget prefix-format test)))
    (widget-put widget :format (concat text "%v"))
    (widget-put widget :keymap nil)
    (when instant-submit
      (widget-put widget :notify (lambda (widget &rest _)
                                   (when (string= (widget-value widget)
                                                  (memrise/get-correct-answer widget))
                                     (funcall submit widget)))))
    (widget-default-create widget)
    ;;    (use-local-map widget-field-keymap)
    (local-set-key (kbd "C-m") (memrise/make-interactive submit widget))
    (memrise/widget-setup-audio widget)
    (when (eq input-method 'default)
      (memrise/setup-default-input-mode widget))
    ;; put cursor into a newly created text input
    (goto-char (widget-field-start widget))))

(defun memrise/setup-default-input-mode (widget)
  (lexical-let ((hint (widget-create
                       'item
                       :format (format "Press %s to turn input mode on/off:\n"
                                       (memrise/get-pretty-binding
                                        memrise/input-mode-key))))
                (buttons (memrise/create-pick-buttons
                          (oref test choices)
                          widget
                          memrise/input-mode-map
                          t)))
    (widget-put widget :buttons (cons hint buttons))
    (local-set-key memrise/input-mode-key
                   (memrise/make-interactive (-partial
                                              #'memrise/switch-input-mode
                                              buttons)))))

(defun memrise/get-pretty-binding (binding)
  "Return a pretty representation of the `BINDING'."
  (propertize (key-description binding) 'face 'memrise-session-keybinding))

(defun memrise/switch-input-mode (buttons)
  "Activate/deactivate memrise/input-mode and corresponding `BUTTONS'."
  (if memrise/input-mode
      (progn
        (memrise/input-mode -1)
        (memrise/deactivate-widgets buttons))
    (memrise/input-mode)
    (memrise/activate-widgets buttons)))

(defun memrise/tapping-widget (test)
  "Create typing `TEST'."
  (widget-create 'memrise/tapping-widget
                 :test test
                 :prefix-format memrise/tapping-format
                 :requires-audio 'after
                 :get-audio #'memrise/get-audio-from-test))

(define-widget 'memrise/tapping-widget 'editable-field
  "Widget for tapping choices for memrise tests."
  :test nil
  :prefix-format ""
  :create 'memrise/tapping-widget-create
  :requires-audio nil
  :instant-submit t
  :assign-keys t
  :on-submit-hook nil
  :min 4
  :max 8
  :submit 'memrise/choice-widget-submit-answer)

(defun memrise/tapping-widget-create (widget)
  "Create a memrise/tapping-widget from `WIDGET'."
  (lexical-let* ((test           (widget-get widget :test))
                 (prefix-format  (widget-get widget :prefix-format))
                 (assign-keys    (widget-get widget :assign-keys))
                 (submit         (widget-get widget :submit))
                 (instant-submit (widget-get widget :instant-submit))
                 (text           (memrise/format-widget prefix-format test)))
    (suppress-keymap (current-local-map) t)
    (widget-put widget :format (concat text "%v"))
    (widget-put widget :keymap nil)
    (widget-default-create widget)
    (local-set-key (kbd "C-m") (memrise/make-interactive submit widget))
    (memrise/widget-setup-audio widget)
    (memrise/setup-tapping-bindings widget)
    (memrise/setup-tapping-choices widget)
    ;; put cursor into a newly created text input
    (goto-char (widget-field-start widget))))

(defun memrise/setup-tapping-bindings (widget)
  (local-set-key (kbd "C-d") (memrise/make-interactive
                              #'memrise/delete-next-tap-word))
  (local-set-key [backspace] (memrise/make-interactive
                              #'memrise/delete-previous-tap-word)))

(defun memrise/delete-next-tap-word ()
  (let ((tap-word (memrise/get-this-or-next-tap-word)))
    (memrise/delete-tap-word tap-word)))

(defun memrise/delete-previous-tap-word ()
  (let ((tap-word (memrise/get-this-or-previous-tap-word)))
    (memrise/delete-tap-word tap-word)))

(defun memrise/delete-tap-word (tap-word)
  (when tap-word
    (delete-region (overlay-start tap-word)
                   (overlay-end tap-word))
    (delete-overlay tap-word)))

(defun memrise/get-this-or-next-tap-word ()
  (or (memrise/get-this-tap-word)
      (memrise/get-next-tap-word)))

(defun memrise/get-this-or-previous-tap-word ()
  (or (memrise/get-this-tap-word)
      (memrise/get-previous-tap-word)))

(defun memrise/get-this-tap-word ()
  (memrise/find-tap-word (point)))

(defun memrise/get-next-tap-word ()
  (let ((next-overlay-pos (next-overlay-change (point))))
    (memrise/find-tap-word next-overlay-pos)))

(defun memrise/get-previous-tap-word ()
  (let ((previous-overlay-pos (previous-overlay-change (point))))
    (memrise/find-tap-word previous-overlay-pos)))

(defun memrise/find-tap-word (where)
  (memrise/find-tap-word-internal (overlays-at where)))

(defun memrise/find-tap-word-internal (overlays)
  (let ((overlay (car overlays)))
    (if (not overlays)
        nil
      (if (overlay-get overlay 'tap)
          overlay
        (memrise/find-tap-word-internal (cdr overlays))))))

(defun memrise/setup-tapping-choices (widget)
  (lexical-let* ((min     (widget-get widget :min))
                 (max     (widget-get widget :max))
                 (choices (memrise/construct-choices
                           (oref test correct)
                           (oref test choices)
                           max min))
                 (buttons (memrise/create-pick-buttons
                           choices widget nil nil
                           '(:action memrise/pick-button-insert-complex-value))))
    (widget-put widget :buttons buttons)))

(defun memrise/deactivate-widgets (widgets)
  "Deactivate given `WIDGETS'."
  (memrise/apply-for-all-widgets widgets :deactivate))

(defun memrise/activate-widgets (widgets)
  "Activate given `WIDGETS'."
  (memrise/apply-for-all-widgets widgets :activate))

(defun memrise/apply-for-all-widgets (widgets command)
  "For all `WIDGETS' apply `COMMAND'."
  (mapc (lambda (x) (widget-apply x command)) widgets))

(defun memrise/pick-button-insert-value (widget &optional _event)
  (let ((parent (widget-get widget :parent)))
    (memrise/goto-field parent)
    (insert (widget-value widget))))

(defun memrise/pick-button-insert-complex-value (widget &optional _event)
  (let* ((parent (widget-get widget :parent))
         (start (or (memrise/goto-field parent)
                    (memrise/goto-next-tap-word)
                    (point)))
         end
         overlay)
    (memrise/pick-button-insert-value widget)
    (insert " ")
    (setq end (point))
    (setq overlay (make-overlay start end))
    (overlay-put overlay 'tap t)))

(defun memrise/goto-next-tap-word ()
  (let ((tap-word (memrise/get-this-or-next-tap-word)))
    (if tap-word
        (goto-char (overlay-end tap-word))
      nil)))

(defun memrise/goto-field (field-widget)
  "Goto editable field of `FIELD_WIDGET' if not there."
  (unless (<= (widget-field-start field-widget)
              (point)
              (widget-field-text-end field-widget))
    (goto-char (widget-field-text-end field-widget))))

(defun memrise/get-correct-answer (widget)
  "Return a correct answer for a test represented by `WIDGET'."
  (oref (widget-get widget :test) correct))

(defun memrise/widget-setup-audio (widget)
  "Define audio behavior for the given memrise `WIDGET'.

Provided `WIDGET' should have the following properties:
* :requires-audio - one of `(before after nil), when to play audio
* :get-audio - a function to retrieve audio from `WIDGET' object"
  (let* ((requires-audio (widget-get widget :requires-audio))
         (on-submit-hook (widget-get widget :on-submit-hook))
         (get-audio      (widget-get widget :get-audio))
         (audio          (funcall get-audio widget))
         (play (-partial 'memrise/play-audio audio)))
    (cond ((eq requires-audio 'before)
           (funcall play)
           (local-set-key (kbd "C-r") (memrise/make-interactive play)))
          ((eq requires-audio 'after)
           (widget-put widget :on-submit-hook
                       (cons (memrise/make-argument-ignoring-lambda play)
                             on-submit-hook))))))

(defun memrise/audio-multiple-choice-widget-play (widget)
  "Play the audio currently picked in the `WIDGET'."
  (memrise/play-audio (widget-value widget)))

(defun memrise/play-audio (audio)
  "Play the given `AUDIO' file."
  (emms-play-file audio))

(defun memrise/format-widget (format test-or-learnable)
  "Apply `FORMAT' to the given `TEST-OR-LEARNABLE'."
  (memrise/format-elements-with-faces format
                                      (memrise/session-format test-or-learnable)
                                      memrise/session-faces))

(defun memrise/widget-choices (test number)
  "For the given `TEST' pick a `NUMBER' of translation choices for a quiz."
  (memrise/construct-choices (oref test correct)
                             (oref test choices)
                             number))

(defun memrise/construct-choices
    (correct incorrect number &optional minimal)
  "Construct a randomized list of choices.

`CORRECT' - correct choice(s), guaranteed to be in the result list.
`INCORRECT' - list of incorrect choices (at least of `NUMBER' - 1 size)
`NUMBER' - number of choices in the result list.
`MINIMAL' - minimal number of incorrect choices."
  (unless minimal
    (setq minimal 0))
  (let* ((concat (if (sequencep correct) 'append 'cons))
         (correct-size (if (sequencep correct)
                           (length correct)
                         1))
         (incorrect-size (max (- number correct-size) minimal))
         (filtered-incorrect
          (memrise/random-sublist incorrect incorrect-size)))
    (memrise/shuffle-list (funcall concat correct filtered-incorrect))))

(defun memrise/session-format (test-or-learnable)
  "Return an alist to use for widget formating.

`TEST-OR-LEARNABLE' is an object to retrieve info to display."
  (let ((prompt (memrise-session-test-prompt))
        (learnable (memrise-session-learnable)))
    (when (memrise-session-test-p test-or-learnable)
      (setq prompt (oref test-or-learnable prompt)))
    (when (memrise-session-learnable-p test-or-learnable)
      (setq learnable test-or-learnable))
    `((text . ,(oref learnable text))
      (translation . ,(oref learnable translation))
      (literal-translation . ,(or (oref learnable literal-translation) ""))
      (prompt . ,(oref prompt text))
      (source . ,(memrise/session-source session))
      (target . ,(memrise/session-target session)))))

(defun memrise/session-itemize-choices (choices)
  "Turn `CHOICES' into items."
  (mapcar (lambda (x) `(item :value ,x)) choices))

(defun memrise/create-pick-buttons (picks widget &optional map deactivate extra-args)
  "Create buttons representing `PICKS' as children of `WIDGET'."
  (let* ((buttons (mapcar (lambda (x)
                            (apply 'widget-create
                                   'memrise/pick-button
                                   :button-suffix ""
                                   :parent widget
                                   (-snoc extra-args
                                          (format "%s" x))))
                          picks)))
    (memrise/assign-buttons-keybindings buttons
                                        memrise/radio-keys
                                        map)
    (when deactivate
      (memrise/deactivate-widgets buttons))
    buttons))

(defun memrise/assign-buttons-keybindings (buttons bindings &optional keymap)
  "Change radion `BUTTONS' shapes, assign `BINDINGS' (for the given `KEYMAP').

New shape would be of a form '[key] (*button*)'.
'[key]'s would be colored rainbow colors provided by 'rainbow-delimiters'.

Optional `KEYMAP' parameter is defaulted to current keymap."
  (unless keymap (setq keymap (current-local-map)))
  (mapc (lambda (args) (apply 'memrise/assign-button-keybinding keymap args))
        (-zip buttons bindings (number-sequence 1 (length buttons)))))

(defun memrise/assign-button-keybinding (keymap button keybinding index)
  "For the given `KEYMAP' change radio `BUTTON' shape and `KEYBINDING',
colors it with a rainbow color `INDEX'."
  (lexical-let ((button button))
    (widget-put button
                :button-prefix
                (propertize (format "[%s] "
                                    (key-description keybinding))
                            'face
                            (rainbow-delimiters-default-pick-face index t nil)))
    ;;to show the new button shape, we need to redraw it
    (memrise/redraw-widget button)
    (define-key keymap keybinding
      (lambda () (interactive) (widget-apply-action button)))))

(defun memrise/assign-labels (labels items)
  "Show `LABELS' instead of `ITEMS'' values."
  (mapc (lambda (pair) (memrise/assign-label (car pair) (cdr pair)))
        (-zip labels items)))

(defun memrise/assign-label (label item)
  "Show `LABEL' instead of `ITEM''s value."
  (widget-put item :format (format "%S\n" label))
  (memrise/redraw-widget item))

(defun memrise/redraw-widget (widget)
  "Redraw `WIDGET' (to update it)."
  (widget-value-set widget (widget-value widget)))

(provide 'memrise-widget)
;;; memrise-widget.el ends here
