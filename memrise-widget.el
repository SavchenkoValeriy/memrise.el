;;; memrise-widget.el --- Collection of memrise widgets -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019  Valeriy Savchenko

;; Author: Valeriy Savchenko <sinmipt@gmail.com>

;; This file is NOT part of GNU Emacs.

;; memrise.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; memrise.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with memrise.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module contains all of the memrise.el widgets and all of the trickery
;; connected to them.

;;; Code:

(require 'memrise-media)
(require 'memrise-request)
(require 'memrise-ui)
(require 'memrise-utils)
(require 'widget)
(require 'wid-edit)
(require 'dash)
(require 'all-the-icons)
(require 'rainbow-delimiters)

(defcustom memrise-radio-keys '([?a] [?s] [?d] [?f] [?g] [?h] [?j] [?k] [?l] [?\;]
                                [?z] [?x] [?c] [?v] [?b] [?n] [?m] [?,] [?.] [?/]
                                [?q] [?w] [?e] [?r] [?t] [?y] [?u] [?i] [?o] [?p])
  "Default keys for picking answers during memrise session."
  :type '(repeat :tag "Keys" (key-sequence :tag "key"))
  :group 'memrise)

(defcustom memrise-input-mode-key [?\C-i]
  "Key to turn on/off memrise button input mode."
  :type '(key-sequence :tag "key")
  :group 'memrise)

(defcustom memrise-submit-key [?\C-m]
  "Key to submit current answer."
  :type '(key-sequence :tag "key")
  :group 'memrise)

(defcustom memrise-replay-audio-key [?\C-r]
  "Key to replay the test's audio."
  :type '(key-sequence :tag "key")
  :group 'memrise)

(defvar memrise-presentation-format
  "${text}\n${translation}\n${literal-translation}"
  "Template for presentation of a new thing.")

(defvar memrise-multiple-choice-format
  "${prompt}\nSelect the correct ${target} for the ${source} above:\n"
  "Template for multiple-choice widget.")

(defvar memrise-reversed-multiple-choice-format
  "${prompt}\nSelect the correct ${source} for the ${target} above:\n"
  "Template for inverted-multiple-choice widget.")

(defvar memrise-audio-multiple-choice-format
  "${prompt}\nChoose the correct ${target} for the ${source}\n"
  "Template for audio-multiple-choice widget.")

(defvar memrise-typing-format
  "${prompt}\nType the ${target} for the ${source} above:\n"
  "Template for typing widget.")

(defvar memrise-tapping-format
  "${prompt}\nArrange the ${target} to translate the ${source} above:\n"
  "Template for tapping widget.")

(defcustom memrise-radio-on
  (all-the-icons-faicon "dot-circle-o" :v-adjust 0.0)
  "Symbol to show for chosen radio button."
  :type 'string
  :group 'memrise)

(defcustom memrise-radio-off
  (all-the-icons-faicon "circle-o" :v-adjust 0.0)
  "Symbol to show for not chosen radio button."
  :type 'string
  :group 'memrise)

(defcustom memrise-audio-radio-on
  (all-the-icons-faicon "play-circle" :v-adjust 0.0)
  "Symbol to show for chosen audio radio button."
  :type 'string
  :group 'memrise)

(defcustom memrise-audio-radio-off
  memrise-radio-off
  "Symbol to show for not chosen audio radio button."
  :type 'string
  :group 'memrise)

(defcustom memrise-pause-after-correct
  0.5
  "Pause (in seconds) to make after correct answer before the next test."
  :type 'float
  :group 'memrise)

(defcustom memrise-pause-after-incorrect
  2.0
  "Pause (in seconds) to make after incorrect answer before the next test."
  :type 'float
  :group 'memrise)

(defcustom memrise-typing-instant-submit
  t
  "Accept answer for a typing test automatically without explicit submit request."
  :type 'boolean
  :group 'memrise)

(defcustom memrise-tapping-instant-submit
  t
  "Accept answer for a tapping test automatically without explicit submit request."
  :type 'boolean
  :group 'memrise)

(defcustom memrise-multiple-choice-instant-submit
  t
  "Accept answer for a multiple choice test automatically without explicit submit request."
  :type 'boolean
  :group 'memrise)

(defcustom memrise-reversed-multiple-choice-instant-submit
  t
  "Accept answer for a reversed multiple choice test automatically without explicit submit request."
  :type 'boolean
  :group 'memrise)

(defun memrise-presentation (learnable)
  "Present a new `LEARNABLE' entity."
  (widget-create 'memrise-presentation-widget
                 :learnable learnable))

(define-widget 'memrise-presentation-widget 'item
  "Widget for presentation of a new word."
  :learnable nil
  :prefix-format memrise-presentation-format
  :requires-audio 'before
  :get-audio #'memrise-get-audio-from-learnable
  :create #'memrise-presentation-widget-create)

(defun memrise-presentation-widget-create (widget)
  "Create a memrise-presentation-widget from `WIDGET'."
  (let* ((learnable      (widget-get widget :learnable))
         (prefix-format  (widget-get widget :prefix-format))
         (text           (memrise-format-widget prefix-format learnable)))
    (widget-put widget :format text)
    (local-set-key memrise-submit-key
                   (memrise-make-interactive
                    (-partial 'memrise-display-next-task widget)))
    (widget-default-create widget)
    (memrise-widget-setup-audio widget)))

(defun memrise-get-audio-from-learnable (widget)
  "Extracts audio file from `WIDGET's learnable."
  (oref (widget-get widget :learnable) audio))

(defun memrise-multiple-choice-widget (test number)
  "Create mutliple choice `TEST'.

\"multiple choice\" stands for choosing one answer in a target
language, for a given translation in a source language.
`TEST' is a memrise-session-test of a 'multiple-choice' kind.
`NUMBER' is a number of choices to show."
  (widget-create 'memrise-choice-widget
                 :test test
                 :prefix-format memrise-multiple-choice-format
                 :requires-audio 'after
                 :instant-submit memrise-multiple-choice-instant-submit
                 :size number))

(defun memrise-reversed-multiple-choice-widget (test number)
  "Create reversed mutliple choice `TEST'.

\"reverse multiple choice\" stands for choosing one answer in a
source language, for a given translation in a target language.
`TEST' is a memrise-session-test of a 'reversed-multiple-choice' kind.
`NUMBER' is a number of choices to show."
  (widget-create 'memrise-choice-widget
                 :test test
                 :prefix-format memrise-reversed-multiple-choice-format
                 :requires-audio 'before
                 :instant-submit memrise-reversed-multiple-choice-instant-submit
                 :size number))

(defun memrise-audio-multiple-choice-widget (test number)
  "Create audio mutliple choice `TEST'.

\"audio multiple choice\" stands for choosing correct audio
with a translation of a given word in a source language.
`TEST' is a memrise-session-test of a 'audio-multiple-choice' kind.
`NUMBER' is a number of choices to show."
  (widget-create 'memrise-choice-widget
                 :test test
                 :prefix-format memrise-audio-multiple-choice-format
                 :on memrise-audio-radio-on
                 :off memrise-audio-radio-off
                 :requires-audio nil
                 :notify (memrise-make-widget-callback
                          'memrise-audio-multiple-choice-widget-play)
                 ;; it doesn't really make sense to do instant submit
                 ;; for audio tests
                 :instant-submit nil
                 :labels (number-sequence 1 8)))

(define-widget 'memrise-choice-widget 'radio-button-choice
  "Multiple choice widget for memrise tests."
  :test nil
  :prefix-format ""
  :requires-audio nil ; one of `(before after ,nil)
  :get-audio #'memrise-get-audio-from-test
  :create #'memrise-choice-widget-create
  :assign-keys t
  :on memrise-radio-on
  :off memrise-radio-off
  :size 4
  :submit #'memrise-choice-widget-submit-answer
  :on-submit-hook nil
  :instant-submit nil
  :labels '())

(defun memrise-choice-widget-create (widget)
  "Create a memrise-choice-widget from `WIDGET'."
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
         (text           (memrise-format-widget prefix-format test))
         (choices        (memrise-widget-choices test size)))
    ;; all child buttons should have the same shape using
    ;; give :on and :off values.
    ;; as long as we don't use glyphs for buttons, define this
    ;; explicitly to reduce creation time
    (widget-put widget :button-args
                `(:on ,on :off ,off :on-glyph nil :off-glyph nil))
    (widget-put widget :format (concat text "%v"))
    (widget-put widget :args (memrise-session-itemize-choices choices))
    (if instant-submit
        (widget-put widget :notify (memrise-make-widget-callback submit))
      (local-set-key memrise-submit-key (memrise-make-interactive
                                         (-partial submit widget))))
    (widget-default-create widget)
    (when assign-keys
      (memrise-assign-buttons-keybindings (widget-get widget :buttons)
                                          memrise-radio-keys))
    (when labels
      (memrise-assign-labels labels (widget-get widget :children)))
    (memrise-widget-setup-audio widget)))

(defun memrise-choice-widget-submit-answer (widget)
  "Read the value of `WIDGET' and submit it as the answer."
  (let* ((test (widget-get widget :test))
         (answer (memrise--get-real-answer-for-the-prompt test))
         (given (memrise--widget-get-answer widget)))
    (if (not given)
        (message "Please, give an answer first!")
      (let ((correct? (memrise--is-answer-correct test given)))
        (if correct?
            (message "Correct!")
          (message "Oops, correct answer is \"%s\"" answer))
        ;; to abandon user from picking other choices after
        ;; submitting her answer, turn off buttons...
        (mapc (lambda (x) (widget-apply x :deactivate))
              (widget-get widget :buttons))
        ;; ...and disable key-bindings
        (memrise-reset-session-bindings)
        (memrise--request-send-answer
         learnable
         (oref session course-id)
         (oref test kind)
         given
         ;; assign points only for answering correctly
         (if correct? (memrise--get-number-of-points test) 0)
         ;; TODO: time the test for real
         5000)
        (memrise-widget-run-hooks (widget-get widget :on-submit-hook) widget)
        (memrise--proceed-to-the-next-test widget (if correct?
                                                      memrise-pause-after-correct
                                                    memrise-pause-after-incorrect))))))

(defun memrise--get-real-answer-for-the-prompt (test)
  "Get the correct answer for the `TEST' to show in the prompt."
  (if (string= "audio_multiple_choice"
               (oref test kind))
      ;; audio questions have mp3 files as answers, that is not very
      ;; readable, so we use a text of the current learnable
      (oref learnable text)
    ;; otherwise the answer will do
    (oref test answer)))

(defun memrise--proceed-to-the-next-test (widget time)
  "Finish up with `WIDGET' and proceed with the next test widget after `TIME' seconds."
  (run-at-time (format "%f sec" time)
               nil
               #'memrise-call-after-all-audio-is-finished
               #'memrise-display-next-task
               widget))

(defun memrise--get-number-of-points (test)
  "Decide on the number of points for the `TEST'."
  ;; TODO: calculate the number of points based on the current session and test
  45)

(defun memrise--widget-get-answer (widget)
  "Get the given answer from the `WIDGET'."
  ;; widget might've overriden the get-answer function
  (let ((get-answer (widget-get widget :get-answer)))
    (if get-answer
        (funcall get-answer widget)
      ;; if no, just take the value
      (widget-value widget))))

(defun memrise--is-answer-correct (test answer)
  "Return t if the `ANSWER' is the correct answer for `TEST'."
  (-contains-p (oref test correct) answer))

(defun memrise-get-audio-from-learnable (widget)
  "Extracts audio file from `WIDGET's learnable."
  (let ((learnable (widget-get widget :learnable)))
    (oref learnable audio)))

(defun memrise-get-audio-from-test (widget)
  "Extracts audio file from `WIDGET's test."
  (let ((test (widget-get widget :test)))
    (oref (oref test prompt) audio)))

(defun memrise-widget-run-hooks (hooks widget)
  "Run `HOOKS' with `WIDGET' as their argument."
  (mapc (lambda (hook) (funcall hook widget)) hooks))

(defun memrise--widget-add-hook (widget hook-name hook)
  "Add `HOOK' for a `HOOK-NAME' event for the given `WIDGET'."
  (widget-put widget hook-name
              (cons hook (widget-get widget hook-name))))

(defun memrise-typing-widget (test)
  "Create typing `TEST'."
  (widget-create 'memrise-text-input-widget
                 :test test
                 :prefix-format memrise-typing-format
                 :instant-submit memrise-typing-instant-submit
                 :requires-audio 'after
                 :get-audio #'memrise-get-audio-from-test))

(define-widget 'memrise-text-input-widget 'editable-field
  "Text input widget for memrise tests."
  :test nil
  :prefix-format ""
  :create #'memrise-text-input-widget-create
  :requires-audio nil
  :instant-submit nil
  :assign-keys t
  :input-method 'default
  :on-submit-hook nil
  :submit #'memrise-choice-widget-submit-answer)

(define-minor-mode memrise-input-mode
  "Memrise mode to press push buttons for input."
  nil nil
  (make-keymap))

(define-widget 'memrise-pick-button 'item
  "Button to pick parts of input"
  :button-prefix ""
  :button-suffix ""
  :parent nil
  :format "%[%v%]\n"
  :action 'memrise-pick-button-insert-value)

(defun memrise-text-input-widget-create (widget)
  "Create a memrise-text-input-widget from `WIDGET'."
  (let* ((test           (widget-get widget :test))
         (prefix-format  (widget-get widget :prefix-format))
         (assign-keys    (widget-get widget :assign-keys))
         (submit         (widget-get widget :submit))
         (instant-submit (widget-get widget :instant-submit))
         (input-method   (widget-get widget :input-method))
         (text           (memrise-format-widget prefix-format test)))
    (widget-put widget :format (concat text "%v"))
    (widget-put widget :keymap nil)
    (when instant-submit
      (widget-put widget :notify #'memrise--smart-instant-submit-hook))
    (widget-default-create widget)
    (local-set-key memrise-submit-key (memrise-make-interactive submit widget))
    (memrise-widget-setup-audio widget)
    (when (eq input-method 'default)
      (memrise-setup-default-input-mode widget))
    ;; put cursor into a newly created text input
    (goto-char (widget-field-start widget))))

(defun memrise-setup-default-input-mode (widget)
  "Setup input mode for the typing `WIDGET'."
  (let* ((hint (widget-create
                'item
                :format (format "Press %s to turn input mode on/off:\n"
                                (memrise-get-pretty-binding
                                 memrise-input-mode-key))))
         (test (widget-get widget :test))
         (buttons (memrise-create-pick-buttons
                   (oref test choices)
                   widget
                   memrise-input-mode-map
                   t)))
    (widget-put widget :buttons (cons hint buttons))
    (local-set-key memrise-input-mode-key
                   (memrise-make-interactive (-partial
                                              #'memrise--switch-input-mode
                                              buttons)))
    (memrise--widget-add-hook widget
                              :on-submit-hook
                              #'memrise--turn-input-mode-off-hook)))

(defun memrise-get-pretty-binding (binding)
  "Return a pretty representation of the `BINDING'."
  (propertize (key-description binding) 'face 'memrise-session-keybinding))

(defun memrise--turn-input-mode-off-hook (widget)
  "Turn off input mode of the `WIDGET' so it doesn't mess with other widgets."
  (memrise--turn-input-mode-off (widget-get widget :buttons)))

(defun memrise--switch-input-mode (buttons)
  "Activate/deactivate memrise-input-mode and corresponding `BUTTONS'."
  (if memrise-input-mode
      (memrise--turn-input-mode-off buttons)
    (memrise--turn-input-mode-on buttons)))

(defun memrise--turn-input-mode-on (buttons)
  "Turn on input mode for the given `BUTTONS'."
  (memrise-input-mode)
  (memrise-activate-widgets buttons))

(defun memrise--turn-input-mode-off (buttons)
  "Turn off input mode for the given `BUTTONS'."
  (memrise-input-mode -1)
  (memrise-deactivate-widgets buttons))

(defun memrise-tapping-widget (test)
  "Create tapping `TEST'."
  (widget-create 'memrise-tapping-widget
                 :test test
                 :prefix-format memrise-tapping-format
                 :requires-audio 'after
                 :get-audio #'memrise-get-audio-from-test))

(define-widget 'memrise-tapping-widget 'editable-field
  "Widget for tapping choices for memrise tests."
  :test nil
  :prefix-format ""
  :create #'memrise-tapping-widget-create
  :requires-audio nil
  :instant-submit memrise-tapping-instant-submit
  :assign-keys t
  :on-submit-hook nil
  :min 4
  :max 8
  :submit #'memrise-choice-widget-submit-answer
  :get-answer #'memrise--tapping-get-answer)

(defun memrise-tapping-widget-create (widget)
  "Create a memrise-tapping-widget from `WIDGET'."
  (let* ((test           (widget-get widget :test))
         (prefix-format  (widget-get widget :prefix-format))
         (assign-keys    (widget-get widget :assign-keys))
         (submit         (widget-get widget :submit))
         (instant-submit (widget-get widget :instant-submit))
         (text           (memrise-format-widget prefix-format test)))
    (suppress-keymap (current-local-map) t)
    (widget-put widget :format (concat text "%v"))
    (widget-put widget :keymap nil)
    (widget-default-create widget)
    (local-set-key memrise-submit-key (memrise-make-interactive submit widget))
    (when instant-submit
      (memrise--widget-add-hook
       widget
       :on-tap-hook
       #'memrise--smart-instant-submit-hook))
    (memrise-widget-setup-audio widget)
    (memrise-setup-tapping-bindings widget)
    (memrise-setup-tapping-choices widget)
    ;; put cursor into a newly created text input
    (goto-char (widget-field-start widget))))

(defun memrise--smart-instant-submit-hook (widget &rest _ignore)
  "Check that the current answer in `WIDGET' is correct and submit it if it is."
  (when (memrise--is-answer-correct
         (widget-get widget :test)
         (memrise--widget-get-answer widget))
    (funcall (widget-get widget :submit) widget)))

(defun memrise-setup-tapping-bindings (widget)
  "Setup bindings for the tapping input `WIDGET'."
  ;; 'delete' deletes the next word
  (local-set-key (kbd "C-d") (memrise-make-interactive
                              #'memrise-delete-next-tap-word))
  ;; and 'backspace' deletes the previous word
  (local-set-key [backspace] (memrise-make-interactive
                              #'memrise-delete-previous-tap-word)))

(defun memrise--tapping-get-answer (widget)
  "Get the answer from the tapping `WIDGET'."
  (save-excursion
    (-unfold #'memrise--tapping-get-answer-internal
             (widget-field-start widget))))

(defun memrise--tapping-get-answer-internal (where)
  "Keep moving to the right (`WHERE') until we have words."
  (goto-char where)
  (-when-let* ((tap-word (memrise-get-this-tap-word))
               (end (overlay-end tap-word))
               (word (buffer-substring-no-properties (overlay-start tap-word)
                                                     (1- end))))
    (cons word end)))

(defun memrise-delete-next-tap-word ()
  "Delet the next tap word in the input field."
  (let ((tap-word (memrise-get-this-or-next-tap-word)))
    (memrise-delete-tap-word tap-word)))

(defun memrise-delete-previous-tap-word ()
  "Delet the previous tap word in the input field."
  (let ((tap-word (memrise-get-this-or-previous-tap-word)))
    (memrise-delete-tap-word tap-word)))

(defun memrise-delete-tap-word (tap-word)
  "Delet the `TAP-WORD' in the input field."
  (when tap-word
    (delete-region (overlay-start tap-word)
                   (overlay-end tap-word))
    (delete-overlay tap-word)))

(defun memrise-get-this-or-next-tap-word ()
  "Get the tap word under the point or the following one."
  (or (memrise-get-this-tap-word)
      (memrise-get-next-tap-word)))

(defun memrise-get-this-or-previous-tap-word ()
  "Get the tap word under the point or the previous one."
  (or (memrise-get-this-tap-word)
      (memrise-get-previous-tap-word)))

(defun memrise-get-this-tap-word ()
  "Get the tap word under the point."
  (memrise-find-tap-word (point)))

(defun memrise-get-next-tap-word ()
  "Get the tap word from in front of the current point."
  (let ((next-overlay-pos (next-overlay-change (point))))
    (memrise-find-tap-word next-overlay-pos)))

(defun memrise-get-previous-tap-word ()
  "Get the tap word from behind of the current point."
  (let ((previous-overlay-pos (previous-overlay-change (point))))
    (memrise-find-tap-word previous-overlay-pos)))

(defun memrise-find-tap-word (where)
  "Get the tap word from `WHERE'."
  (memrise-find-tap-word-internal (overlays-at where)))

(defun memrise-find-tap-word-internal (overlays)
  "Find the tap word overlay from `OVERLAYS'."
  (let ((overlay (car overlays)))
    (if (not overlays)
        nil
      (if (overlay-get overlay 'tap)
          overlay
        (memrise-find-tap-word-internal (cdr overlays))))))

(defun memrise-setup-tapping-choices (widget)
  "Setup the list of tapping words available for the user in `WIDGET'."
  (let* ((min     (widget-get widget :min))
         (max     (widget-get widget :max))
         (test    (widget-get widget :test))
         (choices (memrise-construct-choices
                   ;; correct is always a list
                   (car (oref test correct))
                   (oref test choices)
                   max min))
         (buttons (memrise-create-pick-buttons
                   choices widget nil nil
                   '(:action memrise-pick-button-insert-complex-value))))
    (widget-put widget :buttons buttons)))

(defun memrise-deactivate-widgets (widgets)
  "Deactivate given `WIDGETS'."
  (memrise-apply-for-all-widgets widgets :deactivate))

(defun memrise-activate-widgets (widgets)
  "Activate given `WIDGETS'."
  (memrise-apply-for-all-widgets widgets :activate))

(defun memrise-apply-for-all-widgets (widgets command)
  "For all `WIDGETS' apply `COMMAND'."
  (mapc (lambda (x) (widget-apply x command)) widgets))

(defun memrise-pick-button-insert-value (widget &optional _event)
  "Insert value of a clicked `WIDGET' button into a parent field."
  (let ((parent (widget-get widget :parent)))
    (memrise-goto-field parent)
    (insert (widget-value widget))))

(defun memrise-pick-button-insert-complex-value (widget &optional _event)
  "Insert a selected tap word into the `WIDGET'."
  ;; parent is the input field
  (let* ((parent (widget-get widget :parent))
         ;; where do we start:
         ;; at the end of th input field (if we were not there at all)
         (start (or (memrise-goto-field parent)
                    ;; at the next inserted tap word
                    (memrise-goto-next-tap-word)
                    ;; or just from where we are if nothing from above applies
                    (point)))
         ;; the inserted tap word that we are inserting in front of
         (next-tap-word (memrise-get-this-or-next-tap-word))
         end
         overlay)
    ;; insert the tap word
    (memrise-pick-button-insert-value widget)
    ;; insert a space after EVERY tap word (we couldn't know if it should
    ;; have a space after it or not, but it's safer to put every time)
    (insert " ")
    ;; store the point of where we are after inserting the tap word
    (setq end (point))
    ;; this new area from `start' to `end' is a new inserted tap word
    (setq overlay (make-overlay start end))
    (overlay-put overlay 'tap t)
    ;; if we had a tap word in front...
    (when next-tap-word
      ;; ...we should fix its positions as it now includes itself
      ;; AND the new tap word, so we tell it to start where the new
      ;; one ends
      (move-overlay next-tap-word end (overlay-end next-tap-word)))
    ;; notify parent that "on-tap" event happened
    (memrise-widget-run-hooks (widget-get parent :on-tap-hook) parent)))

(defun memrise-goto-next-tap-word ()
  "Move the point to the next tap word when it makes sense."
  ;; check that there is a tap word in front
  (-when-let (tap-word (memrise-get-this-or-next-tap-word))
    ;; if we are a the beginning of it, there is no need to move
    (unless (= (point) (overlay-start tap-word))
      (goto-char (overlay-end tap-word)))))

(defun memrise-goto-field (field-widget)
  "Goto editable field of `FIELD-WIDGET' if not there."
  (unless (<= (widget-field-start field-widget)
              (point)
              (widget-field-text-end field-widget))
    (goto-char (widget-field-text-end field-widget))))

(defun memrise-get-correct-answer (widget)
  "Return a correct answer for a test represented by `WIDGET'."
  (oref (widget-get widget :test) correct))

(defun memrise-widget-setup-audio (widget)
  "Define audio behavior for the given memrise `WIDGET'.

Provided `WIDGET' should have the following properties:
* :requires-audio - one of `(before after nil), when to play audio
* :get-audio - a function to retrieve audio from `WIDGET' object"
  (let* ((requires-audio (widget-get widget :requires-audio))
         (get-audio      (widget-get widget :get-audio))
         (audio          (funcall get-audio widget))
         (play (-partial 'memrise-play-audio audio)))
    ;; check what we should do with the audio
    (cond ((eq requires-audio 'before)
           ;; `before' - play it RIGHT NOW
           (funcall play)
           ;; and make a binding so the user can REPLAY it
           (local-set-key memrise-replay-audio-key
                          (memrise-make-interactive play)))
          ((eq requires-audio 'after)
           ;; `after' - play it AFTER the user submits her answer.
           ;; This means that we should put it as a submit hook.
           (memrise--widget-add-hook
            widget
            :on-submit-hook
            (memrise-make-argument-ignoring-lambda play))))))

(defun memrise-audio-multiple-choice-widget-play (widget)
  "Play the audio currently picked in the `WIDGET'."
  (memrise-play-audio (widget-value widget)))

(defun memrise-format-widget (format test-or-learnable)
  "Apply `FORMAT' to the given `TEST-OR-LEARNABLE'."
  (memrise-format-elements-with-faces format
                                      (memrise-session-format test-or-learnable)
                                      memrise-session-faces))

(defun memrise-widget-choices (test number)
  "For the given `TEST' pick a `NUMBER' of translation choices for a quiz."
  (memrise-construct-choices (car (oref test correct))
                             (oref test choices)
                             number))

(defun memrise-construct-choices
    (correct incorrect number &optional minimal)
  "Construct a randomized list of choices.

`CORRECT' - correct choice(s), guaranteed to be in the result list.
`INCORRECT' - list of incorrect choices (at least of `NUMBER' - 1 size)
`NUMBER' - number of choices in the result list.
`MINIMAL' - minimal number of incorrect choices."
  (unless minimal
    (setq minimal 0))
  (let* ((correct (if (listp correct) correct (list correct)))
         (correct-size (length correct))
         (incorrect-size (max (- number correct-size) minimal))
         ;; we want to include only non-repeating options that are not correct
         (incorrect-efficient (-distinct (-difference incorrect correct)))
         (filtered-incorrect
          (memrise-random-sublist incorrect-efficient incorrect-size)))
    (memrise-shuffle-list (append correct filtered-incorrect))))

(defun memrise-session-format (test-or-learnable)
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
      (source . ,(oref session source))
      (target . ,(oref session target)))))

(defun memrise-session-itemize-choices (choices)
  "Turn `CHOICES' into items."
  (mapcar (lambda (x) `(item :value ,x)) choices))

(defun memrise-create-pick-buttons (picks widget &optional map deactivate extra-args)
  "Create buttons representing `PICKS' as children of `WIDGET'.

Assign the corresponding bindings to the given key `MAP'.
If `DEACTIVATE' is non-nil deactivate all these buttons.
`EXTRA-ARGS' would be forwarded to the constructor of each of the created buttons."
  (let* ((buttons (mapcar (lambda (x)
                            (apply 'widget-create
                                   'memrise-pick-button
                                   :button-suffix ""
                                   :parent widget
                                   (-snoc extra-args
                                          (format "%s" x))))
                          picks)))
    (memrise-assign-buttons-keybindings buttons
                                        memrise-radio-keys
                                        map)
    (when deactivate
      (memrise-deactivate-widgets buttons))
    buttons))

(defun memrise-assign-buttons-keybindings (buttons bindings &optional keymap)
  "Change radion `BUTTONS' shapes, assign `BINDINGS' (for the given `KEYMAP').

New shape would be of a form '[key] (*button*)'.
'[key]'s would be colored rainbow colors provided by 'rainbow-delimiters'.

Optional `KEYMAP' parameter is defaulted to current keymap."
  (unless keymap (setq keymap (current-local-map)))
  (mapc (lambda (args) (apply 'memrise-assign-button-keybinding keymap args))
        (-zip buttons bindings (number-sequence 1 (length buttons)))))

(defun memrise-assign-button-keybinding (keymap button keybinding index)
  "Change radio `BUTTON' shape and `KEYBINDING' for the given `KEYMAP'.

Color it with a rainbow color `INDEX'."
  (widget-put button
              :button-prefix
              (propertize (format "[%s] "
                                  (key-description keybinding))
                          'face
                          (rainbow-delimiters-default-pick-face index t nil)))
  ;;to show the new button shape, we need to redraw it
  (memrise-redraw-widget button)
  (define-key keymap keybinding
    (lambda () (interactive) (widget-apply-action button))))

(defun memrise-assign-labels (labels items)
  "Show `LABELS' instead of `ITEMS'' values."
  (mapc (lambda (pair) (memrise-assign-label (car pair) (cdr pair)))
        (-zip labels items)))

(defun memrise-assign-label (label item)
  "Show `LABEL' instead of `ITEM''s value."
  (widget-put item :format (format "%S\n" label))
  (memrise-redraw-widget item))

(defun memrise-redraw-widget (widget)
  "Redraw `WIDGET' (to update it)."
  (widget-value-set widget (widget-value widget)))

(provide 'memrise-widget)
;;; memrise-widget.el ends here
