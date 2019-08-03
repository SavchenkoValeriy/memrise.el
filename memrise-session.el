;;; memrise-session.el --- Memrise session, from parsing to execution  -*- lexical-binding: t; -*-

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

;; This module contains all high-level functions related to Memrise
;; learning sessions.  It defines which tests are chosen, the number of
;; choices and so on.

;;; Code:

(require 'memrise-request)
(require 'memrise-widget)
(require 'memrise-session-objects)
(require 'memrise-session-parser)
(require 'memrise-utils)
(require 'dash)
(require 's)

;; TODO: check if we still need it (or ever needed it)
(setq memrise-session-mode-map
      (copy-keymap widget-keymap))

(define-derived-mode memrise-session-mode
  fundamental-mode "Memrise-session"
  "Mode for holding Memrise learning sessions")

(defconst memrise-supported-tests '("multiple_choice"
                                    "reversed_multiple_choice"
                                    "audio_multiple_choice"
                                    "typing"
                                    "tapping")
  "List of supported Memrise tests.")

(defcustom memrise-session-length
  nil
  "Number of tasks in Memrise session (nil for going with the default)."
  :type '(choice (const nil) integer)
  :group 'memrise)

(add-hook 'memrise-session-mode-hook 'memrise-turn-off-completions)

(defun memrise-turn-off-completions ()
  "Turn off completion minor modes during session.

Completion doesn't really help a learning process."
  (when (fboundp 'company-mode)
    (company-mode -1))
  (when (fboundp 'auto-complete-mode)
    (auto-complete-mode -1)))

(defun memrise-session-buffer ()
  "Get buffer for the session (it should always be no more than one)."
  (get-buffer-create "*session*"))

(defun memrise-start-learn-session (course-id)
  "Start a new memrise learn session for `COURSE-ID'."
  (memrise-start-session course-id "learn"))

(defun memrise-start-review-session (course-id)
  "Start a new memrise review/water session for `COURSE-ID'."
  (memrise-start-session course-id "classic_review"))

(defun memrise-start-session (course-id type)
  "Start a new memrise session for `COURSE-ID'.

`TYPE' is the internal Memrise name for learning sessions.

Prefer using `memrise-start-learn-session' and
`memrise-start-review-session' instead."
  (let ((buffer (memrise-session-buffer))
        (json-array-type 'list))
    (with-current-buffer buffer
      (memrise-request-session
       course-id
       type
       'memrise-start-session-internal))))

(defun memrise-start-session-internal (json)
  "Start session for the given `JSON' from Memrise."
  (with-current-buffer (memrise-session-buffer)
    (let ((inhibit-read-only t))
      (kill-all-local-variables)
      (erase-buffer)
      (memrise-session-mode)
      ;; we want to access top-level session object whenever we want
      (make-local-variable 'session)
      ;; the same goes to the currently learned object
      (make-local-variable 'learnable)
      ;; convert `JSON' into memrise.el internal structures
      (setq session (memrise-parse-session json))
      (memrise-display-session)
      (switch-to-buffer (memrise-session-buffer)))))

(defun memrise-display-session ()
  "Display new session in the current buffer."
  (make-local-variable 'main-widget)
  (make-local-variable 'next-task)
  ;; TODO: use UI format strings to make this part customizable
  (widget-insert (oref session course-name))
  (widget-insert "\n")
  (widget-insert (or (oref session title)
                     "Review"))
  (widget-insert "\n\n")
  (memrise-display-tasks (memrise--get-session-tasks)))

(defun memrise--get-session-tasks ()
  "Return a list of session tasks."
  (let ((tasks (oref session tasks)))
    (if memrise-session-length
        (-take memrise-session-length tasks)
      tasks)))

(defun memrise-display-tasks (tasks)
  "Display `TASKS' one by one."
  (-let (((task . rest-tasks) tasks))
    (if (null task)
        ;; no tasks left, we did it!
        (memrise-end-session)
      ;; find learnable from the task
      (setq learnable (assoc-default (oref task learnable-id)
                                     (oref session learnables)))
      (setq next-task (-partial 'memrise-display-next-task-internal rest-tasks))
      (setq main-widget nil)
      (setq main-widget
            ;; task's template is either "template" or "presentation"
            (if (string= (oref task template)
                         "presentation")
                (memrise-presentation learnable)
              ;; "sentinel" stands for test
              (memrise-pick-and-display-test learnable
                                             (oref task learn-level))))
      (widget-setup))))

(defun memrise-end-session ()
  "End the current learning session."
  (message "Learning session is over. Congrats!")
  (kill-buffer (memrise-session-buffer)))

(defun memrise-pick-and-display-test (learnable level)
  "Pick a test for `LEARNABLE' according to the `LEVEL'."
  (let* ((all-tests (oref session tests))
         (tests-for-this-learnable (assoc-default
                                    (oref learnable id)
                                    all-tests))
         (number-of-choices (memrise-decide-number-of-choices level)))
    (memrise-display-test (memrise-pick-test tests-for-this-learnable level)
                          number-of-choices)))

(defconst memrise-minimal-number-of-choices 4)
(defconst memrise-average-number-of-choices 6)
(defconst memrise-maximal-number-of-choices 8)

(defun memrise-decide-number-of-choices (level)
  "Decide on the number of choices in test based on the `LEVEL'."
  (memrise-icase level
                 `((0 . 2) ,memrise-minimal-number-of-choices)
                 `((3 . 4) ,memrise-average-number-of-choices)
                 `((5 . 6) ,memrise-maximal-number-of-choices)
                 ;; words for review have `level' == `nil'
                 `(nil     ,memrise-maximal-number-of-choices)))

(defun memrise-pick-test (tests level)
  "Pick one of the `TESTS' with respect to the given `LEVEL'."
  ;; filter out all not supported tests
  (let ((tests (seq-filter
                (lambda (test) (-contains-p memrise-supported-tests (car test)))
                tests)))
    (if (and level (<= level 1))
        ;; choose "multiple_choice" for the very early levels
        (assoc-default "multiple_choice" tests)
      (cdr (memrise-random-element tests)))))

(defun memrise-display-test (test number)
  "Construct and display `TEST' widget in the session buffer.

Type of widget is selected based on the kind of `TEST'.
`NUMBER' defines the number of options to select from whenever applicable."
  (pcase (oref test kind)
    ("multiple_choice"          (memrise-multiple-choice-widget test number))
    ("reversed_multiple_choice" (memrise-reversed-multiple-choice-widget
                                 test number))
    ("audio_multiple_choice"    (memrise-audio-multiple-choice-widget
                                 test memrise-minimal-number-of-choices))
    ("typing"                   (memrise-typing-widget test))
    ("tapping"                  (memrise-tapping-widget test))))

(defun memrise-display-next-task (widget)
  "Display the next task from the `WIDGET'."
  (interactive)
  (funcall next-task widget))

(defun memrise-display-next-task-internal (tasks widget)
  "Display `TASKS' instead of the `WIDGET'."
  (if widget
      ;; we don't need it anymore
      (widget-delete widget))
  ;; clean up after `WIDGET' - reset all the bindings
  (memrise-reset-session-bindings)
  (memrise-display-tasks tasks))

(defun memrise-reset-session-bindings ()
  "Restore original session bindings."
  (setq memrise-session-mode-map
        (copy-keymap widget-keymap))
  (use-local-map memrise-session-mode-map))

(provide 'memrise-session)
;;; memrise-session.el ends here
