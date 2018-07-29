;;; memrise-session.el --- Memrise session, from parsing to execution  -*- lexical-binding: t; -*-

(require 'memrise-request)
(require 'memrise-widget)
(require 'dash)
(require 'emms)
(require 's)

(setq memrise-session-mode-map
      (copy-keymap widget-keymap))

(define-derived-mode memrise-session-mode fundamental-mode "Memrise-session")

(add-hook 'memrise-session-mode-hook 'memrise/turn-off-completions)

(defun memrise/turn-off-completions ()
  "Turn off completion minor modes during session.
Completion doesn't really help a learning process."
  (company-mode -1)
  (auto-complete-mode -1))

(defun memrise/session-buffer ()
  (get-buffer-create "*session*"))

(defun memrise/start-learn-session (course-id)
  "Starts a new memrise learn session"
  (memrise/start-session course-id "learn"))

(defun memrise/start-review-session (course-id)
  "Starts a new memrise review/water session"
  (memrise/start-session course-id "classic_review"))

(defun memrise/start-session (course-id type)
  "Starts a new memrise session"
  (lexical-let ((buffer (memrise/session-buffer)))
    (with-current-buffer buffer
;;      (memrise/request-session
;;       course-id
;;       type
;;       'memrise/start-session-internal)
      (memrise/start-session-internal session-test)
      (switch-to-buffer buffer))))

(defun memrise/start-session-internal (json)
  (with-current-buffer (memrise/session-buffer)
    (let ((inhibit-read-only t))
      (kill-all-local-variables)
      (erase-buffer)
      (memrise-session-mode)
      (make-local-variable 'session)
      (setq session (memrise/parse-session json))
      (memrise/display-session)
      (switch-to-buffer (memrise/session-buffer)))))

(defun memrise/display-session ()
  (make-local-variable 'main-widget)
  (make-local-variable 'next-task)
  (widget-insert (memrise/session-course-name session))
  (widget-insert "\n")
  (widget-insert (or (memrise/session-title session)
                     "Review"))
  (widget-insert "\n\n")
  (memrise/display-tasks (memrise/session-tasks session)))

(defun memrise/display-tasks (tasks)
  (lexical-let* ((task (car tasks))
                 (learnable (assoc-default (memrise/session-task-learnable-id task)
                                           (memrise/session-learnables session))))
    (setq next-task (-partial 'memrise/display-next-task-internal tasks))
    (setq main-widget nil)
    (setq main-widget
          (if (string= (memrise/session-task-template task)
                       "presentation")
              (memrise/presentation learnable)
            (memrise/pick-and-display-test learnable
                                           (memrise/session-task-learn-level task))))
    (widget-setup)))

(defun memrise/pick-and-display-test (learnable level)
  (let ((tests (memrise/session-learnable-tests learnable))
        (number-of-choices (memrise/decide-number-of-choices level)))
    (memrise/display-test (memrise/pick-test tests level)
                          number-of-choices)))

(defvar memrise/minimal-number-of-choices 4)
(defvar memrise/average-number-of-choices 6)
(defvar memrise/maximal-number-of-choices 8)

(defun memrise/decide-number-of-choices (level)
  (memrise/icase level
    `((1 . 2) ,memrise/minimal-number-of-choices)
    `((3 . 4) ,memrise/average-number-of-choices)
    `((5 . 6) ,memrise/maximal-number-of-choices)
    ;; words for review have `level' == `nil'
    `(nil     ,memrise/maximal-number-of-choices)))

(defun memrise/pick-test (tests level)
  "According to the given `level' picks one of the `tests'"
  (if (eq level 1)
      (assoc-default "multiple_choice")
    (cdr (memrise/random-element tests))))

(defun memrise/display-test (test number)
  (pcase (memrise/session-test-kind test)
    ("multiple_choice"          (memrise/multiple-choice-widget test number))
    ("reversed_multiple_choice" (memrise/reversed-multiple-choice-widget test number))
    ("audio_multiple_choice"    (memrise/audio-multiple-choice-widget
                                 test memrise/minimal-number-of-choices))
    ("typing"                   (memrise/typing-widget test))
    ("tapping"                  (memrise/tapping-widget test))))

(defun memrise/icase (value &rest args)
  (let* ((head (car args))
         (range (car head))
         (result (cadr head)))
    (cond
     ((not head) nil) ;; the list of args is over
     ;; if value \in range -> return corresponding result
     ((memrise/icase-in-range-p value range) result)
     ;; try other arguments
     (t (apply 'memrise/icase value (cdr args))))))

(defun memrise/icase-in-range-p (value range)
  (cond
   ((-cons-pair? range) (and value ;; value can be `nil'
                             (>= value (car range))
                             (<= value (cdr range))))
   ;; if range is not actually a range, simply compare values
   (t (eq value range))))

(defun memrise/display-next-task (widget)
  (interactive)
  (funcall next-task widget))

(defun memrise/display-next-task-internal (tasks widget)
  (if widget
      (widget-delete widget))
  (memrise/reset-session-bindings)
  (memrise/display-tasks (cdr tasks)))

(defun memrise/reset-session-bindings ()
  (setq memrise-session-mode-map
        (copy-keymap widget-keymap))
  (use-local-map memrise-session-mode-map))

(defvar memrise/video-quality 'medium
  "Memrise video quality, one of '(low medium high)")

(defcustom memrise/material-storage-directory
  (concat (file-name-as-directory user-emacs-directory) "memrise")
  "Directory to store data related to request.el."
  :type 'directory
  :group 'memrise)

(defun memrise/test ()
  (interactive)
  (message "%S" (memrise/parse-session session-test)))

(provide 'memrise-session)
