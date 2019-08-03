;;; memrise-dashboard.el --- Memrise dashboard visualization and logic -*- lexical-binding: t -*-

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

;; This module contains all the functionality and logic related to the
;; Memrise dashboard.

;;; Code:

(require 'memrise-request)
(require 'memrise-session)
(require 'memrise-ui)
(require 'all-the-icons)
(require 'jeison)

(defvar memrise-mode-map
  (let ((map (make-keymap)))
    (define-key map "n" 'memrise-dashboard-course-forward)
    (define-key map "p" 'memrise-dashboard-course-backward)
    (define-key map "l" 'memrise-dashboard-course-learn)
    (define-key map "r" 'memrise-dashboard-course-review)
    (define-key map "g" 'memrise-dashboard)
    map)
  "Keymap for a memrise dashboard buffer.")

(define-derived-mode memrise-mode special-mode "Memrise"
  "Mode for holding Memrise dashboards")

(defun memrise-dashboard-buffer ()
  "Get buffer for the dashboard (it should always be no more than one)."
  (get-buffer-create "*dashboard*"))

;;;###autoload
(defun memrise-dashboard ()
  "Shpw your Memrise dashboard in the *dashboard* buffer.

This function is the main entry point to memrise.el.  If the user is not
logged in it will ask for credentials.

The content of the dashboard includes all of the courses *attended* by the user.
Each course includes a number of words learned, a number of words for review, and
a number of difficult words.  Additionally it includes a short description of the
course.

The user can jump between all of her courses and start either learning or review
sessions."
  (interactive)
  (let ((buffer (memrise-dashboard-buffer)))
    (memrise-request-dashboard
     (lambda (data)
       (with-current-buffer buffer
         ;; it is a read-only buffer
         (let ((inhibit-read-only t))
           ;; typical new buffer bolierplate code
           (kill-all-local-variables)
           (erase-buffer)
           (memrise-mode)
           ;; make a buffer-local variable containing all the courses
           (make-local-variable 'courses)
           ;; parse the courses from the JSON data received from Memrise
           (setq courses (memrise-parse-courses data))
           ;; display it in the buffer
           (memrise-display-courses courses buffer)
           (switch-to-buffer buffer)))))))

(jeison-defclass memrise-course nil
  ((name :documentation "The name of the course (i.e. Swedish 3)")
   (description
    :documentation "The course's description (a couple of sentences)")
   (number-of-things :path num_things
                     :documentation "Overall number of things to learn")
   (learned :documentation "A number of things already learned")
   (to-review :path review :documentation "A number of things to review/water")
   (difficult :documentation "A number of difficult words")
   (id :documentation "Memrise ID of the course")
   (start :documentation "Dashboard position where the course widget starts")
   (end :documentation "Dashboard position where the course widget ends"))
  :documentation "A class containing all the information on the course.")

(defun memrise-courses ()
  "Return a list of studied courses."
  (with-current-buffer (memrise-dashboard-buffer)
    courses))

(defun memrise-current-course ()
  "Get the course from under the point."
  (get-text-property (point-marker) 'course))

(defun memrise-next-course (course courses)
  "Return the next (with respect to the `COURSE') from the `COURSES'."
  (let ((next-courses (cdr (member course courses))))
    (if next-courses
        (car next-courses)
      ;; if it was the last element - return the very first course
      (car courses))))

(defun memrise-dashboard-course-learn (course)
  "Start 'learning new words' session for the given `COURSE'."
  (interactive (list (memrise-current-course)))
  (let ((all (oref course number-of-things))
        (learned (oref course learned)))
    (if (eq (- all learned) 0)
        (message "Nothing's left to learn in this course. Did you mean 'review'?")
      (memrise-start-learn-session (oref course id)))))

(defun memrise-dashboard-course-review (course)
  "Start review/water session for the given `COURSE'."
  (interactive (list (memrise-current-course)))
  (let ((to-review (oref course to-review)))
    (if (eq to-review 0)
        (message "Nothing to review in this course. Did you mean 'learn'?")
      (memrise-start-review-session (oref course id)))))

(defun memrise-dashboard-course-forward (course)
  "Move cursor to the next (with respect to the `COURSE') on the dashboard."
  (interactive (list (memrise-current-course)))
  (let ((dest (memrise-next-course course courses)))
    (goto-char (oref dest start))))

(defun memrise-dashboard-course-backward (course)
  "Move cursor to a previous (with respect to the `COURSE') on the dashboard."
  (interactive (list (memrise-current-course)))
  (let ((dest (memrise-next-course course (reverse courses))))
    (goto-char (oref dest start))))

(defun memrise-display-courses (courses buffer)
  "Display `COURSES' in the `BUFFER'."
  (with-current-buffer buffer
    (mapc 'memrise-display-course courses)))

(defcustom memrise-dashboard-format
  "${name}    ${learned}/${all}  ${rev-icon}${review} ${diff-icon}${difficult}\n${description}"
  "Format string to display courses on Memrise dashboard.

{name}        - course name ('Swedish 3')
{learned}     - number of things that the user learned in the course
{all}         - overall number of things in the course
{rev-icon}    - icon to use for words that require review/water
{review}      - number of things to review/water in the course
{diff-icon}   - icon to use for difficult words
{difficult}   - number of words marked as 'difficult' in the course
{description} - course description"
  :group 'memrise)

(defcustom memrise-review-icon
  (all-the-icons-faicon "tint" :v-adjust 0.0)
  "Icon to use for words that require review/water."
  :type 'string
  :group 'memrise)

(defcustom memrise-difficult-icon
  (all-the-icons-faicon "bolt" :v-adjust 0.0)
  "Icon to use for words marked as 'difficult'."
  :type 'string
  :group 'memrise)

(defun memrise-display-course (course)
  "Display `COURSE' in the current buffer."
  ;; TODO: it is a good place to use EIEIO `with-slots'
  (let ((format-objects `((name . ,(oref course name))
                          (learned . ,(oref course learned))
                          (all . ,(oref course number-of-things))
                          (rev-icon . ,memrise-review-icon)
                          (review . ,(oref course to-review))
                          (diff-icon . ,memrise-difficult-icon)
                          (difficult . ,(oref course difficult))
                          (description . ,(oref course description)))))
    (memrise-insert-course
     (memrise-format-elements-with-faces memrise-dashboard-format
                                         format-objects
                                         memrise-dashboard-faces)
     course)
    (insert "\n\n")))

(defun memrise-insert-course (text course)
  "Insert `TEXT' and mark it as `COURSE'."
  (oset course start (point-marker))
  (insert (propertize text 'course course))
  (oset course end (point-marker)))

(defun memrise-parse-courses (courses-list)
  "Return a list of `memrise-course' corresponding to the `COURSES-LIST'."
  (jeison-read '(list-of memrise-course) courses-list 'courses))

(provide 'memrise-dashboard)
;;; memrise-dashboard.el ends here
