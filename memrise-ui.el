;;; memrise-ui.el --- memrise.el UI functions and utilities -*- lexical-binding: t; -*-

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

;; This module contains faces and functions defining the way different
;; text information is depicted on a dashboard and during a learning session.

;;; Code:

(require 's)

(defface memrise-dashboard-name
  '((t :inherit font-lock-keyword-face))
  "Face for the course name to show on a dashboard."
  :group 'memrise-faces)

(defface memrise-dashboard-learned
  '((t :foreground "#2e8b57"))
  "Face for the number of things leraned to show on a dashboard."
  :group 'memrise-faces)

(defface memrise-dashboard-all
  '((t :inherit memrise-dashboard-learned))
  "Face for the overall number of things in the course to show on a dashboard."
  :group 'memrise-faces)

(defface memrise-dashboard-review
  '((t :foreground "#1e90ff"))
  "Face for the number of things to review/water to show on a dashboard."
  :group 'memrise-faces)

(defface memrise-dashboard-difficult
  '((t :foreground "#Ff8c00"))
  "Face for the number of things marked as 'difficult' to show on a dashboard."
  :group 'memrise-faces)

(defun memrise--get-default-height ()
  "Return a default height of text."
  (let ((result (face-attribute 'default :height)))
    ;; default height is not specified (probably in tests)
    (if (or (eq result 'unspecified)
            (<= result 1))
        100
      result)))

(defface memrise-dashboard-description
  `((t :inherit font-lock-comment-face
       :height ,(floor (* 0.7 (memrise--get-default-height)))))
  "Face for course description to show on a dashboard."
  :group 'memrise-faces)

(defface memrise-session-thing
  `((t :height ,(floor (* 1.5 (memrise--get-default-height)))))
  "Face for thing to show during sessions."
  :group 'memrise-faces)

(defface memrise-session-keyword
  '((t :weight bold))
  "Face for keywords (like language names) to show during sessions."
  :group 'memrise-faces)

(defface memrise-session-literal-translation
  '((t :inherit memrise-dashboard-description))
  "Face for literal translations to show during sessions."
  :group 'memrise-faces)

(defface memrise-session-keybinding
  '((t :weight bold))
  "Face for keybinding hints."
  :group 'memrise-faces)

(defvar memrise-dashboard-faces
  '((name . memrise-dashboard-name)
    (learned . memrise-dashboard-learned)
    (all . memrise-dashboard-all)
    (rev-icon . memrise-dashboard-review)
    (review . memrise-dashboard-review)
    (diff-icon . memrise-dashboard-difficult)
    (difficult . memrise-dashboard-difficult)
    (description . memrise-dashboard-description))
  "Mapping of dashboard elements to faces.")

(defvar memrise-session-faces
  '((text . memrise-session-thing)
    (translation . memrise-session-thing)
    (prompt . memrise-session-thing)
    (target . memrise-session-keyword)
    (source . memrise-session-keyword)
    (literal-translation . memrise-session-literal-translation))
  "Mapping of session elements to faces.")

(defun memrise-format-elements-with-faces (format elements faces)
  "Fill the `FORMAT' with `ELEMENTS' and propertize them with `FACES'."
  (let ((faced-elements (memrise-put-element-faces elements faces)))
    (s-format format 'aget faced-elements)))

(defun memrise-put-element-faces (elements faces)
  "Propertize `ELEMENTS' with their corresponding `FACES'."
  (mapcar (lambda (pair)
            (-let (((key . value) pair))
              (cons key (propertize
                         (format "%s" value)
                         'face (assoc-default key faces)))))
          elements))

(provide 'memrise-ui)
;;; memrise-ui.el ends here
