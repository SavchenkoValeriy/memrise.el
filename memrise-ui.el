;;; memrise-ui.el --- memrise.el UI functions and utilities
;;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 's)

(defun memrise/put-element-faces (elements faces)
  (lexical-let ((faces faces))
    (mapcar (lambda (pair)
              (let ((key (car pair))
                    (value (cdr pair)))
                `(,key . ,(propertize
                           (format "%s" value)
                           'face (assoc-default key faces)))))
            elements)))

(defun memrise/format-elements-with-faces (format elements faces)
  (let ((faced-elements (memrise/put-element-faces elements faces)))
    (s-format format 'aget faced-elements)))

(defface memrise-dashboard-name
  '((t :inherit font-lock-keyword-face))
  "Face for the course name to show on a dashboard"
  :group 'memrise/faces)

(defface memrise-dashboard-learned
  '((t :foreground "#2e8b57"))
  "Face for the number of things leraned to show on a dashboard"
  :group 'memrise/faces)

(defface memrise-dashboard-all
  '((t :inherit memrise-dashboard-learned))
  "Face for the overall number of things in the course to show on a dashboard"
  :group 'memrise/faces)

(defface memrise-dashboard-review
  '((t :foreground "#1e90ff"))
  "Face for the number of things to review/water to show on a dashboard"
  :group 'memrise/faces)

(defface memrise-dashboard-difficult
  '((t :foreground "#Ff8c00"))
  "Face for the number of things marked as 'difficult' to show on a dashboard"
  :group 'memrise/faces)

(defface memrise-dashboard-description
  `((t :inherit font-lock-comment-face
       :height ,(floor (* 0.7
                          (face-attribute font-lock-comment-face
                                          :height
                                          nil
                                          'default)))))
  "Face for course description to show on a dashboard"
  :group 'memrise/faces)

(defface memrise-session-thing
  `((t :height ,(floor (* 1.5
                          (face-attribute 'default
                                          :height)))))
  "Face for thing to show during sessions"
  :group 'memrise/faces)

(defface memrise-session-keyword
  '((t :weight bold))
  "Face for keywords (like language names) to show during sessions"
  :group 'memrise/faces)

(defface memrise-session-literal-translation
  '((t :inherit memrise-dashboard-description))
  "Face for literal translations to show during sessions"
  :group 'memrise/faces)

(defface memrise-session-keybinding
  '((t :weight bold))
  "Face for keybinding hints."
  :group 'memrise/faces)

(defvar memrise/dashboard-faces
  '((name . memrise-dashboard-name)
    (learned . memrise-dashboard-learned)
    (all . memrise-dashboard-all)
    (rev-icon . memrise-dashboard-review)
    (review . memrise-dashboard-review)
    (diff-icon . memrise-dashboard-difficult)
    (difficult . memrise-dashboard-difficult)
    (description . memrise-dashboard-description))
  "Mapping of dashboard elements to faces.")

(defvar memrise/session-faces
  '((text . memrise-session-thing)
    (translation . memrise-session-thing)
    (prompt . memrise-session-thing)
    (target . memrise-session-keyword)
    (source . memrise-session-keyword)
    (literal-translation . memrise-session-literal-translation))
  "Mapping of session elements to faces.")

(provide 'memrise-ui)
;;; memrise-ui.el ends here
