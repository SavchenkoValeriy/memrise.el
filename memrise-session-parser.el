;;; memrise-session-parser.el --- A set of functions to parse session JSON -*- lexical-binding: t; -*-

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

;; Memrise has a very complicated format of data JSON received for a session.
;; Even with `jeison' it still requires a bit of manual work.

;;; Code:

(require 'memrise-session-objects)
(require 'memrise-media)

(defun memrise/parse-session (json)
  "Parse `JSON' and create a `memrise-session' object."
  (jeison-read memrise-session json))

;; ===============================================================
;; Learnable parsers
;; ===============================================================

(defun memrise/parse-session-learnables (json)
  "Parse a alist of learnables objects from `JSON'.

Result alist has learnable ID as a key and `memrise-session-learnable'
as value."
  (mapcar 'memrise/parse-session-learnable json))

(defun memrise/parse-session-learnable (json)
  "Parse `memrise-session-learnable' from `JSON'."
  (let* ((result (jeison-read memrise-session-learnable json)))
    (cons (oref result id) result)))

(defun memrise/parse-session-learnable-audio (json)
  "Find a URL for the audio in `JSON' and download it.

Return path to the downloaded file."
  (memrise/process-media
   "audio"
   (memrise/parse-column-value json "Audio")))

(defun memrise/parse-session-learnable-literal-translation (json)
  "Find and return literal translation from `JSON'."
  (memrise/parse-column-value json "Literal translation"))

(defun memrise/parse-column-value (json label)
  "Find `LABEL' column in `JSON' and return its value."
  (let ((column (find-if (lambda (x)
                           (string= (assoc-default 'label x)
                                    label))
                         (assoc-default 'columns json))))
    (assoc-default 'value column)))

;; ===============================================================
;; Test parsers
;; ===============================================================

(defun memrise/parse-session-tests (json)
  "Parse `JSON' and produce an alist of Memrise tests.

Result alist has learnable ID as a key and a list of tests as value."
  (mapcar 'memrise/parse-session-learnable-tests json))

(defun memrise/parse-session-learnable-tests (json)
  "Parse `JSON' and produce a pair: learnable ID and a list of tests."
  (let* ((id (string-to-number (symbol-name (car json))))
         (body (cdr (cdr json)))
         (tests (mapcar #'memrise/parse-session-test body)))
    (cons id tests)))

(defun memrise/parse-session-test (json)
  "Parse `JSON' and return a pair: kind and a `memrise-session-test' object."
  (let* ((result (jeison-read memrise-session-test json))
         (kind (oref result kind)))
    ;; if test is an audio test we should download all audios
    (when (s-contains? "audio" kind)
      ;; for the answer
      (oset result answer (memrise/download-normal-pace-audio
                           (oref result answer)))
      ;; for the correct choices
      (oset result correct (memrise/process-media "audio"
                                                  (oref result correct)))
      ;; and for other choices
      (oset result choices (memrise/process-media "audio"
                                                  (oref result choices))))
    (cons kind result)))

(defun memrise/download-normal-pace-audio (json)
  "Parse `JSON' find a normal pace audio and download it.

Return a path to the downloaded file."
  (let ((audio (jeison-read t json '(0 normal))))
    (when audio
      (memrise/process-media "audio" audio))))

(provide 'memrise-session-parser)
;;; memrise-session-parser.el ends here
