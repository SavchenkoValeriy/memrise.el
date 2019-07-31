;;; memrise-media.el --- Utilities to download media -*- lexical-binding: t; -*-

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

;; This module defines all the functions containing memrise.el
;; interactions with media (audio, video and so on).

;;; Code:

(require 'memrise-utils)

(defvar memrise-video-quality 'medium
  "Memrise video quality, one of '(low medium high).")

(defcustom memrise-media-storage-directory
  (concat (file-name-as-directory user-emacs-directory) "memrise")
  "Directory to store data related to request.el."
  :type 'directory
  :group 'memrise)


(defun memrise-process-media (folder list-or-element)
  (if (listp list-or-element)
      (memrise-download-media folder list-or-element)
    (memrise-download folder list-or-element)))

(defun memrise-download-media (folder urls)
  (mapcar (-partial #'memrise-download folder) urls))

(defun memrise-download (folder url)
  (let* ((file-dir (file-name-as-directory folder))
         (file (concat
                file-dir
                (memrise-hash url)
                "."
                (file-name-extension url))))
    (memrise-download-internal url file)))

(defun memrise-hash (url)
  (md5 url))

(defun memrise-download-internal (what where)
  "Download file from location `WHAT' and puts it by location `WHERE'."
  (let* ((where (concat
                 (file-name-as-directory
                  memrise-media-storage-directory)
                 where))
         (dest-dir (file-name-directory where)))
    (if (file-exists-p where)
        ;; re-use previously downloaded one
        where
      (if (not (file-exists-p dest-dir))
          ;; recursively make directories for downloading
          (make-directory dest-dir t))
      (url-copy-file what where)
      where)))

(provide 'memrise-media)
;;; memrise-media.el ends here
