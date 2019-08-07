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
(require 'emms)

(defvar memrise-video-quality 'medium
  "Memrise video quality, one of '(low medium high).")

(defcustom memrise-media-storage-directory
  (concat (file-name-as-directory user-emacs-directory) "memrise")
  "Directory to store data related to memrise.el."
  :type 'directory
  :group 'memrise)

(defun memrise-process-media (folder list-or-element)
  "Download `LIST-OR-ELEMENT' into the `FOLDER'.

`LIST-OR-ELEMENT' is one (or a list of) media file URL.
Return a path (or a list of paths depending on the type of `LIST-OR-ELEMENT')
to the downloaded media file."
  (if (listp list-or-element)
      (memrise-download-media folder list-or-element)
    (memrise-download folder list-or-element)))

(defun memrise-download-media (folder urls)
  "Download `URLS' into the `FOLDER'."
  (mapcar (-partial #'memrise-download folder) urls))

(defun memrise-download (folder url)
  "Download `URL' into the `FOLDER'."
  (let* ((file-dir (file-name-as-directory folder))
         (file (concat
                file-dir
                (memrise-hash url)
                "."
                (file-name-extension url))))
    (memrise-download-internal url file)))

(defun memrise-hash (url)
  "Calculate memrise.el hash for the given `URL'."
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

(defun memrise-play-audio (audio)
  "Play the given `AUDIO' file."
  ;; 1. turning off `emms-info-asynchronously' seems like the only way
  ;;    to get rid of the "EMMS: All track information loaded." message
  ;; 2. EMMS tries to play next song and prints "No next track in playlist".
  ;;    Setting 'emms-single-track' or using 'emms-toggle-single-track'
  ;;    seems like a good solution, but for some reason doesn't work.
  ;;    As the result, we temporarily refuse EMMS from even trying to play
  ;;    the next track.
  ;;    The worst part is that is still prints "No next track in playlist"
  ;;    sometimes.
  (let ((emms-info-asynchronously nil)
        (emms-player-next-function #'ignore))
    (when audio
      (emms-play-file audio))))

(defun memrise-call-after-all-audio-is-finished (func &rest args)
  "Call `FUNC' with `ARGS' after all audio is over."
  (let ((callback (-partial #'apply func args)))
    (defun memrise-audio-hook ()
      (remove-hook 'emms-player-finished-hook
                   'memrise-audio-hook)
      ;; running callback synchroneously mess up with emms
      (run-at-time "0.0 sec" nil callback))
    (if (not emms-player-playing-p)
        ;; nothing is playing - ready to call
        (funcall callback)
      ;; use emms callbacks to call it after player has finished
      (add-hook 'emms-player-finished-hook
                'memrise-audio-hook))))

(provide 'memrise-media)
;;; memrise-media.el ends here
