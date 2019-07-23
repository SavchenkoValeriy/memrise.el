;;; memrise-media.el --- Utilities to download media -*- lexical-binding: t; -*-

(require 'memrise-utils)

(defun memrise/process-media (folder vector-or-list)
  (let ((result (memrise/download-media
                 folder
                 (if (vectorp vector-or-list)
                     (memrise/vector-to-list vector-or-list)
                   vector-or-list))))
    result))

(defun memrise/download-media (folder urls)
  (mapcar (-partial #'memrise/download folder) urls))

(defun memrise/download (folder url)
  (let* ((file-dir (file-name-as-directory folder))
         (file (concat
                file-dir
                (memrise/hash url)
                "."
                (memrise/get-file-extension url))))
    (memrise/download-internal url file)))

(defun memrise/hash (url)
  (md5 url))

(defun memrise/get-file-extension (url)
  (file-name-extension url))

(defun memrise/download-internal (what where)
  "Download file from location `WHAT' and puts it by location `WHERE'"
  (let* ((where (concat
                 (file-name-as-directory
                  memrise/material-storage-directory)
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
