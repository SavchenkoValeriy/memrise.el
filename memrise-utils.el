;;; memrise-utils.el --- Various useful utils  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dash-functional)
(require 'cl-lib)

(defun memrise/make-argument-ignoring-lambda (func)
  "Return a version of the given `FUNC' that can take any number of arguments."
  (let ((arg func))
    (lambda (&rest _) (funcall arg))))

(defun memrise/make-interactive (fun &rest args)
  "Return interactive version of the given `FUN'.

One can also provide `ARGS' for the `FUN'."
  (lambda () (interactive) (apply fun args)))

(defun memrise/make-widget-callback (fun)
  "Return a version of the given `FUN' applicable for being a widget callback."
  (lambda (widget &rest ignored) (funcall fun widget)))

(defun memrise/random-element (list)
  "Pick a random element from `LIST'."
  (car (memrise/random-sublist list 1)))

(defun memrise/random-sublist (list number)
  "For the given `LIST' return `NUMBER' of random elements."
  (-take number (memrise/shuffle-list list)))

(defun memrise/shuffle-list (list)
  "Shuffle the given `LIST'."
  (sort (cl-copy-list list) (lambda (a b) (eq (random 2) 1))))

(defun memrise/vector-to-list (vector)
  "Convert `VECTOR' into a list."
  (append vector nil))

(defun memrise/integer-for-id (symbol)
  "Convert `SYMBOL' representing an ID into an integer."
  (string-to-number (symbol-name symbol)))

(defun memrise/id-for-integer (integer)
  "Convert `INTEGER' representing an ID into a symbol."
  (intern (number-to-string integer)))

(provide 'memrise-utils)
;;; memrise-utils.el ends here
