;;; memrise-widget.el --- Various useful utils  -*- lexical-binding: t; -*-

(defun memrise/make-argument-ignoring-lambda (func)
  "Return a version of the given `func' that can take any number of arguments"
  (lexical-let ((arg func))
   (lambda (&rest _) (funcall arg))))

(defun memrise/make-interactive (fun &rest args)
  "Return interactive version of the given `fun'"
  (lambda () (interactive) (apply fun args)))

(defun memrise/make-widget-callback (fun)
  "Return a version of the given `fun' applicable for being a widget callback"
  (lambda (widget &rest ignored) (funcall fun widget)))

(defun memrise/random-element (list)
  "Pick a random element from `list'"
  (car (memrise/random-sublist list 1)))

(defun memrise/random-sublist (list number)
  "Return `number' of random elements from `list'"
  (-take number (memrise/shuffle-list list)))

(defun memrise/shuffle-list (list)
  "Shuffle the given `list'"
  (sort (copy-list list) (lambda (a b) (eq (random 2) 1))))

(provide 'memrise-utils)
