;;; memrise-utils.el --- Various useful utils  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dash-functional)
(require 'cl-lib)

(defun memrise-make-argument-ignoring-lambda (func)
  "Return a version of the given `FUNC' that can take any number of arguments."
  (let ((arg func))
    (lambda (&rest _) (funcall arg))))

(defun memrise-make-interactive (fun &rest args)
  "Return interactive version of the given `FUN'.

One can also provide `ARGS' for the `FUN'."
  (lambda () (interactive) (apply fun args)))

(defun memrise-make-widget-callback (fun)
  "Return a version of the given `FUN' applicable for being a widget callback."
  (lambda (widget &rest ignored) (funcall fun widget)))

(defun memrise-random-element (list)
  "Pick a random element from `LIST'."
  (car (memrise-random-sublist list 1)))

(defun memrise-random-sublist (list number)
  "For the given `LIST' return `NUMBER' of random elements."
  (-take number (memrise-shuffle-list list)))

(defun memrise-shuffle-list (list)
  "Shuffle the given `LIST'."
  (sort (cl-copy-list list) (lambda (a b) (eq (random 2) 1))))

(defun memrise-vector-to-list (vector)
  "Convert `VECTOR' into a list."
  (append vector nil))

(defun memrise--integer-for-id (symbol)
  "Convert `SYMBOL' representing an ID into an integer."
  (string-to-number (symbol-name symbol)))

(defun memrise--id-for-integer (integer)
  "Convert `INTEGER' representing an ID into a symbol."
  (intern (number-to-string integer)))

(defun memrise--to-buffer (buffer)
  "Copy contents of the current buffer to `BUFFER'."
  (let ((oldbuf (current-buffer)))
    (with-current-buffer (get-buffer-create buffer)
      (barf-if-buffer-read-only)
      (erase-buffer)
      (save-excursion
        (insert-buffer-substring oldbuf)))))

(defun memrise-icase (value &rest args)
  "Match `VALUE' against the cases provided in `ARGS'.

`ARGS' should be in one of the following forms:
  `((`START' . `END') `RETURN-VALUE'), where `START' and `END' define a
closed interval where the `VALUE' should fall so `RETURN-VALUE' is returned.
  `(`ELEMENT' `RETURN-VALUE') corresponds to the case, in which `RETURN-VALUE'
is returned if `VALUE' equals to numerical `ELEMENT' (a range with only one
point).
  `(nil `RETURN-VALUE') returns `RETURN-VALUE' whenever `VALUE' is nil."
  (let* ((head (car args))
         (range (car head))
         (result (cadr head)))
    (cond
     ((not head) nil) ;; the list of args is over
     ;; if value \in range -> return corresponding result
     ((memrise-icase-in-range-p value range) result)
     ;; try other arguments
     (t (apply 'memrise-icase value (cdr args))))))

(defun memrise-icase-in-range-p (value range)
  "Check that `VALUE' is in `RANGE'.

`RANGE' can be a cons (`START' . `END'), single `ELEMENT' or nil."
  (cond
   ((-cons-pair? range) (and value ;; value can be `nil'
                             (>= value (car range))
                             (<= value (cdr range))))
   ;; if range is not actually a range, simply compare values
   (t (eq value range))))

(provide 'memrise-utils)
;;; memrise-utils.el ends here
