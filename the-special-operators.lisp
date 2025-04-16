(defun foo ()
    (format t "Entering foo~%")
    (block a
          (format t " Entering BLOCK~%")
              (bar #'(lambda () (return-from a)))
                  (format t " Leaving BLOCK~%"))
    (format t "Leaving foo~%"))

(defun bar (fn)
    (format t "  Entering bar~%")
    (baz fn)
    (format t "  Leaving bar~%"))

(defun baz (fn)
    (format t "   Entering baz~%")
    (funcall fn)
    (format t "   Leaving baz~%"))
