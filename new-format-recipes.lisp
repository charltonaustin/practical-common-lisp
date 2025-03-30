(let ((list '(1 2 3 4)))
  (loop for cons on list
        do (format nil "~a" (car cons))
        when (cdr cons) do (format nil ", ")))

(let ((list '(1 2 3 4)))
  (format nil "~{~a~^, ~}" list))

