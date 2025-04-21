(in-package :cl-user)

(defpackage :com.charltonaustin.binary-data
  (:use :common-lisp :com.charltonaustin.macro-utilities)
  (:export :define-binary-class
           :define-tagged-binary-class
           :define-binary-type
           :read-value
           :write-value
           :*in-progress-objects*
           :parent-of-type
           :current-binary-object
           :+null+))

