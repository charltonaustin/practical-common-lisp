(defpackage :com.charltonaustin.url-function-system (:use :asdf :cl :net.aserve :spinneret :com.charltonaustin.macro-utilities))
(in-package :com.charltonaustin.url-function-system)

(defsystem url-function
  :name "url-function"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "0.1"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "define-url-function macro for AllegroServe"
  :long-description ""
  :components
  ((:file "packages")
   (:file "html-infrastructure" :depends-on ("packages")))
  :depends-on (:html :macro-utilities :net.aserve :spinneret))

;; (ql:quickload :zaserve)
;; (ql:quickload :spinneret)
;; compile ./macro-utilities/packages.lisp
;; compile ./macro-utilities/macro-utilities.lisp
