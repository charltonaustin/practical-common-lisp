(in-package :cl-user)

(defpackage :com.charltonaustin.mp3-database
  (:use
   :common-lisp)
  (:export
   :*default-table-size*
   :*mp3-schema*
   :*mp3s*
   :column
   :column-value
   :delete-all-rows
   :delete-rows
   :do-rows
   :extract-schema
   :in
   :insert-row
   :load-database
   :make-column
   :make-schema
   :map-rows
   :matching
   :not-nullable
   :nth-row
   :random-selection
   :schema
   :select
   :shuffle-table
   :sort-rows
   :table
   :table-size
   :with-column-values))
