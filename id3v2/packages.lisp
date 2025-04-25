(defpackage :com.charltonaustin.id3v2
  (:use :common-lisp
   :com.charltonaustin.binary-data
        :com.charltonaustin.pathnames)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :album
   :composer
   :genre
   :encoding-program
   :artist
   :part-of-set
   :track
   :song
   :year
   :size
   :translated-genre))
