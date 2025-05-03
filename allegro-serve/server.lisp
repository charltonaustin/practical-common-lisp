(in-package :com.charltonaustin.url-function-system)

(defmacro standard-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:head (:title ,title))
     (:body
      (:h1 ,title)
      ,@body)))


(defun random-number (request entity)
  (net.aserve:with-http-response (request entity :content-type "text/html")
    (net.aserve:with-http-body (request entity)
      (format
       (net.aserve:request-reply-stream request)
         (standard-page (:title "Random") (:p "Random Number: " (random 1000)))))))


(defun show-query-params (request entity)
  (net.aserve:with-http-response (request entity :content-type "text/html")
    (net.aserve:with-http-body (request entity)
      (format
       (net.aserve:request-reply-stream request)
         (standard-page
             (:title "Query Parameters")
           (if (net.aserve:request-query request)
                (:table :border 1
                        (loop for (k . v) in (net.aserve:request-query request)
                              do (spinneret:with-html (:tr (:td k) (:td v)))))
                (:p "No query parameters.")))))))

; (publish :path "/random-number" :function 'random-number)
; (publish :path "/random-number" :remove t)

