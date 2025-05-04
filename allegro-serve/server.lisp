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
       (let* ((limit-string (or (net.aserve:request-query-value "limit" request) ""))
              (limit (or (parse-integer limit-string :junk-allowed t) 1000)))
         (standard-page (:title "Random") (:p "Random Number: " (random limit))))))))


;; (net.aserve:publish :path "/random-number" :function 'random-number)
;; (net.aserve:publish :path "/random-number" :remove t)


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

;; (net.aserve:publish :path "/show-query-params" :function 'random-number)
;; (net.aserve:publish :path "/show-query-params" :remove t)

(defun simple-form (request entity)
  (net.aserve:with-http-response (request entity :content-type "text/html")
    (net.aserve:with-http-body (request entity)
      (format
       (net.aserve:request-reply-stream request)
       (standard-page (:title "Simple Form")
         (:form :method "POST" :action "/show-query-params"
                (:table
                 (:tr (:td "Foo")
                      (:td (:input :name "foo" :size 20)))
                 (:tr (:td "Password")
                      (:td (:input :name "password" :type "password" :size 20))))
                (:p (:input :name "submit" :type "submit" :value "Okay")
                    (:input ::type "reset" :value "Reset"))))))))


;; (net.aserve:publish :path "/simple-form" :function 'simple-form)
;; (net.aserve:publish :path "/simple-form" :remove t)


(defun show-cookies (request entity)
  (net.aserve:with-http-response (request entity :content-type "text/html")
    (net.aserve:with-http-body (request entity)
      (format
       (net.aserve:request-reply-stream request)
       (standard-page (:title "Cookies")
         (if (null (net.aserve:get-cookie-values request))
             (:p "No cookies.")
             (:table
              (loop for (key . value) in (net.aserve:get-cookie-values request)
                    do (spinneret:with-html (:tr (:td key) (:td value)))))))))))

;; (net.aserve:publish :path "/show-cookies" :function 'show-cookies)
;; (net.aserve:publish :path "/show-cookies" :remove t)

(defun set-cookie (request entity)
  (net.aserve:with-http-response (request entity :content-type "text/html")
    (net.aserve:set-cookie-header request :name "MyCookie" :value "A cookie value")
    (net.aserve:with-http-body (request entity)
      (format
       (net.aserve:request-reply-stream request)
       (standard-page (:title "Set Cookie")
           (:p "Cookie set.")
           (:p (:a :href "/show-cookies" "Look at cookie jar.")))))))

;; (net.aserve:publish :path "/set-cookie" :function 'set-cookie)
;; (net.aserve:publish :path "/set-cookie" :remove t)
