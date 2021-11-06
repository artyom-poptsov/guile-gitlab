(define-module (gitlab client)
  #:use-module (srfi srfi-8)
  #:use-module (ice-9 iconv)
  #:use-module (oop goops)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (json)
  #:export (<client>
            client?
            client-server-uri
            client-get
            client-put
            client-post
            uri-parameters->string
            client-debug?))



(define-class <client> ()
  ;; <string>
  (token
   #:init-value   #f
   #:init-keyword #:token
   #:getter       client-token)

  ;; <uri>
  (server-uri
   #:init-value   #f
   #:init-keyword #:server
   #:getter       client-server-uri)

  ;; <boolean>
  (debug?
   #:init-value   #f
   #:init-keyword #:debug?
   #:getter       client-debug?))



(define-method (client? object)
  (is-a? object <client>))

(define-method (%display (client <client>) (port <port>))
  (format port "#<client ~a ~a>"
          (client-server-uri client)
          (number->string (object-address pipe) 16)))

(define-method (write (client <client>) (port <port>))
  (%display client port))

(define-method (write (client <client>))
  (next-method)
  (%display client (current-output-port)))

(define-method (display (client <client>))
  (next-method)
  (%display client (current-output-port)))



(define-method (uri-parameters->string (params <list>))
  (if (null? params)
      ""
      (string-join (map (lambda (p)
                          (string-append (object->string (car p)) "=" (cdr p)))
                        params)
                   "&")))

;; Build an URI based on the CLIENT parameters, a RESOURCE and a QUERY alist.
;; Returns the new URI.
(define-method (client-build-uri (client   <client>)
                                 (resource <string>)
                                 (query    <list>))
  (let ((server (client-server-uri client)))
    (build-uri (uri-scheme server)
               #:host   (uri-host server)
               #:port   (uri-port server)
               #:path   resource
               #:query  (uri-parameters->string query))))

(define* (client-get client
                     resource
                     #:key
                     (query '()))
  (let ((uri (client-build-uri client resource query)))
    (receive (response body)
        (http-request uri
                      #:headers `((Content-Type  . "application/json")
                                  (Private-Token . ,(client-token client))))
      (when (client-debug? client)
        (display response)
        (newline))
      (and (= (response-code response) 200)
           (json-string->scm (bytevector->string body "UTF-8"))))))

(define* (client-put client resource body
                      #:key
                      (query '()))
  (let ((uri       (client-build-uri client resource query))
        (json-body (scm->json-string body)))
    (receive (response response-body)
        (http-put uri
                  #:headers `((Content-Type  . "application/json")
                              (Private-Token . ,(client-token client)))
                  #:port    (open-socket-for-uri uri)
                  #:body    json-body)
      (when (client-debug? client)
        (display response)
        (newline))
      (json-string->scm (bytevector->string response-body "UTF-8")))))

(define* (client-post client resource body
                      #:key
                      (query '()))
  (let ((uri       (client-build-uri client resource query))
        (json-body (scm->json-string body)))
    (receive (response response-body)
        (http-post uri
                   #:headers `((Content-Type  . "application/json")
                               (Private-Token . ,(client-token client)))
                   #:port    (open-socket-for-uri uri)
                   #:body    json-body)
      (when (client-debug? client)
        (display response)
        (newline))
      (json-string->scm (bytevector->string response-body "UTF-8")))))

;;; client.scm ends here.
