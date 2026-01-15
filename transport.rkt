#lang racket/base

(provide jsonrpc-stdio-server-start
         jsonrpc-stdio-send
         jsonrpc-connection-start
         jsonrpc-connection-send
         jsonrpc-connection-close)

(require rakka
         racket/match
         racket/port
         racket/string
         json
         "server.rkt")

;;; LSP-style stdio transport layer
;;; Messages use Content-Length header format:
;;; Content-Length: <length>\r\n
;;; \r\n
;;; <json-body>

;;; Read a single JSON-RPC message from input port
(define (read-jsonrpc-message in)
  (define headers (read-headers in))
  (cond
    [(eof-object? headers) eof]
    [else
     (define content-length (hash-ref headers 'content-length #f))
     (cond
       [(not content-length)
        (error 'read-jsonrpc-message "Missing Content-Length header")]
       [else
        (read-string content-length in)])]))

;;; Read headers until empty line
(define (read-headers in)
  (define headers (make-hash))
  (let loop ()
    (define line (read-line in 'return-linefeed))
    (cond
      [(eof-object? line) eof]
      [(string=? line "") headers]
      [else
       (define parts (string-split line ": " #:trim? #f))
       (when (>= (length parts) 2)
         (define key (string-downcase (car parts)))
         (define value (string-join (cdr parts) ": "))
         (cond
           [(string=? key "content-length")
            (hash-set! headers 'content-length (string->number value))]
           [(string=? key "content-type")
            (hash-set! headers 'content-type value)]))
       (loop)])))

;;; Write a JSON-RPC message to output port
(define (write-jsonrpc-message out json-obj)
  (define body (jsexpr->string json-obj))
  (define body-bytes (string->bytes/utf-8 body))
  (fprintf out "Content-Length: ~a\r\n\r\n" (bytes-length body-bytes))
  (display body out)
  (flush-output out))

;;; stdio server state
(struct stdio-server-state (impl user-state in out))

;;; Internal GenServer for stdio transport
(struct stdio-transport-server ()
  #:methods gen:server
  [(define (init self args)
     (match-define (list impl init-args in out) args)
     (define user-state (jsonrpc-init impl init-args))
     (ok (stdio-server-state impl user-state in out)))

   (define (handle-call self msg state from)
     (match-define (stdio-server-state impl user-state in out) state)
     (match msg
       ;; Read and process one message
       ['read-one
        (define json-str (read-jsonrpc-message in))
        (cond
          [(eof-object? json-str)
           (reply 'eof state)]
          [else
           (define parsed (parse-jsonrpc-message json-str))
           (cond
             [(jsonrpc-error? parsed)
              ;; Send error response
              (write-jsonrpc-message out (build-error-response #f parsed))
              (reply 'error state)]
             [else
              (match-define (list method params id) parsed)
              (cond
                [id
                 ;; Request - needs response
                 (define-values (response new-user-state)
                   (jsonrpc-handle-request impl method params user-state))
                 (write-jsonrpc-message out (build-full-response id response))
                 (reply 'ok (stdio-server-state impl new-user-state in out))]
                [else
                 ;; Notification - no response
                 (define new-user-state
                   (jsonrpc-handle-notify impl method params user-state))
                 (reply 'ok (stdio-server-state impl new-user-state in out))])])])]

       ;; Send a request/notification to the other side
       [(list 'send-request method params id)
        (define req (hasheq 'jsonrpc "2.0"
                            'method method
                            'params params
                            'id id))
        (write-jsonrpc-message out req)
        (reply 'ok state)]

       [(list 'send-notify method params)
        (define notif (hasheq 'jsonrpc "2.0"
                              'method method
                              'params params))
        (write-jsonrpc-message out notif)
        (reply 'ok state)]))

   (define (handle-cast self msg state)
     (noreply state))

   (define (handle-info self msg state)
     (noreply state))

   (define (terminate self reason state)
     (match-define (stdio-server-state impl user-state _in _out) state)
     (jsonrpc-terminate impl reason user-state))])

;;; Parse JSON-RPC message (same as in server.rkt but local)
(define (parse-jsonrpc-message json-str)
  (with-handlers ([exn:fail? (lambda (e)
                               (jsonrpc-error PARSE-ERROR "Parse error" (exn-message e)))])
    (define obj (string->jsexpr json-str))
    (cond
      [(not (hash? obj))
       (jsonrpc-error INVALID-REQUEST "Request must be an object" #f)]
      [(not (equal? (hash-ref obj 'jsonrpc #f) "2.0"))
       (jsonrpc-error INVALID-REQUEST "Missing or invalid jsonrpc version" #f)]
      [(not (hash-ref obj 'method #f))
       (jsonrpc-error INVALID-REQUEST "Missing method" #f)]
      [else
       (define method (hash-ref obj 'method))
       (define params (hash-ref obj 'params '()))
       (define id (hash-ref obj 'id #f))
       (list method params id)])))

(define (build-error-response id err)
  (hasheq 'jsonrpc "2.0"
          'error (hasheq 'code (jsonrpc-error-code err)
                         'message (jsonrpc-error-message err))
          'id id))

(define (build-full-response id response)
  (cond
    [(jsonrpc-ok? response)
     (hasheq 'jsonrpc "2.0"
             'result (jsonrpc-ok-result response)
             'id id)]
    [(jsonrpc-error? response)
     (define error-obj
       (if (jsonrpc-error-data response)
           (hasheq 'code (jsonrpc-error-code response)
                   'message (jsonrpc-error-message response)
                   'data (jsonrpc-error-data response))
           (hasheq 'code (jsonrpc-error-code response)
                   'message (jsonrpc-error-message response))))
     (hasheq 'jsonrpc "2.0"
             'error error-obj
             'id id)]))

;;; Public API

;; Start a stdio JSON-RPC server
;; impl: an instance implementing gen:jsonrpc-server
;; args: initialization arguments
;; in/out: input/output ports (default to stdin/stdout)
(define (jsonrpc-stdio-server-start impl
                                    #:args [args #f]
                                    #:in [in (current-input-port)]
                                    #:out [out (current-output-port)])
  (gen-server-start (stdio-transport-server) (list impl args in out)))

;; Send a message (request or notification) from server to client
(define (jsonrpc-stdio-send pid method params #:id [id #f])
  (if id
      (gen-server-call pid (list 'send-request method params id))
      (gen-server-call pid (list 'send-notify method params))))

;;; Connection-based transport (for TCP/WebSocket etc.)

(struct connection-server-state (impl user-state in out pending-responses))

(struct connection-transport-server ()
  #:methods gen:server
  [(define (init self args)
     (match-define (list impl init-args in out) args)
     (define user-state (jsonrpc-init impl init-args))
     ;; pending-responses: hash from id -> (channel for response)
     (ok (connection-server-state impl user-state in out (make-hash))))

   (define (handle-call self msg state from)
     (match-define (connection-server-state impl user-state in out pending) state)
     (match msg
       ['read-one
        (define json-str (read-jsonrpc-message in))
        (cond
          [(eof-object? json-str)
           (reply 'eof state)]
          [else
           (with-handlers ([exn:fail? (lambda (e)
                                        (write-jsonrpc-message out
                                          (build-error-response #f (jsonrpc-error PARSE-ERROR "Parse error" (exn-message e))))
                                        (reply 'error state))])
             (define obj (string->jsexpr json-str))
             (cond
               ;; Response to our request
               [(and (hash-has-key? obj 'id)
                     (or (hash-has-key? obj 'result)
                         (hash-has-key? obj 'error)))
                (define id (hash-ref obj 'id))
                (define ch (hash-ref pending id #f))
                (when ch
                  (channel-put ch obj)
                  (hash-remove! pending id))
                (reply 'ok state)]

               ;; Incoming request/notification
               [(hash-has-key? obj 'method)
                (define method (hash-ref obj 'method))
                (define params (hash-ref obj 'params '()))
                (define id (hash-ref obj 'id #f))
                (cond
                  [id
                   (define-values (response new-user-state)
                     (jsonrpc-handle-request impl method params user-state))
                   (write-jsonrpc-message out (build-full-response id response))
                   (reply 'ok (connection-server-state impl new-user-state in out pending))]
                  [else
                   (define new-user-state
                     (jsonrpc-handle-notify impl method params user-state))
                   (reply 'ok (connection-server-state impl new-user-state in out pending))])]

               [else
                (write-jsonrpc-message out
                  (build-error-response #f (jsonrpc-error INVALID-REQUEST "Invalid message" #f)))
                (reply 'error state)]))])]

       [(list 'send-request method params id response-channel)
        (hash-set! pending id response-channel)
        (define req (hasheq 'jsonrpc "2.0"
                            'method method
                            'params params
                            'id id))
        (write-jsonrpc-message out req)
        (reply 'ok state)]

       [(list 'send-notify method params)
        (define notif (hasheq 'jsonrpc "2.0"
                              'method method
                              'params params))
        (write-jsonrpc-message out notif)
        (reply 'ok state)]))

   (define (handle-cast self msg state)
     (noreply state))

   (define (handle-info self msg state)
     (noreply state))

   (define (terminate self reason state)
     (match-define (connection-server-state impl user-state _in _out _pending) state)
     (jsonrpc-terminate impl reason user-state))])

;; Start a connection-based JSON-RPC endpoint
(define (jsonrpc-connection-start impl
                                  #:args [args #f]
                                  #:in in
                                  #:out out)
  (gen-server-start (connection-transport-server) (list impl args in out)))

;; Send a request and wait for response
(define (jsonrpc-connection-send pid method params #:id [id (gensym 'req)])
  (define response-ch (make-channel))
  (gen-server-call pid (list 'send-request method params id response-ch))
  ;; Wait for response (blocking)
  (channel-get response-ch))

;; Close connection
(define (jsonrpc-connection-close pid [reason "normal"])
  (gen-server-stop pid reason))
