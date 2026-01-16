#lang racket/base

(provide read-jsonrpc-message
         write-jsonrpc-message
         jsonrpc-stdio-loop)

(require rakka
         racket/match
         racket/string
         json
         "server.rkt")

;;; LSP-style stdio transport layer
;;; Messages use Content-Length header format:
;;; Content-Length: <length>\r\n
;;; \r\n
;;; <json-body>

;;; Read a single JSON-RPC message from input port
;;; Returns the message string or eof
(define (read-jsonrpc-message in)
  (define headers (read-headers in))
  (cond
    [(eof-object? headers) eof]
    [else
     (define content-length (hash-ref headers 'content-length #f))
     (unless content-length
       (error 'read-jsonrpc-message "Missing Content-Length header"))
     (read-string content-length in)]))

;;; Read headers until empty line
(define (read-headers in)
  (define headers (make-hash))
  (let loop ()
    (define line (read-line in 'any))
    (cond
      [(eof-object? line) eof]
      [(or (string=? line "") (string=? line "\r")) headers]
      [else
       ;; Strip trailing \r if present
       (define clean-line
         (if (and (> (string-length line) 0)
                  (char=? (string-ref line (sub1 (string-length line))) #\return))
             (substring line 0 (sub1 (string-length line)))
             line))
       (define parts (string-split clean-line ": " #:trim? #f))
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

;;; Parse JSON-RPC message
;;; Returns (list method params id) or #f on error
(define (parse-jsonrpc-message json-str)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define obj (string->jsexpr json-str))
    (cond
      [(not (hash? obj)) #f]
      [(not (equal? (hash-ref obj 'jsonrpc #f) "2.0")) #f]
      [(not (hash-ref obj 'method #f)) #f]
      [else
       (define method (hash-ref obj 'method))
       (define params (hash-ref obj 'params '()))
       (define id (hash-ref obj 'id #f))
       (list method params id)])))

;;; Run a JSON-RPC server loop over stdio
;;; jsonrpc-pid: pid of a jsonrpc-server (from jsonrpc-server-start)
;;; in/out: input/output ports
;;; Returns when EOF is received
(define (jsonrpc-stdio-loop jsonrpc-pid
                            #:in [in (current-input-port)]
                            #:out [out (current-output-port)])
  (let loop ()
    (define json-str (read-jsonrpc-message in))
    (cond
      [(eof-object? json-str) 'done]
      [else
       (define parsed (parse-jsonrpc-message json-str))
       (cond
         [(not parsed)
          ;; Parse error
          (write-jsonrpc-message out
                                 (hasheq 'jsonrpc "2.0"
                                         'error (hasheq 'code PARSE-ERROR 'message "Parse error")
                                         'id #f))
          (loop)]
         [else
          (match-define (list method params id) parsed)
          (cond
            [id
             ;; Request - send response with original ID
             (define response (jsonrpc-server-request jsonrpc-pid id method params))
             ;; Replace the internal ID with the client's original ID
             (write-jsonrpc-message out (hash-set response 'id id))
             (loop)]
            [else
             ;; Notification
             (jsonrpc-server-notify jsonrpc-pid method params)
             (loop)])])])))

(module+ test
  (require rackunit)

  ;; Test basic message reading/writing
  (let ()
    (define-values (in out) (make-pipe))
    (write-jsonrpc-message out (hasheq 'test "value"))
    (close-output-port out)
    (define msg (read-jsonrpc-message in))
    (check-equal? (string->jsexpr msg) (hasheq 'test "value"))
    (close-input-port in))

  ;; Simple server for testing
  (struct test-server ()
    #:methods gen:server
    [(define (init self args)
       (ok 0))

     (define (handle-call self msg state from)
       (match msg
         [`("echo" . ,args) (reply args state)]
         [`("add" ,a ,b) (reply (+ a b) state)]
         [_ (reply (jsonrpc-error METHOD-NOT-FOUND "Method not found" #f) state)]))

     (define (handle-cast self msg state)
       (match msg
         [`("set" ,v) (noreply v)]
         [_ (noreply state)]))])

  (with-runtime #:schedulers 1
    ;; Test reading/writing through pipes
    (let ()
      (define-values (client-in server-out) (make-pipe))
      (define-values (server-in client-out) (make-pipe))

      ;; Write a request
      (define req (hasheq 'jsonrpc "2.0" 'method "add" 'params '(3 4) 'id 1))
      (write-jsonrpc-message client-out req)
      (close-output-port client-out)

      ;; Read it
      (define msg (read-jsonrpc-message server-in))
      (check-equal? (string->jsexpr msg) req)

      (close-input-port client-in)
      (close-input-port server-in)
      (close-output-port server-out))

    ;; Test full loop with threading
    (let ()
      (define-values (client-in server-out) (make-pipe))
      (define-values (server-in client-out) (make-pipe))

      ;; Start the server
      (define inner-pid (gen-server-start (test-server) #f))
      (define jsonrpc-pid (jsonrpc-server-start inner-pid))

      ;; Run the loop in a thread
      (define loop-thread
        (thread (lambda ()
                  (jsonrpc-stdio-loop jsonrpc-pid #:in server-in #:out server-out))))

      ;; Send a request
      (write-jsonrpc-message client-out
                             (hasheq 'jsonrpc "2.0" 'method "add" 'params '(10 20) 'id 1))

      ;; Read response
      (define resp (string->jsexpr (read-jsonrpc-message client-in)))
      (check-equal? (hash-ref resp 'jsonrpc) "2.0")
      (check-equal? (hash-ref resp 'result) 30)
      (check-equal? (hash-ref resp 'id) 1)

      ;; Send another request
      (write-jsonrpc-message client-out
                             (hasheq 'jsonrpc "2.0" 'method "echo" 'params '("hello") 'id 2))

      (define resp2 (string->jsexpr (read-jsonrpc-message client-in)))
      (check-equal? (hash-ref resp2 'result) '("hello"))

      ;; Clean up - close output to signal EOF
      (close-output-port client-out)
      (thread-wait loop-thread)

      (jsonrpc-server-stop jsonrpc-pid)
      (close-input-port client-in)
      (close-input-port server-in)
      (close-output-port server-out))
    ))
