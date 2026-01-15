#lang racket/base

(provide
 ;; Server operations
 jsonrpc-server-start
 jsonrpc-server-request/raw
 jsonrpc-server-notify/raw
 jsonrpc-server-request
 jsonrpc-server-notify
 jsonrpc-server-stop
 ;; Response helpers
 jsonrpc-ok
 jsonrpc-ok?
 jsonrpc-ok-result
 jsonrpc-error
 jsonrpc-error?
 jsonrpc-error-code
 jsonrpc-error-message
 jsonrpc-error-data
 ;; Error codes (JSON-RPC 2.0 spec)
 PARSE-ERROR
 INVALID-REQUEST
 METHOD-NOT-FOUND
 INVALID-PARAMS
 INTERNAL-ERROR)

(require rakka
         racket/match
         json)

;;; JSON-RPC 2.0 Error Codes
(define PARSE-ERROR -32700)
(define INVALID-REQUEST -32600)
(define METHOD-NOT-FOUND -32601)
(define INVALID-PARAMS -32602)
(define INTERNAL-ERROR -32603)

;;; Response constructors
(struct jsonrpc-ok (result) #:transparent)
(struct jsonrpc-error (code message data) #:transparent)

;;; Internal GenServer wrapper that bridges rakka's gen:server with gen:jsonrpc-server
(struct jsonrpc-server-wrapper ()
  #:methods gen:server
  [(define (init self args)
     (match-define (list impl) args)
     (ok impl))

   (define (handle-call self msg state from)
     (define impl state)
     (match msg
       ;; JSON-RPC request: (request method params id)
       [(list 'request method params id)
        (define json-response (build-response id (gen-server-call impl (cons method params))))
        (reply json-response impl)]

       [_ (reply (build-response #f (jsonrpc-error INVALID-REQUEST "Invalid message" #f)) state)]))

   (define (handle-cast self msg state)
     (define impl state)
     (match msg
       ;; JSON-RPC notification: (notify method params)
       [(list 'notify method params)
        (gen-server-cast! impl (cons method params))
        (noreply impl)]

       [_ (noreply state)]))

   (define (handle-info self msg state)
     (noreply state))

   (define (terminate self reason state)
     (void))])

;;; Parse JSON-RPC message
;;; Returns (list method params id) or jsonrpc-error
(define (parse-jsonrpc json-str)
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

;;; Build JSON-RPC response hash
;;; response can be:
;;;   - jsonrpc-ok struct
;;;   - jsonrpc-error struct
;;;   - any other value (treated as successful result)
(define (build-response id response)
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
             'id id)]
    [else
     ;; Treat any other value as a successful result
     (hasheq 'jsonrpc "2.0"
             'result response
             'id id)]))

;;; Public API

;; Start a JSON-RPC server
;; impl: an instance implementing gen:jsonrpc-server
;; args: initialization arguments passed to jsonrpc-init
(define (jsonrpc-server-start impl)
  (gen-server-start (jsonrpc-server-wrapper) (list impl)))

;; Send a request and wait for response
;; method: string, the method name
;; params: list or hash, the parameters
;; Returns: hash (the JSON-RPC response)
(define (jsonrpc-server-request pid id method params)
  (gen-server-call pid (list 'request method params id)))

;; Send a notification (no response)
(define (jsonrpc-server-notify pid method params)
  (gen-server-cast! pid (list 'notify method params)))

;; Send a request with raw JSON string
(define (jsonrpc-server-request/raw pid json-str)
  (gen-server-call pid (cons 'request (parse-jsonrpc json-str))))
;; Send a notification with raw JSON string
(define (jsonrpc-server-notify/raw pid json-str)
  (gen-server-cast! pid (cons 'notify (parse-jsonrpc json-str))))

;; Stop the server
(define (jsonrpc-server-stop pid [reason "normal"])
  (gen-server-stop pid reason))

(module+ test
  (require rackunit)

  ;; Example implementation
  (struct echo-server ()
    #:methods gen:server
    [(define (init self args)
       (ok 0))

     (define (handle-call self msg state from)
       (match msg
         [`("get-counter") (reply state state)]
         [_ (reply #f state)]))

     (define (handle-cast self msg state)
       (match msg
         [`("reset") (noreply 0)]
         [`("increment") (noreply (add1 state))]
         [_ (noreply state)]))])


  ;; Test the echo server
  (define pid (jsonrpc-server-start (gen-server-start (echo-server) #f)))

  ;; Test initial counter
  (define resp0 (jsonrpc-server-request pid "get-counter" '()))
  (check-equal? (hash-ref resp0 'jsonrpc) "2.0")
  (check-equal? (hash-ref resp0 'result) 0)

  ;; Test increment
  (jsonrpc-server-notify pid "increment" '())
  (sleep 0.05) ;; Give time for notification to process
  (define resp2 (jsonrpc-server-request pid "get-counter" '()))
  (check-equal? (hash-ref resp2 'result) 1)

  (jsonrpc-server-notify pid "increment" '())
  (sleep 0.05)
  (define resp3 (jsonrpc-server-request pid "get-counter" '()))
  (check-equal? (hash-ref resp3 'result) 2)

  ;; Test get-counter
  (define resp4 (jsonrpc-server-request pid "get-counter" '()))
  (check-equal? (hash-ref resp4 'result) 2)

  ;; Test notification (reset)
  (jsonrpc-server-notify pid "reset" '())
  (sleep 0.05) ;; Give time for notification to process
  (define resp5 (jsonrpc-server-request pid "get-counter" '()))
  (check-equal? (hash-ref resp5 'result) 0)

  ;; Clean up
  (jsonrpc-server-stop pid))
