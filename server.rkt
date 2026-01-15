#lang racket/base

(provide gen:jsonrpc-server
         jsonrpc-server?
         ;; Generic methods
         jsonrpc-init
         jsonrpc-handle-request
         jsonrpc-handle-notify
         jsonrpc-terminate
         ;; Server operations
         jsonrpc-server-start
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
         racket/port
         json
         racket/generic)

;;; JSON-RPC 2.0 Error Codes
(define PARSE-ERROR -32700)
(define INVALID-REQUEST -32600)
(define METHOD-NOT-FOUND -32601)
(define INVALID-PARAMS -32602)
(define INTERNAL-ERROR -32603)

;;; Response constructors
(struct jsonrpc-ok (result) #:transparent)
(struct jsonrpc-error (code message data) #:transparent)

;;; Generic interface for JSON-RPC servers
;;; Users implement this to handle JSON-RPC methods
(define-generics jsonrpc-server
  ;; Initialize server state, returns initial state
  ;; (jsonrpc-init self args) -> state
  (jsonrpc-init jsonrpc-server args)

  ;; Handle a request (expects response)
  ;; (jsonrpc-handle-request self method params state) -> (values response new-state)
  ;; response should be (jsonrpc-ok result) or (jsonrpc-error code message [data])
  (jsonrpc-handle-request jsonrpc-server method params state)

  ;; Handle a notification (no response expected)
  ;; (jsonrpc-handle-notify self method params state) -> new-state
  (jsonrpc-handle-notify jsonrpc-server method params state)

  ;; Called when server terminates
  ;; (jsonrpc-terminate self reason state) -> void
  (jsonrpc-terminate jsonrpc-server reason state))

;;; Internal GenServer wrapper that bridges rakka's gen:server with gen:jsonrpc-server
(struct jsonrpc-server-wrapper (impl)
  #:methods gen:server
  [(define (init self args)
     (match-define (list impl init-args) args)
     (define state (jsonrpc-init impl init-args))
     (ok (cons impl state)))

   (define (handle-call self msg state from)
     (match-define (cons impl user-state) state)
     (match msg
       ;; JSON-RPC request: (request method params id)
       [(list 'request method params id)
        (define-values (response new-user-state)
          (jsonrpc-handle-request impl method params user-state))
        (define json-response (build-response id response))
        (reply json-response (cons impl new-user-state))]

       ;; Raw JSON string request
       [(list 'raw-request json-str)
        (define parsed (parse-jsonrpc json-str))
        (cond
          [(jsonrpc-error? parsed)
           ;; Parse error
           (reply (build-response #f parsed) state)]
          [else
           (match-define (list method params id) parsed)
           (cond
             [id
              ;; It's a request
              (define-values (response new-user-state)
                (jsonrpc-handle-request impl method params user-state))
              (reply (build-response id response) (cons impl new-user-state))]
             [else
              ;; It's a notification, but called as request - handle anyway
              (define new-user-state
                (jsonrpc-handle-notify impl method params user-state))
              (reply #f (cons impl new-user-state))])])]

       [_ (reply (build-response #f (jsonrpc-error INVALID-REQUEST "Invalid message" #f)) state)]))

   (define (handle-cast self msg state)
     (match-define (cons impl user-state) state)
     (match msg
       ;; JSON-RPC notification: (notify method params)
       [(list 'notify method params)
        (define new-user-state (jsonrpc-handle-notify impl method params user-state))
        (noreply (cons impl new-user-state))]

       ;; Raw JSON string notification
       [(list 'raw-notify json-str)
        (define parsed (parse-jsonrpc json-str))
        (cond
          [(jsonrpc-error? parsed)
           ;; Parse error in notification - just ignore
           (noreply state)]
          [else
           (match-define (list method params _id) parsed)
           (define new-user-state (jsonrpc-handle-notify impl method params user-state))
           (noreply (cons impl new-user-state))])]

       [_ (noreply state)]))

   (define (handle-info self msg state)
     (noreply state))

   (define (terminate self reason state)
     (match-define (cons impl user-state) state)
     (jsonrpc-terminate impl reason user-state))])

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
     (hasheq 'jsonrpc "2.0"
             'error (hasheq 'code INTERNAL-ERROR
                           'message "Internal error")
             'id id)]))

;;; Public API

;; Start a JSON-RPC server
;; impl: an instance implementing gen:jsonrpc-server
;; args: initialization arguments passed to jsonrpc-init
(define (jsonrpc-server-start impl [args #f])
  (gen-server-start (jsonrpc-server-wrapper impl) (list impl args)))

;; Send a request and wait for response
;; method: string, the method name
;; params: list or hash, the parameters
;; Returns: hash (the JSON-RPC response)
(define (jsonrpc-server-request pid method params)
  (gen-server-call pid (list 'request method params (gensym 'req))))

;; Send a request with raw JSON string
(define (jsonrpc-server-request/raw pid json-str)
  (gen-server-call pid (list 'raw-request json-str)))

;; Send a notification (no response)
(define (jsonrpc-server-notify pid method params)
  (gen-server-cast! pid (list 'notify method params)))

;; Send a notification with raw JSON string
(define (jsonrpc-server-notify/raw pid json-str)
  (gen-server-cast! pid (list 'raw-notify json-str)))

;; Stop the server
(define (jsonrpc-server-stop pid [reason "normal"])
  (gen-server-stop pid reason))

(module+ test
  (require rackunit)

  ;; Example implementation
  (struct echo-server ()
    #:methods gen:jsonrpc-server
    [(define (jsonrpc-init self args)
       (hasheq 'counter 0))

     (define (jsonrpc-handle-request self method params state)
       (match method
         ["echo"
          (values (jsonrpc-ok params) state)]
         ["increment"
          (define new-counter (add1 (hash-ref state 'counter)))
          (values (jsonrpc-ok new-counter)
                  (hash-set state 'counter new-counter))]
         ["get-counter"
          (values (jsonrpc-ok (hash-ref state 'counter)) state)]
         [_
          (values (jsonrpc-error METHOD-NOT-FOUND
                                 (format "Method not found: ~a" method)
                                 #f)
                  state)]))

     (define (jsonrpc-handle-notify self method params state)
       (match method
         ["reset"
          (hash-set state 'counter 0)]
         [_ state]))

     (define (jsonrpc-terminate self reason state)
       (void))])

  ;; Test the echo server
  (define pid (jsonrpc-server-start (echo-server)))

  ;; Test echo
  (define resp1 (jsonrpc-server-request pid "echo" '("hello" "world")))
  (check-equal? (hash-ref resp1 'jsonrpc) "2.0")
  (check-equal? (hash-ref resp1 'result) '("hello" "world"))

  ;; Test increment
  (define resp2 (jsonrpc-server-request pid "increment" '()))
  (check-equal? (hash-ref resp2 'result) 1)

  (define resp3 (jsonrpc-server-request pid "increment" '()))
  (check-equal? (hash-ref resp3 'result) 2)

  ;; Test get-counter
  (define resp4 (jsonrpc-server-request pid "get-counter" '()))
  (check-equal? (hash-ref resp4 'result) 2)

  ;; Test notification (reset)
  (jsonrpc-server-notify pid "reset" '())
  (sleep 0.1) ;; Give time for notification to process
  (define resp5 (jsonrpc-server-request pid "get-counter" '()))
  (check-equal? (hash-ref resp5 'result) 0)

  ;; Test method not found
  (define resp6 (jsonrpc-server-request pid "unknown" '()))
  (check-true (hash-has-key? resp6 'error))
  (check-equal? (hash-ref (hash-ref resp6 'error) 'code) METHOD-NOT-FOUND)

  ;; Clean up
  (jsonrpc-server-stop pid))
