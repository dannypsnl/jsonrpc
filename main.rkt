#lang racket/base

;;; JSON-RPC 2.0 library with GenServer abstraction (using erl)
;;;
;;; This library provides a GenServer-based abstraction for building
;;; JSON-RPC 2.0 servers.
;;;
;;; Usage:
;;;   (require racket/class erl jsonrpc)
;;;
;;; Define your server using erl's gen-server%:
;;;
;;;   (define my-server%
;;;     (class gen-server%
;;;       (super-new)
;;;       (inherit ok noreply reply)
;;;
;;;       (define/override (init args) (ok initial-state))
;;;       (define/override (handle-call msg from state)
;;;         (match msg
;;;           [`("method-name" . ,params)
;;;            (reply result-value state)]))
;;;       (define/override (handle-cast msg state)
;;;         (match msg
;;;           [`("notification" . ,params) (noreply new-state)]))
;;;       (define/override (handle-info msg state) (noreply state))
;;;       (define/override (terminate reason state) (void))))
;;;
;;; Then start and use it:
;;;
;;;   (define inner-pid (gen-server:start (new my-server%) args))
;;;   (define pid (jsonrpc-server-start inner-pid))
;;;   (jsonrpc-server-request pid "method-name" '(params))
;;;   (jsonrpc-server-notify pid "notification" '())
;;;   (jsonrpc-server-stop pid)
;;;
;;; For stdio transport (LSP-style):
;;;
;;;   (jsonrpc-stdio-loop pid)

(require "server.rkt"
         "transport.rkt")

(provide
 ;; Server operations
 jsonrpc-server-start
 jsonrpc-server-request/raw
 jsonrpc-server-notify/raw
 jsonrpc-server-stop

 ;; Response helpers
 jsonrpc-ok
 jsonrpc-error

 ;; Error codes (JSON-RPC 2.0 spec)
 PARSE-ERROR
 INVALID-REQUEST
 METHOD-NOT-FOUND
 INVALID-PARAMS
 INTERNAL-ERROR

 ;; Transport layer
 read-jsonrpc-message
 write-jsonrpc-message
 jsonrpc-stdio-loop)

(module+ test
  (require rackunit
           "server.rkt")

  ;; Basic test to ensure everything loads
  (check-true (procedure? jsonrpc-server-start))
  (check-true (procedure? jsonrpc-ok))
  (check-equal? PARSE-ERROR -32700)
  (check-equal? METHOD-NOT-FOUND -32601))
