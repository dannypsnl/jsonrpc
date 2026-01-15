#lang racket/base

;;; JSON-RPC 2.0 library with GenServer abstraction (using Rakka)
;;;
;;; This library provides a GenServer-based abstraction for building
;;; JSON-RPC 2.0 servers and clients.
;;;
;;; Usage:
;;;   (require jsonrpc)
;;;
;;; Define your server by implementing gen:jsonrpc-server:
;;;
;;;   (struct my-server ()
;;;     #:methods gen:jsonrpc-server
;;;     [(define (jsonrpc-init self args) ...)
;;;      (define (jsonrpc-handle-request self method params state) ...)
;;;      (define (jsonrpc-handle-notify self method params state) ...)
;;;      (define (jsonrpc-terminate self reason state) ...)])
;;;
;;; Then start and use it:
;;;
;;;   (define pid (jsonrpc-server-start (my-server)))
;;;   (jsonrpc-server-request pid "method-name" '(params))
;;;   (jsonrpc-server-notify pid "notification" '())
;;;   (jsonrpc-server-stop pid)

(require "server.rkt"
         "transport.rkt")

(provide
 ;; Core GenServer interface
 gen:jsonrpc-server
 jsonrpc-server-start
 jsonrpc-server-request
 jsonrpc-server-notify
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

 ;; Stdio transport (LSP-style)
 jsonrpc-stdio-server-start
 jsonrpc-stdio-send

 ;; Connection-based transport
 jsonrpc-connection-start
 jsonrpc-connection-send
 jsonrpc-connection-close)

(module+ test
  (require rackunit
           "server.rkt")

  ;; Basic test to ensure everything loads
  (check-true (procedure? jsonrpc-server-start))
  (check-true (procedure? jsonrpc-ok))
  (check-equal? PARSE-ERROR -32700)
  (check-equal? METHOD-NOT-FOUND -32601))
