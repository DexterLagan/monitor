#lang racket
(require "common.rkt")
(require "config.rkt")
(require "pmap.rkt")
(module+ test (require rackunit))
(provide get-command-output
	 get-remote-command-output
	 run-commands)

;; Macro that defines whichever parameters are fed to it and fills them in from command line
(define-syntax define-command-line-params
  (syntax-rules ()
    ((define-command-line-params appname param1 ...)
     (define-values (param1 ...)
       (command-line #:program appname
                     #:args (param1 ...)
                     (values param1 ...))))))

;; executes a command locally and returns its output as string
(define (get-command-output command) 
  (string-trim (with-output-to-string 
		 (lambda () 
		   (unless (system command)
		     (echo "error running '" command "'"))))))

;; executes a command remotely through SSH and returns its output two strings values: output and error
(define (get-remote-command-output user@host command)
  (let ([e (open-output-string)])
    (parameterize ([current-error-port e])
      (let ((o (get-command-output (string-append "ssh " user@host " '" command "'"))))
    (values o
    	    (get-output-string e))))))

;; run a list of commands on a server, output to screen by default
(define (run-commands commands hostname username)
  (define user@host (string-append username "@" hostname))
    (pmap (lambda (c) 
      (let-values (((output error) (get-remote-command-output user@host c)))
        output)) 
          commands))

; EOF
