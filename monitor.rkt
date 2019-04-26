#lang racket
(require "config.rkt")
(require "common.rkt")
(require "system.rkt")
(require "email.rkt")
(require "pmap.rkt")

;;; purpose

; to connect to a list of servers and gather CPU, RAM, disk usages and top 3 CPU and RAM processes

;;; consts

(define *appname* 		      	"Monitor")
(define *app-config-filename* 	      	"monitor.conf")
(define *app-config-section-length*   	9)
(define *items-config-filename*       	"servers.conf")
(define *items-config-section-length* 	4)
(define *app-config-error* 	 	"Bad program configuration file syntax.")
(define *items-config-error* 	 	"Bad items configuration file syntax.")
(define *email-alert-triggers*   	(list "100%" "CPU Usage: 1."))
(define *default-email-from* 	 	"monitor@mydomain.com")
(define *default-email-subject*  	"Nodixia Monitor Alert")
(define *default-email-template* 	"monitor.html")

;;; defs 

;; returns a fully form email
(define (make-email-contents hostname output)
  (string-append "Hi,<br><br>&nbsp;&nbsp;The following system has triggered an alert based on load. "
		 "Please see the details below and take action as soon as possible:<br><br>"
		 "<b>Host Name: " hostname "</b><br>"
		 (string-join output "<br>")
		 "<br><br><br>"))

;; process one server from a config section
(define (process-item config commands distribution-list)
  (define hostname (get-config-value config "hostname="))
  (define username (get-config-value config "username="))
  (define user@host (string-append username "@" hostname))
  (define output (run-commands commands hostname username))
  (define output-str (string-join output "\n"))
  (if (or (string-contains? output-str "error")
	  (string-contains? output-str "could not"))
      (displayln "Host unreachable.")
      (displayln (string-append output-str "\n")))
  (when (ormap (lambda (s) (string-contains? output-str s)) 
	       *email-alert-triggers*)
    	(displayln "Usage threshold reached. Alert sent.")
    	(define email-contents 
	  (make-email-contents hostname output))
        (send-html-email distribution-list
	   	         *default-email-from*
		         *default-email-subject*
		         *default-email-template*
		         email-contents)))

;;; main

; gather command line parameters: config file location and HTML email template location
(define-command-line-params *appname* app-config-file items-config-file html-template-file)

; read program and items configuration;
(define app-configs 	  (load-config app-config-file *app-config-section-length*))
(unless app-configs 	  (die *app-config-error*))

(define items-configs	  (load-config items-config-file *items-config-section-length*))
(unless items-configs 	  (die *items-config-error*))

(define app-config 	  (first app-configs))
(define commands    (list (get-config-item-or-die app-config "hostname-command=")
			  (get-config-item-or-die app-config "uptime-command=")
		      	  (get-config-item-or-die app-config "cpu-usage-command=")
		       	  (get-config-item-or-die app-config "ram-usage-command=")
			  (get-config-item-or-die app-config "dsk-usage-command=")
			  (get-config-item-or-die app-config "top-cpu-command=")
             		  (get-config-item-or-die app-config "top-ram-command=")))

; load email distribution list, check and convert it:
(define distribution-str  (get-config-item-or-die app-config "distribution-list="))
(unless (and (non-empty-string? distribution-str)
	     (string-contains?  distribution-str "@"))
	     (die *app-config-error*))
(define distribution-list (string-split distribution-str ","))

; process items
(displayln "Starting scan...\n")
(process-config-items process-item
                      items-configs
                      commands
		      distribution-list)
(displayln "All done.\n")

; EOF
