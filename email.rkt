#lang racket
(require net/sendmail)
(provide send-html-email)

;;; constants

(define *email-header-tag* "header-content")
(define *email-footer-tag* "footer-content")

;;; defs

;; send a pre-formatted HTML email using defaults
(define (send-html-email to from subject html-template-file body)
  (when (string? body)  (set! body (string-trim body)))
  (when (list? body)    (set! body (string-join body "\n"))) ; fix body lists
  (let* ((html-template (file->string html-template-file))
 	 (html-content  (string-replace html-template 
				        *email-header-tag* 
				        body))
	 (html-header "MIME-Version: 1.0\nContent-Type: text/html; charset=ISO-8859-1"))
    (send-mail-message from 
		       subject 
		       to 
		       null 
		       null 
		       (string-split html-content "\n") 
		       html-header)))
; unit testt
(module+ test
  (send-html-email "dexterlagan@gmail.com" 
		   "notifier@dexterphoto.com" 
		   "Test Email from Notifier" 
		   "Hi there,\n  This is a test."))


