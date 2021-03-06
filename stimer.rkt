#lang racket/base
;;; A countdown timer
;;; Timer stops upon timout
;;; The only valid user action while running is 'stop, reset,
;;;    and possibly +1 min to be added later.
;;; If necessary,  to install rsond:
;;;     raco pkg install -i rsound  # -i if for a system-wide install
;;; TBD
;;; 1. Clicking display opens dialog to set time.
;;; 2. Audio alarm or exec of exernal prog for alarm sound.
(require racket/gui racket/flonum rsound)
(require (only-in srfi/19 current-time ))

;; ----------------- General Declaraions and Definitions  ----------------------
(define *taup (make-object color% #xc0 #xc0 #xb0))

;; .............................................................................
(define (positive x) (> x 0))

(define (msecs->timestr ms)
  (let*-values((( hrs mins0) (quotient/remainder   (round (/ ms 1000)) 3600))
               ((mins secs) (quotient/remainder  mins0 60)))
    (format "~a:~a:~a" (~02d hrs) (~02d mins) (~02d secs))))

(define (~02d n)
  (~a (if (flonum? n) (fl->exact-integer n) n) #:pad-string "0"
      #:align 'right
      #:min-width 2))
;
;; ----------------------------- Default Parameters ------------------------------
(struct params (fg bg timestr) #:prefab)
(define %defaults (params "brown" *taup   "00:20:00"))
(define %params %defaults)

;;; ================================== UI ========================================
;;; -------------------------------- User Commands -------------------------------
(define (reset)
  (send seconds-timer stop) 
  (set! time-started #f) (set! running #f)
  (set! msecs-remaining %interval-msecs )
  (set! prev-elapsed 0)
  (update))

;; A toggle associated with the start/stop button
(define (start-stop)
  (define (start) 
    (when (and msecs-remaining (> msecs-remaining 0))
      (set! time-started  (current-inexact-milliseconds))
      (send seconds-timer start 1000)      
      (set! running #t)
      (send start-stop-button set-label "Stop")
      (update)))
  (define  (stop)        
    (send seconds-timer stop)
    (set! prev-elapsed (elapsed-msecs))
    (set! running #f)  
    (send start-stop-button set-label "Start")
    (update))
  (if running (stop) (start)) )

;;; Initialize to an H:M:S countdown
;;; Set the timer only if not already running.
(define (set-countdown h m s)      
  (let ((msecs (*  1000 (+ ( * h 3600) (* m 60) s))))
    (set! %interval-msecs msecs)
    (set! msecs-remaining msecs)
    (update)
    msecs))

(define (do-timeout-actions)
  (play ding))
;;; ------------------------------------------------------------------------------
(define frame (new frame%
                   [label "Countdown"]
                   [width 512]
                   [height 180]))

;;; ............................. Canvas ..........................................
;;; Subliclassing canvas% in order to capture events
(define clock-canvas%
  (class canvas% 
    (inherit set-canvas-background)
    ;; Capture left mouse click...
    (define/override (on-event mev)
      (when (eq? (send mev get-event-type) 'left-down ) (println 'ok.)))

    (super-new [parent frame]
               [paint-callback
                (lambda (canvas dc)              
                  (send dc set-scale 8 8)
                  (send dc set-text-foreground (params-fg %params))                
                  (send dc draw-text (params-timestr %params) 1 0))])

    (set-canvas-background (params-bg %params))))

(define canvas (new clock-canvas%))

;;; ...............................................................................
; Add a horizontal panel to the dialog, with centering for buttons
(define pane (new horizontal-pane% [parent frame] (stretchable-height #f)
                  [alignment '(center center)]))
(define start-stop-button (new button% (parent pane) (label "Start") 
                               (callback (lambda (_btn _evt)(start-stop)))))
(define reset-button (new button% (parent pane) (label "Reset")
                          (callback (lambda(_b _e)(reset)))))

;;;=============================================================================
;;; ----------------------------------------------------------------------------
;; Timer(s)
(define msecs-remaining 0)
(define %interval-msecs #f)
(define time-started #f)
(define prev-elapsed 0)
(define running #f)
;;; ............................................................................
;;; Accumulated elapsed time while acually running w/o reset
(define (elapsed-msecs)
  (+ prev-elapsed (if time-started
                      (-    (current-inexact-milliseconds) time-started)
                      0)))

(define seconds-timer
  (let ((on-seconds-timeout 
         (lambda()
           (set! msecs-remaining ( - %interval-msecs (elapsed-msecs)))
           (cond 
             ( (positive msecs-remaining) (update))
             (else (send seconds-timer stop) 
                   (set! running #f)
                   (update) 
                   (do-timeout-actions))))))
    (new timer% (notify-callback on-seconds-timeout) (interval #f))))


;; ----------------------------------------------------------------------------
;; update the display
(define (update)
  (set! %params (struct-copy params %params (timestr (msecs->timestr msecs-remaining))))
  (send canvas refresh)
  (when (not running ) (send start-stop-button set-label "Start")) 
  msecs-remaining)



;;; ----------------------------------------------------------------------------
;; Initial display
(send frame show #t)

;;; ===========================================================================
;; Test
(set-countdown 0 0 20)
