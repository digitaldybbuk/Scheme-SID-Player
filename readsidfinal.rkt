#lang racket/gui
;;Load RSOUND LIBRARIES
(require (planet "main.rkt" ("clements" "rsound.plt" 1 (= 9))))
(require (planet clements/rsound/draw))
;;END Loading RSOUND LIBRARIES



;;Open sid-dump file
;(define SID-FILE (file->lines "iame.dmp"))




(define (enumerate-interval low high) (if (= low high) (list high) (cons low (enumerate-interval (+ low 1) high))))

(define (inc x)
  (+ x 1))
  
(define string-null? '())


;string-split taken from http://schemecookbook.org/Cookbook/StringSplit
(define (string-split str . rest)
                ; maxsplit is a positive number
  (define (split-by-whitespace str maxsplit)
    (define (skip-ws i yet-to-split-count)
      (cond
        ((>= i (string-length str)) '())
        ((char-whitespace? (string-ref str i))
          (skip-ws (inc i) yet-to-split-count))
        (else (scan-beg-word (inc i) i yet-to-split-count))))
    (define (scan-beg-word i from yet-to-split-count)
      (cond
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word i from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((char-whitespace? (string-ref str i))
          (cons (substring str from i) 
            (skip-ws (inc i) (- yet-to-split-count 1))))
        (else (scan-word (inc i) from yet-to-split-count))))
    (skip-ws 0 (- maxsplit 1)))

                ; maxsplit is a positive number
                ; str is not empty
  (define (split-by-charset str delimeters maxsplit)
    (define (scan-beg-word from yet-to-split-count)
      (cond
        ((>= from (string-length str)) '(""))
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word from from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((memq (string-ref str i) delimeters)
          (cons (substring str from i) 
            (scan-beg-word (inc i) (- yet-to-split-count 1))))
        (else (scan-word (inc i) from yet-to-split-count))))
    (scan-beg-word 0 (- maxsplit 1)))

                        ; resolver of overloading...
                        ; if omitted, maxsplit defaults to
                        ; (inc (string-length str))
  (if (not (string? str)) '()
    (if (null? rest) 
      (split-by-whitespace str (inc (string-length str)))
      (let ((charset (car rest))
          (maxsplit
            (if (pair? (cdr rest)) (cadr rest) (inc (string-length str)))))
        (cond
          ((not (positive? maxsplit)) '())
          ((null? charset) (split-by-whitespace str maxsplit))
          (else (split-by-charset str charset maxsplit))))))
)


  ;;pul = pulse 
  (define (pul t)
    (cond ((= (sin t) 0) 0)
          ((> (sin t) 0) 1)
          ((< (sin t) 0) -1)
          )) 
  (define fourier-scalar (/ 8 (* pi pi)))
;  
;  (define (tri-approx k)
;    (lambda (t)
;    (* fourier-scalar (expt -1 k) (/ (sin (* (+ (* 2 k) 1) t)) (expt (+ (* 2 k) 1) 2)))))
;     
  (define (tri-ap t low high total)
    (if (= low high)
        (* fourier-scalar total)
        (tri-ap t (+ low 1) high (+ total  (* (expt -1 low) (/ (sin (* (+ (* 2 low) 1) t)) (expt (+ (* 2 low) 1) 2)))  ))))
        
     
  
  ;;tri = triangle ;angular freq = 1 (2pi/2pi)
  (define (tri t)
    ;((tri-approx 1000) t)) ;1000 fourier calculations to produce triangle wave! (a function that takes a value t is returned)
    (tri-ap t 0 4 0))
    ;(* fourier-scalar (+ (- (+ ( - (sin t) ( / (sin (* 3 t)) 9)) (/ (sin (* 5 t)) 25)) (/ (sin (* 7 t)) 49)) (/ (sin (* 9 t)) 81))          ))
        


;;find line x in file
(define (line x FILE)
  (list-ref FILE x))

(define (frame-ref x FILE) ;;note that frame 0 exists, it is the first frame
  (if (not (> x -1)) '()
  (list-ref FILE (+ x 7))))


;;"| Frame | Freq Note/Abs WF ADSR Pul | Freq Note/Abs WF ADSR Pul | Freq Note/Abs WF ADSR Pul | FCut RC Typ V |"
;;"|  2181 | 1739 (F-4 B5) .. .... ... | ....  ... ..  .. .... ... | 04F4 (- 0012) .. .... 8E0 | .... .. ... . |"
;;Once we are in the file... we have this structure:

;> (line 1000 SID-FILE)
;"|   998 | 2B2F (- 00A7) .. .... ... | 224D (- 0083) .. .... ... | ....  ... ..  .. .... ... | 7100 .. ... . |"

;(map string-split 


;;Freq: gives back hex representation of the frequency of the current frame, given a frame number
;;because Frequency is in hex, we MUST do a string-append #x to the string, so that conversion to 
;;frequency will work. Otherwise it will not convert the hex to decimal correcctly
;;example: #x04F4 = 1268 Hz
;;THE SID 6581 HAS 3 VOICES, HENCE 3 FREQUENCIES ARE READ FOR EACH FRAME, FREQ1,FREQ2,FREQ3

(define (conv pred)
  (if (eq? pred #f)
      0
      (* pred 0.0596) ))


;;Fout = (Fn * 0.0596) Hz
(define (Freq frame-no)
  (list (conv (string->number (string-append "#x" (list-ref (string-split (frame-ref frame-no SID-FILE)) 3))))
        (conv (string->number  (string-append "#x" (list-ref (string-split (frame-ref frame-no SID-FILE)) 10))))
        (conv (string->number  (string-append "#x" (list-ref (string-split (frame-ref frame-no SID-FILE)) 17)))))
  
  
  )
 
;The SID 6581 Datasheet indicates that the waveform is a 4 digit binary digit...in our wavetable it is a 2 digit 8 bit hex
;
(define (Waveform frame-no) 
(lambda (SID-FILE)
  (list (string->number (string-append "#x" (list-ref (string-split (frame-ref frame-no SID-FILE)) 6)))
        (string->number (string-append "#x" (list-ref (string-split (frame-ref frame-no SID-FILE)) 13)))
        (string->number (string-append "#x" (list-ref (string-split (frame-ref frame-no SID-FILE)) 20)))
        )))
        
        
        
(define (Note/Abs frame-no)
   (sixth (string-split (frame-ref frame-no SID-FILE))))

;;test
(define samplerate 22000)
(define sr/inv (/ 1 samplerate))


;;test2
;;pul,sin,sqr
  (define (Sawtooth frames amp freq s-rate)
   (fun->mono-rsound frames s-rate (scale amp (sawtooth-wave freq s-rate))))
  
    (define (Square frames amp freq s-rate)
    (fun->mono-rsound frames s-rate (scale amp (square-wave freq s-rate))))

    (define (Sine frames amp freq s-rate)
    (fun->mono-rsound frames s-rate (scale amp (sine-wave freq s-rate))))

    
    
;(define tunepart (map Freq (enumerate-interval 1 2999)))
  ;;freq of each voice 1 2 3...for 200 frames...lets test it

  (define (makeframe f)
    
;    
;    ;Signals A B and C, the three voices
    

    (define sigA ;lead
      (if (not (= (first f) 0))      
      (rsound->signal/left (Sawtooth FRAMERATE .4 (first f) samplerate))
      silence))
    

   (define (sigB t)
          (* 1 (tri (* t (second f) twopi sr/inv))))
     
  (define (sigC t)
    (* 0.4 (pul (* t (third f) twopi sr/inv))))

    (define (silence t)
      0)
    
    ;;FRAMERATE is how many frames will contain each note in the SID FILE. 
    (define FRAMERATE 441) ;
   
    (define AB  (signal-+s (list (rsound->signal/right (fun->mono-rsound FRAMERATE samplerate sigB))
                                 (rsound->signal/right (fun->mono-rsound FRAMERATE samplerate sigA))
                                 
                                 ))) ;first two waveforms summed together
    
    
    (define AC  (signal-+s (list (rsound->signal/left (fun->mono-rsound FRAMERATE samplerate sigC));sigC 
                                 (rsound->signal/left (fun->mono-rsound FRAMERATE samplerate sigA));sigA
                                
                                 ))) ;first two waveforms summed together
    
    
   ; (define ABC (signal-+s (list AB sigC)))
    ;(define Mono-All(fun->mono-rsound FRAMERATE samplerate AB))
   (define Stereo-All (funs->stereo-rsound FRAMERATE samplerate AB AC))

    Stereo-All;return waveform
 
  ;  Mono-All ;;return the complete waveform
 )    
     
  ;;now we have list of rsounds we can combine...
;  

  
  
  (define SONG1 (file->lines "iame.dmp"))
  (define SONG2 (file->lines "syncopated.dmp"))
  (define SONG3 (file->lines "synthony.dmp"))
  (define SONG4 (file->lines "alibi.dmp"))

  (define SID-FILE SONG1)
  (define EOF (string->number (second (string-split (last SID-FILE))))) ;last line frame number is EOF
  (define mapped-tune (map Freq (enumerate-interval 1 EOF))) ;about 120s/2min
  (define pieces (map makeframe mapped-tune))
  (define song1 (rsound-append* pieces)) 

(set! SID-FILE SONG2)
(set! mapped-tune (map Freq (enumerate-interval 1 EOF))) ;about 120s/2min
(set! pieces (map makeframe mapped-tune))
(define song2 (rsound-append* pieces)) 

(set! SID-FILE SONG3)
(set! mapped-tune (map Freq (enumerate-interval 1 EOF))) ;about 120s/2min
(set! pieces (map makeframe mapped-tune))
(define song3 (rsound-append* pieces)) 
  
(set! SID-FILE SONG4)
(set! mapped-tune (map Freq (enumerate-interval 1 EOF))) ;about 120s/2min
(set! pieces (map makeframe mapped-tune))
(define song4 (rsound-append* pieces)) 
     
;;GUI PART
; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Sid Song Player"]
                          [width 112]
                          [height 144]
                          [style '(no-resize-border)] ))
                         
;lets make rows, each row will be a panel first,second,third...etc
(define first-panel (new horizontal-panel% [parent frame]
                                     [alignment '(center center)]))
(define second-panel (new horizontal-panel% [parent frame]
                                     [alignment '(center center)]))
(define third-panel (new horizontal-panel% [parent frame]
                                     [alignment '(center center)]))
(define fourth-panel (new horizontal-panel% [parent frame]
                                     [alignment '(center center)]))
(define fifth-panel (new horizontal-panel% [parent frame]
                                     [alignment '(center center)]))

(define sixth-panel (new horizontal-panel% [parent frame]
                                     [alignment '(center center)]))

(define seventh-panel (new horizontal-panel% [parent frame]
                                     [alignment '(center center)]))


(define msg (new message% [parent first-panel]
                          [label "Choose Song to Play (all songs by Laxity)"]))

;make buttons
;;FIRST ROW OF NUMBERS
(new button% [parent second-panel]
             [label "IAME"]
             (callback (lambda (button event)
                         (begin (stop-playing) (rsound-play song1))  )))
(new button% [parent second-panel]
             [label "Syncopated"]
             (callback (lambda (button event)
                         (begin (stop-playing) (rsound-play song2))  )))
(new button% [parent second-panel]
             [label "Synthony"]
             (callback (lambda (button event)
                          (begin (stop-playing) (rsound-play song3))   )))
(new button% [parent second-panel]
             [label "Alibi"]
             (callback (lambda (button event)
                        (begin (stop-playing) (rsound-play song4))     )))
;;FIRST ROW OF NUMBERS
(new button% [parent third-panel]
             [label "Draw"]
             (callback (lambda (button event)
                         (begin  (rsound-draw song1))  )))
(new button% [parent third-panel]
             [label "Draw"]
             (callback (lambda (button event)
                         (begin (rsound-draw song2))  )))
(new button% [parent third-panel]
             [label "Draw"]
             (callback (lambda (button event)
                          (begin  (rsound-draw song3))   )))
(new button% [parent third-panel]
             [label "Draw"]
             (callback (lambda (button event)
                        (begin (rsound-draw song4))     )))


;Autodial button, gets data from autodial-data field, and convert it to a list, then send it to DIAL...
(new button% [parent seventh-panel]
             [label "Stop"]
             (callback (lambda (button event)
                         (stop-playing)    )))


(send frame show #t)