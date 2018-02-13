#lang racket
;;Load RSOUND LIBRARIES
(require (planet "main.rkt" ("clements" "rsound.plt" 1 (= 9))))
(require (planet clements/rsound/draw))
;;END Loading RSOUND LIBRARIES



;;Open sid-dump file
(define SID-FILE (file->lines "storm.dmp"))
;(define SID-FILE '())

(define (pad8 ls)
  (let ((zeros (- 8 (length ls))))
       (if (> (length ls) 7)
         ls ;just return no need to pad
               (pad8 (cons 0 ls)))))
  
  
(define dec->bin;convert decimal to binary, Useful when deciphering SID waveform datasheet
	 (lambda (d)
		(cond
                  ((eq? d #f) (list 0)) ;if our table contains ... this will be #f, which should be 0
                  ((< d 1) (list 0))
                  ((= d 1) (list 1))
                  ((> d 1) (append
                            (dec->bin (floor (/ d 2))) ;floor function gives back the remainder
                               (list (if (= (modulo d 2) 0) 0 1))))))) ;divide by 2 evenly 0, else 1
                           
;;OUR FOUR WAVEFORMS + SILENCE

;;SILENCE
;;NOISE
;;PULSE
;;TRIANGLE (TWO VERSIONS HERE, ONE CALCULATED WITH FOURIER SERIES AND SIMPLIFIED VERSION)
;;SAWTOOTH
   (define (silence t)
      0)
   (random-seed (random 10))
   ;;noi = noise (random number between -1 and 1
   (define (noi t)
     
     (* (expt  -1 (random 5)) (random) t))
   
   
   ;;pul = pulse 
   (define (pul t)
     (cond ((= (sin t) 0) 0)
           ((> (sin t) 0) 1)
           ((< (sin t) 0) -1)
           )) 
  
  
  (define fourier-scalar (/ 8 (* pi pi)))

  ;;high can also be referred to number of harmonics
  ;;tri-ap is used by tri to get a triangle wave using fourier series
  (define (tri-ap t low high total)
    (if (= low high)
        (* fourier-scalar total)
        (tri-ap t (+ low 1) high (+ total  (* (expt -1 low) (/ (sin (* (+ (* 2 low) 1) t)) (expt (+ (* 2 low) 1) 2)))  ))))
        
  ;;tri = triangle ;angular freq = 1 (2pi/2pi)
  (define (tri t)
    ;((tri-approx 10) t)) ;(a function that takes a value t is returned)
    (tri-ap t 0 4 0))

  ;;an example of a fourier series written out for first 5 harmonics
  ;(* fourier-scalar (+ (- (+ ( - (sin t) ( / (sin (* 3 t)) 9)) (/ (sin (* 5 t)) 25)) (/ (sin (* 7 t)) 49)) (/ (sin (* 9 t)) 81))          ))
        
(define (saw t)
  (* 2 (- t (floor (+ t .5)))))

(define (tri2 t) ;triangle is the abs of sawtooth...
  (abs (saw t)))





;;for better abstraction, to be used in deciphering waveform, Control Register (Register 04)
(define bit0/gate eighth )    ;gate bit
(define bit1/sync seventh)  ;sync bit
(define bit2/ringmod sixth) ;ringmod bit
(define bit3/test fifth) ;test bit (if high, locks osc 1 until cleared(low)) 
(define bit4/tri fourth) ;triangle
(define bit5/saw third)  ;sawtooth
(define bit6/sqr second) ;squarewave
(define bit7/noi first)  ;noise bit





(define last-wave-sigA tri) ;default value is triangle wave
(define last-wave-sigB tri) ;
(define last-wave-sigC tri) ;



(define (getwaveform R04 whichsignal) ;register 04 bits, update current waveform, return waveform function
  (cond ((= (bit4/tri R04) 1) (begin (set! whichsignal tri) tri)) 
        ((= (bit5/saw R04) 1) (begin (set! whichsignal saw)  saw))
        ((= (bit6/sqr R04) 1) (begin (set! whichsignal sqr)  sqr))
        ((= (bit7/noi R04) 1) (begin (set! whichsignal noi)  noi))
        (else
              whichsignal)
        
        )) ;noise,test,ringmod not set yet





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
  (list (pad8 (dec->bin (string->number (string-append "#x" (list-ref (string-split (frame-ref frame-no SID-FILE)) 6)))))
        (pad8 (dec->bin (string->number (string-append "#x" (list-ref (string-split (frame-ref frame-no SID-FILE)) 13)))))
        (pad8 (dec->bin (string->number (string-append "#x" (list-ref (string-split (frame-ref frame-no SID-FILE)) 20)))))
        ))
        
        
(define (Freq+Waveform frame-no)
   (append (Freq frame-no) (Waveform frame-no)) )
        
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

  (define (makeframe f) ;f is the mapped-tune, (map Freq (enumerated-interval 0 2000)) mapping over the song
    ;list structure
    ;( Freq1 Freq2 Freq3 Waveform1 Waveform2 Waveform3 )
    ;   V1   V2     V3     V1        V2       V3
    ;V1 = Voice1, V2 = Voice2, V3 = Voice3
    
    
    
    ;first
    ;second
    ;third
    
    ;third
    ;fourth
    ;fifth
;    
;    ;Signals A B and C, the three voices
    
    ;;figure out Waveform, ADSR, and Pulse
    ;(define WaveformA (
    ;  (define WaveformB
    ;    (define WaveformC
    
    
   ;(define (sigA t)
    ;      (* 1 (sin (* t (first f))) twopi sr/inv))

    
 ;;last-wave-sigX is a global variable indicating the lastwave used, it will be modified as 
 ;;the bits are read from register 04
    
    ;;getwaveform will determine from register 04 which waveform to use, and which signal asked for it
      (define (sigA t)
   (* 0.5 ((getwaveform (fourth f) last-wave-sigA) (* t (first f) twopi sr/inv))))
;  (* 0.1 (pul (* t (first f) twopi sr/inv))))
   (define (sigB t)
          (* 0.5 ((getwaveform (fifth f) last-wave-sigB) (* t (second f) twopi sr/inv))))
;              (* 0.4 (pul (* t (second f) twopi sr/inv))))
  (define (sigC t)
    (* 0.5 ((getwaveform (sixth f) last-wave-sigC) (* t (third f) twopi sr/inv))))
;  (* 0.9 (pul (* t (third f) twopi sr/inv))))

   
    (define (sigA2 t)
      (* 0.5 (pul (* t (first f) twopi sr/inv))))

    (define (sigB2 t)
          (* 0.5 (saw (* t (second f) twopi sr/inv))))

    
    (define (sigC2 t)
          (* 0.5 (tri (* t (second f) twopi sr/inv))))

    
    
    ;;FRAMERATE is how many frames will contain each note in the SID FILE. 
    (define FRAMERATE 441) ;
    
;    (define sigA ;lead
 ;     (if (not (= (first f) 0))      
  ;    (rsound->signal/left (Square FRAMERATE .5 (first f) samplerate))
   ;   silence))
    
   
      
    
 ;  (define sigB
 ;       (if (not (= (second f) 0))      
 ;           (rsound->signal/left (Sawtooth FRAMERATE .7 (second f)  samplerate))
 ;       silence))
    
 ;   (define sigC
 ;     (if (not (= (third f) 0))      
 ;       (rsound->signal/left (Sinewave FRAMERATE 1 (third f) samplerate))
 ;       silence)) 
    
    
    ;A First Voice
    ;B Second Voice
    ;C Rythym Voice?
    
    (define AB  (signal-+s (list (rsound->signal/right (fun->mono-rsound FRAMERATE samplerate sigA))
                                 (rsound->signal/right (fun->mono-rsound FRAMERATE samplerate sigB))
                                 (rsound->signal/left (fun->mono-rsound FRAMERATE samplerate sigA2))
                                 (rsound->signal/left (fun->mono-rsound FRAMERATE samplerate sigC2))
                                                              
                                 
                                 ))) ;first two waveforms summed together
    
    
    (define AC  (signal-+s (list (rsound->signal/left (fun->mono-rsound FRAMERATE samplerate sigC)) 
                                 (rsound->signal/left (fun->mono-rsound FRAMERATE samplerate sigA))
                                 (rsound->signal/left (fun->mono-rsound FRAMERATE samplerate sigB2))
                                
                                 ))) ;first two waveforms summed together
    
    
   ; (define ABC (signal-+s (list AB sigC)))
    ;(define Mono-All(fun->mono-rsound FRAMERATE samplerate AB))
   (define Stereo-All (funs->stereo-rsound FRAMERATE samplerate AB AC))

    Stereo-All;return waveform
 
  ;  Mono-All ;;return the complete waveform
 )    
     
  ;;now we have list of rsounds we can combine...
;  

  
(define mapped-tune (map Freq+Waveform (enumerate-interval 1 2999))) ;about 120s/2min
(define pieces (map makeframe mapped-tune))
(define song (rsound-append* pieces)) 
  
  
     
     
     
     
     

;(define Frame (cadr z))
;(define Freq  (cadddr z))

