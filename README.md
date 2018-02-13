# Scheme-SID-Player
Manuel F. Castillo
Organization of Programming Languages
Final Project - Emulating the Sid 6581

readsid.rkt - FINAL PROJECT SUBMISSION, and code that ran during presentation
readsid2.rkt - my attempt at reading register 04 of the wavetable to select which exact waveform, and use of noise
alibi.dmp - laxity's alibi in sid dmp format
syncopated.dmp - laxity's syncopated in sid dmp format
synthony.dmp - laxity's synthony in sid dmp format
iame.dmp - laxity's iame in sid dmp format
storm.dmp - Stormlord 2 C64 game music created by laxity in sid dmp format

In my project I used several techniques developed in class, such as anonymous lambdas and function mapping. To create
triangle waves I used the fourier series implemented by using recursion that returned a function as its result. It was
a little tricky because its not simply returning a function but a function of one argument that has been recursively
created.

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

I was particularly happy I was working in scheme when I had to implement the pulse function,
it just seemed to come so naturally and easily, and one look at the code explains all:


;;pul = pulse
(define (pul t)
(cond ((= (sin t) 0) 0)
((> (sin t) 0) 1)
((< (sin t) 0) -1)
))

The noise function was also easy, but I think it needs a bit more work because there should be a frequency cut-off at
the value t, but besides that it sounds perfectly like white noise:

(define (noi t)
(* (expt  -1 (random 5)) (random) t))




I used the SID 6581 data sheet http://www.waitingforfriday.com/index.php/Commodore_SID_6581_Datasheet  and http://synthdiy.com/files/2001/6581.pdf
to determine which waves to use in readsid-fourer.rkt (in readsid.rkt I simply made it use sawtooth waves and squarewaves).
The reason why I have two file submissions is because although the waveforms are being selected in the second one directly from the wavetable,
it does not sound completely accurate. I think this is attributed to the fact that I did not have time to completely implement the ADSR and pulse width
modulation that the SID chip uses.

Here as you can see, I create a way to test the bits given by the 2-digit hex value in the wavetable that is converted to binary.
In order to convert the hex to binary it was converted to decimal and then to binary using this procedure:

(define dec->bin;convert decimal to binary, Useful when deciphering SID waveform datasheet
(lambda (d)
(cond
((eq? d #f) (list 0)) ;if our table contains ... this will be #f, which should be 0
((< d 1) (list 0))
((= d 1) (list 1))
((> d 1) (append
(dec->bin (floor (/ d 2))) ;floor function gives back the remainder
(list (if (= (modulo d 2) 0) 0 1))))))) ;divide by 2 evenly 0, else 1

The problem here was that I would get 6 digit binary numbers, so I had to pad them with zeros in order to read them
accurately, so I created this procedure:

(define (pad8 ls)
(let ((zeros (- 8 (length ls))))
(if (> (length ls) 7)
ls ;just return no need to pad
(pad8 (cons 0 ls)))))






;;for better abstraction, to be used in deciphering waveform, Control Register (Register 04)
(define bit0/gate eighth )    ;gate bit
(define bit1/sync seventh)  ;sync bit
(define bit2/ringmod sixth) ;ringmod bit
(define bit3/test fifth) ;test bit (if high, locks osc 1 until cleared(low))
(define bit4/tri fourth) ;triangle
(define bit5/saw third)  ;sawtooth
(define bit6/sqr second) ;squarewave
(define bit7/noi first)  ;noise bit



For some reason at times there was no waveform selected, so I had to create default waveforms.


(define last-wave-sigA tri) ;default value is triangle wave
(define last-wave-sigB tri) ;
(define last-wave-sigC tri) ;

I use set! so that I can "remember" which waveform was last selected for that particular signal (sigA, sigB or sigC). That signal was passed
through the variable "whichsignal", so whichever signal it was, that was the signal that would be updated. Somewhat confusing but crucial.

(define (getwaveform R04 whichsignal) ;register 04 bits, update current waveform, return waveform function
(cond ((= (bit4/tri R04) 1) (begin (set! whichsignal tri) tri))
((= (bit5/saw R04) 1) (begin (set! whichsignal saw)  saw))
((= (bit6/sqr R04) 1) (begin (set! whichsignal sqr)  sqr))
((= (bit7/noi R04) 1) (begin (set! whichsignal noi)  noi))
(else
whichsignal)

)) ;noise,test,ringmod not set yet







HOW TO RUN THE CODE:

in order to run readsid.rkt, you simply start the code, it will load.
It may take some time to fully load! When it's done, it will show a GUI that
is self explanatory.

In order to run readsid2.rkt, you simply load it and type on the
interactive window:
(rsound-play song)

It should then play.











