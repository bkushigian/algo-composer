#lang rosette

;;; notes.rkt: This module defines notes. A note is represented as an integer
;;; corresponding its MIDI value (thus C4 is defined to bo 60)

(require "io.rkt")
(provide
  (all-from-out 'notes)
  (all-from-out 'intervals)

  pc-up                  ; Ascending list of diatonic C ionian notes
  pc-down                ; Descending list of diatonic C ionian notes
  make-sym-harm          ; Make a symbolic harmony for constraints
  get-notes-in-interval  ; Get all diatonic notes in an interval
  get-surrounding-notes
  parallel?              ; Are the two melodic movements parallel?
  in-range?              ; Is lo <= num < hi?
  step-up?               ; Does a note ascend stepwise to a second note?
  step-down?             ; Does a note descend stepwise to a second note?
  skip-up?               ; Does the first note approach the second note by skip from below?
  skip-down?             ; Does the first note approach the second note by skip from above?
  step?
  skip?
  valid-melodic-minor6?  ; If there is a minor6 skip up, is it followed by a descending step?
  perfect?               ; Is an interval perfect?
  pitch-class            ; Return the number in [0, 12) representing the pitch
  same-pitch-class?      ; Do two notes belong to the same pitch class?
  list->clause           ; Transform (possibly nested) lists of constraints into a single constraint
  )


(module notes racket
        (provide (all-defined-out))
        (define C_ 0)
        (define D_ 2)
        (define E_ 4)
        (define F_ 5)
        (define G_ 7)
        (define A_ 9)
        (define B_ 11)
        (define C#_ 1)
        (define D#_ 3)
        (define E#_ F_)
        (define F#_ 6)
        (define G#_ 8)
        (define A#_ 10)
        (define B#_ C_)
        (define Cb_ B_)
        (define Db_ C#_)
        (define Eb_ D#_)
        (define Fb_ E_)
        (define Gb_ F#_)
        (define Ab_ G#_)
        (define Bb_ A#_)
        (define (add-oct-offset octs note) (+ note (* 12 octs)))

        (define (C [oct 4]) (add-oct-offset (+ oct 1) C_))
        (define (D [oct 4]) (add-oct-offset (+ oct 1) D_))
        (define (E [oct 4]) (add-oct-offset (+ oct 1) E_))
        (define (F [oct 4]) (add-oct-offset (+ oct 1) F_))
        (define (G [oct 4]) (add-oct-offset (+ oct 1) G_))
        (define (A [oct 4]) (add-oct-offset (+ oct 1) A_))
        (define (B [oct 4]) (add-oct-offset (+ oct 1) B_))
        (define (C# [oct 4]) (add-oct-offset (+ oct 1) C#_))
        (define (D# [oct 4]) (add-oct-offset (+ oct 1) D#_))
        (define (E# [oct 4]) (add-oct-offset (+ oct 1) E#_))
        (define (F# [oct 4]) (add-oct-offset (+ oct 1) F#_))
        (define (G# [oct 4]) (add-oct-offset (+ oct 1) G#_))
        (define (A# [oct 4]) (add-oct-offset (+ oct 1) A#_))
        (define (B# [oct 4]) (add-oct-offset (+ oct 1) B#_))
        (define (Cb [oct 4]) (add-oct-offset (+ oct 1) Cb_))
        (define (Db [oct 4]) (add-oct-offset (+ oct 1) Db_))
        (define (Eb [oct 4]) (add-oct-offset (+ oct 1) Eb_))
        (define (Fb [oct 4]) (add-oct-offset (+ oct 1) Fb_))
        (define (Gb [oct 4]) (add-oct-offset (+ oct 1) Gb_))
        (define (Ab [oct 4]) (add-oct-offset (+ oct 1) Ab_))
        (define (Bb [oct 4]) (add-oct-offset (+ oct 1) Bb_)))

(module intervals racket
        (provide (all-defined-out))
        (define uni 0)
        (define min2 1)
        (define maj2 2)
        (define min3 3)
        (define maj3 4)
        (define dim4 4)
        (define per4 5)
        (define aug4 6)
        (define dim5 6)
        (define per5 7)
        (define aug5 8)
        (define min6 8)
        (define maj6 9)
        (define min7 10)
        (define maj7 111)
        (define octave 12)
        (define min9 13)
        (define maj9 14)
        (define min10 15)
        (define maj10 16)
        (define dim11 16)
        (define per11 17)
        (define aug11 18)
        (define dim12 18)
        (define per12 19)
        (define aug12 20)
        (define min13 20)
        (define maj13 21))


(require 'notes)
(require 'intervals)

;;; Pitch Classes of diatonic C ionian going up
(define pc-up (for*/list ([i (in-range -2 9)] [note (list C D E F G A B)]) (note i)))

;;; Pitch Classes of diatonic C ionian going down
(define pc-down (reverse pc-up))

; Make a system
(define (make-sym-harm cantus)
  (define-symbolic* h integer? [(length cantus)])
  h)

(define (get-notes-in-interval note interval)
  (define x (remainder note 12))
  (define neg? (< x 0))
  (define oct (- (quotient note 12) (if neg? 1 0)))  ; The octave offset
  (define pc (if neg? (+ 12 x) x))                 ; The pitch class
  (take (member note (if (< interval 0) pc-down pc-up)) (abs interval)))

(define (get-surrounding-notes note #:above [above 0] #:below [below 10])
  (define x (remainder note 12))
  (define neg? (< x 0))
  (define oct (- (quotient note 12) (if neg? 1 0)))  ; The octave offset
  (define pc (if neg? (+ 12 x) x))                 ; The pitch class
  (define notes-above (take (member note pc-up) above))
  (define notes-below (take (member note pc-down) below))
  (if (or (null? notes-above) (null? notes-below))
    (append (reverse notes-below) notes-above)
    (append (reverse notes-below) (drop notes-above 1))))

;;; RANGE CONSTRAINTS

;;; PREDICATES

;; Do lines (c1 c2) and (h1 h2) move in parallel?
(define (parallel? c1 c2 h1 h2)
  (|| (and (< c1 c2) (< h1 h2)) (and (> c1 c2) (> h1 h2))))


(define (in-range? num lo hi)
  (and (<= lo num) (< num hi)))

(define (step-up? n1 n2)
  (define interval (- n2 n1))
  (or (eq? interval 1) (eq? interval 2)))

(define (step-down? n1 n2)
  (step-up? n2 n1))

(define (step? n1 n2)
  (or (step-up? n1 n2) (step-down? n1 n2)))

(define (skip-up? n1 n2)
  (> 2 (- n2 n1)))

(define (skip-down? n1 n2)
  (skip-up? n2 n1))

; Does n1->n2 count as a skip?
(define (skip? n1 n2)
   (or (skip-up? n1 n2) (skip-down? n1 n2)))

(define (valid-melodic-minor6? n1 n2 n3)
  (=> (= (- n2 n1) min6) (step-down? n1 n2)))

; Perfect consonances include Unison, Fifth, Octave
(define (perfect? c1 s1)
  (define interval (- c1 s1))
  (or (= interval 0) (= interval 7) (= interval 12)))

(define (pitch-class n) (remainder n 12))

(define (same-pitch-class? n1 n2)
  (= (remainder (- n1 n2) 12) 0))

;;; Flatten a list of constraints to a conjunction. This handles nested lists
(define (list->clause xs)
  (if (list? xs) (apply && (map list->clause xs)) xs))


