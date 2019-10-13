#lang rosette

(require "io.rkt")

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

(define (C [oct 4]) (add-oct-offset (+ oct 1) C_))
(define (D [oct 4]) (add-oct-offset (+ oct 1) D_))
(define (E [oct 4]) (add-oct-offset (+ oct 1) E_))
(define (F [oct 4]) (add-oct-offset (+ oct 1) F_))
(define (G [oct 4]) (add-oct-offset (+ oct 1) G_))
(define (A [oct 4]) (add-oct-offset (+ oct 1) A_))
(define (B [oct 4]) (add-oct-offset (+ oct 1) B_))

(define notes (hash 'A A_ 'B B_ 'C C_ 'D D_ 'E E_ 'F F_ 'G G_))  ; Diatonic for now

(define (add-oct-offset octs note) (+ note (* 12 octs)))

;;; Define pitch classes going up
(define pc-up (for*/list ([i (in-range -1 9)] [note (list C D E F G A B)]) (note i)))
;;; Define pitch classes going down
(define pc-down (for*/list ([i (in-range 8 -2 -1)] [note (list B A G F E D C)]) (note i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERATE CONSRAINTS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Make a system
(define (make-sym-harm cantus)
  (define-symbolic* h integer? [(length cantus)])
  h)

(define (make-system cantus)
  (list cantus (make-sym-harm cantus)))

(define (get-notes-in-interval note interval)
  (define x (remainder note 12))
  (define neg? (< x 0))
  (define oct (- (quotient note 12) (if neg? 1 0)))  ; The octave offset
  (define pc (if neg? (+ 12 x) x))                 ; The pitch class
  (take (member note (if (< interval 0) pc-down pc-up)) (abs interval)))

;;; RANGE CONSTRAINTS

; Return a set of constraints determining the possible range of a note
(define (range-constraints note sym)
  (define rng (get-notes-in-interval note -10))
  (apply || (map (λ (x) (= x sym)) rng)))

;; Helper function for range-constraints*
;(define (_range-constraints* cantus syms constraints)
;  (match (cons cantus syms)
;    [(cons null null) constraints]
;    [(cons (cons x xs) (cons y ys))  (_range-constraints* xs ys (and (range-constraints x y) constraints))]
;    [_ #f] ;; Fail
;    ))

; Apply range-constraints to a list of notes and a list of symbolic notes
(define (range-constraints* cantus syms)
  (match (cons cantus syms)
         ;;; Empty? Vacuously harmonizes!
         [(cons '() '()) (list #t)]
         ;;; One note left? They only need to be peftect
         [(cons (cons c '()) (cons h '())) (list (perfect? c h))]
         ;;; Only two notes left? The second-to-last pair must be a minor third
         ;;; if c.f. is on top or a major sixth if c.f. is on bottom. The final
         ;;; interval must be perfect, and the harmony must approach its final
         ;;; note with a half step from below
         [(cons (cons c1 (cons c2 '())) (cons h1 (cons h2 '())))
          (list (step-up? h1 h2) (or (= 3 (- c1 h1)) (= 9 (- h1 c1))) (perfect? c2 h2))]
         [(cons (cons c cs) (cons h hs))
          (cons (range-constraints c h) (range-constraints* cs hs))]))

;;; PREDICATES

;; Do lines (c1 c2) and (h1 h2) move in parallel?
(define (parallel? c1 c2 h1 h2)
  (|| (and (< c1 c2) (< h1 h2)) (and (> c1 c2) (> h1 h2))))

; Do these two notes constitute a valid melodic interval?
(define (legal-melodic-movement? n1 n2)
  (define interval (- n1 n2))
  (or (and (>= interval -5) (<= interval 5))
      (= interval -7)
      (= interval 7)
      (= interval 8)))

(define (in-range? num lo hi)
  (and (<= lo num) (< num hi)))

; Todo: this is stolen from legal-melodic-movement? and should be updated if
; needed
(define (legal-harmonic-interval? n1 n2)
  (define interval (- n1 n2))
  (or (in-range? interval -5 -2)
      (in-range? interval 3 5)
      (in-range? interval 7 10)
      (in-range? interval -9 -6)
      (= interval 0)
      (= interval 12)))

(define (legal-harmonic-movement? c1 c2 h1 h2)
  (and (=> (perfect? c2 h2) (not (parallel? c1 c2 h1 h2)))
       #t))
(define (step-up? n1 n2)
  (define interval (- n2 n1))
  (or (eq? interval 1) (eq? interval 2)))

(define (step-down? n1 n2)
  (define interval (- n2 n1))
  (or (eq? interval 1) (eq? interval 2)))

(define (skip-up? n1 n2)
  (> 2 (- n2 n1)))

(define (skip-down? n1 n2)
  (> 2 (- n1 n2)))

; Does n1->n2 count as a skip?
(define (skip? n1 n2)
   (or (skip-up? n1 n2) (skip-down? n1 n2)))

(define (valid-melodic-minor6? n1 n2 n3)
  (=> (= (- n2 n1) 8) (step-down? n1 n2)))

; Perfect consonances include Unison, Fifth, Octave
(define (perfect? c1 s1)
  (define interval (- c1 s1))
  (or (= interval 0) (= interval 7) (= interval 12)))

(define (pitch-class n) (remainder n 12))

(define (same-pitch-class? n1 n2)
  (= (remainder (- n1 n2) 12) 0))

;;; VOICING CONSTRAINTS
(define (voicing-constraints c1 c2 s1 s2)
  '())

(define (melodic-constraints* line)
  (match line
         [(cons n1 (cons n2 (cons n3 cs)))
          (list* (legal-melodic-movement? n1 n2)
                 (valid-melodic-minor6? n1 n2 n3)
                 (melodic-constraints* (cons n2 (cons n3 cs))))]
         [(cons n1 (cons n2 cs))
          (list* (legal-melodic-movement? n1 n2)
                 (melodic-constraints* (cons n2 cs)))]
         [_ '()]))

(define (harmonic-constraints* cantus harm)
  (match (cons cantus harm)
         [(cons (cons c1 (cons c2 cs))
                (cons h1 (cons h2 hs)))
          (list* (legal-harmonic-movement? c1 c2 h1 h2)
                 (legal-harmonic-interval? c1 h1)
                 (harmonic-constraints* (cons c2 cs) (cons h2 hs)))]
         [(cons (cons c1 cs)
                (cons h1 hs))
          (list* (legal-harmonic-interval? c1 h1)
                 (harmonic-constraints* cs hs))]
         [(cons '() '()) (list #t)]
         [_ (list #f)]))


(define (boundary-constraints cantus harmony)
  (define l (length cantus))

  (and (= (first harmony) (last harmony))
       (or (= (first harmony) (first cantus))
           (= (first harmony) (add-oct-offset -1 (first cantus))))))

(define (constraints cantus)
  (define harmony (make-sym-harm cantus))
  (cons (list (range-constraints* cantus harmony)
              (melodic-constraints* harmony)
              (harmonic-constraints* cantus harmony)
              (boundary-constraints cantus harmony))
        harmony))

;;; Flatten a list of constraints to a conjunction. This handles nested lists
(define (list->clause xs)
  (if (list? xs) (apply && (map list->clause xs)) xs))

(define (harmonize cantus)
  (define c-and-h (constraints cantus))
  (define consts (car c-and-h))
  (define harmony (cdr c-and-h))
  (define model (solve (assert (list->clause consts))))
  (if (unsat? model)
    #f
    (for/list ([h harmony]) (model h))))

(define (harmonize-and-export cantus [note-dur 4] [tempo 110] [instrs (list 0 1 2 3 4 5 6)])
  (define harm (harmonize cantus))
  (if harm
    (display (export-system (list cantus harm) note-dur tempo instrs))
    (display "Couldn't harmonize the system :(")))


;;;; HELPER VALUES FOR DEBUG

(define cf  (list (C 5) (D 5) (E 5) (D 5) (C 5)))
(define cp (make-sym-harm cf))
(define system  (list cf cp))
(define rcs (range-constraints* cf cp))
(define mcs (melodic-constraints* cp))
(define hcs (harmonic-constraints* cf cp))
(define cp-concrete (harmonize cf))
(define exported (export-system (list cf cp-concrete)))
