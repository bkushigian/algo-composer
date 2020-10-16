#lang rosette

(require "../notes.rkt")
(require "../io.rkt")


; Return a set of constraints determining the possible range of a note
(define (range-constraints note sym)
  (define rng (get-surrounding-notes note #:above 3 #:below 12))
  (apply || (map (λ (x) (= x sym)) rng)))

; Apply range-constraints to a list of notes and a list of symbolic notes
(define (range-constraints* cantus syms)
  (match (cons cantus syms)
         ;;; Empty? Vacuously harmonizes!
         [(cons '() '()) (list #t)]
         ;;; One note left? They only need to be perfect
         [(cons (cons c '()) (cons h '())) (list (perfect? c h))]
         ;;; Only two notes left? The second-to-last pair must be a minor third
         ;;; if c.f. is on top or a major sixth if c.f. is on bottom. The final
         ;;; interval must be perfect, and the harmony must approach its final
         ;;; note with a half step from below
         [(cons (cons c1 (cons c2 '())) (cons h1 (cons h2 '())))
          (list (step-up? h1 h2) (or (= 3 (- c1 h1)) (= 9 (- h1 c1))) (perfect? c2 h2))]
         [(cons (cons c cs) (cons h hs))
          (cons (range-constraints c h) (range-constraints* cs hs))]))

; Do these two notes constitute a valid melodic interval?
(define (legal-melodic-movement? n1 n2)
  (define interval (- n1 n2))
  (or (and (>= interval -5) (<= interval 5))
      (= interval -7)
      (= interval 7)
      (= interval 8)))

(define (legal-harmonic-movement? c1 c2 h1 h2)
  (and (=> (perfect? c2 h2) (not (parallel? c1 c2 h1 h2)))
       #t))

(define (legal-harmonic-interval? n1 n2)
  (define interval (- n1 n2))
  (or (in-range? interval -5 -2)
      (in-range? interval 3 5)
      (in-range? interval 7 10)
      (in-range? interval -9 -6)
      (= interval 0)
      (= interval 12)))

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

(define (constrain-parallel-motion cantus harmony [percentage 0.5])
  (define motions (- (length cantus) 1))
  (define max-parallels (exact->inexact (real->integer (* percentage motions))))
  (define num-parallels 
    (foldl (lambda (nts acc) (if (apply parallel? nts) (+ 1 acc) acc))
           0
           (for/list ([c cantus]  [c2 (rest cantus)] [h harmony] [h2 (rest harmony)])
                     (list c c2 h h2))))
  (list (<= num-parallels max-parallels)))

(define (constraints cantus #:max-parallel-percent [max-parallel-percent 0.5])
  (define harmony (make-sym-harm cantus))
  (cons (list (range-constraints* cantus harmony)
              (melodic-constraints* harmony)
              (harmonic-constraints* cantus harmony)
              (boundary-constraints cantus harmony)
              (constrain-parallel-motion cantus harmony max-parallel-percent))
        harmony))


(define (harmonize-lower cantus)
  (define c-and-h (constraints cantus))
  (define consts (car c-and-h))
  (define harmony (cdr c-and-h))
  (define model (solve (assert (list->clause consts))))
  (if (unsat? model)
    #f
    (for/list ([h harmony]) (model h))))

(define (harmonize-and-export cantus #:dur [note-dur 4] #:tempo [tempo 110] #:instruments [instrs (list 0 1 2 3 4 5 6)])
  (define harm (harmonize cantus))
  (if harm
    (display (export-system (list cantus harm) #:dur note-dur #:tempo tempo #:instruments instrs))
    (display "Couldn't harmonize the system :(")))
;;;; HELPER VALUES FOR DEBUG

(define cf  (list (C 5) (D 5) (E 5) (D 5) (C 5)))
(define cp (make-sym-harm cf))
(define system  (list cf cp))
(define rcs (range-constraints* cf cp))
(define mcs (melodic-constraints* cp))
(define hcs (harmonic-constraints* cf cp))
(define cp-concrete (harmonize-lower cf))
(define exported (export-system (list cf cp-concrete)))
