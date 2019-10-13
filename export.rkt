#lang racket

(provide export-system)

(define (get-octave note) (-  (quotient note 12) 1))
(define (get-pitchclass note)
  (define pc (remainder note 12))
  (list-ref (list "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B" "B#") pc))

(define (note-name note) (format "~a~a" (get-pitchclass note) (get-octave note)))

(define (mk-instrument n) (format "\"~a\":{\"volume\":1,\"delay\":false,\"reverb\":false}" n))

(define (write-note start note dur instr)
  (format "~a ~a ~a ~a;" start (note-name note) dur instr))

(define (write-note* notes instr [note-dur 4])
  (apply string-append
         (for/list ([(note i) (in-indexed notes)])
                   (write-note (* note-dur i) note note-dur instr))))

(define (write-system* system [note-dur 4] [instrs (list 0 1 2 3 4 5)])
  (apply string-append
         (for/list ([(line instr) (in-indexed system)])
                   (write-note* line (list-ref instrs instr) note-dur))))
(define (export-system system [note-dur 4] [tempo 110] [instrs (list 0 1 2 3 4 5)])
  (define instruments (format "{\"instruments\":{~a}}"
                              (string-join (for/list ([(line i) (in-indexed system)])
                                        (mk-instrument (list-ref instrs i))) ",")))
  (define header (format "~a|~a|" instruments tempo))
  (define notes (write-system* system note-dur instrs))
  (string-append header notes))
