;++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++
;;; If conditions
;++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Controlling the robot orientation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (north? robot-or)
  (if (eq? robot-or 'north) 
      #t 
      #f
  )
)

(define (west? robot-or)
  (if (eq? robot-or 'west) 
      #t 
      #f
  )
)

(define (south? robot-or)
  (if (eq? robot-or 'south) 
      #t 
      #f
  )
)
(define (east? robot-or)
  (if (eq? robot-or 'east) 
      #t 
      #f
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Controlling the robot orientation ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Controlling if there is a mark in the cell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mark? maze robot-coordinates)
  (cond ((< 0 (goto maze robot-coordinates)) #t)
         (else #f)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Controlling if there is a wall in front of the robot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (wall? maze robot-coordinates robot-or)
  (cond ((eq? 'w (check-out maze robot-coordinates robot-or)) #t)
        (else #f)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;
; Controlling the word
;;;;;;;;;;;;;;;;;;;;;;;;

(define (if? word)
  (if (eq? word 'if)
      #t
      #f
  )
)

(define (turn-left? word)
  (if (eq? word 'turn-left)
      #t
      #f
  )
)

(define (put-mark? word)
  (if (eq? word 'put-mark)
      #t
      #f
  )
)

(define (get-mark? word)
  (if (eq? word 'get-mark)
      #t
      #f
  )
)

(define (step? word)
  (if (eq? word 'step)
      #t
      #f
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;
; Controlling the word
;;;;;;;;;;;;;;;;;;;;;;;;


;++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++
;;; Main functions
;++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++

;;;;;;;;;;;;;;;;;;;;;
; Stepping operation
;;;;;;;;;;;;;;;;;;;;;

(define (step maze rb-coords rb-or)
  (cond ((wall? maze rb-coords rb-or) rb-coords)
        ((north? rb-or) (list (car rb-coords) (- (cadr rb-coords) 1) ))
        ((west? rb-or)  (list (- (car rb-coords) 1) (cadr rb-coords) ))
        ((south? rb-or) (list (car rb-coords) (+ (cadr rb-coords) 1) ))
        ((east? rb-or)  (list (+ (car rb-coords) 1) (cadr rb-coords) ))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;
; Turning left operation
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (turn-left robot-or) 
  (cond ((north? robot-or) 'west)
        ((west? robot-or) 'south)
        ((south? robot-or) 'east)
        ((east? robot-or) 'north)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;
; Putting mark operation
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put-mark maze rb-coords)
  (define old-val (goto maze rb-coords))
  (subst (+ old-val 1) maze rb-coords)
)

;;;;;;;;;;;;;;;;;;;;;;;;;
; Getting mark operation
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-mark maze rb-coords)
  (define old-val (goto maze rb-coords))
  (subst (- old-val 1) maze rb-coords)
)

;;; Continue later. We need to figure out how are we going to handle with the situation variable.
;;; PROGRAM FINISHED, GIVE AS A LIST?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simulate function - Main operation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (simulate rb-state expr program lm)

  (define maze      (car rb-state) )
  (define rb-coords (cadr rb-state) )
  (define rb-or     (caddr rb-state) )
  
  (define (simulate-helper expr acsec maze rb-coords rb-or l lm program)
    
    (cond ( (= lm 0) (list acsec maze rb-coords rb-or l lm) )
        ( (= l 0) (list acsec maze rb-coords rb-or l lm) )
        ( (null? expr) (list acsec maze rb-coords rb-or l lm) )
        (#t (execute-proc (list expr) acsec maze rb-coords rb-or l lm program))
        )
  )
  
  (let ( (result (simulate-helper expr '() maze rb-coords rb-or 1 (+ lm 1) program) ) )
  (list (car result) (list (ge result 1) (ge result 2) (ge result 3) ) ) )
)
  
        
;++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++
;;; Helper functions
;++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Skips "procedure <name>" to "<body>"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (jump-into-proc proc)
  (cond ((list? (car proc)) (car proc))
        (#t (jump-into-proc (cdr proc)))
        )
)

;;;;;;;;;;;;;;;;;;;;;;
; Gets procedure name
;;;;;;;;;;;;;;;;;;;;;;

(define (get-proc-name proc)
    (cadr proc)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Finds procedure from procedure-name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-procedure proc-name program)
  (define (find-proc-helper proc-name program q)
    (cond   ((null? program) (- 1)) 
            ((eq? (get-proc-name (car program)) proc-name) q)
            (#t (find-proc-helper proc-name (cdr program) (+ q 1)))
            )
    )
  (find-proc-helper proc-name program 0)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Gets the procedure from program.
; If it's not inside the program list, returns empty list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-proc program proc-name)
  (define proc-order (find-procedure proc-name program))
  
  (cond ((= (- 1) proc-order) '())
        (#t (ge program proc-order))
  )
)

;legality CDR MUHABBETI IF KISMI
(define (execute-proc proc acsec maze rb-coords rb-or l lm program)
  
  (cond ( (= l 0) (list acsec maze rb-coords rb-or 0 lm) )
        ( (= lm 0) (list acsec maze rb-coords rb-or 0 lm) )
        ( (null? proc) (list acsec maze rb-coords rb-or 1 lm) )
        ( (list? (car proc) ) (let ( (l (execute-proc (car proc) acsec maze rb-coords rb-or l lm program) ) )
                             (execute-proc (cdr proc) (car l) (ge l 1) (ge l 2) (ge l 3) (ge l 4) lm program) ) )
        
        ( (if? (car proc) ) ;;Needs check eleman hesabi cdr falan filan GE 2 KOYUYORUM
                            (cond ( (answer-q (cadr proc) maze rb-coords rb-or) 
                                    (execute-proc (list (ge proc 2)) acsec maze rb-coords rb-or l lm program) )
                                  (#t (execute-proc (list (ge proc 3)) acsec maze rb-coords rb-or l lm program ) )
                             )
                            )
        
        ( (turn-left? (car proc) ) (execute-proc (cdr proc) (append acsec '(turn-left) ) maze rb-coords (turn-left rb-or) l lm program) )

        ( (step? (car proc) )      (cond ( (wall? maze rb-coords rb-or ) (list acsec maze rb-coords rb-or 0 lm) )
                                       (#t (execute-proc (cdr proc) (append acsec '(step) ) maze (step maze rb-coords rb-or) rb-or l lm program) )
                                       )
                                   )
        ( (put-mark? (car proc) )  (execute-proc (cdr proc) (append acsec '(put-mark) ) (put-mark maze rb-coords) rb-coords rb-or l lm program) )

        ( (get-mark? (car proc) )  (cond ( (mark? maze rb-coords) (execute-proc (cdr proc) (append acsec '(get-mark) ) (get-mark  maze rb-coords) rb-coords rb-or l lm program))
                                         ( #t (list acsec maze rb-coords rb-or 0 lm) ) )
                                   )
        ( (null? (get-proc program (car proc) ) ) (list acsec maze rb-coords rb-or l lm)  )
        (#t (let ( (z (execute-proc (jump-into-proc (get-proc program (car proc) ) ) acsec maze rb-coords rb-or l (- lm 1) program) ) )
                      (execute-proc (cdr proc) (car z) (ge z 1) (ge z 2) (ge z 3) (ge z 4) lm program) ) )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; returns element from the list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (ge l q)
  (cond ((null? l) l)
        ((= 0 q) (car l))
        (#t (ge (cdr l) (- q 1)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; returns list from the list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (ge2 l q)
  (cond ((null? l) l)
        ((= 0 q) l)
        (#t (ge2 (cdr l) (- q 1)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Returns the result of the if condition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (answer-q q maze rb-coords rb-or)
  (cond ((eq? q 'wall?)  (wall? maze rb-coords rb-or))
        ((eq? q 'mark?)  (mark? maze rb-coords))
        ((eq? q 'north?) (north? rb-or))
        )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Change element inside the list of lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (subst new maze pos)
    (define (apply-xy fn x y maze)
      (define (apply-at fn list pos)
        (cond ((= pos 0) (cons (fn (car list)) (cdr list)))
              (#t (cons (car list) (apply-at fn (cdr list) (- pos 1))))
              )
        )    

      (apply-at (lambda (line) (apply-at fn line y)) maze x)
      )

    (apply-xy (lambda (x) new) (cadr pos) (car pos) maze)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check out a value inside the maze
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-out maze rb-coords rb-or)
  (cond ((north? rb-or) (goto maze (list (car rb-coords) (- (cadr rb-coords) 1) )))
        ((west? rb-or)  (goto maze (list (- (car rb-coords) 1) (cadr rb-coords) )))
        ((south? rb-or) (goto maze (list (car rb-coords) (+ (cadr rb-coords) 1) )))
        ((east? rb-or)  (goto maze (list (+ (car rb-coords) 1) (cadr rb-coords) )))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Returns the value inside the maze
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (goto maze rb-coords)  
  (define (goto-helper l x)
      (cond ((= x 0) (car l))
        (else (goto-helper (cdr l) (- x 1)))
      )
    )

  (goto-helper (goto-helper maze (cadr rb-coords) ) 
               (car rb-coords) )
)
