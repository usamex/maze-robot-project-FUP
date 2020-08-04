
(define (create-programs pop-size depth)
  (define (helper rep acc)
    (cond ((= rep 0) acc)
      (else (helper (- rep 1) (append acc (list (list (list 'procedure 'start (random-code depth) ) ) ) ) ) ) 
    )
  )
  (helper pop-size '())
)
(define (print-result ll)
  (display (car ll))
  (newline)
  (if (null? (cdr ll)) '() (print-result (cdr ll)))
)
(define (evolve pairs threshold stack_size)
  (define (evolve-helper population pairs threshold stack_size pop-size)
    (print-result (evaluate population pairs threshold stack_size))
    (evolve-helper (generate-new-programs (sort-by-fitness population pairs threshold stack_size) pop-size) pairs threshold stack_size pop-size )
    
  )
  (define first-gen (create-programs 10 2))
  (evolve-helper first-gen pairs threshold stack_size 10)
)


(define (generate-new-programs population pop-size)
  (define mutate-op (mutate-select-prog population (floor (/ pop-size 10)) ) )
  (define crossover-op (crossover-select-prog population (* (floor (/ pop-size 10)) 8) ) )
  (define select-op (select-new-prog population (floor (/ pop-size 10)) ) )
  (append mutate-op (append crossover-op select-op) )
)

(define (mutate-select-prog population repetition)
  (define (helper pop rep acc)
    (cond ((= rep 0) acc)
      (else (helper pop (- rep 1) (append acc (list (list (mutate (select population 5) ) ) ) ) ) ) 
    )
  )
  (helper population repetition '())
)


(define (crossover-select-prog population repetition)
  (define (helper pop rep acc)
    (cond ((= rep 0) acc)
      (else (helper pop (- rep 1) (append acc (list (list (crossover (select population 5) (select population 5) ) ) ) ) ) ) 
    )
  )
  (helper population repetition '())
)

(define (select-new-prog population repetition)
  (define (helper pop rep acc)
    (cond ((= rep 0) acc)
      (else (helper pop (- rep 1) (append acc (list (select population 5) ) ) ) ) 
    )
  )
  (helper population repetition '())
)

(define (seconds lst)
  (map cadr lst)
)

(define (sort-by-fitness population pairs threshold stack_size)
  (seconds (evaluate population pairs threshold stack_size))
)

(define (mutate i) (list 'procedure 'start (replace-random-subtree (jump-into-proc (car i)) (random-code 2))) )

(define (crossover i j) (list 'procedure 'start (replace-random-subtree (jump-into-proc (car i) ) (random-subtree (jump-into-proc (car j) ) ) ) ) )


(define select-random
  (lambda (ls)
    (let ((len (length ls)))         ;; find out how long the list is
      (list-ref ls (random len)))
  )
)  ;; pick one from 0 to the end

(define (random-list size-list limit)
  (define (random-list-helper size-list limit acc)
    (cond ((= 0 size-list) acc)
      (else (random-list-helper (- size-list 1) limit (append acc (list (random limit) ) ) ) )
    )
  )
  (random-list-helper size-list limit '())
)

(define (select population tournament-size)
  (let ((size (length population)))
    (list-ref population (nMin (random-list tournament-size size) ) ) 
  )
)










;NOTE - Utility Funcs


(define (list-head list k)
  (sublist list 0 k))


(define (sublist l x y)
  (define (sublist-helper l x y sub)
    (if (or (= x y) (<= (length l) x)) sub (sublist-helper l (+ x 1) y (append sub (list (list-ref l x)))))
  )
  (sublist-helper l x y '())
)

(define (congruential-rng seed)
  (let ((a 16807 #|(expt 7 5)|#)
        (m 2147483647 #|(- (expt 2 31) 1)|#))
    (let ((m-1 (- m 1)))
      (let ((seed (+ (remainder seed m-1) 1)))
        (lambda (b)
          (let ((n (remainder (* a seed) m)))
            (set! seed n)
            (quotient (* (- n 1) b) m-1)))))))

(define random (congruential-rng 12345))

(define (codesize code)
  (cond ((null? code) 0)
    ((list? code) (calc-len-prog code))
    (else 1)
  )
)



(define (nMin alist)
  (if (null? (cdr alist))
      (car alist)
      (if (< (car alist) (nMin (cdr alist)))
          (car alist)
          (nMin (cdr alist))
      )
  )
)

(define select-random2
  (lambda (ls)
    (let* ((len (length ls)) (rand-num (random len)) )         ;; find out how long the list is
      (cons rand-num (list (list-ref ls rand-num) ) )
    )
  )
)  ;; pick one from 0 to the end


(define action-table '(turn-left step put-mark get-mark) )
(define question-table '('(if wall?) '(if mark?)))
(define functions '(turn-left step put-mark get-mark '(if mark?) '(if wall?)))
(define (random-func-selection ll)
    (select-random ll ) 
)

(define (random-code depth)
  (define (random-code-helper depth code)
    (cond ((= 0 depth) code)
      (else (random-code-helper (- depth 1) (append code (list (random-func-selection functions) ) ) ) ) 
    )
  )
  (random-code-helper depth '())
)

(define (random-subtree i)
  (define selection (select-random2 i) )
  (cond ((eq? (cadr selection) 'if) (list 'if (list-ref (+ (car selection) 1) i) ) )
        (else (cadr selection))
  )
)

; Hatali olabilir bunun nedeni de listleri degerlendirmememiz. flatten islemi onceden yapilabilir. Bakilacak denenecek

(define (replace-random-subtree i replacement)
  (define (repl-rand-sub-help i replacement change new)
    (cond ( (and (= 0 change) (eq? (car i) 'if) ) (append new (append replacement (cdr (cdr i)) ) ) )
          
          ( (and (= 0 change) (not (null? new) ) (eq? (car (list-tail new (- (length new) 1) ) ) 'if) ) 
            (append (append (list-head new (-  (length new) 1) ) (list replacement) ) (cdr i) ) )
          
          ( (= 0 change) (append (append new (list replacement) ) (cdr i) ) )
          (else (repl-rand-sub-help (cdr i) replacement (- change 1) (append new (list (car i) ) ) ) )
    )
  )
  (repl-rand-sub-help i replacement (random (length i)) '() )
)







;ANCHOR - SECOND HOMEWORK CODE DON'T TOUCH



(define (is-less? result1 result2)

    (cond ( (or (null? result1) (null? result2) (< (car result1) (car result2) ) ) #t)

          ( (> (car result1) (car result2) ) #f)

          (else (is-less? (cdr result1) (cdr result2) ) )

    ) 

)



;NOTE - This function inserts values to the sorted list.

; if the list is empty then return a single-element list

; if current element >= value insert value in current position

; otherwise keep building the list by adding current element and advancing recursion



(define (insert-result all-results new-result)

    (cond ( (null? (car new-result) ) all-results)

          ( (null? all-results) (list new-result) )

          ( (is-less? (car new-result) (car (car all-results) ) ) (cons new-result all-results) )

          (#t (cons (car all-results) (insert-result (cdr all-results) new-result ) ) )

    )

)



(define (evaluate prgs pairs threshold stack-size)

  (define (evaluate-helper prgs pairs threshold stack-size overall-results)

    (cond ( (null? prgs) overall-results)

          (else (let*  ( (result (solve (car prgs) pairs threshold stack-size '(0 0 0 0) #t) )

                         (prg-result (list result (car prgs)))

                       )

                      (evaluate-helper (cdr prgs) pairs threshold stack-size (insert-result overall-results prg-result) )

                )

          )

    )

  )

  (evaluate-helper prgs pairs threshold stack-size '())

)



(define (add num ll q)

  (define (add-helper num ll q n-ll)

    (cond ((= q 0) (append n-ll (list (+ (car ll) num) ) (cdr ll) ) )

          (else (add-helper num (cdr ll) (- q 1) (append n-ll (list (car ll) ) ) ) )

    )

  )

  (add-helper num ll q '())

)



(define (solve prg pairs threshold stack-size result first)

  

  (define (solve-helper prg pair threshold stack-size result first)

    (define output (simulate (car pair) 'start prg stack-size) )

    ;listed in lexicographical order

    (define delays  (list 

                          (delay (man-dist-maze (car (cadr output) ) (car (cadr pair)) 0) )

                          (delay (robot-conf-dist (cdr (cadr output) ) (cdr (cadr pair) ) ) )

                          (delay (calc-len-prog prg) ) 

                          (delay (calc-len-prog (car output) ) ) 

                    )

    )



    (define (solver delays threshold result first q)

      (cond ( (= q (length delays) ) result)

            ( (and (not first) (= q 2) ) (solver delays threshold result #f (+ q 1) ) )

            ( (> (+ (force (ge delays q) ) (ge result q) ) (ge threshold q) ) '() )

            (else (solver delays threshold (add (force (ge delays q) ) result q) first (+ q 1) ) )

      )

    )



    (solver delays threshold result first 0)

  )

  

  (cond ((null? pairs) result)

        (else (let ( (new-result (solve-helper prg (car pairs) threshold stack-size result first) ) )

                    (if (null? new-result ) '() (solve prg (cdr pairs) threshold stack-size new-result #f) )

              )

        )

  )

)





;NOTE - This produces the First and the Second Outputs. Use man-dist-maze for first output and robot-conf-dist for the second output

(define (man-dist num1 num2) (abs (- num1 num2) ) )



(define (man-dist-maze maze1 maze2 sum)

  (define (man-dist-list list1 list2 sum)

      (cond ( (null? list1) sum)

            ( (null? list2) sum)

            ( (not (number? (car list1) ) ) (man-dist-list (cdr list1) list2 sum) )

            ( (not (number? (car list2) ) ) (man-dist-list list1 (cdr list2) sum) )

            (#t (man-dist-list (cdr list1) (cdr list2) (+ sum (man-dist (car list1) (car list2) ) ) ) )

      )

  )



  (cond ((null? maze1) sum)

        ((null? maze2) sum)

        (#t (let ( (sm (man-dist-list (car maze1) (car maze2) 0) ) )

                   (man-dist-maze (cdr maze1) (cdr maze2) (+ sum sm) ) 

            ) )

  )

)



(define (robot-conf-dist rb-state1 rb-state2)

    

    (define x-dist (man-dist (car (car rb-state1) ) (car (car rb-state2) ) ) )

    (define y-dist (man-dist (cadr (car rb-state1) ) (cadr (car rb-state2) ) ) )

    (define rb-or1  (cadr rb-state1) )

    (define rb-or2  (cadr rb-state2) )



    (if (eq? rb-or1 rb-or2)

      (+ x-dist y-dist)

      (+ 1 x-dist y-dist)

    )

)



;NOTE This produces the Third and the Fourth Outputs. Use calc-len-prog 



;;;;;;;;;;;;;;;;;;;;;;

; Third output

; The length of the program is the number of commands, tests, procedure calls and procedure declarations in the whole program. 

; We do not count the keywords "procedure" and "if" nor empty sequences.

; Fourth output

; Number of steps (commands) needed by the program to produce the final configuration.

; It is the number of commands in the output of the simulation for the program.

;;;;;;;;;;;;;;;;;;;;;;

(define (len-of-list ll)

    (define (len-of-list-helper ll s) (if (null? ll) s (len-of-list-helper (cdr ll) (+ s 1) ) ) )

    (len-of-list-helper ll 0)

)



(define (is-element? ll w)

    (cond ( (null? ll) #f)

          ( (eq? (car ll) w) #t)

          (#t (is-element? (cdr ll) w) )

          )

)



(define (remove-els pred elim-lst lst)

    (cond ( (null? lst) '() )

          ( (not (pred elim-lst (car lst) ) ) (cons (car lst) (remove-els pred elim-lst (cdr lst) ) ) )

          (else (remove-els pred elim-lst (cdr lst) ) )

    )

)





    

(define (flatten-lst lst)

    (define (iter lst acc)

      (cond

        ( (null? lst) acc)

          ( (list? (car lst) ) (iter (cdr lst) (iter (car lst) acc) ) )

          (else (iter (cdr lst) (append acc (list (car lst) ) ) ) )

      )

    )

    (iter lst '() )

)

(define (calc-len-prog prg)

    (define flat-prg (flatten-lst prg))

    (define flagged-words '(procedure if) )

    (len-of-list (remove-els is-element? flagged-words flat-prg) ) 

)





;ANCHOR - FIRST HOMEWORK CODE DON'T TOUCH



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

  (cond ((list? (ge proc 2)) (ge proc 2))

        (#t (list (ge proc 2) ) )

  )

)

 

;;;;;;;;;;;;;;;;;;;;;;

; Gets procedure name

;;;;;;;;;;;;;;;;;;;;;;

 

(define (get-proc-name proc) 

  (cadr proc) )

 

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
