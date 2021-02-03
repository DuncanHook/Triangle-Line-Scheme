;Defines a point in the form (x . y) where x can be accessed
;with car, and y can be accessed with cdr
(define (make-point number1 number2) (cons number1 number2))  

;Rounds a floating point number to a certain
;amount of decimals
(define (round-float a x)
(let ((power (expt 10 x)))
   (/ (round (* power a)) power)))

;Checks to see if the area of a line is zero
;if it is this procedure returns #t, if not then #f
(define (is-line a b c)
(let ((X1 (car a)) (Y1 (cdr a))
       (X2 (car b)) (Y2 (cdr b))
       (X3 (car c)) (Y3 (cdr c)))
     (=(/(+(* X1 (- Y2 Y3)) (* X2 (- Y3 Y1)) (* X3 (- Y1 Y2)))2) 0)))


;finds the distance between two points, basically deciding
;the length of a side of the triangle.
(define (distance a b) 
  (let ((X1 (car a)) (Y1 (cdr a))
         (X2 (car b)) (Y2 (cdr b)))
         (round-float (sqrt (+ (expt (- X2 X1) 2) (expt (- Y2 Y1) 2))) 5)))

;Finds the perimeter of the triangle by adding together
;the lengths of all the sides. 
(define (perimeter a b c)
(let ((X1 (car a)) (Y1 (cdr a))
       (X2 (car b)) (Y2 (cdr b))
       (X3 (car c)) (Y3 (cdr c)))
(round-float (+ (distance (make-point X1 Y1) (make-point X2 Y2)) 
   (distance (make-point X1 Y1) (make-point X3 Y3))
   (distance (make-point X2 Y2) (make-point X3 Y3))) 5)))


;finds the area between 3 points
(define (area a b c)
(let ((X1 (car a)) (Y1 (cdr a))
       (X2 (car b)) (Y2 (cdr b))
       (X3 (car c)) (Y3 (cdr c)))
     (round-float (abs(/(+(* X1 (- Y2 Y3)) (* X2 (- Y3 Y1)) (* X3 (- Y1 Y2)))2)) 5)))

;Uses the law of cosines to to find an angle of a given corner
;of the triangle. 
(define (angle a b c)
(let ((X (distance a b))
       (Y (distance b c))
       (Z (distance a c)))
(acos (/ (-(+(expt Y 2) (expt Z 2)) (expt X 2)) (* Y Z 2)))))


;combines all of the aspects of a triangle and outputs
;them in an easily readable way. 
(define (calculate-triangle a b c)
(display "Side 1 = ")
(display (distance a b))
(newline)
(display "Side 2 = ")
(display (distance b c))
(newline)
(display "Side 3 = ")
(display (distance a c))
(newline)
(display "Perimeter = ")
(display (perimeter a b c))
(newline)
(display "Area = ")
(display (area a b c))
(newline)
(display "Angle 1 = ")
(display (round-float (angle a b c) 5))
(display "  ")
(display (round-float (/(* 180 (angle a b c)) 3.141592653589793) 5))
(newline)
(display "Angle 2 = ")
(display (round-float (angle b c a) 5))
(display "  ")
(display (round-float (/(* 180 (angle b c a)) 3.141592653589793) 5))
(newline)
(display "Angle 3 = ")
(display (round-float (angle c a b) 5))
(display "  ")
(display (round-float (/(* 180 (angle c a b)) 3.141592653589793) 5)))