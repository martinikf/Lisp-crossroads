;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Martinik.lisp -  - 2. úkol PP3
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída semaphore
;;;

#|
DOKUMENTACE
-----------

Před načtením souboru načtěte knihovnu micro-hraphics, a soubory 04_light.lisp, 05.lisp

NOVÉ VLASTNOSTI

semaphore-type:   Typ semaforu, lze přepínat za běhu. Jsou definovány 2 stavy: :vehicle, :pedestrian.
semaphore-phase:  Reprezentuje aktuální fázi semaforu.
phase-count:      Počet fází semaforu. Pouze ke čtení.
radius:           Poloměr světla v semaforu, ovlivňuje celkovou velikost semaforu.
lights-count      Vrátí počet světel v semaforu. Pouze ke čtení.

NOVÉ ZPRÁVY

next-phase        Přepne semafor do další fáze. Pokud je zpráva poslána s nepovinným argumentem window, automaticky překreslí okno.

|#

;(defvar *semaphores*)
;(setf *semaphores*
;      '(:vehicle 3 (:phases (1 0 0) (1 1 0) (0 0 1) (0 1 0)) 
;      (:pedestrian 2 (:phases ) ))

(defclass semaphore (abstract-picture)
  ((semaphore-type :initform :vehicle)
   (semaphore-phase :initform 0)
   (phase-count :initform 4)
   (radius :initform 10)))

(defmethod initialize-instance ((s semaphore) &key)
  (call-next-method)
  (make-items s))

;;;
;;; Vlastnosti
;;;

(defmethod radius ((s semaphore))
  (slot-value s 'radius))

(defmethod set-radius ((s semaphore) value)
  (unless (> value 0)
    (error "Radius should be a non negative number"))
  (setf (slot-value s 'radius) value)
  s)

(defmethod semaphore-type ((s semaphore))
  (slot-value s 'semaphore-type))

(defmethod do-set-semaphore-type ((s semaphore) type phase-count)
  (setf (slot-value s 'semaphore-type) type)
  (setf (slot-value s 'phase-count) phase-count)
  (make-items s T)
  s)

(defmethod set-semaphore-type ((s semaphore) value)
  (case value
    (:pedestrian (do-set-semaphore-type s :pedestrian 2))
    (:vehicle (do-set-semaphore-type s :vehicle 4))
    (otherwise (error "Unknown semaphore-type")))
  s)

(defmethod semaphore-phase ((s semaphore))
  (slot-value s 'semaphore-phase))

(defmethod set-semaphore-phase ((s semaphore) value)
  (unless (< value (phase-count s))
    (error "Semaphore phase is out of bounds"))
  (setf (slot-value s 'semaphore-phase) value)
  (refresh-lights s)
  s)

(defmethod phase-count ((s semaphore))
  (slot-value s 'phase-count))

(defmethod lights-count ((s semaphore))
  (case (semaphore-type s)
    (:pedestrian 2)
    (:vehicle 3)))

;;;
;;; Zprávy
;;;

(defmethod next-phase ((s semaphore) &optional window)
  (if (= (- (phase-count s) (semaphore-phase s)) 1)
      (set-semaphore-phase s 0)
    (set-semaphore-phase s (1+ (semaphore-phase s))))
  (refresh-lights s)
  (when window (redraw window))
  s)

(defmethod refresh-lights ((s semaphore))
  (cond 
   ((eql (semaphore-type s) :vehicle)
    (case (semaphore-phase s)
      (0 (set-lights s T nil nil))
      (1 (set-lights s T T nil))
      (2 (set-lights s NIL NIL T))
      (3 (set-lights s NIL T NIL))))
   ((eql (semaphore-type s) :pedestrian)
    (case (semaphore-phase s)
      (0 (set-lights s T NIL))
      (1 (set-lights S NIL T))))
   (t (error "Semaphore phases are not defined, refresh-lights")))
  s)

(defmethod set-lights ((s semaphore) L1 L2 &optional L3)
  (let ((items (items s)))
    (if L1
        (turn-on (car items))
      (turn-off (car items)))
    (if L2
        (turn-on (cadr items))
      (turn-off (cadr items)))
    (when (eql (semaphore-type s) :vehicle)
      (if L3
          (turn-on (caddr items))
        (turn-off (caddr items))))
    s))

;;;
;;; Vytvoření items
;;;
;Funguje jen pro 4 rotace,

(defmethod make-items ((s semaphore) &optional type-change)
  (let ((old (when type-change (items s)))
        (item-list (make-item-list s)))
    (item-list-lights s item-list)
    (if type-change
        (let* ((x1 (x (car (items (car (last old))))))
               (y1 (y (car (items (car (last old))))))
               (x2 (x (cadr (items (car (last old))))))
               (y2 (y (cadr (items (car (last old))))))
               (angle (cond
                       ((= x1 x2) (if (< y1 y2)
                                      (/ pi 2)
                                    (- (/ pi 2))))
                       (t (if (< x1 x2)
                              0
                            pi)))))
          (setf item-list (list (car item-list) (cadr item-list) (cadddr item-list)))
          (move (rotate (do-set-items s item-list) angle (make-instance 'point)) x1 y1))
      (do-set-items s item-list))))

(defmethod make-item-list ((s semaphore))
  (list (set-radius (move (make-instance 'light) 
                          (slot-value s 'radius) (slot-value s 'radius))
                    (slot-value s 'radius))
        (set-radius (move (make-instance 'light)
                          (slot-value s 'radius) (* 3 (slot-value s 'radius)))
                    (slot-value s 'radius))
        (when (eql (semaphore-type s) :vehicle) 
          (set-radius (move (make-instance 'light) 
                            (slot-value s 'radius) (* (slot-value s 'radius) 5))
                      (slot-value s 'radius)))
        (set-filledp 
         (set-items (make-instance 'polygon) 
                    (list
                     (make-instance 'point) 
                     (move (make-instance 'point) 
                           (* 2 (slot-value s 'radius))
                           0)
                     (move (make-instance 'point) 
                           (* 2 (slot-value s 'radius))
                           (* 2 (slot-value s 'radius) (lights-count s)))
                     (move (make-instance 'point) 
                           0
                           (* 2 (slot-value s 'radius) (lights-count s)))))
         T)))

(defmethod item-list-lights ((s semaphore) item-list)
  (set-on-color (car item-list) :red)
  (if (eql (semaphore-type s) :vehicle) 
      (progn
        (turn-off (set-on-color (cadr item-list) :orange))
        (turn-off (set-on-color (caddr item-list) :green)))
    (progn
      (turn-off (set-on-color (cadr item-list) :green)))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída crossroads
;;;

#|
DOKUMENTACE
-----------
NOVÉ VLASTNOSTI

crossroads-phase:   Fáze ve které se nachazí semafory v křižovatce.
phase-count:        Počet fází křižovatky. Pouze ke čtení, je určená daným programem.
program:            Program semaforů v křižovatce.
semaphores:         Vrátí seznam všech semaforů v křižovatce. Pouze ke čtení.
items:              Seznam grafických objektů a semaforů.

NOVÉ ZPRÁVY

next-phase          Přepne křižovatku do další fáze a nastaví všem svým semaforům fázi podle programu. Pokud obashuje nepovinný parametr window, překreslí okno.

|#

(defclass crossroads (abstract-picture)
  ((crossroads-phase :initform 0)
   (program :initform '())))

(defmethod check-items ((c crossroads) item-list)
  (do-check-items c item-list))

;;;
;;; Vlastnosti
;;;

(defmethod crossroads-phase ((c crossroads))
  (slot-value c 'crossroads-phase))

(defmethod set-crossroads-phase ((c crossroads) value)
  (unless (< value (phase-count c))
    (error "Crossroads phase is out of bounds"))
  (setf (slot-value c 'crossroads-phase) value)
  (set-semaphores c)
  c)

(defmethod program ((c crossroads))
  (slot-value c 'program))

(defmethod set-program ((c crossroads) program)
  (setf (slot-value c 'program) program)
  c)

(defmethod semaphores ((c crossroads))
  (let ((list '()))
    (dolist (item (items c))
      (when (typep item 'semaphore)
        (setf list (cons item list))))
    (reverse list)))

(defmethod phase-count ((c crossroads))
  (length (program c)))

;;;
;;; Zprávy
;;;

(defmethod next-phase ((c crossroads) &optional window)
  (if (= (- (phase-count c) (crossroads-phase c)) 1)
      (set-crossroads-phase c 0)
    (set-crossroads-phase c (1+ (crossroads-phase c))))
  (when window (redraw window))
  c)

(defmethod set-semaphores ((c crossroads))
  (let ((semaphores (semaphores c)))
    (dolist (item (nth (crossroads-phase c) (program c)))
      (set-semaphore-phase (car semaphores) item)
      (setf semaphores (cdr semaphores)))
    c))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testování
;;;

(defun junction ()
  (let* ((s1 (move (rotate (make-instance 'semaphore) (/ pi 2) (make-instance 'point)) 125 60))
         (s2 (move (make-instance 'semaphore) 105 130))
         (s3 (move (rotate (make-instance 'semaphore) (- (/ pi 2)) (make-instance 'point)) 175 150))
         (s4 (move (rotate (make-instance 'semaphore) pi (make-instance 'point)) 195 80))
         (sp1 (move (rotate (set-semaphore-type (set-radius (make-instance 'semaphore) 5) :pedestrian) (/ pi 2) (make-instance 'point)) 110 100))
         (sp2 (move (set-semaphore-type (set-radius (make-instance 'semaphore) 5) :pedestrian) 145 135))
         (sp3 (move (rotate (set-semaphore-type (set-radius (make-instance 'semaphore) 5) :pedestrian) (- (/ pi 2)) (make-instance 'point)) 190 110))
         (sp4 (move (rotate (set-semaphore-type (set-radius (make-instance 'semaphore) 5) :pedestrian) pi (make-instance 'point)) 155 75))
         (road (set-items (make-instance 'picture) (list 
                                                     (set-filledp (set-color (set-items (make-instance 'polygon) (list
                                                                                                                  (move (make-instance 'point) 20 110)
                                                                                                                  (move (make-instance 'point) 20 100)
                                                                                                                  (move (make-instance 'point) 70 100)
                                                                                                                  (move (make-instance 'point) 70 110))) :white) T)
                                                     (set-filledp (set-color (set-items (make-instance 'polygon) (list
                                                                                                                  (move (make-instance 'point) 280 110)
                                                                                                                  (move (make-instance 'point) 280 100)
                                                                                                                  (move (make-instance 'point) 230 100)
                                                                                                                  (move (make-instance 'point) 230 110))) :white) T)
                                                     (set-filledp (set-color (set-items (make-instance 'polygon) (list
                                                                                                                  (move (make-instance 'point) 155 5)
                                                                                                                  (move (make-instance 'point) 145 5)
                                                                                                                  (move (make-instance 'point) 145 50)
                                                                                                                  (move (make-instance 'point) 155 50))) :white) T)
                                                     (set-filledp (set-color (set-items (make-instance 'polygon) (list
                                                                                                                  (move (make-instance 'point) 155 205)
                                                                                                                  (move (make-instance 'point) 145 205)
                                                                                                                  (move (make-instance 'point) 145 160)
                                                                                                                  (move (make-instance 'point) 155 160))) :white) T)
                                                     (set-filledp (set-color (set-items (make-instance 'polygon) (list 
                                                                                                                  (move (make-instance 'point) 0 80)
                                                                                                                  (move (make-instance 'point) 0 130)
                                                                                                                  (move (make-instance 'point) 125 130)
                                                                                                                  (move (make-instance 'point) 125 210)
                                                                                                                  (move (make-instance 'point) 175 210)
                                                                                                                  (move (make-instance 'point) 175 130)
                                                                                                                  (move (make-instance 'point) 297 130)
                                                                                                                  (move (make-instance 'point) 297 80)
                                                                                                                  (move (make-instance 'point) 175 80)
                                                                                                                  (move (make-instance 'point) 175 0)
                                                                                                                  (move (make-instance 'point) 125 0)
                                                                                                                  (move (make-instance 'point) 125 80))) :gray) T))))
          (cros (set-program (set-items (make-instance 'crossroads) 
                                        (list s1 s2 s3 s4 sp1 sp2 sp3 sp4 road)) 
                             '((0 0 0 0 1 1 1 1) (0 1 0 1 0 0 0 0) 
                               (0 2 0 2 0 0 0 0) (0 3 0 3 0 0 0 0) 
                               (0 0 0 0 1 1 1 1) (1 0 1 0 0 0 0 0) 
                               (2 0 2 0 0 0 0 0) (3 0 3 0 0 0 0 0)))))
    cros))

(defun start-junction (junction window)
  (loop
   (next-phase junction window)
   (sleep 3)))
    
#|
(setf w (set-shape (make-instance 'window) (junction)))
(start-junction (shape w) w)
|#