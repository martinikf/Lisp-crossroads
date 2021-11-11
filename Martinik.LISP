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

V globální proměnné *semaphores* je uložený seznam seznamů, který reprezentuje typy semaforů. 
Lze tak jednoduše přidat nový semafor přidáním: (:název (barvy) (fáze)), kde i-tý prvek seznamu zastupuje i-té světlo od vrchu.

NOVÉ VLASTNOSTI

semaphore-type:   Typ semaforu, lze přepínat za běhu, ale restartuje pozici do levého horního rohu.
semaphore-phase:  Reprezentuje aktuální fázi semaforu.
phase-count:      Počet fází semaforu. Pouze ke čtení.
radius:           Poloměr světla v semaforu, ovlivňuje celkovou velikost semaforu.


NOVÉ ZPRÁVY

next-phase        Přepne semafor do další fáze. Pokud je zpráva poslána s nepovinným argumentem window, automaticky překreslí okno.

|#

(defvar *semaphores*)
(setf *semaphores*
      '((:vehicle (:red :orange :green) ((1 0 0) (1 1 0) (0 0 1) (0 1 0)))
      (:pedestrian (:red :green) ((1 0) (0 1)))
      (:monster (:red :orange :green :blue :purple) ((1 1 0 0 0) (1 0 1 0 1) (0 0 1 1 1)))))

(defclass semaphore (abstract-picture)
  ((semaphore-type :initform :vehicle)
   (semaphore-phase :initform 0)
   (radius :initform 10)))

(defmethod initialize-instance ((s semaphore) &key)
  (call-next-method)
  (make-items s))

;;;
;;; Vlastnosti
;;;

(defmethod definition ((s semaphore))
  (find (semaphore-type s) *semaphores* :key #'car))

(defmethod lights ((s semaphore))
  (length (colors s)))

(defmethod colors ((s semaphore))
  (cadr (definition s)))

(defmethod phases ((s semaphore))
  (caddr (definition s)))

(defmethod phase-count ((s semaphore))
  (length (phases s)))

(defmethod radius ((s semaphore))
  (slot-value s 'radius))

(defmethod set-radius ((s semaphore) value)
  (unless (> value 0)
    (error "Radius should be a non negative number"))
  (setf (slot-value s 'radius) value)
  s)

(defmethod semaphore-type ((s semaphore))
  (slot-value s 'semaphore-type))

(defmethod set-semaphore-type ((s semaphore) value)
  (unless (find value *semaphores* :key #'car)
     (error "Unknown semaphore type"))
  (setf (slot-value s 'semaphore-type) value)
  (make-items s)
  s)

(defmethod semaphore-phase ((s semaphore))
  (slot-value s 'semaphore-phase))

(defmethod set-semaphore-phase ((s semaphore) value)
  (unless (< value (phase-count s))
    (error "Semaphore phase is out of bounds"))
  (setf (slot-value s 'semaphore-phase) value)
  (set-phase s)
  s)

;;;
;;; Zprávy
;;;

(defmethod next-phase ((s semaphore))
  (if (= (- (phase-count s) (semaphore-phase s)) 1)
      (set-semaphore-phase s 0)
    (set-semaphore-phase s (1+ (semaphore-phase s))))
  s)

(defmethod set-phase ((s semaphore))
  (let ((items (items s))
        (phase (nth (semaphore-phase s) (phases s))))
    (dotimes (i (lights s))
      (unless (typep (nth i items) 'light)
        (error "Inconsistent semaphore items in set-phase"))
      (if (= (nth i phase) 1)
          (turn-on (nth i items))
        (turn-off (nth i items))))
    s))

(defmethod make-items ((s semaphore))
   (let ((lights '()))
    (dotimes (i (lights s))
      (setf lights (cons 
                    (turn-off (set-radius (set-on-color (move (make-instance 'light) (radius s) (* (+ (* 2 i) 1) (radius s))) (nth i (colors s))) (- (radius s) 1))) 
                    lights)))
    (setf lights (reverse lights))
    (setf (cdr (last lights))
                     (list (set-filledp  (set-items 
                                          (make-instance 'polygon)
                                          (list 
                                           (move (make-instance 'point) 0 0)
                                           (move (make-instance 'point) (* 2 (radius s)) 0)
                                           (move (make-instance 'point) (* 2 (radius s)) (* 2 (radius s) (lights s)))
                                           (move (make-instance 'point) 0 (* 2 (radius s) (lights s)))))
                                         T)))
    (do-set-items s lights)
    (set-phase s)
    s))

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

(defclass crossroads (picture)
  ((crossroads-phase :initform 0)
   (program :initform '())))

;;;
;;; Vlastnosti
;;;

(defmethod crossroads-phase ((c crossroads))
  (slot-value c 'crossroads-phase))

(defmethod set-crossroads-phase ((c crossroads) value)
  (unless (< value (phase-count c))
    (error "Crossroads phase is out of bounds"))
  (setf (slot-value c 'crossroads-phase) value)
  (set-semaphores-phase c)
  c)

(defmethod program ((c crossroads))
  (slot-value c 'program))

(defmethod set-program ((c crossroads) program)
  (setf (slot-value c 'program) program)
  (set-semaphores-phase c)
  c)

(defmethod semaphores ((c crossroads) &optional (shape c))
  (let ((sems (list :delete)))
    (dolist (item (items shape))
      (cond ((typep item 'semaphore) (nconc sems (list item)))
            ((typep item 'abstract-picture) (nconc sems (semaphores c item)))))
    (cdr sems)))

(defmethod phase-count ((c crossroads))
  (length (program c)))

;;;
;;; Zprávy
;;;

(defmethod next-phase ((c crossroads))
  (if (= (- (phase-count c) (crossroads-phase c)) 1)
      (set-crossroads-phase c 0)
    (set-crossroads-phase c (1+ (crossroads-phase c))))
  c)

(defmethod set-semaphores-phase ((c crossroads))
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
                                        (list s1 (set-items (make-instance 'picture) (list (set-items (make-instance 'crossroads) (list s2)))) s3 (set-items (make-instance 'picture) (list (set-items (make-instance 'crossroads) (list (set-items (make-instance 'picture) (list s4)))))) (set-items (make-instance 'crossroads) (list sp1 (set-items (make-instance 'picture) (list sp2)) sp3 (set-items (make-instance 'crossroads) (list sp4)))) road)) 
                             '((0 0 0 0 1 1 1 1) (0 1 0 1 0 0 0 0) 
                               (0 2 0 2 0 0 0 0) (0 3 0 3 0 0 0 0) 
                               (0 0 0 0 1 1 1 1) (1 0 1 0 0 0 0 0) 
                               (2 0 2 0 0 0 0 0) (3 0 3 0 0 0 0 0)))))
    cros))

(defun start-junction (junction window)
  (loop
   (redraw window)
   (next-phase junction)
   (sleep 3)))
    
#|
(semaphores (junction))
(setf w (set-shape (make-instance 'window) (junction)))
(start-junction (shape w) w)
|#