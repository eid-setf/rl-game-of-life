(defpackage #:rl-game-of-life
  (:use #:cl #:cl-raylib))

(in-package #:rl-game-of-life)

;;;; ---

(defvar +board-size+ 32)

(defvar *board* (make-array (list +board-size+ +board-size+) :initial-element nil))

(defun bound (n)
  (cond
    ((< n 0) (1- +board-size+))
    ((> n (1- +board-size+)) 0)
    (t n)))

(defun bref (i j)
  (aref *board* (bound i) (bound j)))

(defun (setf bref) (val i j)
  (setf (aref *board* (bound i) (bound j)) val))

(defun number-of-neighboars (x y)
  (loop for ci from -1 to 1
        for i = (+ x ci)
        ;; when (<= 0 i (1- +board-size+))
        sum (loop for cj from -1 to 1
                  for j = (+ y cj)
                  when (not (and (= i x) (= j y))) ;don't count the cell
                  count (bref i j))))

(defun next-gen ()
  (let ((acc (make-array (list +board-size+ +board-size+))))
    (dotimes (i +board-size+)
      (dotimes (j +board-size+)
        (setf (aref acc i j) (aref *board* i j))
        (let ((non (number-of-neighboars i j)))
          (if (aref *board* i j)        ;when cell is live
              (if (or (< non 2) (> non 3))
                  (setf (aref acc i j) nil))
              (if (= non 3)
                  (setf (aref acc i j) t))))))
    (setf *board* acc)
    nil))

;;;; Terminal interface

(defun print-board ()
  (dotimes (i +board-size+)
    (dotimes (j +board-size+)
      (if (aref *board* i j)
          (format t "#")
          (format t ".")))
    (format t "~%")))

(defun terminal-main ()
  (print-board)
  (loop (sleep 0.3)
        (next-gen)
        (format t "~%~%~%")
        (print-board)))

;;;; Raylib interface

(defvar screen-size 640)  ;should be a multiple of +board-size+
(defvar square-size (/ screen-size +board-size+))

(defvar frame-counter 0)
(defvar paused? t)

(defun pos->square-index (z)
  "Returns the square index `pos' is in."
  (floor (/ (cond
              ((< z 0) 0)
              ((>= z screen-size) (1- screen-size))
              (t z))
            square-size)))

(defun draw ()
  (with-drawing
    (clear-background :raywhite)
    (if paused?
        (let ((x (get-mouse-x))
              (y (get-mouse-y)))
          (draw-rectangle (* square-size (pos->square-index x))
                          (* square-size (pos->square-index y))
                          square-size square-size
                          :gray)))
    (dotimes (i +board-size+)
      (dotimes (j +board-size+)
        (if (aref *board* i j)
            (draw-rectangle (* i square-size) (* j square-size)
                            square-size square-size
                            :black))
        ;; draw the grid
        (draw-rectangle-lines (* i square-size) (* j square-size)
                              square-size square-size
                              :lightgray)))))

(defun main ()
  (with-window (screen-size screen-size "Game of Life - Press 'P' to edit, 'E' to clear the board")
    (set-target-fps 60)
    (loop until (window-should-close)
          do (if (is-key-pressed :key-p)
                 (setf paused? (not paused?)))
             (if (is-key-pressed :key-e)
                 ;; clear board
                 (setf *board*
                       (make-array (list +board-size+ +board-size+) :initial-element nil)))
             (cond
               (paused?
                (let ((x (get-mouse-x))
                      (y (get-mouse-y)))
                  ;; drawing
                  (if (is-mouse-button-down :mouse-button-left)
                      ;; here aref is used because we don't want drawing to wrap
                      (setf (aref *board*
                                  (pos->square-index x)
                                  (pos->square-index y))
                            t))
                  ;; erasing
                  (if (is-mouse-button-down :mouse-button-right)
                      ;; here aref is used because we don't want erasing to wrap
                      (setf (aref *board*
                                  (pos->square-index x)
                                  (pos->square-index y))
                            nil))))
               (t (if (= (mod frame-counter 20) 0)
                      (next-gen))))
             (draw)
             (incf frame-counter))))
