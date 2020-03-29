;;;;; Field Of View (FOV) utility routines for Common LISP

(defpackage #:cl-fov
  (:use #:cl)
  (:export #:bline #:circle-adjacent #:circle-swing #:raycast #:shadowcast))
(in-package #:cl-fov)

(declaim (optimize (speed 3)))
(deftype strictly-positive () '(integer 1 *))

; the Rosetta "Bitmap/Bresenham's line algorithm" code is probably
; faster than this more particular implementation
(defun bline (linef x0 y0 x1 y1)
  "Bresenham's line algorithm with a callback for each point and the ability to abort the line walk early"
  (declare (integer x0 y0 x1 y1))
  (let* ((dx (abs (- x1 x0)))
         (sx (if (< x0 x1) 1 -1))
         (dy (abs (- y1 y0)))
         (sy (if (< y0 y1) 1 -1))
         (err (/ (if (> dx dy) dx (- dy)) 2)))
    (declare (integer sx sy))
    (loop :do
          (unless (funcall linef x0 y0) (return))
          (when (and (eq x0 x1) (eq y0 y1)) (return))
          (let ((e2 err))
            (when (> e2 (- dx))
              (incf err (- dy))
              (incf x0 sx))
            (when (< e2 dy)
              (incf err dx)
              (incf y0 sy))))))

; utility routine for SHADOWCAST
(defun cast-light (startx starty radius blockf litf radiusf
                   row light-start light-end xx xy yx yy)
  (let ((blocked nil) (new-start 0.0))
    (loop :for j :from row :to radius :do
      (loop :with dy = (- j) :for dx :from dy :to 0 :do (block DIST-LOOP
        (let ((rslope (/ (+ dx 0.5) (- dy 0.5)))
              (lslope (/ (- dx 0.5) (+ dy 0.5))))
          (cond
            ((< light-start rslope) (return-from DIST-LOOP))
            ((> light-end lslope) (loop-finish)))
          (let ((curx (+ startx (* dx xx) (* dy xy)))
                (cury (+ starty (* dx yx) (* dy yy))))
            (when (funcall radiusf dx dy)
              (funcall litf curx cury dx dy))
            (if blocked
              (if (funcall blockf curx cury)
                (progn
                  (setf new-start rslope)
                  (return-from DIST-LOOP))
                (progn
                  (setf blocked nil)
                  (setf light-start new-start)))
              (when (and (funcall blockf curx cury) (< j radius))
                (setf blocked t)
                (unless (< light-start lslope)
                  (cast-light startx starty radius blockf litf radiusf
                              (+ j 1) light-start lslope xx xy yx yy))
                (setf new-start rslope)))))))
      (when blocked (loop-finish)))))

; could also do CIRCLE-CACHED (see the cached_circle sub of the
; Game::RaycastFOV Perl module); this is mostly so that there is a
; function with minimal arguments to support with RAYCAST
(defun circle-adjacent (fn startx starty &rest unused)
  "calls fn with the adjacent 8-way points to the given startx and starty"
  (declare (integer startx starty)
           (ignore unused))
  (dolist (p '((1 . 0) (1 . 1) (0 . 1) (-1 . 1)
               (-1 . 0) (-1 . -1) (0 . -1) (1 . -1)))
    (funcall fn (+ startx (car p)) (+ starty (cdr p)))))

(defun circle-swing (fn startx starty radius rotate-by)
  "calls the callback fn for each unique point determined by the given radius and how much the heading should rotate-by each turn within 2pi radians"
  (declare (integer startx starty)
           (strictly-positive radius))
  (setf rotate-by (abs rotate-by))
  (loop :with frad = (+ 0.5 radius)
        :and heading = 0.0
        :and visited = (make-hash-table :test 'equal)
        :while (< heading (* pi 2))
        :do (let* ((nx (+ startx (truncate (* frad (cos heading)))))
                   (ny (+ starty (truncate (* frad (sin heading)))))
                   (where (cons nx ny)))
              (unless (gethash where visited)
                (funcall fn nx ny)
                (setf (gethash where visited) t))
              (incf heading rotate-by))))

; radius is optional as there maybe circle functions that use a set
; radius; other circle types may need more arguments that get dumped
; into args
(defun raycast (circlef linef startx starty &optional radius &rest args)
  "raycast to each point produced by the circlef calling linef for each point between the start and the circle points"
  (apply circlef (lambda (cx cy) (bline linef startx starty cx cy)) startx
           starty radius args))

; this is very generic; points are not checked whether they are within
; the map. much of the logic is punted to the callback functions for
; whether a point is blocked, how to light up a point, and whether the
; dx,dy are within the allowed radius
(defun shadowcast (startx starty radius blockf litf radiusf)
  "shadowcast implementation adapted from roguebasin"
  (funcall litf startx starty 0 0)
  (dolist
      (mult
       '((1 0 0 1) (0 1 1 0) (0 -1 1 0) (-1 0 0 1) (-1 0 0 -1) (0 -1 -1 0)
         (0 1 -1 0) (1 0 0 -1)))
    (apply #'cast-light startx starty radius blockf litf radiusf 1 1.0 0.0
           mult)))
