;;;; Soli Deo Gloria
;;;; spalakod@cs.cmu.edu

#lang racket

(require json)

;;;; A Tiling Window Manager for OS X

(require
 (file "/Applications/Zephyros.app/Contents/Resources/libs/zephyros.rkt"))

;; initialize vars
(define screens (all-screens))

(define screen-dimensions (make-hash))

(map
 (lambda (x)
   (hash-set!
    screen-dimensions x (frame-without-dock-or-menu x)))
 screens)

(define main-scr (screen (focused-window)))

(define default-width (inexact->exact
                       (*
                        (hash-ref
                         (hash-ref screen-dimensions main-scr) 'w)
                        (/ 60 100))))

(define scum-width (-
                    (hash-ref
                     (hash-ref
                      screen-dimensions main-scr) 'w)
                    default-width))

;; Single screen layout:
;; One main panel - like XMonad.
;; A Side pane that contains the remaining windows
(define (onescr/scale-main-window)
  (set-frame
   (focused-window)
   (make-hash
    (list
     (cons 'x 0)
     (cons 'y 0)
     (cons 'w default-width)
     (cons 'h (hash-ref
               (hash-ref
                screen-dimensions main-scr) 'h))))))

(define (onescr/scale-non-focused-windows)
  (let* ((main-win (focused-window))
         
         (rem-wins (filter
                    (lambda (x)
                      (not (=  x main-win)))
                    (visible-windows)))
         (y-values (y-grid
                    (hash-ref
                     (hash-ref
                      screen-dimensions main-scr) 'h)
                    (length rem-wins)))
         
         (x-values (map
                    (lambda (_) default-width) rem-wins)))
    (map
     (lambda (win-x-y)
       (let ((win (first win-x-y))
             (x-y (second win-x-y)))
         (focus-window win)
         (set-top-left win x-y)))
     (map
      list
      rem-wins
      (map
       (lambda (x-y)
         (let ((x (first x-y))
               (y (second x-y)))
           (make-hash
            (list
             (cons 'x x)
             (cons 'y y)))))
       (map list x-values y-values))))

    (focus-window main-win)))

;; provides coords to stack windows
;; along the vertical axis
(define (y-grid y-len num-windows)
  (for/list [(i (range num-windows))]
    (* i 20)))

(define (onescr/scale-windows)
  (onescr/scale-main-window)
  (onescr/scale-non-focused-windows))
