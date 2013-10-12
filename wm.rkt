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

(define (set-dims win w h)
  (set-size
   win (make-hash
        (list
         (cons 'w w)
         (cons 'h h)))))

(define (set-pos win x y)
  (set-top-left
   win (make-hash
        (list
         (cons 'x x)
         (cons 'y y)))))

(define (set-pos-dim win x y w h)
  (set-frame
   win (make-hash (list (cons 'x x)
                        (cons 'y y)
                        (cons 'w w)
                        (cons 'h h)))))

;; Single screen layout:
;; One main panel - like XMonad.
;; A Side pane that contains the remaining windows
(define (onescr/scale-main-window)
  (set-pos-dim
   (focused-window)
   0
   0
   default-width
   (hash-ref
    (hash-ref
     screen-dimensions main-scr) 'h)))

(define (onescr/scale-non-focused-windows)
  (let* ((main-win  (focused-window))
         
         (rem-wins  (filter
                     (lambda (x)
                       (not (=  x main-win)))
                     (all-windows)))

         (stk-wins  (rest rem-wins))

         (tos-win   (first rem-wins))

         (positions (stacked-pos rem-wins)))
    
    (stack stk-wins (drop-last positions))
    (place-at-bottom tos-win (last positions))
    (focus-window tos-win)
    (focus-window main-win)))

(define (drop-last lst)
  (take lst (- (length lst) 1)))

(define (stacked-pos wins)
  (map
   (lambda (i)
     (list default-width (* i 30)))
   (range (length wins))))

(define (stack wins positions)
  (map
   (lambda (win-x-y)
     (let ((win (first win-x-y))
           (x   (first (second win-x-y)))
           (y   (second (second win-x-y))))
       (set-pos-dim win x y scum-width 500)))
   (map
    list wins positions))
  (map focus-window wins))

(define (place-at-bottom win pos)
  (let* ((x (first pos))
         (y (second pos))
         (w scum-width)
         (h (- (hash-ref
                (hash-ref
                 screen-dimensions main-scr) 'h)
               y)))
    (set-pos-dim win x y w h)))

(define (onescr/scale-windows)
  (onescr/scale-main-window)
  (onescr/scale-non-focused-windows))
