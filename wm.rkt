;;;; Soli Deo Gloria
;;;; spalakod@cs.cmu.edu

#lang racket

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


