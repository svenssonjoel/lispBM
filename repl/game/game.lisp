
;; grid of rooms
;; extract room x/y by (ix (ix map-of-rooms x) y)  
(define map-of-rooms
    '(("test_room.lisp" not-a-room)
      ("snake_room.lisp" not-a-room)))

(define game-state '((room-cid . -1)))

;; Load and execute a room. after a room function completes to execute
;; it can garbage collected. 
(define load-room
    (lambda (pos)
      (let (((x . y) pos)
            (room-file (ix (ix map-of-rooms y) x))
            (room-h (fopen room-file "r"))
            (room (load-file room-h))
            (room-fun (read-eval-program room))
            (room-cid (spawn room-file room-fun)))
        {
        (fclose room-h)
        (setq game-state (setassoc game-state 'room-cid room-cid))
        })))

;; Load the tile system
(define load-tile-system (lambda ()
  {
    (var tile-file (fopen "room_tiles.lisp" "r"))
    (var tile-code (load-file tile-file))
    (fclose tile-file)
    (read-eval-program tile-code)
  }))

(load-tile-system)

(define game-running t)

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event) {
        (var room-cid (assoc game-state 'room-cid))
        (if (> room-cid 0) {
            (print "killing the room thread")
            (kill room-cid 0)
            (wait room-cid)
            })
        (custom-destruct w)
        (setq game-running nil)
        }
        (progn  
          (yield 5000)
          (event-loop w)))))


;; Define player interface 

(define look (lambda (x)
               (send (assoc game-state 'room-cid) `(look ,x))))

(define go (lambda (x)
             (send (assoc game-state 'room-cid) `(go ,x ,(assoc game-state 'main-cid)))))

(define open (lambda (x)
               (send (assoc game-state 'room-cid) `(open ,x))))

(define help
    (lambda ()
        {
        (print "You interact with the environment by writing code in the REPL.")
        (print "The following commands control the players interaction with the room.")
        (print " - (look <argument>) : example (look 'wizard) to look at the wizard")
        (print " - (go <argument>) : example (go 'north)")
        (print " - (open <argument>) : example (open 'door)")
        (print "")
        (print "The arguments vary depending on room but should make sense if looking at the graphical representation")
        (print "")
        (print "Arbitrary lisp code can be entered into the REPL in order to solve the rooms.")
        }))
            


;; Start the game 
(sdl-init)
(define win (sdl-create-window "GAME" 400 400))
(define rend (sdl-create-soft-renderer win))

(sdl-renderer-set-color rend 0 0 0)
(sdl-clear rend)
(sdl-renderer-set-color rend 255 255 255)
(sdl-set-active-renderer rend) ;; Connect the renderer to the display library

(define font-file (fopen "Ubuntu-Regular.ttf" "r"))
(define font (load-file font-file))
(define ttf (ttf-prepare font 32 'indexed4 "abcdefghijklmnopqrstuvxyz1234567890+-*/"))
(define aa-green '(0 17408 39168 65280))
(define disp (img-buffer 'rgb332 400 400))

(spawn 100 event-loop win)

(setq game-state (acons 'disp disp game-state))

;;(print game-state)

;; TODO: put everything needed by the room code into game-state.

;; testing and example

(define room-x 0)
(define room-y 1)
(define done nil)

(load-room (cons room-x  room-y))

(define main-loop
    (lambda () {
      (setq game-state (acons 'main-cid (self) game-state))
      (loopwhile (not done) {
       (recv ((room-change (? dir))
              (match dir
                     (north (setq room-y (- room-y 1)))
                     (south (setq room-y (+ room-y 1)))
                     (east  (setq room-x (+ room-x 1)))
                     (west  (setq room-x (- room-x 1)))))
             (_ (print "You are a bit sneaky!"))
             )
       (if (and (< room-y (length map-of-rooms))
                (< room-x (length (ix map-of-rooms room-y))))
           (if (eq (type-of (ix (ix map-of-rooms room-y) room-x)) type-array)
               (load-room (cons room-x  room-y))
               (printf "Your magic is strong!"))
           (print "Where are you going?"))
       })
      (print "exit main loop")
      }))

(spawn main-loop)
                    
        
 
               
                      
                       
