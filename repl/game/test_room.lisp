
(define test_room_persistant_assoc '())

;; Create the room tile map with variety: 0=floor, 1=wall, 2=hieroglyph, 3=door
(define room-tiles [ 1 2 1 3 3 1 2 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 1 1 1 1 1 1 1 ])

(print "Room tiles loaded:")
(print room-tiles)


;; room thread 
(lambda (game-state)
  {
    ;; Get display buffer from game state
    (var disp (assoc game-state 'disp))
    
    (loopwhile t {

        ;; Render the room using the consolidated function
        (render-room-from-tiles disp room-tiles 80 120 280 250)
        (disp-render disp 0 0 (list))
          
        ;; Handle messages
        (recv-to 0.1  ; Wait 10ms for messages
                   (look
                    {
                    (print "You are in an ancient pyramid chamber. Hieroglyphs cover the north wall, and a sealed door blocks the way forward.")
                    })
                   
                   ((examine hieroglyphs) 
                    {
                    (print "The hieroglyphs seem to be Lisp expressions: (+ ? 7) = 12. What is ?")
                    })
                   
                   ((solve (? expr))
                    {
                    (if (eq expr 5)
                        {
                        (setq test_room_persistant_assoc 
                              (acons 'door-open t test_room_persistant_assoc))
                        (print sender "The seal glows and the door grinds open!")
                        }
                        (print sender "The hieroglyphs remain unchanged. Try again."))
                    })
                   
                   ((go north )
                    {
                    (if (assoc test_room_persistant_assoc 'door-open)
                        (print sender '(room-change 0 . 1))
                        (print sender "The door is sealed shut."))
                    })
                   
                   (('quit) break)    ; Ad
                   d quit message handler
                   (('no-more) break)
                   
                   (timeout ())
                   ((? x) (print x)))  ; Timeout - continue loop
          })
    })
