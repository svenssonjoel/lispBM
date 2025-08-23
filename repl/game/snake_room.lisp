

(define ernst-hugo '(sn se we we ws))

(define snake-room-persistant-assoc
    (acons 'player-y 300
    (acons 'player-x 300
    (acons 'wizard-y 300
    (acons 'wizard-x 200
    (acons 'cleared nil              
               '()))))))

;; Create the room tile map with variety: 0=floor, 1=wall, 2=hieroglyph, 3=door
(define room-tiles [ 1 2 1 3 3 1 2 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 1 1 1 1 1 1 1 ])

;; room thread 
(lambda (game-state)
  {
  ;; Get display buffer from game state
  (var disp (assoc game-state 'disp))
  
  (print "An evil snake is blocking the exit of this room!")
  (print "You must vanquish the evil monstrosity!")
  (print "To defeat this foe, you must know its name!")
  (print "Today you are lucky, I know the beast's name and it is ernst-hugo.")
  (print "Free a path to the door by reciting the correct incantation in the LANGUAGE OF THE GODS!")

  (loopwhile t {

        (if (not (assoc snake-room-persistant-assoc 'cleared))
            (cond ((eq nil ernst-hugo) {
                   (print "Excellent! You cleared a path to the door.")
                   (setassoc snake-room-persistant-assoc 'cleared t)
                   })
                  ((<= (length ernst-hugo) 1) {
                   (print "Fantastic! You cleared a path to the door.")
                   (print "Virtously you let the snake live.")
                   (setassoc snake-room-persistant-assoc 'cleared t)
                   })
                  )
            )
        

        (img-clear disp)
        (render-room-from-tiles disp room-tiles)
        (render-snake-from-path disp 100 100 ernst-hugo)

        (render-wizard disp
                       (assoc snake-room-persistant-assoc 'wizard-x)
                       (assoc snake-room-persistant-assoc 'wizard-y))
        (render-player disp
                       (assoc snake-room-persistant-assoc 'player-x)
                       (assoc snake-room-persistant-assoc 'player-y))

        (disp-render disp 0 0 (list))

        ;; Handle messages
        (recv-to 0.1  ; Wait 10ms for messages
                 (look
                  {
                  (print "its dark")
                  })
                 
                 ((examine wizard) 
                  {
                  (print "The wizard is old and wise.")
                  })
                 
                 ((go north )
                  {
                  (if (assoc snake-room-persistant-assoc 'door-open)
                      (print sender '(room-change 0 . 1))
                      (print sender "The door is sealed shut."))
                  })
                 
                 (('quit) break)    ; Add quit message handler
                 (('no-more) break)
                 
                 (timeout ())
                 ((? x) (print x)))  ; Timeout - continue loop
        })
  })
