

(define ernst-hugo '(sn se we we ws))

(define snake-room-persistant-assoc
    (acons 'player-y 300
    (acons 'player-x 300
    (acons 'wizard-y 300
    (acons 'wizard-x 200
    (acons 'cleared nil
    (acons 'door-open nil              
               '())))))))

;; Create the room tile map with variety: 0=floor, 1=wall, 2=hieroglyph, 3=door
(define room-tiles [ 1 2 1 5 6 1 2 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 1
                     1 1 1 1 1 1 1 1 ])

(define snake-room-done nil)

;; room thread
(lambda ()
  {
  ;; Get display buffer from game state
  (var disp (assoc game-state 'disp))
  
  (print "An evil snake is blocking the exit of this room!")
  (print "You must vanquish the evil monstrosity!")
  (print "To defeat this foe, you must know its name!")
  (print "Today you are lucky, I know the beast's name and it is ernst-hugo.")
  (print "Free a path to the door by reciting the correct incantation in the LANGUAGE OF THE GODS!")

  (loopwhile (not snake-room-done) {

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
                 ((look wizard)
                  (print "The wizard is old and wise."))
                 ((look snake)
                  (print "The snake is quite scary.\n"))
                 ((look door)
                  (if (assoc snake-room-persistant-assoc 'door-open)
                      (print "The door towards the north is open.\n")
                      (print "The door towards the north is closed.\n")))
                 ((look _) {
                  (print "There is a wizard and a snake in this room!")
                  (print "To the north, behind the snake, there is a closed stone door.\n")
                  })
                  
                 ((go north (? cid))
                  (if (not (assoc snake-room-persistant-assoc 'door-open))
                      (print "The door is sealed shut.")
                      {
                      ;(print "You leave through the door towards the north")
                      (send  (assoc game-state 'main-cid) '(room-change north))
                      (setq snake-room-done t)
                      }
                  ))
                 
                 ((open door)
                  {
                  (if (assoc snake-room-persistant-assoc 'cleared)
                      {
                      (open-door room-tiles 3 0)
                      (open-door room-tiles 4 0)
                      (setassoc snake-room-persistant-assoc 'door-open t)
                      (print "The door opens with a grinding sound of ancient stone.")
                      }
                      (print "Impossible, there is a giant snake in the way"))
                  })
                 
                 (quit break)    ; Add quit message handler
                 (no-more break)
                 
                 (timeout ())
                 ((? x) (print x)))  ; Timeout - continue loop
        })
  (print "Leaving the snake room.")
  })
