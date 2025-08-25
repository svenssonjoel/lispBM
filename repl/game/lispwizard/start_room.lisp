
(define start-room-persistant-assoc
    (acons 'player-y 100
    (acons 'player-x 200
    (acons 'wizard-y 100
    (acons 'wizard-x 100
    (acons 'cleared t
    (acons 'door-open nil              
               '())))))))

(define room-tiles [ 1 2 1 1 1 1 2 1
                     2 0 0 0 0 0 0 2
                     1 0 0 0 0 0 0 1
                     1 0 0 0 0 0 0 7
                     1 0 0 0 0 0 0 8
                     1 0 0 0 0 0 0 1
                     2 0 0 0 0 0 0 2
                     1 2 1 1 1 1 2 1 ])

(define start-room-done nil)

;; room thread
(lambda ()
  {
  ;; Get display buffer from game state
  (var disp (assoc game-state 'disp))

;;  (print "The wizard leans towards you and whispers:")
;;  (print "\"The door is blocked by a serpentine monstrosity.\"")
;;  (print "\"To battle this beast you must determine its name.\"")
;;  (print "\"look at the snake (look snake) for clues to its identity.\"")

  (loopwhile (not start-room-done) {

        (if (not (assoc start-room-persistant-assoc 'cleared))
            () ;; room clear logic
            )
        

        (img-clear disp)
        (render-room-from-tiles disp room-tiles)

        (render-wizard disp
                       (assoc start-room-persistant-assoc 'wizard-x)
                       (assoc start-room-persistant-assoc 'wizard-y))
        (render-player disp
                       (assoc start-room-persistant-assoc 'player-x)
                       (assoc start-room-persistant-assoc 'player-y))

        (disp-render disp 0 0 (list))

        ;; Handle messages
        (recv-to 0.1  ; Wait 10ms for messages
                 ((look wizard) {
                  (print "The wizard is old and wise.")
                  })
                 ((look door)
                  (print "There is a door leading east."))                  
                 ((look _) {
                  (print "ugga")
                  })
                  
                 ((go east)
                  (if (not (assoc start-room-persistant-assoc 'door-open))
                      (print "The door is sealed shut.")
                      {
                      (send  (assoc game-state 'main-cid) '(room-change east))
                      (setq start-room-done t)
                      }
                      )
                  )
                 ((go _)
                  (print "There is no passage in that direction!"))
                                  
                 ((open door)
                  {
                  (if (assoc start-room-persistant-assoc 'cleared)
                      {
                      (open-door room-tiles 7 3)
                      (open-door room-tiles 7 4)
                      (setassoc start-room-persistant-assoc 'door-open t)
                      (print "The door opens with a grinding sound of ancient stone.")
                      }
                      (print "Impossible! The door is locked!"))
                  })
                 
                 (quit break)    ; Add quit message handler
                 (no-more break)
                 
                 (timeout ())
                 ((? x) (print x)))  ; Timeout - continue loop
        })
  (print "Leaving the start room.")
  })
