;; Room Tile System
;; 8x8 grid = 64 bytes, each tile is 50x50 pixels (400x400 total)
;; Tile types: 0=empty, 1=wall, 2=wall+hieroglyph, 3=door

;; Tile rendering functions
(define render-tile (lambda (img tile-x tile-y tile-type)
  {
    (var pixel-x (* tile-x 50))
    (var pixel-y (* tile-y 50))
    
    (cond 
      ((eq tile-type 0) 
        {
          ;; Empty space - dark stone floor
          (img-rectangle img pixel-x pixel-y 50 50 0x202020)
          ;; Add subtle floor texture
          (img-rectangle img (+ pixel-x 10) (+ pixel-y 10) 4 4 0x303030)
          (img-rectangle img (+ pixel-x 35) (+ pixel-y 35) 4 4 0x303030)
        })
        
      ((eq tile-type 1)
        {
          ;; Wall - light gray stone
          (img-rectangle img pixel-x pixel-y 50 50 0x808080)
          ;; Add texture lines
          (img-line img pixel-x (+ pixel-y 12) (+ pixel-x 49) (+ pixel-y 12) 0x606060)
          (img-line img pixel-x (+ pixel-y 25) (+ pixel-x 49) (+ pixel-y 25) 0x606060)
          (img-line img pixel-x (+ pixel-y 37) (+ pixel-x 49) (+ pixel-y 37) 0x606060)
          ;; Vertical texture
          (img-line img (+ pixel-x 15) pixel-y (+ pixel-x 15) (+ pixel-y 49) 0x606060)
          (img-line img (+ pixel-x 35) pixel-y (+ pixel-x 35) (+ pixel-y 49) 0x606060)
          ;; Add right and bottom border lines
          (img-line img (+ pixel-x 49) pixel-y (+ pixel-x 49) (+ pixel-y 49) 0x606060)
          (img-line img pixel-x (+ pixel-y 49) (+ pixel-x 49) (+ pixel-y 49) 0x606060)
        })
        
      ((eq tile-type 2)
        {
          ;; Wall with hieroglyph
          (img-rectangle img pixel-x pixel-y 50 50 0x808080)
          ;; Add texture
          (img-line img pixel-x (+ pixel-y 12) (+ pixel-x 49) (+ pixel-y 12) 0x606060)
          (img-line img pixel-x (+ pixel-y 37) (+ pixel-x 49) (+ pixel-y 37) 0x606060)
          ;; Add right and bottom border lines
          (img-line img (+ pixel-x 49) pixel-y (+ pixel-x 49) (+ pixel-y 49) 0x606060)
          (img-line img pixel-x (+ pixel-y 49) (+ pixel-x 49) (+ pixel-y 49) 0x606060)
          ;; Add hieroglyph - larger cross pattern
          (img-rectangle img (+ pixel-x 20) (+ pixel-y 8) 10 34 0xFF4000)
          (img-rectangle img (+ pixel-x 8) (+ pixel-y 20) 34 10 0xFF4000)
          ;; Add dots for decoration
          (img-rectangle img (+ pixel-x 12) (+ pixel-y 12) 6 6 0xFF4000)
          (img-rectangle img (+ pixel-x 32) (+ pixel-y 32) 6 6 0xFF4000)
        })
        
      ((eq tile-type 3)
        {
          ;; Door - changes based on state
          (var door-open (assoc test_room_persistant_assoc 'door-open))
          (if door-open
            ;; Open door - dark opening with depth
            {
              (img-rectangle img pixel-x pixel-y 50 50 0x000000)
              ;; Add some depth shading
              (img-rectangle img pixel-x pixel-y 50 8 0x404040)
              (img-rectangle img pixel-x (+ pixel-y 42) 50 8 0x404040)
            }
            ;; Closed door - wall with large seal
            {
              (img-rectangle img pixel-x pixel-y 50 50 0x808080)
              ;; Large golden seal in center
              (img-rectangle img (+ pixel-x 12) (+ pixel-y 12) 26 26 0xFFFF00)
              (img-rectangle img (+ pixel-x 18) (+ pixel-y 18) 14 14 0xFF4000)
              ;; Inner detail
              (img-rectangle img (+ pixel-x 22) (+ pixel-y 22) 6 6 0xFFFF00)
            })
        })
        
)
  }))

;; Render entire room from tile array (8x8) with background and characters
(define render-room-from-tiles (lambda (img tile-array wizard-x wizard-y player-x player-y)
  {
    ;; Clear background
    (img-rectangle img 0 0 400 400 0x202020)
    
    ;; Render tiles
    (var tile-index 0)
    (var y 0)
    (loopwhile (< y 8)
      {
        (var x 0)
        (loopwhile (< x 8)
          {
            (var tile-type (bufget-u8 tile-array tile-index))
            (render-tile img x y tile-type)
            (setq x (+ x 1))
            (setq tile-index (+ tile-index 1))
          })
        (setq y (+ y 1))
      })
    
    ;; Render characters at specified positions
    (render-wizard img wizard-x wizard-y)
    (render-player img player-x player-y)
  }))

;; Helper function to set a tile in the byte array
(define set-tile (lambda (tile-array x y tile-type)
  (bufset-u8 tile-array (+ (* y 8) x) tile-type)))

;; Helper function to get a tile from the byte array  
(define get-tile (lambda (tile-array x y)
  (bufget-u8 tile-array (+ (* y 8) x))))

;; Character rendering functions (not bound to tile grid)
(define render-wizard (lambda (img x y)
  {
    ;; Wizard robe - dark blue
    (img-rectangle img (+ x 18) (+ y 15) 14 30 0x0000FF)
    ;; Wizard hat - pointed hat
    (img-rectangle img (+ x 22) (+ y 8) 6 12 0x0000FF)
    (img-rectangle img (+ x 24) (+ y 5) 2 8 0x0000FF)
    ;; Face - light skin
    (img-rectangle img (+ x 22) (+ y 12) 6 6 0xFFCCBB)
    ;; Staff - brown with crystal
    (img-rectangle img (+ x 12) (+ y 10) 3 25 0x8B4513)
    (img-rectangle img (+ x 10) (+ y 8) 7 4 0x00FFFF)
  }))

(define render-player (lambda (img x y)
  {
    ;; Player body - green tunic
    (img-rectangle img (+ x 20) (+ y 18) 10 22 0x00AA00)
    ;; Head - light skin
    (img-rectangle img (+ x 22) (+ y 12) 6 6 0xFFCCBB)
    ;; Hair - brown
    (img-rectangle img (+ x 21) (+ y 10) 8 4 0x8B4513)
    ;; Arms - skin colored
    (img-rectangle img (+ x 16) (+ y 20) 4 12 0xFFCCBB)
    (img-rectangle img (+ x 30) (+ y 20) 4 12 0xFFCCBB)
    ;; Legs - brown pants
    (img-rectangle img (+ x 20) (+ y 40) 4 8 0x654321)
    (img-rectangle img (+ x 26) (+ y 40) 4 8 0x654321)
  }))