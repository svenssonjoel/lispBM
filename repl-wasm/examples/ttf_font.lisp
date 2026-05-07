(wasm-create-canvas 400 80 "TTF Hello World")

(define img (img-buffer 'indexed16 400 80))

(import "https://raw.githubusercontent.com/google/fonts/main/ufl/ubuntu/Ubuntu-Regular.ttf" 'font-data)

(define font (ttf-prepare font-data 50 'indexed16 "Hello, World!"))

(define colors
  (list 0x000000 0x111111 0x222222 0x333333
        0x444444 0x555555 0x666666 0x777777
        0x888888 0x999999 0xAAAAAA 0xBBBBBB
        0xCCCCCC 0xDDDDDD 0xEEEEEE 0xFFFFFF))

(ttf-text img 10 55 colors font "Hello, World!")

(disp-render img 0 0 colors)
