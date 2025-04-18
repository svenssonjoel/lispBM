# LispBM Display Library

The display extensions contains a graphics library designed for platforms for with very limited memory resources. The drawing routines in the library operate on rectangular images (arrays) of pixels , called an image buffer. 

The values stored in image buffers represents colors via an encoding determined by the image buffer pixel format. A pixel buffer has one of the following formats: 

   - indexed2 : 2 colors (1 bit per pixel)
   - indexed4 : 4 colors (2 bits per pixel)
   - indexed16 : 16 colors (4 bits per pixel)
   - rgb332 : 8Bit color
   - rgb565 : 16bit color
   - rgb888 : 24bit color

Note that the RAM requirement of a 100x100 image is; 

   - at indexed2: 1250 Bytes
   - at indexed4: 2500 Bytes
   - at indexed16: 5000 Bytes
   - at rgb332: 10000 Bytes
   - at rgb565: 20000 Bytes
   - at rgb888; 30000 Bytes

So on an embedded platform you most likely not be able to be working with rgb565, rgb888 other than in very limited areas. 

At the low-level end of things you will want to display graphics onto an display. The interface towards the low-level end needs to be implemented for the particular hardware platform and display. For examples of this see [vesc_express](https://github.com/vedderb/vesc_express/tree/main/main/display). The LBM linux REPL has SDL and png backends for the display library. 

the display library is specifically designed to allow for using many colors simultaneously on screen, without needing to use full screen high-color buffers. This is done by delaying the choice of color mapping in the `indexed2`, `indexed4` and `indexed16` images until they are presented on screen. 

images are rendered onto a display using the function `disp-render`. `disp-render` takes an image, a position (x,y) where to draw the image, and a colormapping that can be expressed as a list of colors. for example: 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(disp-render llama-bin 10 10 '(0x000000 0xFFFFFF))
```


</td>
<td>

<img src=./images/disp-img1.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render llama-bin 20 20 '(0x000000 0xFF0000))
```


</td>
<td>

<img src=./images/disp-img2.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render llama-bin 30 30 '(0x000000 0x00FF00))
```


</td>
<td>

<img src=./images/disp-img3.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render llama-bin 30 30 '(0x000000 0x0000FF))
```


</td>
<td>

<img src=./images/disp-img4.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-clear)
```


</td>
<td>

<img src=./images/disp-img5.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(disp-render img-100-100 0 0 '(0x000000 0xFFFFFF))
```


</td>
<td>

<img src=./images/disp-img6.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render img-100-100 0 100 '(0x000000 0xFF0000))
```


</td>
<td>

<img src=./images/disp-img7.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render img-100-100 100 0 '(0x000000 0x00FF00))
```


</td>
<td>

<img src=./images/disp-img8.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render img-100-100 100 100 '(0x000000 0x0000FF))
```


</td>
<td>

<img src=./images/disp-img9.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render img-100-100 200 0 '(0x000000 0x00FFFF))
```


</td>
<td>

<img src=./images/disp-img10.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render img-100-100 200 100 '(0x000000 0xFF00FF))
```


</td>
<td>

<img src=./images/disp-img11.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>


# Reference


### img-buffer

Allocate an image buffer from lbm memory or from a compactable region. The form of an `img-buffer` expression is `(img-buffer opt-dm format width height)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define my-img (img-buffer 'indexed2 320 200))
```


</td>
<td>

```clj
[1 64 0 200 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
 (define my-dm (dm-create 10000))
 (define my-img (img-buffer my-dm 'indexed2 320 200))
```


</td>
<td>


```clj
[1 64 0 200 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```


</td>
</tr>
</table>




---


### img-buffer?

Checks if the argument is likely to be an image buffer. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-buffer? llama-bin)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-buffer? 'apa)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>




---


### img-blit

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-blit my-img llama-bin 10 10 -1)
```


</td>
<td>

<img src=./images/disp-img12.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-blit my-img llama-bin 10 10 -1 '(rotate 128 128 45))
```


</td>
<td>

<img src=./images/disp-img13.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-blit my-img llama-bin 10 10 -1 '(scale 0.500000f32))
```


</td>
<td>

<img src=./images/disp-img14.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-arc

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1)
```


</td>
<td>

<img src=./images/disp-img15.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1 '(dotted 15 15))
```


</td>
<td>

<img src=./images/disp-img16.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1 '(filled))
```


</td>
<td>

<img src=./images/disp-img17.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1 '(thickness 10))
```


</td>
<td>

<img src=./images/disp-img18.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1 '(rounded))
```


</td>
<td>

<img src=./images/disp-img19.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1 '(dotted 15 15) '(resolution 3))
```


</td>
<td>

<img src=./images/disp-img20.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1 '(thickness 10) '(rounded))
```


</td>
<td>

<img src=./images/disp-img21.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-circle

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-circle my-img 100 100 80 1)
```


</td>
<td>

<img src=./images/disp-img22.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle my-img 100 100 80 1 '(thickness 5))
```


</td>
<td>

<img src=./images/disp-img23.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle my-img 100 100 80 1 '(dotted 14 14))
```


</td>
<td>

<img src=./images/disp-img24.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle my-img 100 100 80 1 '(filled))
```


</td>
<td>

<img src=./images/disp-img25.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-circle my-img 100 100 80 1 '(dotted 14 14) '(resolution 6))
```


</td>
<td>

<img src=./images/disp-img26.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-circle-sector

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-circle-sector my-img 220 40 40 90 200 1)
```


</td>
<td>

<img src=./images/disp-img27.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle-sector my-img 220 40 40 90 200 1 '(thickness 3))
```


</td>
<td>

<img src=./images/disp-img28.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-circle-segment

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-circle-segment my-img 100 100 80 0 100 1)
```


</td>
<td>

<img src=./images/disp-img29.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle-segment my-img 100 100 80 0 100 1 '(filled))
```


</td>
<td>

<img src=./images/disp-img30.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-line

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-line my-img 0 0 320 200 1)
```


</td>
<td>

<img src=./images/disp-img31.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-line my-img 0 200 320 0 1 '(thickness 5))
```


</td>
<td>

<img src=./images/disp-img32.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-line my-img 0 0 320 200 1 '(dotted 4 20))
```


</td>
<td>

<img src=./images/disp-img33.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-rectangle

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-rectangle my-img 10 10 120 180 1)
```


</td>
<td>

<img src=./images/disp-img34.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-rectangle my-img 10 10 120 180 1 '(filled))
```


</td>
<td>

<img src=./images/disp-img35.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-rectangle my-img 10 10 120 180 1 '(rounded 45))
```


</td>
<td>

<img src=./images/disp-img36.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-setpix

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-setpix my-img 10 10 1)
```


</td>
<td>

<img src=./images/disp-img37.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-text

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-text my-img 40 40 1 0 font "LispBM")
```


</td>
<td>

<img src=./images/disp-img38.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-text my-img 40 120 1 0 font "LispBM" 'up)
```


</td>
<td>

<img src=./images/disp-img39.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-text my-img 40 40 1 0 font "LispBM" 'down)
```


</td>
<td>

<img src=./images/disp-img40.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-triangle

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-triangle my-img 30 60 160 120 10 180 1)
```


</td>
<td>

<img src=./images/disp-img41.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-triangle my-img 30 60 160 120 10 180 1 '(filled))
```


</td>
<td>

<img src=./images/disp-img42.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-triangle my-img 30 60 160 120 10 180 1 '(dotted 14 14))
```


</td>
<td>

<img src=./images/disp-img43.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---

# Examples

These examples are leaving out the details on how to setup and initialize any particular display you may have connected to your embedded system. For information on how to initialize a display on a VESC EXPRESS platform see [vesc_express display documentation](https://github.com/vedderb/vesc_express/tree/main/main/display). 


### Example: Sierpinski triangle

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
 (define w 320)
 (define h 200)
 (define corners (list (cons 10 (- h 10)) (cons (- w 10) (- h 10)) (cons (/ w 2) 10)))
 (define s-img (img-buffer 'indexed2 w h))
 (defun point (p) (img-setpix s-img (car p) (cdr p) 1))
 (defun mid-point (p1 p2) (progn 
    (let ((x (/ (+ (car p1) (car p2)) 2))
          (y (/ (+ (cdr p1) (cdr p2)) 2)))
         (cons x y))))
 (defun sierp (n corners p) (if (= n 0) nil (let ((i (mod (rand) 3))
      (target (ix corners i))
      (mid (mid-point p target)))
     (progn 
         (point mid)
         (sierp (- n 1) corners mid)))))
 (sierp 25000 corners (car corners))
 (disp-render s-img 0 0 '(0 16777215))
```


</td>
<td>

<img src=./images/disp-img44.png >

</td>
<td>


```clj
t
```


</td>
</tr>
</table>




---


### Example: rotated llama

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
 (import "images/lama2.bin" 'pic)
 (define img (img-buffer 'indexed2 320 200))
 (img-blit img pic 10 10 -1 '(rotate 128 128 45))
 (disp-render img 0 0 '(0 16711680))
```


</td>
<td>

<img src=./images/disp-img45.png >

</td>
<td>


```clj
t
```


</td>
</tr>
</table>

Note that `import` is a feature of the VESC integration of LispBM and not really a part of core LispBM. The LispBM REPL does not have an import feature currently. 

In the "Desktop" LispBM REPL the rotated llama examples looks as follows. 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
 (define pic (load-file (fopen "images/lama2.bin" "r")))
 (define img (img-buffer 'indexed2 320 200))
 (img-blit img pic 10 10 -1 '(rotate 128 128 45))
 (disp-render img 100 0 '(0 16711680))
```


</td>
<td>

<img src=./images/disp-img46.png >

</td>
<td>


```clj
t
```


</td>
</tr>
<tr>
<td>


```clj
 (disp-clear)
 (define pic (load-file (fopen "images/lama2.bin" "r")))
 (define img128x128 (img-buffer 'indexed2 128 128))
 (img-blit img128x128 pic 0 0 -1 '(scale 0.500000f32) '(rotate 128 128 45))
 (disp-render img128x128 10 10 '(0 16711680))
 (img-clear img128x128)
 (img-blit img128x128 pic 0 0 -1 '(scale 0.500000f32) '(rotate 128 128 -45))
 (disp-render img128x128 148 10 '(0 65280))
```


</td>
<td>

<img src=./images/disp-img47.png >

</td>
<td>


```clj
t
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Animation </td>
</tr>
<tr>
<td>


```clj
 (define pic (load-file (fopen "images/lama2.bin" "r")))
 (define img (img-buffer 'indexed2 128 128))
 (define m (/ 360.000000f32 100.000000f32))
 (disp-clear)
 (loopfor i 0 (< i 100) (+ i 1)
      (progn 
          (var rot (list 'rotate 128 128 (* i m)))
          (img-blit img pic 0 0 -1 '(scale 0.500000f32) rot)
          (disp-render img 10 10 '(0 16711680))))
```


</td>
<td>

<img src=./images/disp-anm1.gif >

</td>
</tr>
</table>




---

This document was generated by LispBM version 0.30.3 

