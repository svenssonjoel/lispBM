

;; This example uses file operations implemented
;; on top of the MEMFS provided by emscripten.

(def f (fopen "data.bin" "w"))                                                
(def buf (bufcreate 8))                                                         
(bufset-f32 buf 0 3.14)                                                       
(bufset-u32 buf 4 42u32)                                                      
(fwrite f buf)                                                                
(fclose f)      

(def txt (fopen "notes.txt" "w"))
(fwrite-str txt "hello from lispbm\n")                                          
(fwrite-str txt "second line\n")                                                
(fclose txt)

(def r (fopen "notes.txt" "r"))
(def contents (load-file r))                                                  
(fclose r)
(print contents)                                                              

(print (flist))

(wasm-save-file "data.bin")
(wasm-save-file "notes.txt")
