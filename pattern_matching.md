
# Random thoughts about adding pattern matchint to lispBM 


I want to add a match and a recv primitive. The `match` should implement pattern matching as a programming abstraction. 
The `recv` implements reception of messages for concurrency purposes. 

There are so many choices though... I dont know where to start! 



## a sketch

`(recv (pattern1 . what-to-do1)
       (pattern2 . what-to-do2)
	   (...)
	   (patternN . what-to-doN))`

