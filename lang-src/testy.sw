((:lambda Y (
  (Y (:lambda self (:lambda n
   (equals 0 1
   (equals 1 1
   (add (self (add n -1)) (self (add n -2)))
  ))))))
  (:lambda f ((:lambda x (f (x x))) (:lambda x (f (x x)))))
) 4)
