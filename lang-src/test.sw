[
#main = (case 4 [
      3 = "Hello"
      #_ = "Test"
])
#case = (lambda #val #(lambda #dict #([
      true = (dict val)
      false = (dict #_)
] (elemof dict val))))
]
