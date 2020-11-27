# Languich
A random language I made for no apparent reason
So now you can do things like:
'''
(println (+ 1 2 3 4))
(var a)
(set a 12)
(var b 123)
(println (+ a b))
(var fizzbuzz 
  (lambda (depth num) 
    (if (<= num depth)
      (exec 
        (if (% num 3) (display "Fizz")
          (if (% num 3) (display "Buzz")
            (if (% num 15) (display "FizzBuzz")
              (display num)
            )
          )
        )
      )
    )
  )
)
(fizzbuzz 100 1)
'''