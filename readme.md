# Languich
A random language I made for no apparent reason
So now you can do things like:

```racket
(println (+ 1 2 3 4))
(var a)
(set a 12)
(var b 123)
(println (+ a b))
(var fizzbuzz)
(var fizzbuzz 
  (lambda (depth num) 
    (if (<= num depth)
      (exec 
        (if (== (% num 15) 0) (println "FizzBuzz")
          (if (== (% num 5) 0) (println "Buzz")
            (if (== (% num 3) 0) (println "Fizz")
              (println num))))
        (fizzbuzz depth (+ num 1))))))
(fizzbuzz 100 1)
```