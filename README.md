# series

this is a sand box to solve some Project Euler problems and in tribut to Euler to use plenty of series methods to do so. I should build at least one generally useful thing that works for something else hence library.

## Usage

right now these are evolving for use according to random problems for example:
https://projecteuler.net/problem=7
(last (take 10001 primes))
https://projecteuler.net/problem=10
(reduce + (take-while #(> 2000000 %) primes))
https://projecteuler.net/problem=122
(reduce +
  (map
    #(fn [goal]
      (first
        (nth
	  (last
	    (take-while #(some? (peek %))
	      (iterate
	        q-search-122
		[[(sorted-set 1 2)] goal (list (bin-time-122 goal)) false]))) 2)))
  (range 2 201)))
https://projecteuler.net/problem=166
(reduce +
  (map
    #(count
      (make-magic (list (zipmap (range 0 16) (repeat 16 nil))) % 9 0 '(0 1 2 3 4 5 6 7 15 11 10 14 12 8 9 13))) (range 0 37))

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
