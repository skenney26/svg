
(= letters* (table))

(= (letters* #\a) '(" ***"
										"*   *"
										"*****"
										"*   *"
										"*   *"))

(= (letters* #\b) '("****"
										"*   *"
										"****"
										"*   *"
										"****"))

(= (letters* #\c) '(" ****"
										"*"
										"*"
										"*"
										" ****"))

(= (letters* #\d) '("****"
										"*   *"
										"*   *"
										"*   *"
										"****"))

(= (letters* #\e) '("*****"
										"*"
										"***"
										"*"
										"*****"))

(= (letters* #\f) '("*****"
										"*"
										"***"
										"*"
										"*"))

(= (letters* #\g) '("*****"
										"*"
										"*  **"
										"*   *"
										"*****"))

(= (letters* #\h) '("*   *"
										"*   *"
										"*****"
										"*   *"
										"*   *"))

(= (letters* #\i) '("*****"
										"  *"
										"  *"
										"  *"
										"*****"))

(= (letters* #\j) '("*****"
										"  *"
										"  *"
										"  *"
										"***"))

(= (letters* #\k) '("*   *"
										"*  *"
										"***"
										"*  *"
										"*   *"))

(= (letters* #\l) '("*"
										"*"
										"*"
										"*"
										"*****"))

(= (letters* #\m) '("*   *"
										"** **"
										"* * *"
										"*   *"
										"*   *"))

(= (letters* #\n) '("*   *"
                    "**  *"
                    "* * *"
                    "*  **"
                    "*   *"))

(= (letters* #\o) '(" ***"
										"*   *"
										"*   *"
										"*   *"
										" ***"))

(= (letters* #\p) '("*****"
										"*   *"
										"*****"
										"*"
										"*"))

(= (letters* #\q) '("****"
										"*  *"
										"*  *"
										"* **"
										"****"
										"    *"))

(= (letters* #\r) '("****"
										"*   *"
										"****"
										"*  *"
										"*   *"))

(= (letters* #\s) '("*****"
										"*"
										"*****"
										"    *"
										"*****"))

(= (letters* #\t) '("*****"
										"  *"
										"  *"
										"  *"
										"  *"))

(= (letters* #\u) '("*   *"
										"*   *"
										"*   *"
										"*   *"
										"*****"))

(= (letters* #\v) '("*   *"
										"*   *"
										"*   *"
										" * *"
										"  *"))

(= (letters* #\w) '("*     *"
										"*     *"
										"*  *  *"
										" * * *"
										"  * *"))

(= (letters* #\x) '("*   *"
										" * *"
										"  *"
										" * *"
										"*   *"))

(= (letters* #\y) '("*   *"
										" * *"
										"  *"
										"  *"
										"  *"))

(= (letters* #\z) '("*****"
										"   *"
										"  *"
										" *"
										"*****"))

(= (letters* 0)   '("*****"
										"*   *"
										"*   *"
										"*   *"
										"*****"))

(= (letters* 8)   '("*****"
										"*   *"
										"*****"
										"*   *"
										"*****"))

(= points* (table))

(def strings->points (ss (o symbol #\*))
	(accum a
		(eachi s y ss
			(eachi c x s
				(if (is c symbol)
						(a (list x y)))))))

(def gen-points ()
	(each k (keys letters*)
		(= (points* k) (strings->points (letters* k)))))

(gen-points)



(svgop sanfran req
	(with (x 200
				 randcirc [circ (+ (* (car  _) 40) (between 15 50))
												(+ (* (cadr _) 40) (between 15 50))
												(between 3 10)
												(randcolor)
												.6])
		(each letter '(s a n)
			(move x 100
				(++ x 200)
				(each p (points* letter)
					(repeat 25
						(randcirc p)))))
		(move 300 330
			(let x 0
				(each letter '(f r a n)
					(move x 0
						(++ x 200)
						(each p (points* letter)
							(repeat 25
								(randcirc p)))))))))









