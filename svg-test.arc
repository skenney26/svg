; visit http://arcxs.posterous.com/ for more advanced examples

(svgop test-sqr req
	(sqr 10 20 30 'red 1 'silver 3))

(svgop use-sqr req
	(let s (sqr 100 100 100 (randcolor))
		(use s)))

(svgop test-sqr0 req
	(sqr0 100 "#f93b57"))

(svgop test-circ req
	(circ 100 200 50 'blue .5 'green 5))

(svgop test-circ0 req
	(circ0 200 (randcolor) .3 'pink 10))

(svgop use-circ0 req
	(let c (circ0 200 (randcolor) .3 'pink 10)
		(use c)))

(svgop test-poly req
	(poly (list 0 0 400 25
							300 200 50 150)
				'lime))

(svgop use-poly req
	(withs (x 0 y 0
					p (poly (list x y 400 25
												300 200 50 150)
									'orange 1 'yellow 20))
		(use p)
		(use p 200)))

(svgop test-poly0 req
	(poly0 (list 200 0 300 100 200 200) 'green .6))

(svgop use-poly0 req
	(let p (poly0 (list 200 0 300 100 200 200) (randcolor) .4)
		(step x 20 200 20
			(use p x))))

(svgop test-roundpoly req
	(roundpoly (list 100 100 400 200
									 500 300 200 300 150 150)
						 'lime .8 'silver 30))

(svgop test-roundpoly0 req
	(roundpoly0 (list 300 100 100 300) "#4567ab" 1 "#987abc" 40))

(svgop test-tri req
	(tri 50 50 300 200 200 300 (randcolor) .4 (randcolor) 15))

(svgop test-tri0 req
	(tri0 500 100 250 400 'pink))

(svgop test-move req
	(move 300 100
		(sqr 0 0 50 'navy)))

(svgop use-move req
	(let m (move 300 100
					 (sqr 0 0 50 'maroon))
		(use m 100)
		(use m 200)))

(svgop w/move req
	(w/svg (m (move 300 100
							(sqr 0 0 50 'maroon)))
		(use m 100)
		(use m 200)))

(svgop test-movex req
	(movex 100
		(sqr0 75 'green)))

(svgop test-movey req
	(movey 100
		(sqr0 75 'green)))

(svgop test-stepmove req
	(stepmove 0 200 20
		(sqr0 50 'red .3)))

(svgop test-stepmovex req
	(stepmovex 0 200 20
		(sqr0 50 'red .3)))

(svgop use-stepmove req
	(let s (stepmove 0 400 20
					 (sqr0 50 (randcolor) .3))
		(stepmovex 100 500 100
			(use s))))

(svgop test-stepmovey req
	(stepmovey 0 200 20
		(sqr0 50 'red .3)))

(svgop test-rot req
	(move 200 200
		(rot 30
			(sqr0 100 'pink))))

(svgop use-rot req
	(let r (rot 15
					 (sqr0 200 'pink .7))
		(use r 100 0)
		(use r 400 300)))

(svgop w/rot req
	(w/svg (r (rot 15
							(sqr0 200 'pink .7)))
		(use r 100 0)
		(use r 400 300)))

(svgop test-steprot req
	(move 300 300
		(steprot 0 345 15
			(circ 100 0 50 'blue .4))))

(svgop test-rotmid req
	(rotmid 45 200 200
		(sqr0 400 (randcolor))))

(svgop test-oval req
	(oval 300 300 200 100 'blue 1 'silver 10))

(svgop w/oval0 req
	(w/svg (o (oval0 200 100 'blue .7 'silver 14))
		(stepmove 100 300 20
			(use o))))

(svgop test-scale req
	(scale 10 10
		(sqr0 10 'purple)))

(svgop test-scalex req
	(scalex 10
		(sqr0 10 'purple)))

(svgop test-scaley req
	(scaley 10
		(sqr0 10 'purple)))

(svgop test-stepscale req
	(stepscale 1 5 1
		(circ0 25 (randcolor) .2)))

(svgop use-stepscale req
	(let s (stepscale 1 7 1
					 (sqr0 20 'orange .3))
		(use s 400 200)))

(svgop w/stepscale req
	(w/svg (s (stepscale 1 7 1
							(sqr0 20 'orange .3)))
		(use s 400 200)))

(svgop test-rect req
	(rect 10 10 300 150 'lime 1 'purple 3))

(svgop test-rect0 req
	(rect0 300 150 'lime 1 'purple 3))

(svgop test-bg req
	(bg 'lime .5))

(svgop test-skewx req
	(skewx 20
		(rect0 200 100 'red)))

(svgop test-skewy req
	(skewy 20
		(rect0 200 100 'red)))

(svgop test-skew req
	(skew 10 20
		(rect0 200 100 'red)))

(svgop use-skew req
	(let s (skew 10 20
					 (rect0 200 100 'red))
		(use s 200 100 .4)))

(svgop w/skew req
	(w/svg (s (skew 10 20
							(rect0 200 100 'red)))
		(use s 200 100 .4)))

(svgop test-flipx req
	(move 300 300
		(flipx
			(skew 10 20
				(rect0 200 100 'lime)))))

(svgop test-flipy req
	(move 300 300
		(flipy
			(skew 10 20
				(rect0 200 100 'lime)))))

(svgop test-flip req
	(move 300 300
		(flip
			(skew 10 20
				(rect0 200 100 'lime)))))

(svgop test-rad req
	(circ 200 200 100 (rad 'black 1 0
												 'red   1 1)))

(svgop test-rad1-1 req
	(circ 200 200 100 (rad1 'red)))

(svgop test-rad1-2 req
	(circ 200 200 100 (rad1 'red nil)))

(svgop test-rad2 req
	(circ 200 200 100 (rad2 'navy 'black)))

(svgop test-rad3 req
	(circ 200 200 100 (rad3 'navy (randcolor) 'black)))

(svgop test-rad4 req
	(circ 300 300 250
				(apply rad4 (n-of 4 (randcolor)))))

(svgop let-rad4 req
	(let r (apply rad4 (n-of 4 (randcolor)))
		(circ 400 200 100 r)
		(circ 200 400 100 r)
		(circ 400 400 100 r)))

(svgop test-rad5 req
	(circ 300 300 250
				(rad5 'white 'yellow 'orange 'red 'black)))

(svgop test-blur req
	(blur 5
		(sqr 50 50 200 'orange 1 'yellow 3)))

(svgop test-mask req
	(mask (sqr0 50 'black)
				(circ0 150 'olive)))

(svgop test-grad req
	(sqr0 300 (grad 0 'yellow 1 0
										'aqua   1 1)))

(svgop let-grad req
	(let g (grad 90 'yellow 1 0
									'aqua   1 1)
		(sqr0 300 g)))

(svgop test-grad1 req
	(sqr0 300 (grad1 0 'aqua)))

(svgop test-grad2 req
	(sqr0 300 (grad2 0 'aqua 'black)))

(svgop test-grad3 req
	(sqr0 300 (grad3 45 'aqua 'silver 'black)))

(svgop test-grad4 req
	(sqr0 400 (apply grad4 0 (n-of 4 (randcolor)))))

(svgop test-grad5 req
	(sqr0 400 (apply grad5 0 (n-of 5 (randcolor)))))

(svgop test-line req
	(line 50 50 600 100 'navy 7))

(svgop use-line req
	(let l (line 0 0 200 200 'silver 3)
		(use l 100 0)
		(use l 200 0)))

(svgop w/line req
	(w/svg (l (line 0 0 200 200 'silver 3))
		(use l 100 0)
		(use l 200 0)))

(svgop test-line0 req
	(line0 700 400 'purple 4 .8))

(svgop test-svglink req
	(svglink "test-line"
					 (sqr 50 50 50 'orange)))

(svgop use-svglink req
	(let l (svglink "test-line"
									(sqr 50 50 50 'red))
		(use l 150)))

(svgop test-path req
	(path "m 0 0 l 200 50 q 300 300 300 400" 'lime))

(svgop test-roundrect req
	(roundrect 100 100 400 200 20 10 'black))

(svgop test-roundrect0 req
	(roundrect0 100 20 5 5 'navy 1 'silver 1))

(svgop test-text req
	(text "Peanut Butter Fudge" 100 200 'blue 'helvetica 40 'normal 1))

(svgop test-image req
	(image "lib.jpg" 10 10 630 484))

(svgop test-image0 req
	(scale .5 .5
		(image0 "lib.jpg" 630 484)))

(svgop test-roundpath req
	(roundpath "m 10 10 q 100 100 200 0" 'navy 7))

(svgop test-curve req
	(curve 10 10 100 100 200 0 'navy 7))

(svgop test-curve0 req
	(curve0 100 100 200 0 'purple 13))

(svgop test-shape req
	(shape "m 50 50 q 200 0 250 250" 'orange 1 'silver 2))

(svgop test-ring req
	(bg 'black)
	(ring 300 300 50 200 'green 3))

(svgop test-ring0 req
	(bg 'black)
	(ring0 50 200 'green 5))

(svgop test-mid req
	(bg 'maroon)
	(mid
		(steprot 0 345 15
			(ring0 400 10 'white))))

(svgop test-corners req
	(corners (circ0 100 'red)))

(svgop test-sides req
	(sides (circ0 100 'red)))


