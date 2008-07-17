; skenney26@gmail.com

(mac svg (name parm . body)
  (w/uniq gs
	 `(defop-raw ,name (,gs ,parm)
			(w/stdout ,gs
				(prn svg-header*)
				(prn)
				(tag (svg xmlns "http://www.w3.org/2000/svg"
									xmlns:xlink "http://www.w3.org/1999/xlink")
					,@body)))))

; redefine start-tag and tag-options from html.arc

(def start-tag (spec)
  (if (atom spec)
      `(pr "<" ',spec ">")
      `(do (pr "<" ',(car spec))
           ,@(tag-options (pair (cdr spec)))
           (pr ">"))))

(def tag-options (options)
	(if (no options)
			nil
			(let ((opt val) . rest) options
				(cons (opstring opt val)
							(tag-options rest)))))

(mac mx (expr)
	`(ppr (macex1 ',expr)))

(mac step (v init end by . body)
	(w/uniq (gi ge gtest gupdate)
		`(withs (,v nil ,gi ,init ,ge ,end
						 ,gtest		(if (< ,gi ,ge) <= >=)
						 ,gupdate (if (< ,gi ,ge) + -))
			(loop (set ,v ,gi)
						(,gtest ,v ,ge)
						(set ,v (,gupdate ,v ,by))
				,@body))))

(mac defs body
 `(tag defs ,@body))

(def circ (x y r f (o o 1) (o s 'none) (o sw 1))
	(tag (circle cx x cy y r r fill f opacity o stroke s stroke-width sw)))

(def oval (x y rx ry f (o o 1) (o s 'none) (o sw 1))
	(tag (ellipse cx x cy y rx rx ry ry fill f opacity o stroke s stroke-width sw)))

(def rect (x y w h f (o o 1) (o s 'none) (o sw 1))
	(tag (rect x x y y width w height h fill f opacity o stroke s stroke-width sw)))

(def roundrect (x y w h rx ry f (o o 1) (o s 'none) (o sw 1))
	(tag (rect x x y y width w height h rx rx ry ry
						 fill f opacity o stroke s stroke-width sw)))

(def sqr (x y w f (o o 1) (o s 'none) (o sw 1))
	(rect x y w w f o s sw))

(def roundsqr (x y w r f (o o 1) (o s 'none) (o sw 1))
	(roundrect x y w w r r f o s sw))

(mac trans (transform . body)
	`(tag (g transform ,transform)
		,@body))

(mac move (x y . body)
	`(trans (string "translate(" ,x "," ,y ")")
		,@body))

(mac rot (angle . body)
	`(trans (string "rotate(" ,angle ")")
		,@body))

(mac rotc (a cx cy . body)
	`(trans (string "rotate(" ,a "," ,cx "," ,cy ")")
		,@body))

(mac skewx (x . body)
	`(trans (string "skewX(" ,x ")")
		,@body))

(mac skewy (y . body)
	`(trans (string "skewY(" ,y ")")
		,@body))

(mac skew (x y . body)
	`(skewx ,x
		(skewy ,y ,@body)))

(mac scale (x y . body)
	`(trans (string "scale(" ,x "," ,y ")")
		,@body))

(def bg (f (o o 1))
	(rect 0 0 "100%" "100%" f o))

(def randcolor ()
  (tostring
    (pr "#")
    (repeat 6 (pr (rand-choice 0 1 2 3 4 5 6 7 8 9 'a 'b 'c 'd 'e 'f)))))

(def between (x y)
	(+ (rand (+ (- y x) 1)) x))

(def around (x y)
	(+ x (between (- y) y)))

(mac grad (id angle . colors)
	`(tag (linearGradient id ,id gradientTransform (string "rotate(" ,angle ")"))
		,@(map gradstop (tuples colors 3))))

(mac rad (id . colors)
	`(tag (radialGradient id ',id cx "50%" cy "50%" r "50%" fx "50%" fy "50%")
		,@(map gradstop (tuples colors 3))))

(def gradstop (args)
 `(tag (stop stop-color ,args.0 offset ,args.1 stop-opacity ,args.2)))

(mac url (id)
 `(string "url(#" ',id ")"))

(def path (d f (o o 1) (o s 'none) (o sw 1))
	(tag (path d d fill f opacity o stroke s stroke-width sw)))

(def line (ps s (o sw 1) (o o 1))
	(tag (polyline points (tostring:prall ps "" " ")
								 fill 'none stroke s stroke-width sw opacity o)))

(def polygon (ps f (o o 1) (o s 'none) (o sw 1))
	(tag (polygon points (tostring:prall ps "" " ")
								fill f opacity o stroke s stroke-width sw)))

(mac svglink (href . body)
 `(tag (a xlink:href ,href)
		,@body))

(def image (href x y w h (o o 1))
	(tag (image xlink:href href x x y y width w height h opacity o)))

(mac use (href (o x 0) (o y 0))
 `(tag (use xlink:href (string "#" ',href) x ,x y ,y)))

(mac mid ids
 `(do ,@(map (fn (id)
							 `(use ,id "50%" "50%"))
					ids)))

(mac group xs
 `(do ,@(map (fn ((id expr))
							 `(tag (g id ',id) ,expr))
					(pair xs))))


