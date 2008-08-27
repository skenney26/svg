; skenney26@gmail.com


; *** This is in the middle of a major rewrite ***
; the examples at http://arcxs.posterous.com/ should work though


; respond from srv.arc has been redefined to allow multiple response types
; use defop for html and svgop for svg
; redirects haven't been re-implemented yet



; server changes/additions

(= svg-header* "HTTP/1.0 200 OK
Content-Type: image/svg+xml; charset=utf-8
Connection: close")

(= (srv-header* 'png)
"HTTP/1.0 200 OK
Content-Type: image/png
Connection: close")

(= (srv-header* 'text/javascript) 
"HTTP/1.0 200 OK
Content-Type: text/javascript; charset=utf-8
Connection: close")
				
; redefining defop, respond, and static-filetype from srv.arc

(mac defop (name parm . body)
  (w/uniq gs
    `(defop-raw ,name (,gs ,parm) 
       (w/stdout ,gs
				(prn header*)
				(prn)
				,@body))))

(def respond (str op args cooks ip)
  (w/stdout str
    (aif (srvops* op)
          (let req (inst 'request 'args args 'cooks cooks 'ip ip)
            (if (redirector* op)
                (do (prn rdheader*)
                    (prn "Location: " (it str req))
                    (prn))
                (it str req)))							; this is the only change so far
         (static-filetype op)
          (do (prn (srv-header* it))
              (prn)
              (w/infile i (string op)
                (whilet b (readb i)
                  (writeb b str))))
          (respond-err str unknown-msg*))))

(def static-filetype (sym)
  (let fname (string sym)
    (and (~find #\/ fname)
         (case (last (check (tokens fname #\.) ~single))
           "gif"  'gif
           "jpg"  'jpg
					 "png"  'png
           "css"  'text/html
           "txt"  'text/html
           "html" 'text/html
					 "js"		'text/javascript
           ))))


(= grads* nil)

; overwrite or shadow grads*?

(mac svgop (name parm . body)
  (w/uniq gs
    `(defop-raw ,name (,gs ,parm)
       (w/stdout ,gs
				(prn svg-header*)
				(prn)
				(tag (svg xmlns "http://www.w3.org/2000/svg"
									xmlns:xlink "http://www.w3.org/1999/xlink")
					(let grads* nil
						,@body))))))



; redefine start-tag and tag-options from html.arc
; this allows us to avoid keeping track of tag attributes

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



; svg api

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

(mac rots (id start stop (o by 1))
	(w/uniq (gv gi)
	 `(with (,gv nil ,gi ,id)
			(step ,gv ,start ,stop ,by
				(rot ,gv (use ,gi))))))

(mac rotc (angle cx cy . body)
 `(trans (string "rotate(" ,angle "," ,cx "," ,cy ")")
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

(def bg (color (o opacity 1))
	(rect 0 0 "100%" "100%" color opacity))

(def randcolor ()
  (tostring
    (pr "#")
    (repeat 6 (pr (rand-choice 0 1 2 3 4 5 6 7 8 9 'a 'b 'c 'd 'e 'f)))))

(def between (x y)
	(+ (rand (+ (- y x) 1)) x))

(def around (x y)
	(+ x (between (- y) y)))

(def grad (id angle . colors)
	(tag (linearGradient id id gradientTransform (string "rotate(" angle ")"))
		(each (sc so o) (tuples colors 3)
			(tag (stop stop-color sc stop-opacity so offset o)))))

(def rad (id . colors)
	(tag (radialGradient id id cx "50%" cy "50%" r "50%" fx "50%" fy "50%")
		(each (sc so o) (tuples colors 3)
			(tag (stop stop-color sc stop-opacity so offset o)))))

; should r also be passed as an argument to radc ?

(def radc (id cx cy . colors)
	(tag (radialGradient id id cx cx cy cy r "50%" fx "50%" fy "50%")
		(each (sc so o) (tuples colors 3)
			(tag (stop stop-color sc stop-opacity so offset o)))))

(def path (d s (o sw 1) (o o 1))
	(tag (path d d stroke s stroke-width sw opacity o fill 'none)))

(def roundpath (d s (o sw 1) (o o 1))
	(tag (path d d stroke s stroke-width sw opacity o
						 stroke-linejoin 'round stroke-linecap 'round fill 'none)))

(def shape (d f (o o 1) (o s 'none) (o sw 1))
	(tag (path d d fill f opacity o stroke s stroke-width sw)))

(def line (ps s (o sw 1) (o o 1))
	(tag (polyline points (tostring:prall ps "" " ")
								 fill 'none stroke s stroke-width sw opacity o)))

(def poly (ps f (o o 1) (o s 'none) (o sw 1))
	(tag (polygon points (tostring:prall ps "" " ")
								fill f opacity o stroke s stroke-width sw)))

(def tri (x1 y1 x2 y2 x3 y3 f (o o 1) (o s 'none) (o sw 1))
	(poly (list x1 y1 x2 y2 x3 y3) f o s sw))

(mac svglink (href . body)
 `(tag (a xlink:href ,href)
		,@body))

(def image (href x y w h (o o 1))
	(tag (image xlink:href href x x y y width w height h opacity o)))



(mac svgid args
 `(tag defs
		,@(map (fn ((id val))
						`(let i ,id
							 (if (in ',(car val) 'grad 'rad)
									 (do (push i grads*)
											 (,(car val) i ,@(cdr val)))
									 (tag (g id i) ,val))))
					 (pair args))))

(mac use (id (o x 0) (o y x))
 `(if (mem ,id grads*)
			(string "url(#" ,id ")")
			(tag (use xlink:href (string "#" ,id) x ,x y ,y))))

(mac mid ids
 `(do ,@(map (fn (id)
							 `(use ,id "50%"))
						 ids)))



; iterators

(mac step (v init end by . body)
	(w/uniq (gi ge gtest gupdate)
		`(withs (,v nil ,gi ,init ,ge ,end
						 ,gtest		(if (< ,gi ,ge) <= >=)
						 ,gupdate (if (< ,gi ,ge) + -))
			(loop (set ,v ,gi)
						(,gtest ,v ,ge)
						(set ,v (,gupdate ,v ,by))
				,@body))))

(mac pass (x y low high . body)
	(w/uniq gh
		`(with (,x nil ,y nil ,gh ,high)
			(loop (do (set ,x ,low)
								(set ,y ,gh))
						(<= ,x ,gh)
						(do (set ,x (+ ,x 1))
								(set ,y (- ,y 1)))
				,@body))))

(mac saw (x y init mid . body)
	(w/uniq (gi gm gup)
		`(with (,x nil ,y nil ,gi ,init ,gm ,mid ,gup t)
			(loop (do (set ,x ,gi)
								(set ,y ,gi))
						(>= ,x ,gi)
						(do (if (is ,x ,gm) (set ,gup nil))
								(set ,x ((if ,gup + -) ,x 1))
								(set ,y (+ ,y 1)))
				,@body))))

(mac hop (start group var expr . body)
	`(each ,var (map [_ ,start]
								(tuples ,expr ,group))
		,@body))



; tests

(svgop test-svgid req
	(svgid 'grad1 (grad 0 (randcolor) "0%" 1
												(randcolor) "100%" 1)
				'circ1 (circ 100 100 100 (randcolor)))
	(bg (use 'grad1))
	(use 'circ1))

(svgop test-step req
	(svgid 'sqrs (step x 10 100 10
								(sqr x 100 50 'orange .4)))
	(use 'sqrs))

(mac mx (expr)
 `(ppr (macex1 ',expr)))



; examples

(svgop swirl req
	(svgid 'arm1	(step r 4 40 4
								(rot r
									(move (* r 3) 0
										(circ 0 0 r 'white .2 'red 2))))
			 'arms1 (rots 'arm1 120 360 120)
			 'arm2	(step r 4 60 4
								(rot r
									(move (* r 3) 0
										(circ 0 0 r 'white .1 "#00ffff" 2))))
			 'arms2 (rots 'arm2 60 300 120))
	(bg 'black)
	(mid 'arms1 'arms2))



; new mid

(mac mid (expr)
	(w/uniq u
	 `(do (svgid ',u ,expr)
				(use ',u "50%" "50%"))))

(mac mid exprs
	(let us (map [uniq] exprs)
	 `(list ,@(map (fn (u e)
								`(do (svgid ,u ,e)
										 (use ,u "50%" "50%")))
							 us exprs))))



(mac mid exprs
 `(do ,@(map (fn (e)
							`(let u (uniq)
								 (svgid u ,e)
								 (use u "50%" "50%")))
						 exprs)))

(svgop test1 req
	(mid (circ 0 0 200 'silver)))

(svgop test2 req
	(mid (circ 0 0 200 'silver)
			 (circ 0 0 100 'blue)))

(svgop test3 req
  (mid (step r 1080 0 6
				 (rot r
					 (oval 0 0 r 20 'none 1 'black)))))

(svgop test4 req
  (mid (step s 3 1 .5
         (scale s s
           (shape "M 100 20
                   Q 0 0 80 70
                   Q 0 0 -50 90
                   Q 0 0 -90 40
                   Q 0 0 -40 -70
                   Q 0 0 40 -80
                   Q 0 0 100 20 Z"
                  'purple .3)))))

(svgop test5 req
  (mid (circ 0 0 150 'yellow)
       (circ -50 -50 30 'black)
       (circ 50 -50 30 'black)
       (roundpath "M -100 40 Q 0 140 100 40" 'black 15)))

(svgop test6 req
  (bg "#00ff00" .4))

(svgop test7 req
  (bg 'black)
  (mid	(step a 120 360 120
          (rot a
            (step b 4 40 4
              (rot b
                (move (* b 3) 0
                  (circ 0 0 b 'white .2 'yellow 2))))))
        (step a 60 300 120
          (rot a
            (step b 4 60 4
              (rot b
                (move (* b 3) 0
                  (circ 0 0 b 'white .1 "#00ffff" 2))))))))








