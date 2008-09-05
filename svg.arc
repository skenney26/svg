; skenney26@gmail.com


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



(mac svgop (name parm . body)
  (w/uniq gs
    `(defop-raw ,name (,gs ,parm)
       (w/stdout ,gs
				(prn svg-header*)
				(prn)
				(tag (svg xmlns "http://www.w3.org/2000/svg"
									xmlns:xlink "http://www.w3.org/1999/xlink")
					,@body)))))



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
	(tag (circle cx x cy y r r fill f
							 opacity o stroke s stroke-width sw)))

(def oval (x y rx ry f (o o 1) (o s 'none) (o sw 1))
	(tag (ellipse cx x cy y rx rx ry ry fill f
								opacity o stroke s stroke-width sw)))

(def ring (x y rx ry s (o sw 1) (o o 1))
	(tag (ellipse cx x cy y rx rx ry ry fill 'none
								stroke s stroke-width sw opacity o)))

(def rect (x y w h f (o o 1) (o s 'none) (o sw 1))
	(tag (rect x x y y width w height h fill f
						 opacity o stroke s stroke-width sw)))

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
 `(trans (string "translate(" ,x " " ,y ")")
		,@body))

(mac movex (x . body)
 `(move x 0 ,@body))

(mac movey (y . body)
 `(move 0 y ,@body))

(mac moves (v start stop by . body)
 `(let ,v nil
		(step ,v ,start ,stop ,by
			(move ,v ,v ,@body))))

(mac rot (angle . body)
 `(trans (string "rotate(" ,angle ")")
		,@body))

(mac rots (v start stop by . body)
 `(let ,v nil
		(step ,v ,start ,stop ,by
			(rot ,v ,@body))))

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
	`(trans (string "scale(" ,x " " ,y ")")
		,@body))

(mac scales (v start stop by . body)
 `(let ,v nil
		(step ,v ,start ,stop ,by
			(scale ,v ,v ,@body))))

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

(def grad (angle . colors)
	(let u (uniq)
		(tag (linearGradient id u gradientTransform (string "rotate(" angle ")"))
			(each (color opac off) (tuples colors 3)
				(tag (stop stop-color color stop-opacity opac offset off))))
		(string "url(#" u ")")))

(def grad1 (angle color)
	(let u (uniq)
		(tag (linearGradient id u gradientTransform (string "rotate(" angle ")"))
			(tag (stop stop-color color stop-opacity 1 offset 0))
			(tag (stop stop-color color stop-opacity 0 offset 1)))
		(string "url(#" u ")")))

(def grad2 (angle color1 color2)
	(let u (uniq)
		(tag (linearGradient id u gradientTransform (string "rotate(" angle ")"))
			(tag (stop stop-color color1 stop-opacity 1 offset 0))
			(tag (stop stop-color color2 stop-opacity 1 offset 1)))
		(string "url(#" u ")")))

(def rad colors
	(let u (uniq)
		(tag (radialGradient id u cx "50%" cy "50%" r "50%" fx "50%" fy "50%")
			(each (color opac off) (tuples colors 3)
				(tag (stop stop-color color stop-opacity opac offset off))))
		(string "url(#" u ")")))



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


(def spaces args
	(tostring (apply prs args)))

(def line (ps s (o sw 1) (o o 1))
	(tag (polyline points (tostring:prall ps "" " ")
								 fill 'none stroke s stroke-width sw opacity o)))

(def poly (ps f (o o 1) (o s 'none) (o sw 1))
	(tag (polygon points (apply spaces ps)
								fill f opacity o stroke s stroke-width sw)))

(def poly (ps f (o o 1) (o s 'none) (o sw 1))
	(let u (uniq)
		(tag (polygon id u points (apply spaces ps)
									fill f opacity o stroke s stroke-width sw))
		u))

(def tri (x1 y1 x2 y2 x3 y3 f (o o 1) (o s 'none) (o sw 1))
	(poly (list x1 y1 x2 y2 x3 y3) f o s sw))

(mac svglink (href . body)
 `(tag (a xlink:href ,href)
		,@body))

(def image (href x y w h (o o 1))
	(tag (image xlink:href href x x y y width w height h opacity o)))

; incorporate these into the above definitions soon

(def svg-id (id)
	(string "#" id))

(def svg-url (id)
	(string "url(#" id ")"))

(mac blur (n . body)
 `(withs (u (uniq)
					url (svg-url u))
		(tag (filter id u)
			(tag (feGaussianBlur stdDeviation ,n)))
		(tag (g filter url)
			,@body)
		url))

(def curve (x1 y1 x2 y2 s (o w 1) (o o 1))
	(let u (uniq)
		(tag (path d (tostring:prs "M 0 0 Q" x1 y1 x2 y2)
							 stroke s stroke-width w opacity o fill 'none))
		u))

(def use (id (o x 0) (o y x))
	(tag (use xlink:href (string "#" id) x x y y)))

; abstract mid, left, etc into poser, a mac-writing mac

(mac mid exprs
 `(do ,@(map (fn (e)
							`(let u (uniq)
								 (tag defs
									 (tag (g id u) ,e))
								 (use u "50%" "50%")))
						 exprs)))

(mac left exprs
 `(do ,@(map (fn (e)
							`(let u (uniq)
								 (tag defs
									 (tag (g id u) ,e))
								 (use u 0 "50%")))
						 exprs)))

(mac botleft exprs
 `(do ,@(map (fn (e)
							`(let u (uniq)
								 (tag defs
									 (tag (g id u) ,e))
								 (use u 0 "100%")))
						 exprs)))

(mac right exprs
 `(do ,@(map (fn (e)
							`(let u (uniq)
								 (tag defs
									 (tag (g id u) ,e))
								 (use u "100%" "50%")))
						 exprs)))

(mac top exprs
 `(do ,@(map (fn (e)
							`(let u (uniq)
								 (tag defs
									 (tag (g id u) ,e))
								 (use u "50%" 0)))
						 exprs)))



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

; each with index
; (eachi e i '(a b c) (prn i #\space e))

(mac eachi (v i expr . body)
	(w/uniq (gseq g)
		`(let ,gseq ,expr
			 (if (alist ,gseq)
			 			((afn (,g ,i)
							 (when (acons ,g)
							 	 (let ,v (car ,g)
								 	 ,@body
									 (self (cdr ,g) (++ ,i)))))
						 ,gseq 0)
					 (forlen ,i 0 ,gseq
						 (let ,v (,gseq ,i) ,@body))))))

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


(mac mx (expr)
 `(ppr (macex1 ',expr)))


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

(svgop test8 req
  (bg 'red .5)
  (mid (rots a 120 360 120
         (rots b 4 40 4
           (move (* b 3) 0
             (circ 0 0 b 'white .3 'black 2))))
       (rots a 60 300 120
         (rots b 4 60 4
           (move (* b 3) 0
             (circ 0 0 b 'white .15 'black 2))))))

(svgop test9 req
  (mid (rots r 1080 0 6
         (oval 0 0 r 20 'none 1 'black))))

(svgop test10 req
  (mid (scales s 3 1 .5
         (shape "M 100 20
                 Q 0 0 80 70
                 Q 0 0 -50 90
                 Q 0 0 -90 40
                 Q 0 0 -40 -70
                 Q 0 0 40 -80
                 Q 0 0 100 20 Z"
                'purple .3))))

(svgop test11 req
	(bg 'black)
  (mid (rots r 1080 0 6
         (ring 0 0 r 20 'white))))

(svgop test12 req
	(bg 'black)
  (mid (rots r 1080 0 6
         (ring 0 0 r 20 'white 3 .4))))

(svgop test13 req
	(bg 'black)
  (mid (rots r 1080 0 6
         (ring 0 0 r 20 'white 5 .3))))

(svgop test14 req
	(bg 'black)
  (mid (rots r 1080 0 6
         (ring 0 0 r 20 'maroon 25 .1))))

(svgop test15 req
	(circ 100 100 50
				(grad 0 (randcolor) 1 "0%"
								(randcolor) 1 "100%")))

(svgop test16 req
	(let g (grad 0 (randcolor) 1 "0%"
								 (randcolor) 1 "100%")
		(circ 100 100 50 g)
		(sqr  100 300 100 g)))


; move all tests into another file

(svgop blur1 req
	(blur 5 (sqr 100 100 100 'orange)))

(svgop blur2 req
  (bg 'black)
  (left (blur 10
          (step y -200 200 20
            (curve 500 0 1500 y 'orange)))))

(svgop blur3 req
  (bg 'black)
  (left (blur 2
          (step y -400 400 40
            (curve 500 0 1500 y 'white .5)))))

(svgop blur4 req
  (bg 'black)
  (top (blur 7
         (rots r 45 135 15
           (step y -200 200 20
             (curve 400 0 1200 y 'red))))))

(svgop test-grad1 req
	(sqr 100 100 400
			 (grad1 0 (randcolor))))

(svgop test-grad2 req
	(sqr 100 100 400
			 (grad2 0 (randcolor) (randcolor))))










