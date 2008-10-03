; skenney26@gmail.com
; http://arcxs.posterous.com/


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



; ITERATORS


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
						(for ,i 0 (- (len ,gseq) 1)
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

; (meet i j 0 10 (prn i " " j))

(mac meet (x y low high . body)
 `(with (,x nil ,y nil)
		(loop (do (set ,x ,low)
							(set ,y ,high))
					(<= ,x ,y)
					(do (set ,x (+ ,x 1))
							(set ,y (- ,y 1)))
			,@body)))

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




; SVG API

(def svgid (id)
	(string "#" id))

(def svgurl (id)
	(string "url(#" id ")"))

(mac group (id . body)
	(w/uniq gi
	 `(let ,gi ,id
			(tag (g id ,gi) ,@body)
			(svgid ,gi))))

(def oval (x y rx ry fill (o opacity 1) (o stroke 'none) (o stroke-width 1))
	(let u (uniq)
		(tag (ellipse id u cx x cy y rx rx ry ry fill fill
									opacity opacity stroke stroke stroke-width stroke-width))
		(svgid u)))

(def oval0 (rx ry fill (o opacity 1) (o stroke 'none) (o stroke-width 1))
	(oval 0 0 rx ry fill opacity stroke stroke-width))

(def ring (x y rx ry color (o stroke-width 1) (o opacity 1))
	(oval x y rx ry 'none opacity color stroke-width))

(def ring0 (rx ry color (o stroke-width 1) (o opacity 1))
	(ring 0 0 rx ry color stroke-width opacity))

(def circ (x y radius fill (o opacity 1) (o stroke 'none) (o stroke-width 1))
	(oval x y radius radius fill opacity stroke stroke-width))

(def circ0 (radius fill (o opacity 1) (o stroke 'none) (o stroke-width 1))
	(circ 0 0 radius fill opacity stroke stroke-width))

(def roundrect (x y width height rx ry fill
								(o opacity 1) (o stroke 'none) (o stroke-width 1))
	(let u (uniq)
		(tag (rect id u x x y y width width height height rx rx ry ry fill fill
							 opacity opacity stroke stroke stroke-width stroke-width))
		(svgid u)))

(def roundrect0 (width height rx ry fill
								 (o opacity 1) (o stroke 'none) (o stroke-width 1))
	(roundrect 0 0 width height rx ry fill opacity stroke stroke-width))

(def rect (x y width height fill
					 (o opacity 1) (o stroke 'none) (o stroke-width 1))
	(roundrect x y width height 0 0 fill opacity stroke stroke-width))

(def rect0 (width height fill (o opacity 1) (o stroke 'none) (o stroke-width 1))
	(rect 0 0 width height fill opacity stroke stroke-width))

(def roundsqr (x y width r fill
							 (o opacity 1) (o stroke 'none) (o stroke-width 1))
	(roundrect x y width width r r fill opacity stroke stroke-width))

(def roundsqr0 (width r fill (o opacity 1) (o stroke 'none) (o stroke-width 1))
	(roundsqr 0 0 width r fill opacity stroke stroke-width))

(def sqr (x y width fill (o opacity 1) (o stroke 'none) (o stroke-width 1))
	(roundsqr x y width 0 fill opacity stroke stroke-width))

(def sqr0 (width fill (o opacity 1) (o stroke 'none) (o stroke-width 1))
	(sqr 0 0 width fill opacity stroke stroke-width))

(def line (x1 y1 x2 y2 color (o width 1) (o opacity 1))
	(let u (uniq)
		(tag (polyline points (tostring:prs x1 y1 x2 y2) fill 'none
									 stroke color stroke-width width opacity opacity))
		(svgid u)))

(def line0 (x y color (o width 1) (o opacity 1))
	(line 0 0 x y color width opacity))

(mac transform (trans . body)
 `(let u (uniq)
		(tag (g id u transform ,trans)
			,@body)
		(svgid u)))

(mac move (x y . body)
 `(transform (string "translate(" ,x " " ,y ")")
		,@body))

(mac movex (x . body)
 `(move ,x 0 ,@body))

(mac movey (y . body)
 `(move 0 ,y ,@body))

; still working on stepmove, steprot, etc

(mac stepmove (v init end by . body)
 `(let ,v nil
		(step ,v ,init ,end ,by
			(move ,v ,v ,@body))))

(mac stepmove (v init end by . body)
 `(with (,v nil u (uniq))
		(tag (g id u)
			(step ,v ,init ,end ,by
				(move ,v ,v ,@body)))
		(svgid u)))

(mac stepmove (init end by . body)
	(w/uniq (id v)
	 `(group ',id
			(step ,v ,init ,end ,by
				(move ,v ,v
					,@body)))))

(mac stepmovex (init end by . body)
	(w/uniq (id v)
	 `(group ',id
			(step ,v ,init ,end ,by
				(move ,v 0
					,@body)))))

(mac stepmovey (init end by . body)
	(w/uniq (id v)
	 `(group ',id
			(step ,v ,init ,end ,by
				(move 0 ,v 
					,@body)))))



(mac rot (angle . body)
 `(transform (string "rotate(" ,angle ")")
		,@body))

(mac steprot (v init end by . body)
 `(with (,v nil u (uniq))
		(tag (g id u)
			(step ,v ,init ,end ,by
				(rot ,v ,@body)))
		(svgid u)))

(mac rotmid (angle cx cy . body)
 `(transform (string "rotate(" ,angle " " ,cx " " ,cy ")")
		,@body))

(mac skewx (x . body)
 `(transform (string "skewX(" ,x ")")
		,@body))

(mac skewy (y . body)
 `(transform (string "skewY(" ,y ")")
		,@body))

(mac skew (x y . body)
 `(skewx ,x
		(skewy ,y ,@body)))

(mac scale (x y . body)
 `(transform (string "scale(" ,x " " ,y ")")
		,@body))

(mac scalex (x . body)
 `(scale ,x 1 ,@body))

(mac scaley (y . body)
 `(scale 1 ,y ,@body))

(mac stepscale (v init end by . body)
 `(with (,v nil u (uniq))
		(tag (g id u)
			(step ,v ,init ,end ,by
				(scale ,v ,v ,@body)))
		(svgid u)))

(mac flipx body
 `(scale -1 1 ,@body))

(mac flipy body
 `(scale 1 -1 ,@body))

(mac flip body
 `(scale -1 -1 ,@body))

(def rad colors
	(let u (uniq)
		(tag (radialGradient id u cx "50%" cy "50%" r "50%" fx "50%" fy "50%")
			(each (color opac off) (tuples colors 3)
				(tag (stop stop-color color stop-opacity opac offset off))))
		(svgurl u)))

(mac rad1 (color)
	(w/uniq c
	 `(let ,c ,color
			(rad ,c 1 0 ,c 0 1))))

(mac rad2 (color1 color2)
 `(rad ,color1 1 0
			 ,color2 1 1))

(def video (vid x y w h)
	(let u (uniq)
		(tag (video id u xlink:href vid
								x x y y width w height h))
		(svgid u)))

(mac mask (m . body)
 `(with (um (uniq) ub (uniq))
		(tag defs
			(tag (mask id um maskContentUnits "objectBoundingBox")
				,m)
			(tag (g id ub)
				,@body))
		(tag (use xlink:href (svgid ub)
							mask (svgurl um)))))

(mac view (x y w h . body)
 `(withs (u (uniq) s (svgid u))
		(tag (symbol id u viewBox (tostring:prs ,x ,y ,w ,h))
			,@body)
		(use s)
		s))

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

; hack - beware
(def div args
	(coerce (rem #\, (num (apply / args))) 'int))

(def half (x)
	(div x 2))

(def negdiv args
	(- (apply div args)))

(def neghalf (x)
	(negdiv x 2))

(def path (pathstr color (o width 1) (o opacity 1))
	(let u (uniq)
		(tag (path id u d pathstr stroke color
							 stroke-width width opacity opacity fill 'none))
		(svgid u)))

(def roundpath (pathstr color (o width 1) (o opacity 1))
	(let u (uniq)
		(tag (path id u d pathstr stroke color stroke-width width opacity opacity
							 stroke-linejoin 'round stroke-linecap 'round fill 'none))
		(svgid u)))


; everything in the SVG API section above here has been rewritten
; need to finish below




(def shape (pathstr fill (o opac 1) (o stroke 'none) (o width 1))
	(let u (uniq)
		(tag (path id u d pathstr fill fill
							 opacity opac stroke stroke stroke-width width))
		(svgid u)))

(def text (str x y fill (o opacity) (o size) (o font) (o weight))
	(let u (uniq)
		(tag (text id u x x y y fill fill opacity opacity
							 font-size size font-family font font-weight weight)
			(pr str))
		(svgid u)))


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



(def radc (id cx cy . colors)
	(tag (radialGradient id id cx cx cy cy r "50%" fx "50%" fy "50%")
		(each (sc so o) (tuples colors 3)
			(tag (stop stop-color sc stop-opacity so offset o)))))



(def spaces args
	(tostring (apply prs args)))

(def lines (ps s (o sw 1) (o o 1))
	(tag (polyline points (tostring:prall ps "" " ")
								 fill 'none stroke s stroke-width sw opacity o)))

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

(mac blur (n . body)
 `(withs (u (uniq)
					url (svgurl u))
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
	(tag (use xlink:href id x x y y)))


(def tri0 (x2 y2 x3 y3 fill (o opacity 1) (o stroke 'none) (o stroke-width 1))
	(poly (list 0 0 x2 y2 x3 y3) fill opacity stroke stroke-width))





(mac poser (name x y)
 `(mac ,name exprs
		(w/uniq gs
		 `(group ',gs
				,@(map (fn (expr)
								 (w/uniq g
									`(do (tag defs
												 (group ',g ,expr))
											 (use (svgid ',g) ,,x ,,y))))
							 exprs)))))

(mac posers args
 `(do ,@(map (fn (x)
							`(poser ,@x))
						 (tuples args 3))))

(posers topleft				 0			0
				top				 "50%"			0
				topright	"100%"			0
				right			"100%"	"50%"
				botright	"100%" "100%"
				bot				 "50%" "100%"
				botleft				 0 "100%"
				left					 0	"50%"
				mid				 "50%"	"50%")

(mac defsvg body
	(w/uniq u
	 `(do (tag defs
					(group ',u ,@body))
				(svgid ',u))))

(mac w/svg (parms . body)
 `((fn ,(map1 car (pair parms))
		,@body)
	 ,@(map (fn (p)
					 `(defsvg ,(cadr p)))
					(pair parms))))



(mac mx (expr)
 `(ppr (macex1 ',expr)))


(mac corners body
	(w/uniq g
	 `(w/svg (,g ,@body)
			(topleft	(use ,g))
			(topright (use ,g))
			(botright (use ,g))
			(botleft	(use ,g)))))

(mac sides body
	(w/uniq g
	 `(w/svg (,g ,@body)
			(top	 (use ,g))
			(right (use ,g))
			(bot	 (use ,g))
			(left	 (use ,g)))))

(svgop bigtest1 req
	(w/svg (c (topleft
							(circ0 100 'silver)
							(circ0	75 'maroon)
							(circ0	50 'silver)
							(circ0  25 'maroon)))
		(mid (use c))))

(svgop bigtest2 req
	(w/svg (c (topleft
							(circ0 100 'silver)
							(circ0	75 'maroon)
							(circ0	50 'silver)
							(circ0  25 'maroon)))
		(corners (use c))))

(svgop bigtest3 req
	(w/svg (c (topleft
							(circ0 100 'silver)
							(circ0	75 'maroon)
							(circ0	50 'silver)
							(circ0  25 'maroon)))
		(corners (use c))
		(sides (circ0 50 'lime))))



; sin and cos were added to ac.scm

(= pi 3.14159)

(mac sincos (x y init end by . body)
	(w/uniq g
	 `(with (,(carif x) nil ,(carif y) nil)
			(step ,g ,init ,end ,by
				(with (,(carif x) (* (sin ,g) ,(if (acons x) (cadr x) 1))
							 ,(carif y) (* (cos ,g) ,(if (acons y) (cadr y) 1)))
					,@body)))))

(mac sinwave (x y init end by . body)
 `(with (,x nil ,(carif y) nil)
		(step ,x ,init ,end ,by
			(let ,(carif y) (* (sin ,x) ,(if (acons y) (cadr y) 1))
				,@body))))

(mac coswave (x y init end by . body)
 `(with (,x nil ,(carif y) nil)
		(step ,x ,init ,end ,by
			(let ,(carif y) (* (cos ,x) ,(if (acons y) (cadr y) 1))
				,@body))))

(svgop waves req
  (let (c1 c2 c3) (n-of 3 (randcolor))
    (step y 200 400 100
      (movey y
        (step x 0 1400 2
          (circ x (* (sin x) 25) 5 (rotate c1 c2 c3) .5))))))

(svgop redwaves req
  (bg 'black)
  (stepmovey 200 400 100
    (sinwave x (y 25) 0 1400 2
      (circ x y 5 'red .5))))

(svgop sc req
  (bg 'black)
  (mid
    (sincos (x 150) (y 150) 0 12 1
      (circ x y 150 'red .25 'white))))


