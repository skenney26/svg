; skenney26@gmail.com
; http://arcxs.posterous.com/


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
                (it str req)))				; this is the only change so far
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


; redefine start-tag and tag-options from html.arc
; no need to keep track of tag attributes

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


; svg request

(mac svgop (name parm . body)
  (w/uniq gs
    `(defop-raw ,name (,gs ,parm)
       (w/stdout ,gs
				(prn svg-header*)
				(prn)
				(tag (svg xmlns "http://www.w3.org/2000/svg"
									xmlns:xlink "http://www.w3.org/1999/xlink")
					,@body)))))


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
; useful for nesting - see letters.arc

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


; svg

(def svgid (id)
	(string "#" id))

(def svgurl (id)
	(string "url(#" id ")"))

(def randcolor ()
  (tostring
    (pr "#")
    (repeat 6
			(pr (rand-choice 0 1 2 3 4 5 6 7 8 9 'a 'b 'c 'd 'e 'f)))))

(def between (x y)
	(+ (rand (+ (- y x) 1)) x))

(def around (x y)
	(+ x (between (- y) y)))

(mac group body
	(w/uniq id
	 `(do (tag (g id ',id) ,@body)
				(svgid ',id))))

(mac w/svg (parms . body)
 `(with (,@(mappend (fn (x) `(,x nil))
										(map car (pair parms))))
		(tag defs
			,@(map (fn ((x y))
							`(= ,x (group ,y)))
						 (pair parms)))
		,@body))

(mac defsvg (name parms . body)
 `(def ,name ,parms
		(group
			(tag ,@body))))

(defsvg use (id (o x 0) (o y x) (o o 1))
	(use xlink:href id x x y y opacity o))

(defsvg rect (x y w h f (o o 1) (o s 'none) (o n 1))
	(rect x x y y width w height h fill f
				opacity o stroke s stroke-width n))

(def rect0 (w h f (o o 1) (o s 'none) (o n 1))
	(rect 0 0 w h f o s n))

(def bg (f (o o 1))
	(rect 0 0 "100%" "100%" f o))

(defsvg roundrect (x y w h rx ry f (o o 1) (o s 'none) (o n 1))
	(rect x x y y width w height h rx rx ry ry fill f
				opacity o stroke s stroke-width n))

(def roundrect0 (w h rx ry f (o o 1) (o s 'none) (o n 1))
	(roundrect 0 0 w h rx ry f o s n))

(defsvg sqr (x y w f (o o 1) (o s 'none) (o n 1))
	(rect x x y y width w height w fill f
				opacity o stroke s stroke-width n))

(def sqr0 (w f (o o 1) (o s 'none) (o n 1))
	(sqr 0 0 w f o s n))

(defsvg circ (x y r f (o o 1) (o s 'none) (o n 1))
	(circle cx x cy y r r fill f
					opacity o stroke s stroke-width n))

(def circ0 (r f (o o 1) (o s 'none) (o n 1))
	(circ 0 0 r f o s n))

(defsvg oval (x y rx ry f (o o 1) (o s 'none) (o n 1))
	(ellipse cx x cy y rx rx ry ry fill f
								opacity o stroke s stroke-width n))

(def oval0 (rx ry f (o o 1) (o s 'none) (o n 1))
	(oval 0 0 rx ry f o s n))

(def ring (x y rx ry color (o stroke-width 1) (o opacity 1))
	(oval x y rx ry 'none opacity color stroke-width))

(def ring0 (rx ry color (o stroke-width 1) (o opacity 1))
	(ring 0 0 rx ry color stroke-width opacity))


(defsvg poly (ps f (o o 1) (o s 'none) (o n 1))
	(polygon points (tostring (apply prs ps)) fill f
					 opacity o stroke s stroke-width n))

(def poly0 (ps f (o o 1) (o s 'none) (o n 1))
	(poly (join (list 0 0) ps) f o s n))

(defsvg roundpoly (ps f (o o 1) (o s 'none) (o n 1))
	(polygon points (tostring (apply prs ps)) fill f
					 opacity o stroke s stroke-width n
					 stroke-linejoin 'round stroke-linecap 'round))

(def roundpoly0 (ps f (o o 1) (o s 'none) (o n 1))
	(roundpoly (join (list 0 0) ps) f o s n))

(def tri (x1 y1 x2 y2 x3 y3 f (o o 1) (o s 'none) (o n 1))
	(poly (list x1 y1 x2 y2 x3 y3) f o s n))

(def tri0 (x1 y1 x2 y2 f (o o 1) (o s 'none) (o n 1))
	(tri 0 0 x1 y1 x2 y2 f o s n))

(defsvg line (x1 y1 x2 y2 color (o w 1) (o o 1))
	(polyline points (tostring:prs x1 y1 x2 y2) stroke color
						stroke-width w opacity o fill 'none))

(def line0 (x y color (o w 1) (o o 1))
	(line 0 0 x y color w o))

(defsvg path (d s (o w 1) (o o 1))
	(path d d stroke s
				stroke-width w opacity o fill 'none))

(defsvg roundpath (d s (o w 1) (o o 1))
	(path d d stroke s stroke-width w opacity o
				stroke-linejoin 'round stroke-linecap 'round fill 'none))

(defsvg curve (x1 y1 x2 y2 x3 y3 s (o w 1) (o o 1))
	(path d (tostring:prs 'm x1 y1 'q x2 y2 x3 y3)
				stroke s stroke-width w opacity o fill 'none))

(def curve0 (x1 y1 x2 y2 s (o w 1) (o o 1))
	(curve 0 0 x1 y1 x2 y2 s w o))

(defsvg shape (d f (o o 1) (o s 'none) (o w 1))
	(path d d fill f opacity o stroke s stroke-width w))

(def text (str x y fill (o font) (o size) (o weight) (o opacity))
	(group
		(tag (text x x y y fill fill font-family font
							 font-size size font-weight weight opacity opacity)
			(pr str))))

(defsvg image (href x y w h (o o 1))
	(image xlink:href href x x y y width w height h opacity o))

(def image0 (href w h (o o 1))
	(image href 0 0 w h o))

(mac transform (trans . body)
	(w/uniq id
	 `(do (tag (g id ',id transform ,trans)
					,@body)
				(svgid ',id))))

(mac move (x y . body)
 `(transform (string "translate(" ,x " " ,y ")")
		,@body))

(mac movex (x . body)
 `(move ,x 0 ,@body))

(mac movey (y . body)
 `(move 0 ,y ,@body))

(mac stepmove (init end by . body)
	(w/uniq v
	 `(group
			(step ,v ,init ,end ,by
				(move ,v ,v
					,@body)))))

(mac stepmovex (init end by . body)
	(w/uniq v
	 `(group
			(step ,v ,init ,end ,by
				(move ,v 0
					,@body)))))

(mac stepmovey (init end by . body)
	(w/uniq v
	 `(group
			(step ,v ,init ,end ,by
				(move 0 ,v
					,@body)))))

(mac rot (angle . body)
 `(transform (string "rotate(" ,angle ")")
		,@body))

(mac steprot (init end by . body)
	(w/uniq v
	 `(group
			(step ,v ,init ,end ,by
				(rot ,v
					,@body)))))

(mac rotmid (angle cx cy . body)
 `(transform (string "rotate(" ,angle " " ,cx " " ,cy ")")
		,@body))

(mac scale (x y . body)
 `(transform (string "scale(" ,x " " ,y ")")
		,@body))

(mac scalex (x . body)
 `(scale ,x 1 ,@body))

(mac scaley (y . body)
 `(scale 1 ,y ,@body))

(mac stepscale (init end by . body)
	(w/uniq v
	 `(group
			(step ,v ,init ,end ,by
				(scale ,v ,v
					,@body)))))

(mac skewx (x . body)
 `(transform (string "skewX(" ,x ")")
		,@body))

(mac skewy (y . body)
 `(transform (string "skewY(" ,y ")")
		,@body))

(mac skew (x y . body)
 `(skewx ,x
		(skewy ,y ,@body)))

(mac flipx body
 `(scale -1 1 ,@body))

(mac flipy body
 `(scale 1 -1 ,@body))

(mac flip body
 `(scale -1 -1 ,@body))

(def grad (angle . args)
	(let id (uniq)
		(tag (linearGradient id id gradientTransform (string "rotate(" angle ")"))
			(each (c o f) (tuples args 3)
				(tag (stop stop-color c stop-opacity o offset f))))
		(svgurl id)))

(def grad1 (angle color)
	(grad angle color 1 0
							color 0 1))

(def grad2 (angle c1 c2)
	(grad angle c1 1 0 c2 1 1))

(def grad3 (angle c1 c2 c3)
	(grad angle c1 1 0 c2 1 .5 c3 1 1))

(def grad4 (angle c1 c2 c3 c4)
	(grad angle c1 1 0 c2 1 .33 c3 1 .66 c4 1 1))

(def grad5 (angle c1 c2 c3 c4 c5)
	(grad angle c1 1 0 c2 1 .25 c3 1 .5 c4 1 .75 c5 1 1))

(def rad args
	(let id (uniq)
		(tag (radialGradient id id cx .5 cy .5 r .5 fx .5 fy .5)
			(each (c o f) (tuples args 3)
				(tag (stop stop-color c stop-opacity o offset f))))
		(svgurl id)))

(def rad1 (color (o middle t))
	(if middle
			(rad color 1 0 color 0 1)
			(rad color 0 0 color 1 1)))

(def rad2 (color1 color2)
	(rad color1 1 0 color2 1 1))

(def rad3 (c1 c2 c3)
	(rad c1 1 0 c2 1 .5 c3 1 1))

(def rad4 (c1 c2 c3 c4)
	(rad c1 1 0 c2 1 .33 c3 1 .66 c4 1 1))

(def rad5 (c1 c2 c3 c4 c5)
	(rad c1 1 0 c2 1 .25 c3 1 .5 c4 1 .75 c5 1 1))

(mac blur (n . body)
	(w/uniq id
	 `(do (tag (filter id ',id)
					(tag (feGaussianBlur stdDeviation ,n)))
				(tag (g filter (svgurl ',id))
					,@body))))

(mac svglink (href . body)
 `(group
		(tag (a xlink:href ,href)
			,@body)))

(mac poser (name x y)
 `(mac ,name args
	 `(group
			,@(map (fn (a)
							 (w/uniq g
								`(w/svg (,g ,a)
									 (use ,g ,,x ,,y))))
						 args))))

(mac posers args
 `(do ,@(map (fn (a)
							`(poser ,@a))
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

(mac mx (expr)
 `(ppr (macex1 ',expr)))

(= pi 3.14159)

; add sine and cosine to ac.scm before uncommenting
; http://arcxs.posterous.com/waves

;(mac sincos (x y init end by . body)
;	(w/uniq g
;	 `(with (,(carif x) nil ,(carif y) nil)
;			(step ,g ,init ,end ,by
;				(with (,(carif x) (* (sin ,g) ,(if (acons x) (cadr x) 1))
;							 ,(carif y) (* (cos ,g) ,(if (acons y) (cadr y) 1)))
;					,@body)))))

;(mac sinwave (x y init end by . body)
; `(with (,x nil ,(carif y) nil)
;		(step ,x ,init ,end ,by
;			(let ,(carif y) (* (sin ,x) ,(if (acons y) (cadr y) 1))
;				,@body))))

;(mac coswave (x y init end by . body)
; `(with (,x nil ,(carif y) nil)
;		(step ,x ,init ,end ,by
;			(let ,(carif y) (* (cos ,x) ,(if (acons y) (cadr y) 1))
;				,@body))))
