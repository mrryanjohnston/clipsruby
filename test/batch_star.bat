(assert
	(foo 1 2 3)
	(foo 4 5)
	(foo 6)
	(bar asdf)
	(bar hjkl "123"))

(defrule do
	?f <- (foo ? ?)
	=>
	(retract ?f))

(defrule another
	?f <- (bar ?)
	=>
	(retract ?f))

(run)
