-- Universidade Federal de Pernambuco
-- Centro de Informática (CIn)
-- Alunos: Victor Sin Yu Chen & Lucas de Souza Albuquerque
-- Login: vsyc & lsa2

---------------------------------------------------------------------------------------------

{-Trabalho 10, Questão 1-}
	{-Tipos-}
{-
--------------------------------------------
1) (foldr (+)).(.).map

---

(.) :: (b->c)->(a->b)->(a->c)
map :: (a1->b1)->[a1]->[b1]
. :: (u->v)->(t->u)->(t->v)

(u->v) = (.)
u = (b->c)
v = (a->b)->(a->c)
(t->u) = map
t = (a1->b1)
u = [a1]->[b1]

u = u
(b->c) = [a1]->[b1]
b = [a1]
c = [b1]

t->v = (.).map =
	(a1->b1)->(a->[a1])->(a->[b1])

----

foldr :: (a->b->b) -> b -> [a] -> b
foldr(+) =
	Num b -> b -> [b] -> b

----

. :: (u->v)->(t->u)->(t->v)

(u->v) = foldr(+) = b->[b]->b
u = b
v = [b]->b
(t->u) = (.).map
t = (a1->b1)
u = (a->[a1])->(a->[b1])

u = u
b = (a->[a1])->(a->[b1])

foldr(+).(.).map = 
	Num (a->[a1])->(a->[b1]) :: 
		(a1->b1) -> [(a->[a1])->(a->[b1])] -> (a->[a1])->(a->[b1])
--------------------------------------------
2) (\x y z -> foldr z x y).map

---

foldr :: (a->b->b) -> b -> [a] -> b
(\x y z -> foldr z x y) :: b -> [a] -> (a->b->b) -> b

---

map :: (a1->b1)->[a1]->[b1]
. :: (u->v)->(t->u)->(t->v)

(u->v) = (\x y z -> foldr z x y) =  b -> [a] -> (a->b->b) -> b
u = b
v = ([a] -> (a->b->b) -> b)
(t->u) = map = (a1->b1)->[a1]->[b1]
t = (a1->b1)
u = [a1]->[b1]

u = u
b = [a1]->[b1]

t->v = (\x y z -> foldr z x y).map = 
	(a1->b1) -> [a] -> (a-> ([a1]->[b1])-> [a1]->[b1]) -> [a1]->[b1]
--------------------------------------------
3) map.((.) (foldr (++) (foldr (++) [] [[1], [2]])))

----
foldr :: (a->b->b) -> b -> [a] -> b
(++) :: [t] -> [t] -> [t]

foldr(++) [] [[1],[2]]
(a->b->b) = (++) = [t] -> [t] -> [t]
b = [t]
foldr(++) [] [[1],[2]] = 
	Num a => [a]

----
foldr(++) [] [[1],[2]] : Num a => [a]
(++) :: [t] -> [t] -> [t]

foldr (++) (foldr (++) [] [[1], [2]])
(a->b->b) = (++) = [t] -> [t] -> [t]
a = [t]
[a] = [[t]]
b = [t]

foldr (++) (foldr (++) [] [[1], [2]]) = 
	Num a => [[a]] -> [a]

----
foldr (++) (foldr (++) [] [[1], [2]]) : Num a => [[a]] -> [a]
(.) :: (u->v)->(t->u)->(t->v)
(.) (foldr (++) (foldr (++) [] [[1], [2]]))

(u->v) = foldr (++) (foldr (++) [] [[1], [2]]) = Num a => [[a]] -> [a]
u = Num a => [[a]]
v = Num a => [a]

(t->u)->(t->v) = (.) (foldr (++) (foldr (++) [] [[1], [2]])) = 
	Num a => (t -> [[a]]) -> t -> [a]
--------------------------------------------
4) (foldr).(.)$(!!) = (foldr.(.))(!!)

----
foldr :: (a->b->b) -> b -> [a] -> b
(.) :: (b1->c1)->(a1->b1)->(a1->c1)
. :: (u->v)->(t->u)->(t->v)

(u->v) = foldr = (a->b->b) -> b -> [a] -> b
u = (a->b->b)
v = (b->[a]->b)
(t->u) = (.) = (b1->c1)->(a1->b1)->(a1->c1)
t = (b1->c1)
u = (a1->b1)->(a1->c1)

u = u 
(a->b->b) = (a1->b1)->(a1->c1)
a = (a1->b1)
(b->b) = (a1->c1)
b = a1 = c1

t->v = foldr.(.) = 
	(b1 -> b) -> b -> [b -> b1] -> b

----
$ : (t->u) -> t -> u
(!!) : [a] -> Int -> a

(t->u) = foldr.(.) = (b1 -> b) -> b -> [b -> b1] -> b
t = (b1 -> b)
u = (b -> [b -> b1] -> b)
t = [a] -> Int -> a
b1 = [a]
b = Int -> a

u = (foldr).(.)$(!!) = 
	(Int -> a) -> [(Int -> a) -> [a]] -> Int -> a
--------------------------------------------
-}