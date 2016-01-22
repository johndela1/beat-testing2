nums = [100,50,50]

mkSmother inertia = \old new->round((old*inertia + new*(1-inertia))/100)
sm = mkSmother(50)

foo [] _ = []
foo (new:xs) old = old -- [sm(old, new)] ++ foo(xs sm(old,new))

x=foo([])


main = print("hgey")
