terna :: Int -> [(Int, Int, Int)]
terna x = [(a,b,c)|a<-[1..x], b<-[1..x], c<-[1..x], (a^2)+(b^2) == (c^2)]

terna' :: Int -> [(Int, Int)]
terna' x = [(a+b+c,a*b*c) |a<-[1..x], b<-[1..x], c<-[1..x], (a^2)+(b^2) == (c^2)]

specialTriplet = find ((== 1000) . fst) (terna' 500)
