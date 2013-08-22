s:([s:`s1`s2`s3`s4`s5]
 name:`smith`jones`blake`clark`adams;
 status:20 10 30 20 30;
 city:`london`paris`paris`london`athens)

p:([p:`p1`p2`p3`p4`p5`p6]
 name:`nut`bolt`screw`screw`cam`cog;
 color:`red`green`blue`red`blue`red;
 weight:12 17 17 14 12 19;
 city:`london`paris`rome`london`paris`london)

sp:([]
1 s:`s$`s1`s1`s1`s1`s4`s1`s2`s2`s3`s4`s4`s1;	/ foreign key ('$' here is an operator which takes params on the right and "converts" them into 's' table which is on the left)
 p:`p$`p1`p2`p3`p4`p5`p6`p1`p2`p2`p2`p4`p5;	/ foreign key
 qty:300 200 400 200 100 100 300 400 200 200 300 400)

show s
show p
show sp

show "----- selects -----"
show select from p where weight=17
show select p, p.city from sp
show meta sp
show select qty by s from sp
show ungroup select qty by s from sp


exit 0