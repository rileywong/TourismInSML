Control.Print.printLength := 100;


fun remove(x,L) = if (L=[]) then []
	else if x=hd(L)then remove(x,tl(L))
	else hd(L)::remove(x,tl(L));

fun removedupl(L) =
	if (L=[]) then []
	else hd(L)::removedupl(remove(hd(L),tl(L)));

fun map(f,L) = if (L=[]) then []
	else f(hd(L))::(map(f,tl(L)));

fun myInterleave(x,[]) = [[x]]
	| myInterleave(x,h::t) =
		(x::h::t)::(map((fn l => h::l), myInterleave(x,t)));

fun appendAll(nil) = nil
	| appendAll(z::zs) = z @ (appendAll(zs));

fun permute(nil) = [[]]
	| permute(h::t) = appendAll(
		map((fn l => myInterleave(h,l)), permute(t)));

fun makeList(x)= if x = 0 then []
	else x :: makeList(x-1); 

fun getIndex(perm,location) = 
	if location = hd perm then 0
	else 1 + getIndex(tl perm, location); 

fun getMin(x,y) = 
	if x > y then y
	else x

fun notmember(X,[]) = true
	| notmember(X,Y::Ys) =
		if (X=Y) then false
		else notmember(X,Ys);

fun transHelp(tup,[]) = []
	| transHelp(tup:(int * int * int),pref :(int * int * int) list) =
		if #1(tup) = #1(hd pref) then
			if #3(tup) = #2(hd pref)  then
				(#1(tup),#2(tup),#3(hd pref)) :: transHelp(tup,tl pref)
			else if #2(tup) = #3(hd pref)  then
				(#1(tup),#2(hd pref),#3(tup)) :: transHelp(tup,tl pref)
			else transHelp(tup,tl pref)
		else
		 transHelp(tup,tl pref);

fun generateTransitivity([]) = []
	| generateTransitivity(pref::prefTail) = 
		(pref :: transHelp(pref,prefTail))  @ generateTransitivity(prefTail@transHelp(pref,prefTail));


fun sumError(perm,[]) = 0
	| sumError(perm, rule :: ruleTail :(int * int * int) list) = 
		if getIndex(perm,#2(rule)) > getIndex(perm,#3(rule)) then
			1 + sumError(perm, ruleTail)
		else 
			sumError(perm,ruleTail);


fun getMinErrorOfPermutes([],rules) = 999999
	| getMinErrorOfPermutes(perm ::permTail ,rules ) = 
		getMin(sumError(perm,removedupl(generateTransitivity(rules))), getMinErrorOfPermutes(permTail,rules));


fun violations(NumberOfPeople:int, NumberOfLocations:int, 
	NumberOfPreferences:int, Preferences:(int * int * int) list)=
	getMinErrorOfPermutes(permute(makeList(NumberOfLocations)),Preferences);



(*transHelp(tup,prefTail);

transHelp((1,2,3),[(1,3,4),(2,4,5),(1,1,2)]);
generateTransitivity([(2,2,3),(1,3,4),(1,1,2)]);
generateTransitivity([(2,2,3),(2,3,4),(2,1,2)]);
generateTransitivity([(1,1,3),(1,3,4),(1,4,10)])
sumError([1,2,3,4],[(1,2,3),(1,3,2),(4,4,1),(4,1,4)]);
sumError([1,2,3,4],[(1,2,3),(1,3,2),(4,3,2),(4,2,1)]);
getMinErrorOfPermutes(permute(makeList(4)),[(2,2,3),(1,3,4),(1,1,2)]);
getMinErrorOfPermutes(permute(makeList(4)),[(2,2,3),(1,3,2),(1,1,2)]);
getMinErrorOfPermutes(permute(makeList(4)),[(1,1,2),(1,2,4),(1,4,3),(2,2,1),(2,1,3),(3,3,4),(3,4,1),(4,4,3),(4,3,2)]);
getMinErrorOfPermutes(permute(makeList(4)),[(1,1,2),(1,2,3),(2,3,2),(2,2,1)]);

getMinErrorOfPermutes(permute(makeList(4)),[(1,1,2),(1,2,4),(2,1,3),(2,3,4)]);
getMinErrorOfPermutes(permute(makeList(3)),[(1,1,2),(1,2,3),(2,3,2),(2,2,1)]);
getMinErrorOfPermutes(permute(makeList(4)),[(1,1,2),(1,2,3),(2,2,1),(2,1,3),(3,3,4),(3,4,1),(4,4,3),(4,3,2)]);

getMinErrorOfPermutes(permute(makeList(4)),[(1,1,2),(1,2,4),(1,4,3),(2,2,1),(2,1,3),(3,3,4),(3,4,1),(4,4,3),(4,3,2)]);
6
getMinErrorOfPermutes(permute(makeList(4)),[(1,1,2),(1,2,3),(1,3,4),(2,2,3),(2,3,4),(2,4,1),(3,3,4),(3,4,1),(3,1,2),(4,4,3),(4,3,2),(4,2,1)]);
9

generateTransitivity([(1,1,2),(1,2,3),(1,3,4),(2,2,3),(2,3,4),(2,4,1),(3,3,4),(3,4,1),(3,1,2),(4,4,3),(4,3,2),(4,2,1)]);

violations(4,4,8,[(1,1,2),(1,2,3),(2,4,3),(2,3,2),(3,2,4),(3,4,1),(4,1,3),(4,3,4)]);
violations(4,4,12,[(1,1,2),(1,2,3),(1,3,4),(2,2,3),(2,3,4),(2,4,1),(3,3,4),(3,4,1),(3,1,2),(4,4,3),(4,3,2),(4,2,1)]);

removedupl(generateTransitivity([(1,1,2),(1,2,3),(1,3,4),(2,2,3),(2,3,4),(2,4,1),(3,3,4),(3,4,1),(3,1,2),(4,4,3),(4,3,2),(4,2,1)]));

fun union([],L2) = L2
	| union(X::Xs,L2) =
		if member(X,L2) then union(Xs,L2)
		else X::union(Xs,L2);

fun subset([],L2) = true
	| subset(L1,[]) = if(L1=[])
		then true
		else false
	| subset(X::Xs,L2) =
		if member(X,L2)
			then subset(Xs,L2)
			else false;
 

fun product_one(X,[]) = []
| product_one(X,Y::Ys) =
(X,Y)::product_one(X,Ys);

product_one(1,[2,3]);
[(1,2),(1,3)] 

fun product([],L2) = []
| product(X::Xs,L2) =
union(product_one(X,L2),
product(Xs,L2));



true andalso false;

- val five = 3+2;
val five = 5 : int

fun <identifier> (<parameters>) =
<expression>;

if ((x>y) andalso (x>z)) then x
else (if (y>z) then y else z);
val max = fn : int * int * int -> int

max(3,2,2);

fun factorial(x) = if x=0 then 1
else x*factorial(x-1);

	 #1(t1);
val it = 1 : int //to access certain indices of a tuple

[1]::[2,3];  "::" called read cons
Error: operator and operand don’t agree

*)