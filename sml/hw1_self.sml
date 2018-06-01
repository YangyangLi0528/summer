fun sum_list(xs : int list)=
    if null xs
    then 0
    else hd xs + sum_list(tl xs)
			 

fun countdown (x : int)=
    if x=0
    then []
    else x ::countdown(x-1)

fun append(xs : int list,ys:int list)=
    if null xs
    then ys
    else (hd xs):: append((tl xs),ys)

fun sum_pair_list(xs : (int * int) list) =
    if null xs
    then 0
    else #1 (hd xs) + #2(hd xs) + sum_pair_list(tl xs)

fun firsts(xs : (int * int) list) =
    if null xs
    then []
    else (#1 (hd xs)) :: firsts(tl xs)
			       
			 
fun mul_list(xs : int list)=
    if null xs
    then 1
    else hd xs * mul_list(tl xs)
			 
fun silly1(z:int)=
    let
	val x= if z > 0 then z else 34
	val y= x + z +9
    in
	if x > y then x *2 else y*y
    end
	
				    
fun count (from : int,to : int)=
    if from = to
    then to ::[]
    else from :: count(from+1,to)
		      
		
fun countup_from1(x:int)=
    let fun count(from : int)=
	    if from =x
	    then x::[]
	    else from :: count(from+1)
    in count(1)
    end
	
fun bad_max (xs:int list)=
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else if hd xs > bad_max(tl xs)
    then hd xs
    else bad_max(tl xs)
		

fun good_max(xs : int list)=
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else
	let val tl_ans = good_max(tl xs)
	in if hd xs > tl_ans
	   then hd xs
	   else tl_ans
	end

fun good_max_option(xs : int list)=
    if null xs
    then NONE
    else let val tl_ans = good_max_option(tl xs)
	 in if isSome tl_ans andalso valOf tl_ans > hd xs
	    then tl_ans
	    else SOME(hd xs)
	 end
fun good_max_option2(xs : int list)=
    if null xs
    then NONE
    else let(* fine to assume argument nonempty because it is local*)
	fun max_nonempty(xs:int list)=
		 if null (tl xs)
		 then hd xs
		 else let val tl_ans = max_nonempty(tl xs)
		      in if hd xs > tl_ans
			 then hd xs
			 else tl_ans
		      end
	 in SOME(max_nonempty xs)
	 end
	     
				  
fun append (xs:int list,ys:int list)=
    if null xs
    then ys
    else hd (xs):: append(tl(xs),ys)
			 

	
		
fun countup(from :int,to:int)=
    if from = to
    then to::[]
    else from :: countup(from+1,to)

fun countdown(from : int,to :int)=
    if from = to
    then to :: []
    else from :: countdown(from-1,to)
	
	
	
	
fun sum_two_pairs (pr1 : int * int,pr2 :int * int)=
    (#1 pr1)+ (#2 pr1) + (#1 pr2) + (#2 pr2)
					
fun div_mod(x:int,y:int)=
    (x div y,x mod y)

fun sort_pair(pr : int*int)=
    if(#1 pr) < (#2 pr)
    then pr
    else (#2 pr,#1pr)
	
	
			  
