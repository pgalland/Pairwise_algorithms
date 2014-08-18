

type coverage = Covered | Uncovered 

type value = Assigned of int | Anything | Untouched

exception No_anything_or_untouched_here


let init_list ~f ~length =
  let rec aux f i l = 
    if i<length then 
      aux f (i+1) ((f i)::l)
    else
      l
  in
  aux f 0 []



let print_test_ipo test =
  Array.iter 
    (fun value -> match value with
      | Anything -> print_string "Anything" 
      | Untouched -> print_string "Untouched"
      | Assigned k -> print_int k)
    test;
  print_string "\n"

let print_test_list_ipo testsuite =
  List.iter (fun test -> print_test_ipo test) testsuite 




(* Ajoute toutes les pairs couvertes par le test à la table de hachage coveredPairs *)
let add_pairs_from_ipo coveredPairs test = 
    
    let n = Array.length test in
    
    for i=1 to (n-1) do
        for j=0 to (i-1) do
	  match test.(i), test.(j) with 
	    |Assigned vi, Assigned vj -> Hashtbl.replace coveredPairs (i,j,vi,vj) Covered
	    | _, _ -> raise No_anything_or_untouched_here
        done;
    done  


(* Parcoure la testsuite et pour chaque test marque comme couvertes les paires couvertes par ce test 
dans la table coveredPairs, puis vérifie que toutes les paires de la table sont couvertes  *)
let check_ipo domains testsuite =
    
    let n = Array.length domains in
    let dmax = Array.fold_left max (-1) domains in
    let coveredPairs = Hashtbl.create (dmax*dmax*n) in (*il faut faire plus grand *)
    
    for i=1 to (n-1) do
        for j=0 to (i-1) do
            
            for vi=0 to (domains.(i)-1) do
                for wj=0 to (domains.(j)-1) do
                
                    Hashtbl.add coveredPairs (i,j,vi,wj) Uncovered;
                done;
            done;
        done;
    done;
    List.iter (add_pairs_from_ipo coveredPairs) testsuite ;
    Hashtbl.fold (fun _ coverageStatus precedent -> (precedent&&(coverageStatus==Covered))) coveredPairs true
   






(*Crée une suite de test initiale, qui contient le produit cartésien des
deux premiers paramètres *)
let make_initial_testsuite domains =
    
    let n = Array.length domains in
    let testsuite = ref [] in
    
    for i=0 to (domains.(0)-1) do
        for j=0 to (domains.(1)-1) do
            let test = Array.make n Untouched in
            test.(0) <- Assigned i;
            test.(1) <- Assigned j;
            testsuite := test::(!testsuite)
        done;
    done;
    
    !testsuite



let horizontal_growth domains testsuite i =

    let n = Array.length domains in
    let dmax = Array.fold_left max (-1) domains in
    let uncoveredPairs = Hashtbl.create (dmax*dmax*n) in

    (*Set all the pairs between parameter pi and the pk parameters
    as uncovered*)
    
    for k=0 to (i-1) do
        
        for vk=0 to (domains.(k)-1) do
            for wi=0 to (domains.(i)-1) do
                Hashtbl.add uncoveredPairs (k,i,vk,wi) Uncovered;
            done;
        done;
    done;

    
    (*function that returns the numbers of uncovered pairs
    that assigning valuefori to the i-th place of the test
    would cover*)
    let number_of_covering_pairs test valuefori =
      
      let aux (total,k) valueOfkInTest =
	if k<i then begin
	  match valueOfkInTest with
	    | Untouched -> raise No_anything_or_untouched_here
	    | Anything -> raise No_anything_or_untouched_here
	    | Assigned vk -> begin
	      if Hashtbl.mem uncoveredPairs (k,i,vk,valuefori) then 
		(total+1),(k+1)
	      else
		total,(k+1)
	    end
	end
	else
	  total,k
      in
      
      let total,_ = Array.fold_left aux (0,0) test in total
    
    in

    let testNb = ref 0 in

    let rec horizontal_growth_aux testsuite = 
        match testsuite with
        | [] -> []
        | test::rest -> 
	  begin
	  if !testNb < domains.(i) then begin
	    test.(i) <- Assigned !testNb;
	    (*Set as covered the pairs now covered with testNb *)
	    for k=0 to (i-1) do
	      match test.(k) with
		| Anything | Untouched -> raise No_anything_or_untouched_here
		| Assigned vk -> Hashtbl.remove uncoveredPairs (k,i,vk,!testNb)
	    done;
            testNb := !testNb + 1;
	    test::(horizontal_growth_aux rest)
	  end
	  else 
            begin

		(*Find the best value for parameter i for the test, the one
		  which covers the most uncovered pairs*)
	      let valuesOfi = init_list ~f:(fun k -> k) ~length:(domains.(i)) in
	      
	      let bestvi,_ = List.fold_left 
		(fun (maxvi,maxCovered) valueFori -> 
		  let pairsCovered = number_of_covering_pairs test valueFori in
		  if pairsCovered > maxCovered then 
		    (valueFori,pairsCovered)
		  else
		    (maxvi, maxCovered))
		(-1,-1) valuesOfi 
	      in
	      
	      test.(i) <- Assigned bestvi ;
	      
		(*Set as covered the pairs now covered with bestvi *)

	      for k=0 to (i-1) do
		match test.(k) with
		  | Anything | Untouched -> raise No_anything_or_untouched_here
		  | Assigned vk -> Hashtbl.remove uncoveredPairs (k,i,vk,bestvi)
	      done;
	      
	      test::(horizontal_growth_aux rest)
	    end
	  end

    in
            	  
    (horizontal_growth_aux testsuite), uncoveredPairs


	  
let vertical_growth  domains uncoveredPairs = 
  
  let n = Array.length domains in

  let rec vertical_growth_aux (k,i,vk,vi) _ testsuiteprime =
    match testsuiteprime with
      | [] -> 
	begin
	let test = Array.init n (fun j -> if j<i then Anything else Untouched) in
	test.(k) <- Assigned vk;
	test.(i) <- Assigned vi;
	[test]
	end
      | test::rest -> 
	begin
	if ( (test.(i)=(Assigned vi))&&(test.(k)=Anything) ) then 
	  begin
	  test.(k) <- Assigned vk;
	  test::rest
	  end
	else test::(vertical_growth_aux (k,i,vk,vi) Uncovered rest)

	end
  in
  
  let testsuiteprime = Hashtbl.fold vertical_growth_aux uncoveredPairs [] in
  

  (* reste à remplacer les Anything *)
  let count = Array.make n 0 in

  List.iter (fun test -> 
    Array.iteri 
      (fun k value -> if value==Anything then test.(k) <- Assigned count.(k); 
	count.(k) <- ((count.(k)+1) mod domains.(k))) 
      test) 
    testsuiteprime;
  testsuiteprime
    
    
let ipo domains =
 
  let testsuite = ref (make_initial_testsuite domains) in
  
  let n = Array.length domains in


  for i=2 to (n-1) do
    
    let testlist,uncoveredPairs = horizontal_growth domains !testsuite i in
    
    testsuite := testlist ;


    let testsuiteprime = vertical_growth domains uncoveredPairs in

    testsuite := testsuiteprime@(!testsuite);

  done;

  !testsuite
  

let () =
    let domains = [|3;3;3;3|] in
    let testsuite = ipo domains in
    print_string (string_of_bool (check_ipo domains testsuite));
    print_string "\n";
    print_string "size : ";
    print_int (List.length testsuite);
    print_string "\n";
    print_test_list_ipo testsuite
      
