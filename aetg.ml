

type coverage = Covered | Uncovered 


(*auxiliarry functions (not relevant to the algorithm) *)



let init_list ~f ~length =
  let rec aux f i l = 
    if i<length then 
      aux f (i+1) ((f i)::l)
    else
      l
  in
  aux f 0 []




let print_test test =
  Array.iter (fun value -> print_int value) test;
  print_string "\n"


let print_test_list testsuite = 
  List.iter (fun test -> print_test test) testsuite



let add_pairs_from coveredPairs test = 
    
    let n = Array.length test in
    
    for i=1 to (n-1) do
        for j=0 to (i-1) do
	  Hashtbl.replace coveredPairs (i,j,test.(i),test.(j)) Covered
        done;
    done  

let check domains testsuite =
    
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
    List.iter (add_pairs_from coveredPairs) testsuite ;
    Hashtbl.fold (fun _ coverageStatus precedent -> (precedent&&(coverageStatus==Covered))) coveredPairs true
   




(* ******************************************************** *)
(* AETG *)
(* ******************************************************** *)

let give_best_param_value domains uncoveredPairs =
  let n = Array.length domains in

  let count = Hashtbl.create (n*domains.(0)) in

  Hashtbl.iter (fun (k,i,vk,vi) _ -> begin
    if not (Hashtbl.mem count (k,vk)) then Hashtbl.add count (k,vk) 1
    else Hashtbl.replace count (k,vk) (1+Hashtbl.find count (k,vk));
    
    if not (Hashtbl.mem count (i,vi)) then Hashtbl.add count (i,vi) 1
    else Hashtbl.replace count (i,vi) (1+Hashtbl.find count (i,vi))
  end
  ) uncoveredPairs;

  let bestp,bestv,_ =  Hashtbl.fold (fun (i,vi) countForThem (imax, vimax, countmax) ->
    if countForThem > countmax then (i,vi,countForThem)
    else (imax, vimax, countmax))
    count
    (-1,-1,-1)
  in
  bestp,bestv

(*Retourne la valeur pour le paramètre numero paramNbinOrdering dans l'ordering
telle que le nombre de paires uncovered qu'elle forme avec les autres paramètres déjà 
attribués soit le plus haut possible*)
let best_value_for_param domains ordering uncoveredPairs paramNbinOrdering test =

  let pki = List.nth ordering paramNbinOrdering in
  let candidateValues = Array.make (domains.(pki)) 0 in
  let assignedParam = Core_kernel.Core_list.sub ordering ~pos:0 ~len:paramNbinOrdering in
  
  Hashtbl.iter (fun (k,i,vk,vi) _ -> begin
    if (k==pki)&&(List.mem i assignedParam)&&(test.(i)==vi) then candidateValues.(vk) <- candidateValues.(vk)+1
    else begin
      if (i==pki)&&(List.mem k assignedParam)&&(test.(k)==vk) then candidateValues.(vi) <- candidateValues.(vi)+1
    end
  end) uncoveredPairs;
  
  let (bestValue,numberOfPairsCovered,_) = Array.fold_left (fun (bestValue,maxScore,currentValue) score -> begin
    if score > maxScore then (currentValue,score,currentValue+1)
    else (bestValue,maxScore,currentValue+1)
  end) (0,-1,0) candidateValues in

  bestValue,numberOfPairsCovered






let aetg domains m = 
  let n = Array.length domains in
  let uncoveredPairs = Hashtbl.create (n*domains.(0)*domains.(0)) in
  let random = Core_kernel.Core_random.State.make_self_init () in

  (*initialise les paires non couvertes*)
  
  for i=1 to (n-1) do
    for k=0 to (i-1) do
            
      for vi=0 to (domains.(i)-1) do
        for vk=0 to (domains.(k)-1) do
                
          Hashtbl.add uncoveredPairs (k,i,vk,vi) Uncovered;
        done;
      done;
    done;
  done;  



  let rec aetg_aux testsuite =

    if (Hashtbl.length uncoveredPairs == 0) then testsuite
    else begin
      let bestp,bestv = give_best_param_value domains uncoveredPairs in

      let candidateTests = Array.make m [||] in
      let scores = Array.make m 0 in

      (*make m candidate tests *)

      for candidateNb=0 to (m-1) do
	let l = init_list ~f:(fun k -> k) ~length:n in
	let ll = List.filter (fun k -> if (k==bestp) then false else true) l in
	let ordering = bestp::(Core_kernel.Core_list.permute ~random_state:random ll) in

	let test = Array.make n 0 in
	test.(bestp) <- bestv;

	for i=1 to (n-1) do
	  let bestValue, nbOfPairsCov = best_value_for_param domains ordering uncoveredPairs i test in
	  test.(List.nth ordering i) <- bestValue;
	  scores.(candidateNb) <- (scores.(candidateNb)+nbOfPairsCov)
	done;
	candidateTests.(candidateNb) <- test
      done;

      (*choose the best candidate *)

      let bestCandidateNb,_,_ = Array.fold_left (fun (bestNb,bestScore,currentNb) score -> begin
	if score > bestScore then (currentNb,score,currentNb+1)
	else (bestNb,bestScore,currentNb+1)
      end) (0,-1,0) scores in

      let newTest = candidateTests.(bestCandidateNb) in

      (*remove the coveredPairs *)
      
      for i=1 to (n-1) do
	for k=0 to (i-1) do
	  Hashtbl.remove uncoveredPairs (k,i,newTest.(k),newTest.(i))
	done
      done;

      aetg_aux (newTest::testsuite)
    end
  in
  
  aetg_aux []
  


let () =
    let domains = [|3;3;3|] in
    let testsuite = aetg domains 50 in

    print_string (string_of_bool (check domains testsuite));
    print_string "\n";
    print_string "size : ";
    print_int (List.length testsuite);
    print_string "\n";
    print_test_list testsuite
      
