
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
(* AETG - ACA *)
(* ******************************************************** *)

  
let fitness uncoveredPairs test =
  
  let n = Array.length test in
  let result = ref 0 in

  for i=1 to (n-1) do
    for k=0 to (i-1) do
      if Hashtbl.mem uncoveredPairs (k,i,test.(k),test.(i)) then result := !result+1
    done;
  done;

  !result



let make_new_graph domains = 
  
  let n = Array.length domains in
  let graph = Hashtbl.create (n*domains.(0)) in

  for i=0 to (n-1) do
    for vi=0 to (domains.(i)-1) do
      (* pheromone and then heuristic in the second couple *)
      Hashtbl.add graph (i,vi) (0.,0.) 
    done;
  done;

  graph


let make_new_Ci_tables domains =
  
  let n = Array.length domains in
  let table = Array.make n [||] in

  for i=0 to (n-1) do
    table.(i) <- (Array.make domains.(i) 0.)
  done;

  let cimax, cimin = Array.make n 0., Array.make n 0. in
  
  table,cimax,cimin


let update_Ci_tables (ctable,cimax,cimin) newTest =

  let n = Array.length newTest in

  for i=0 to (n-1) do
    let vi = newTest.(i) in
    ctable.(i).(vi) <- ctable.(i).(vi)+.1.;
    if ctable.(i).(vi) > cimax.(i) then cimax.(i) <- ctable.(i).(vi);
    
    cimin.(i) <- Array.fold_left (fun currentMin cij -> begin
      if cij<currentMin then cij
      else currentMin
    end) cimin.(i) ctable.(i)

  done


let initialize_graph (ctable,cimax,cimin) tInitial graph =
  
  let set_one_edge (i,vi) (_,_) =
    
    let heuristic = (cimax.(i)-.ctable.(i).(vi)+.1.)/.(cimax.(i)-.cimin.(i)+.1.) in
    Hashtbl.replace graph (i,vi) (tInitial,heuristic)

  in

  Hashtbl.iter set_one_edge graph 
  
exception Problem_with_proba

let choose_edge domains alpha beta i graph =

  let sum = ref 0. in

  for vi=0 to (domains.(i)-1) do
    let phero,heuri = Hashtbl.find graph (i,vi) in
    sum := !sum +. ((phero**(alpha))*.(heuri**(beta)))
  done;

  let choices = init_list ~f:(fun vi -> begin
    let phero,heuri = Hashtbl.find graph (i,vi) in
    (((phero**(alpha))*.(heuri**(beta)))/.(!sum)),vi end)
    ~length:(domains.(i)) 
  in
  let sortedChoices = List.sort (fun (p1,_) (p2,_) -> compare p1 p2) choices in

  let rec choose_aux sortedChoices randomP =
    match sortedChoices with
      | [] -> raise Problem_with_proba
      | [(_,edge)] -> edge
      | (p,edge)::rest -> if randomP <= p then edge else choose_aux rest randomP
  in
  
  choose_aux sortedChoices (Random.float 1.)


let generate_path domains alpha beta graph =

  let n = Array.length domains in
  let newPath = Array.make n 0 in
  
  for i=0 to (n-1) do

    newPath.(i) <- choose_edge domains alpha beta i graph
  done;
  
  newPath


let integrate_elite_if_deserving antFitness antPath elitePaths elitePaths_score =

  let eliteSize = Array.length elitePaths in

  let isBetter = ref false in
  let isBetter_index = ref (-1) in

  let i = ref 0 in

  while (not !isBetter)&&(!i<eliteSize) do
    if antFitness>elitePaths_score.(!i) then begin
      isBetter := true;
      isBetter_index := !i
    end;
    i := !i+1
  done;

  if !isBetter then begin
    
    for k=(!isBetter_index) to (eliteSize-2) do
      elitePaths.(k+1)<-elitePaths.(k);
      elitePaths_score.(k+1) <- elitePaths_score.(k+1)
    done;
    elitePaths.(!isBetter_index) <- antPath;
    elitePaths_score.(!isBetter_index) <- antFitness
  end

    



let aca ~domains ~tInitial ~numberOfRounds ~numberOfAnts ~eliteSize ~alpha ~beta ~rho ~q ~stagCond = 
  let n = Array.length domains in
  let uncoveredPairs = Hashtbl.create (n*domains.(0)*domains.(0)) in
  let (ctable,cimax,cimin) = make_new_Ci_tables domains in
  Random.self_init ();
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

  

  let rec aca_aux testsuite =
    if (Hashtbl.length uncoveredPairs == 0) then testsuite
    else begin
      let graph = make_new_graph domains in
      initialize_graph (ctable,cimax,cimin)  tInitial graph;

      let bestCandidate = ref [||] in
      let bestCandidate_score = ref (-1) in
      let formerScore = ref (-1) in
      let stagnationLength = ref 0 in
      (*invariant : elite is ordered from best to worst*)
      let elitePaths = Array.init eliteSize (fun _ -> Array.make n 0) in
      let elitePaths_score = Array.make eliteSize (-1) in

      for roundNb=1 to numberOfRounds do
	
	let chosenPaths = Array.make numberOfAnts [||] in
	let chosenPaths_score = Array.make numberOfAnts 0 in


	for antNb=0 to (numberOfAnts-1) do
	  chosenPaths.(antNb) <- generate_path domains alpha beta graph;

	  (* look if it deserves elite or best *)
	  let antFitness = fitness uncoveredPairs chosenPaths.(antNb) in
	  chosenPaths_score.(antNb) <- antFitness;
	  (*elite*)
	  integrate_elite_if_deserving antFitness chosenPaths.(antNb) elitePaths elitePaths_score;
	  (*best*)
	  if antFitness>(!bestCandidate_score) then begin
	    bestCandidate := chosenPaths.(antNb);
	    bestCandidate_score := antFitness
	  end;
	  
	done;

	(*update pheromone *)
	Hashtbl.iter (fun (i,vi) (phero,heuri) -> Hashtbl.replace graph (i,vi) ((rho*.phero),heuri))
	  graph;

	for antNb=0 to (numberOfAnts-1) do
	  
	  Array.iteri (fun i vi -> begin
	    let antFitness = float_of_int chosenPaths_score.(antNb) in
	    let (phero,heuri) = Hashtbl.find graph (i,vi) in
	    let di = float_of_int domains.(i) in
	    Hashtbl.replace graph (i,vi) (phero+.q*.di*.(antFitness/.(float_of_int !bestCandidate_score)),heuri)
	  end)
	    chosenPaths.(antNb);
	done;

	for eliteNb=0 to (eliteSize-1) do
	  
	  Array.iteri (fun i vi -> begin
	    let (phero,heuri) = Hashtbl.find graph (i,vi) in
	    let sigma = float_of_int eliteSize in
	    let di = float_of_int domains.(i) in
	    Hashtbl.replace graph (i,vi) (phero+.sigma*.q*.di,heuri)
	  end)
	    elitePaths.(eliteNb);
	done;
	
	
	(*stagnation *)
	assert (!bestCandidate_score>=(!formerScore));
	
	if (!bestCandidate_score==(!formerScore)) then stagnationLength := !stagnationLength+1
	else formerScore := !bestCandidate_score;
	
	

	if !stagnationLength > stagCond then begin
	  (*set pheromone to initial *)
	  Hashtbl.iter (fun (i,vi) (_,heuri) -> begin
	    Hashtbl.replace graph (i,vi) (tInitial,heuri)
	  end) graph;
	  stagnationLength := 0 
	end;

      done;
      

      (*select the best candidate *)
	  
      let newTest = !bestCandidate in

      (*remove the coveredPairs *)
      
      for i=1 to (n-1) do
	for k=0 to (i-1) do
	  Hashtbl.remove uncoveredPairs (k,i,newTest.(k),newTest.(i))
	done
      done;
      
      (*update the ci table *)
      
      update_Ci_tables (ctable,cimax,cimin) newTest;
	  
      aca_aux (newTest::testsuite)
    end

  in

  aca_aux []








let () =
    
  let domains = [|3;3;3|] in
    
  let testsuite = aca ~domains ~tInitial:0.4 ~numberOfRounds:100 ~numberOfAnts:20 ~eliteSize:2 ~alpha:1.6 ~beta:0.2
    ~rho:0.5 ~q:0.01 ~stagCond:5 in
   
  print_string (string_of_bool (check domains testsuite));
  print_string "\n";
  print_string "size : ";
  print_int (List.length testsuite);
  print_string "\n";
  print_test_list testsuite
      

                 
