
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
(* AETG - GA *)
(* ******************************************************** *)



let make_random_tests domains m = 
  let n = Array.length domains in
  
  let rec make_random_aux nb listOfTests =
    if nb==0 then listOfTests 
    else begin
      let test = Array.init n (fun k -> Random.int domains.(k)) in
      make_random_aux (nb-1) (test::listOfTests)
    end
  in

  make_random_aux m []
    
  
let fitness uncoveredPairs test =
  
  let n = Array.length test in
  let result = ref 0 in

  for i=1 to (n-1) do
    for k=0 to (i-1) do
      if Hashtbl.mem uncoveredPairs (k,i,test.(k),test.(i)) then result := !result+1
    done;
  done;

  !result

let fitness_max uncoveredPairs testsuite =
  
  List.fold_left (fun maxscore test -> begin
    let score = fitness uncoveredPairs test in
    if score > maxscore then score 
    else maxscore
  end) (-1) testsuite


let select_fittest uncoveredPairs testsuite =
  
  let elite,score = List.fold_left (fun (bestTest,bestScore) newTest -> begin
     let newScore = fitness uncoveredPairs newTest in
     if newScore > bestScore then (newTest,newScore)
     else (bestTest,bestScore)
  end) ([||],-1) testsuite in

  (Array.copy elite),score

let tournament_match uncoveredPairs test1 test2 fittestP =
  let f1 = fitness uncoveredPairs test1 in
  let f2 = fitness uncoveredPairs test2 in

  let r = Random.float 1. in

  if f1 >= f2 then begin
    if r <= fittestP then Array.copy test1
    else Array.copy test2
  end
  else begin (* f2 > f1 *)
    if r <= fittestP then Array.copy test2
    else Array.copy test1
  end


let make_mating_pool uncoveredPairs sizeOfPool basePopulation fittestP  =
  
  let sizeOfPopulation = List.length basePopulation in
  let matingPool = ref [] in

  for i=1 to sizeOfPool do
    let k = Random.int sizeOfPopulation in
    let l = Random.int sizeOfPopulation in
    
    let choosen = tournament_match uncoveredPairs (List.nth basePopulation k) (List.nth basePopulation l) fittestP in
    matingPool := choosen::(!matingPool)
  done;

  !matingPool
    

let crossover test1 test2  =
  
  let n = Array.length test1 in
  
  for k=0 to (n-1) do
    let exchange = Random.bool () in
    if exchange then begin
      let test1k = test1.(k) in
      test1.(k) <- test2.(k) ;
      test2.(k) <- test1k 
    end
  done


let crossover_matingPool matingPool randomState pCrossover=
  
  let selectedPool = List.filter (fun _ -> (Random.float 1.) <= pCrossover) matingPool in
  let pool = Core_kernel.Core_list.permute ~random_state:randomState selectedPool in

  let rec crossover_matingPool_aux pool =
    match pool with
      | [] -> ()
      | [_] -> ()
      | t1::t2::rest -> crossover t1 t2; crossover_matingPool_aux rest
  in

  crossover_matingPool_aux pool


let mutation_matingPool domains matingPool mutationP =

  let n = Array.length domains in

  let mutate_aux test =
    
    for k=0 to (n-1) do
      if (Random.float 1.) <= mutationP then test.(k) <- Random.int domains.(k)
    done

  in

  List.iter mutate_aux matingPool



  

let ga ~domains ~sizeOfPop ~numberOfCandidates ~stagCond ~fittestP ~pCrossover ~mutationP ~massiveMutationP = 
  let n = Array.length domains in
  let uncoveredPairs = Hashtbl.create (n*domains.(0)*domains.(0)) in
  let randomState = Core_kernel.Core_random.State.make_self_init () in
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

  let rec ga_aux testsuite =
    if (Hashtbl.length uncoveredPairs == 0) then testsuite
    else begin
      let bestCandidates = Array.make numberOfCandidates ([||],0) in
      

      let pop = ref (make_random_tests domains sizeOfPop) in
      let popScore = fitness_max uncoveredPairs !pop in
      let formerScore, currentScore, stagnationLength = ref popScore, ref popScore, ref 0 in
      
      for candidateNb=0 to (numberOfCandidates-1) do
	
	bestCandidates.(candidateNb) <- select_fittest uncoveredPairs !pop;

	let matingPool = make_mating_pool uncoveredPairs sizeOfPop !pop fittestP in

	crossover_matingPool matingPool randomState pCrossover;
	mutation_matingPool domains matingPool mutationP;

	pop := (fst bestCandidates.(candidateNb))::matingPool;
	currentScore := fitness_max uncoveredPairs !pop;

	assert (!currentScore >= !formerScore);

	if !currentScore == !formerScore then stagnationLength := !stagnationLength + 1
	else begin
	  stagnationLength := 0;
	  formerScore := !currentScore
	end;

	if !stagnationLength > stagCond then begin
	  mutation_matingPool domains !pop massiveMutationP;
	  stagnationLength := 0 ;
	  let score = fitness_max uncoveredPairs !pop in
	  currentScore := score;
	  formerScore := score
	end;
	    
      done;

      (*select the best candidate *)
	  
      let newTest,_ = Array.fold_left (fun (bestTest,bestScore) (test,score) -> begin
	if score > bestScore then (test,score)
	else (bestTest,bestScore) 
      end) ([||],-1) bestCandidates in


      (*remove the coveredPairs *)
      
      for i=1 to (n-1) do
	for k=0 to (i-1) do
	  Hashtbl.remove uncoveredPairs (k,i,newTest.(k),newTest.(i))
	done
      done;
	  
      ga_aux (newTest::testsuite)
    end
  in

  ga_aux []




let () =
    
  let domains = [|3;3;3|] in
    
  let testsuite = ga ~domains:domains ~sizeOfPop:25 ~numberOfCandidates:100 ~stagCond:3 ~fittestP:0.8 ~pCrossover:0.75 ~mutationP:0.03 ~massiveMutationP:0.25 in
   
  print_string (string_of_bool (check domains testsuite));
  print_string "\n";
  print_string "size : ";
  print_int (List.length testsuite);
  print_string "\n";
  print_test_list testsuite
      

                 
