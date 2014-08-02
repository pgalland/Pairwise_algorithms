(*Random algorithm for pairwise testset generation *)


type coverage = Covered | Uncovered 



(*auxiliarry functions *)

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
   



(*algorithm*)


let ra ~domains = 
  let n = Array.length domains in
  let uncoveredPairs = Hashtbl.create (n*domains.(0)*domains.(0)) in
  Random.self_init ();
  (*initialize uncovered pairs*)
  
  for i=1 to (n-1) do
    for k=0 to (i-1) do
            
      for vi=0 to (domains.(i)-1) do
        for vk=0 to (domains.(k)-1) do
                
          Hashtbl.add uncoveredPairs (k,i,vk,vi) Uncovered;
        done;
      done;
    done;
  done;  

  let rec ra_aux testsuite =
    if (Hashtbl.length uncoveredPairs == 0) then testsuite
    else begin

      let newTest = Array.init n (fun k -> Random.int domains.(k)) in

      (*remove the coveredPairs *)
      
      for i=1 to (n-1) do
	for k=0 to (i-1) do
	  Hashtbl.remove uncoveredPairs (k,i,newTest.(k),newTest.(i))
	done
      done;
	  
      ra_aux (newTest::testsuite)
    end
  in

  ra_aux []





let () =
    let domains = [|3;3;3|] in
    let testsuite = ra ~domains in

    print_string (string_of_bool (check domains testsuite));
    print_string "\n";
    print_string "size : ";
    print_int (List.length testsuite);
    print_string "\n";
    print_test_list testsuite
      
