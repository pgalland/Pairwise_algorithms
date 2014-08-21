Pairwise_algorithms
===================

Implementation of several algorithms for generation of pairwise test sets (in OCaml). Some functions of the Jane Street Standard Library are used, therefore the files must be built with Jane Street Compiler (command corebuild). 

* In Parameter Order (IPO) [Link to paper](http://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=731623&url=http%3A%2F%2Fieeexplore.ieee.org%2Fxpls%2Fabs_all.jsp%3Farnumber%3D731623) - ipo1.ml
* AETG algorithm [Link to paper](http://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=605761&url=http%3A%2F%2Fieeexplore.ieee.org%2Fxpls%2Fabs_all.jsp%3Farnumber%3D605761) - aetg.ml
* A genetic algorithm [Link to paper](http://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=1342808&url=http%3A%2F%2Fieeexplore.ieee.org%2Fxpls%2Fabs_all.jsp%3Farnumber%3D1342808) - ga.ml
* A ant colony algorithm [Link to paper](http://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=1342808&url=http%3A%2F%2Fieeexplore.ieee.org%2Fxpls%2Fabs_all.jsp%3Farnumber%3D1342808) - aca.ml
* A completely random method (randomly generate tests until it is pairwise) - ra.ml
