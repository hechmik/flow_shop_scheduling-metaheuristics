# Flow Shop Scheduling: different R implementations

In this repository I will upload a couple of R Scripts for solving the Flow Shop Scheduling Problem.

For this kind of problem the input data is a matrix that represents the single execution time required by *m* different machines for executing *n* different jobs.

In particular these algorithms will try to find the best job combination that minimize the makespan value, that is the amount of time required to process all *n* jobs on all *m* machines under these constraints:
- Each job must be processed by all the machines following the same order. This means that the the i-th operation of the job must be executed by the i-th machine.
- Machines cannot work on more than one process simultaneously
- Because of the two previous constraints it is assumed, for complexity reasons, that the order in which jobs are processed is exactly the same for all the machines.

In particular in this repository you can find the following algorithms:
- Implementation of an algorithm that mimics a Genetical Algorithm: it implements crossover, elitism and mutation
- Implementation of a Particle Swarm Optimization algorithm. For doing so I used as a reference the excellent paper **Tasgetiren, Mehmet & Liang, Yun-Chia & Şevkli, Mehmet & Gencyilmaz, Gunes. (2004). Particle Swarm Optimization Algorithm for Makespan and Maximum Lateness Minimization in Permutation Flowshop Sequencing Problem. Proceedings of the Fourth International Symposium on Intelligent Manufacturing Systems.**, that can be viewed at the following [ResearchGate page](https://www.researchgate.net/publication/252625779_Particle_Swarm_Optimization_Algorithm_for_Makespan_and_Maximum_Lateness_Minimization_in_Permutation_Flowshop_Sequencing_Problem). I want to clearly state that any error in the implementation is exclusively my responsibility, therefore I highly encourage you to read the original paper for having a better and more precise overview of the topic: if you find any inconsistency or error please open an issue or a PR here.

As dataset I used the [Taillard Scheduling instances](http://mistic.heig-vd.ch/taillard/problemes.dir/ordonnancement.dir/ordonnancement.html), which are the de-facto standard for this kind of problems: in particular I worked on **"Flow shop scheduling instances"**.
For loading the different instances the quickest way is to use the [ReadTaillard.R script](https://github.com/jmsallan/heuristics/blob/master/FlowShop/ReadTaillard.R) developed and upload on GitHub by [jmsallanis](https://github.com/jmsallan).

If you find any issue/bug, something is not clear or you have any double please open an issue so that other people will benefit more.
