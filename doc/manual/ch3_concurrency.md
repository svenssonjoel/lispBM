
# Chapter 3: Concurrency
LispBM (LBM) supports concurrently executing processes and let's processes
communicate with eachother via message passing. Currently the runtime
system that execute LBM programs is entirely sequential and does not make
use of multiple cores, if present. Concurrency abstractions are very useful
even with there being any actual parallelism. Each operation performed by an
LBM process can be considered atomic in relationship to any other LBM process.

One use-case is to run LBM in parallel with some larger embedded application
written in C. The operations performed by LBM processes are not atomic in
relation to operations performed on the C side. This is only a problem if the
C program is accessing data that is stored in the LBM runtime system (such
as on the LBM heap, stack or process queues). This is something an LBM integrator
must consider when writing the interface between the C application and the LBM
runtime system. This is a topic for a future chapter of this manual. 

LBM implements so-called cooperative concurrency which means that processes
run uninterrupted until the time that the process itself executes a `yield`.
So no LBM process will ever preempt any other.


## Getting started with concurrent LBM












