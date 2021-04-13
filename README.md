# A gentle introduction to Conflict-free replicated data types

Conflict-free replicated data types, or CRDT for short, enable seamless optimistic replication of 
data in distributed system. They achieve that goal by pairing sets with merge operations, which together allow
many states to always converge by construction.

This is a conference talk aimed at programmers, together with code examples and interactive demos, that introduces
conflict-free replicated data types. It assumes no prior knowledge of CRDTs. Although the notion of CRDT itself doesn't require knowledge of a specific programming language, this presentation
makes use of examples in PureScript, which, depending on your programming background, you will find more or less easy to read. 

Feel free to look at the slides, play around with the demos, and take a look at the code.

## Glossary

### Strong consistency

A concurrency model for multithreaded applications and distributed systems, in which all updates strictly sequential across the entire system.
Therefore, only one consistent state can be observed by all the participants in the system.
In the case of distributed systems, strong consistency is typically achieved using consensus protocols which
are rather expansive.

### Eventual consistency (or optimistic replication)

A concurrency model in which views of the system are allowed to temporarily diverge (i.e. nodes
in the system can observe different states at the same time), but will eventually converge to the same state
if no new updates are made.

Eventually consistent systems still need a form a consensus to reconciliate conflicting updates, but
not in real time. This is why they are way faster than strongly consistent systems with regard to
write performance, as writes require no coordination among the nodes.

### Strong eventual consistency

Strong eventual consistency (SEC) is eventual consistency with additional safety guarantees:
SEC systems are guaranteed to reconciliate concurrent updates whereas eventually consistent systems
roll back conflicting updates when they cannot figure out a way to solve conflicts.

Strong eventual consistency is achieved by implementing conflict-resolution strategies that
work in all possible cases. Conflict-free replicated data types (CRDTs) are exactly that: data
structures together with a conflict resolution function that guarantees data convergence.

### State-based CRDT

A state-based CRDT is a data structure, together with a binary operation that can produce a single
state out of two possibly different states.
More formally, a state-based CRDT is a tuple `(S, s0, q, u, m)`, where
- S is the set of possible states for the CRDT
- s0 is the initial state of the CRDT
- q is the query function that lets a client read the current state of the object
- u is the update function that lets a client alter the state of the object
- m is a binary merge function that should obey the following laws
  - associativity: `merge(x, merge(y, z)) = merge(merge(x, y), z)` for all x, y and z, i.e. the parentheses
  don't matter when calling `merge`
  - commutativity: `merge(x, y) = merge(y, x)` for all x and y, i.e. states can be merged in any order
  - idempotence: `merge(x, x) = x` for all x, i.e. calling `merge` multiple times doesn't affect the outcome. Idempotence guarantees that, in the event of a network failure, sending duplicate updates
  to a node still results in the correct state.

### Event-based CRDT

A data-structure whose updates are encoded by operations, and operations are sent over the network.
A new state can be produced given the current state and an operation.
When the structure is modified, the replica responsible for the update generates one or many operations,
applies them locally, and then propagates them across the network. Operation-based CRDTs guarantee that,
when operations are successfully propagated, all replicas converge to the same state.

Like merging state-based CRDTs, applying operations is associative and commutative, i.e. operations
can be applied in any order, however, unlike it isn't necessarily idempotent. It is the responsibility
of the transport layer to make sure operations are properly delivered, and not applied more than once.

## References and going further

Here are the papers, blog articles and videos that have helped me prepare this talk.
*A gentle introduction to CRDTs*, and the associated materials, wouldn't have existed without them.

- Roberto Baldoni and Michel Raynal. 2002. Fundamentals of Distributed Computing: A Practical Tour of Vector Clock Systems. IEEE Distributed Systems Online.
- Vitor Enes, Paulo Sérgio Almeida, Carlos Baquero, and João Leitão. 2019. Efficient Synchronization of State-based CRDTs. arXiv:1803.02750 (March 2019). Retrieved January 12, 2021 from <http://arxiv.org/abs/1803.02750>
- Kingsbury, Kyle. 2013. The trouble with timestamps. Aphyr. Retrieved April 12, 2021 from <https://aphyr.com/posts/299-the-trouble-with-timestamps>
- Nuno Preguiça. 2018. Conflict-free Replicated Data Types: An Overview. arXiv:1806.10254 (June 2018). Retrieved January 11, 2021 from <http://arxiv.org/abs/1806.10254>
- Marc Shapiro, Nuno Preguiça, Carlos Baquero, and Marek Zawirski. 2011. Conflict-Free Replicated Data Types. In Stabilization, Safety, and Security of Distributed Systems, Xavier Défago, Franck Petit and Vincent Villain (eds.). Springer Berlin Heidelberg, Berlin, Heidelberg, 386–400. DOI: <https://doi.org/10.1007/978-3-642-24550-3_29>
- Marc Shapiro, Nuno Preguiça, Carlos Baquero, and Marek Zawirski. 2011. A comprehensive study of Convergent and Commutative Replicated Data Types. Inria – Centre Paris-Rocquencourt ; INRIA. Retrieved January 11, 2021 from <https://hal.inria.fr/inria-00555588>
- Lars Hupel. An introduction to Conflict-Free Replicated Data Types: Part 1. Lars Hupel’s website. Retrieved April 13, 2021 from <https://lars.hupel.info/topics/crdt/01-intro/>
- CRDT: Conflict-free Replicated Data Types | by Anton Zagorskii | Medium. Retrieved April 13, 2021 from <https://medium.com/@amberovsky/crdt-conflict-free-replicated-data-types-b4bfc8459d26>

- Luka Jacobowitz. 2020. Monoids, monoids, monoids. Konfy. Retrieved April 13, 2021 from <https://www.youtube.com/watch?v=pLpxRnAPteA>

I also recommend the website [crdt.tech](https://crdt.tech/), created by Martin Kleppmann, Annette Bieniusa, and Marc Shapiro, which features a lot of resources to learn about CRDTs, including papers, talks and definitions.

## Get in touch

- My personal e-mail address is *hey@guillaumebogard.dev*.
- You can also find me on Twitter [@bogardguillaume](https://twitter.com/bogardguillaume).

Feel free to suggest improvements and corrections by opening a [pull request](https://github.com/gbogard/crdts-introduction/pulls).

## License

The source code fir this website and the slides of the talk are released under the [Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0) license. Feel free to reuse the code and examples,
as long as you credit me as the original author. 