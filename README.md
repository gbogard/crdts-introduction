# A gentle introduction to Conflict-free replicated data types

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
I've also added links to talks and papers you might find interesting.

- 
## Reaching out

## License
