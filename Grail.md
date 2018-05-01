# Grail

Grail is a family of theorem provers for type-logical grammars.


* [Grail 0](https://github.com/RichardMoot/Grail0)
* [Grail 2](https://github.com/RichardMoot/Grail2)
* [Grail 3](https://github.com/RichardMoot/Grail) proof net parser for multimodal categorial grammars.
* [Grail Light](https://github.com/RichardMoot/GrailLight) chart parser for multimodal categorial grammars, specialized for wide-coverage French parsing.
* [LinearOne](https://github.com/RichardMoot/LinearOne) a theorem prover for first-order linear logic. Can output natural deduction/sequent proofs for the Lambek calculus, hybrid type-logical grammars, and (a fragment of) the Displacement calculus.

# Which prover/parser is right for me?

### Logic

[LinearOne](https://github.com/RichardMoot/LinearOne) can be used as a theorem prover for the Displacement calculus and for hybrid type-logical grammars. All other provers use a version of multimodal categorial grammars. 

### Prolog license

[Grail 2](https://github.com/RichardMoot/Grail2) is the most user-friendly system for beginners, but it requires a [SICStus Prolog](https://sicstus.sics.se) license. All other theorem provers use the free [SWI Prolog](http://www.swi-prolog.org).

### User-defined structural rules

### Natural deduction output

### Comparison

The following table presents a comparison of the different theorem provers. Giving, for each prover, the following information.

* the Prolog type,
* whether or not it produces natural deduction (ND) output,
* whether or not it produces graph/proof net output of the proof search (Grail Light does not used graphs internally, so this option is listed as not applicable/NA there),
* whether or not there is an interactive proof search mode,
* whether or not the prover is complete,
* and whether or not the system allows the user to define his own set of structural rules (subject to some restrictions to guarantee decidability); this option only applied to multi-modal categorial grammars so this is listed as not applicable/NA for LinearOne).

Prover | Prolog | ND | Graph | Interactive | Complete | User-defined SR
-------|--------|------|------|------------|----------|----------------
[Grail 0](https://github.com/RichardMoot/Grail0) | SWI | + | - | - | + | + |
Grail 2 | SICStus | + | + | + | + | +
Grail 3 | SWI | - | + | + | + | + | +
Grail Light | SWI | + | NA | + | - | -
LinearOne | SWI | + | + | - | + | NA


# References

