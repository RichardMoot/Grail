# Grail

The Grail family of theorem provers have been designed to work with a variety of modern [type-logical frameworks](https://plato.stanford.edu/entries/typelogical-grammar/), including multimodal type-logical grammars (Moortgat, 2011), NL_cl_ (Barker and Shan, 2014), the Displacement calculus (Morrill, Valentín and Fadda, 2011) and hybrid type-logical grammars (Kubota and Levine, 2012).

The tools give a transparent way of implementing grammars and testing their consequences, providing a natural deduction proof in the specific type-logical gram- mar for each of the readings of a sentence. None of this replaces careful reflection by the grammar writer, of course, but in many cases, computational testing of hand-written grammars will reveal surprises, showing unintended consequences of our grammar and such unintended proofs (or unintended _absences_ of proofs) help us improve the grammar. Computational tools also help us speed up grammar development, for example by allowing us to compare several alternative solutions to a problem and investigate where they make different predictions.


* [Grail 0](https://github.com/RichardMoot/Grail0) bare-bones proof net parser for multimodal categorial grammars.
* [Grail 2](https://github.com/RichardMoot/Grail2) interactive parser for multimodal categorial grammars, using proof nets and term labeling.
* [Grail 3](https://github.com/RichardMoot/Grail) interactive proof net parser for multimodal categorial grammars.
* [Grail Light](https://github.com/RichardMoot/GrailLight) chart parser for multimodal categorial grammars, specialized for wide-coverage French parsing.
* [LinearOne](https://github.com/RichardMoot/LinearOne) a theorem prover for first-order linear logic. Can output natural deduction/sequent proofs for the Lambek calculus, hybrid type-logical grammars, and (a fragment of) the Displacement calculus.

# Which prover/parser is right for me?

### Logic

[LinearOne](https://github.com/RichardMoot/LinearOne) can be used as a theorem prover for the Displacement calculus and for hybrid type-logical grammars. All other provers use a version of multimodal categorial grammars.

Multimodal categorial grammars are rather flexible and the Lambek calculus has a very simple instantiation as a multimodal grammar (using a single, associative mode). Other logics also have a multimodal instantiation. Examples are  NL_cl_ (Barker and Shan, 2014) and the Displacement calculus (Valentín 2014 gives a multimodal version).

### Prolog license

[Grail 2](https://github.com/RichardMoot/Grail2) is the most user-friendly system for beginners, but it requires a [SICStus Prolog](https://sicstus.sics.se) license. All other theorem provers use the free [SWI Prolog](http://www.swi-prolog.org).

### Natural deduction output

With the exception of [Grail 3](https://github.com/RichardMoot/Grail), all provers provide natural deduction output.

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
[Grail 2](https://github.com/RichardMoot/Grail2) | SICStus | + | + | + | + | +
[Grail 3](https://github.com/RichardMoot/Grail) | SWI | - | + | + | + | + | +
[Grail Light](https://github.com/RichardMoot/GrailLight) | SWI | + | NA | + | - | -
[LinearOne](https://github.com/RichardMoot/LinearOne) | SWI | + | + | - | + | NA


# References

Barker, C. and Shan C. (2014) Continuations and Natural Language. Oxford Studies in Theoretical
Linguistics, Oxford University Press

Kubota, Y. and Levine, R. (2012) Gapping as like-category coordination. In: Béchet, D., Dikovsky, A. (eds)
Logical Aspects of Computational Linguistics, Springer, Nantes, Lecture Notes in Computer
Science, vol 7351, pp 135–150

Moortgat, M. (2011) Categorial type logics. In: van Benthem J, ter Meulen A (eds) Handbook of
Logic and Language, North-Holland Elsevier, Amsterdam, chap 2, pp 95–179

Morrill, G., Valentín, O. and Fadda, M. (2011) The displacement calculus. Journal of Logic, Language and Information 20(1):1–48

Valentín, O. (2014) The hidden structural rules of the discontinuous Lambek calculus. In: Casadio, C.,
Coecke, B., Moortgat, M., Scott, P. (eds) Categories and Types in Logic, Language, and Physics: Essays dedicated to Jim Lambek on the Occasion of this 90th Birthday, no. 8222 in Lecture Notes in Artificial Intelligence, Springer, pp 402–420
