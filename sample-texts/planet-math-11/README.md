# PlanetMatha Colada

This directory contains an ongoing project to translate
[PlanetMath](https://planetmath.org) into Colada.

This project description also appears as a TeX file
(planet-math-project.tex) in the docs directory.

This is a document describing the proposed project to translate the
entire body of PlanetMath (PM) into the Colada controlled natural
language.  A pdf document containing all PlanetMath would run (an
estimated) several thousand pages.


References are [PlanetMath](https://planetmath.org) and [PlanetMath
github tex source files](https://github.com/planetmath).



## Introduction

This project will begin with the translation of the Number Theory
sections of PM (repository [github repository for PM Number
Theory](https://github.com/planetmath/11_Number_theory)).  (Future
plans include the translation of other PlanetMath repositories.)

The final product will consist of many TeXa Colada files that pass
through its parsing tools without error.

## File divisions

Generally, each PlanetMath file will correspond with a Colada file of
the same name.

## File Structure

File name conventions and directories should follow PlanetMath.  The
topics are given in CamelCase (no dashes are underscores), and the
first characters of the filename give the MSC subject classification.
The names of files should agree with [MSC
names](https://cran.r-project.org/web/classifications/MSC-2010.html).
For example, MSC gives

``` 
11Mxx: Zeta and L-functions: analytic theory 
``` 
This corresponds with a Colada file `11M-ZetaAndLFunctionsAnalyticTheory`.


## Missing Definitions 

Definitions that are not supplied by PlanetMath can in rare cases be
left as a incomplete stub.  A `Fiat` file will record all incomplete
definitions.  However, it is strongly preferred that these definitions
be inserted from another public source such as Wikipedia with an
external link.  Eventually the Fiat file should be empty.

A definition from another source should be placed in a separate
section.

Definitions should always come from public sources. No paywalls and no
registration should block access the referenced definition source.
The use of copyrighted material should fall within licensed use, fair
use, or should be used in a transformative way to avoid copyright
claims by outside parties.

In choosing the source of an external definition, the long term public
availability, stability of links, mathematical suitability (is this
the definition a skilled mathematician would use?), formalizability
should be considered.

## Common Definitions

A separate *Foundations* file will provide many of the common core
definitions of concepts such as sets, lists, finiteness, natural
numbers, integers, rational numbers, real numbers, and so forth.

A separate *Synonyms* file will provide many of the common synonyms
*least upper bound/supremum/sup*, etc.  Specialized synonyms should
appear in the separate files.

Each word goes through a very crude *singularization* process as it is
tokenized.  (This algorithm can produce some strange results such as
singularizing *series* to *sery*, but in practice this is not a
problem unless an author tries to give distinct definitions to *sery*
and *series*.)  Words with the same singularization are treated as the
same.  This means that words such as *number, numbers*, or *zero,
zeros*, or *body, bodies* are respectively treated the same and do not
need explicit synonym declarations.  The singularization algorithm
does not consider the part of speech of the word, so that for example
verbs are *singularized* just as readily *intersects, intersect*, etc.

Number theory definitions that get used regularly in other files
should be moved to a common `CommmonCore.tex` file that is imported by
all.  This file will also contain common notation.

## Definitions from other MSC domains

Some definitions outside number theory will be needed.  For example,
some definitions depend on definitions from general topology.  Thus,
as the project progresses, the project will spill into other areas,
and other directories will be created that contain common definitions
from other MSC domains.  A file `External.tex` will record external
dependencies on definitions.

## Linking 

Links should follow PlanetMath conventions.  

## What to translate 

Notation, definitions, and theorems should be translated.  Remarks and
proofs should not be translated.  However, the remarks and proofs
should remain in the Colada file (but confined to remark sections, so
that they are not parsed as Colada text).

The aim is to obtain Colada files that are as readable as the original
PlanetMath files.  Except for added definitions, the Colada files
should be nearly the same length as the PlanetMath files.  Any
significant loss of readability or expansion in length should trigger
discussion, and better solutions should be found.

## Logical Dependencies 

Circularities and inequivalent redefinitions must be avoided.  The
Synonym, Foundation, Fiat, and CommonCore files will be loaded first,
then the separate number theory files from beginning to end.  Files
must be orderable so that no definition is encountered before it is
defined.

Definitions, notations and macros that appear in sections with local
scope are by default not globally visible.  All synonyms are globally
visible.

Eventually, we expect to have a tool that will extract the
declarations made and terms used in each file, then reconstruct a
dependency tree for all files in the directory.  Any volunteers?

## Dealing with unusual content 

Some articles with unusual content can be omitted, such as
[ReverseAndAddSequence](11B99-196sReverseAndAddSequenceTo1000Terms.tex).
A file `PlanetMathComments.tex` should describe significant departures
between Planet Math and this project.  Errors in the PlanetMath
documents should be corrected in the Colada files and documented in
`PlanetMathErrata.tex` (and they will be periodically forwarded to
PlanetMath maintainers).  Definitions and theorems that lack formal
rigor should be reported in the Errata.

## License

The license for PlanetMath is Creative Commons Attribution-ShareAlike
3.0 Unported.  This project will be released under a compatible
license. 

## Statistics

```
Number of articles in planetMath/11_Number_theory by MSC subject code.
$ ls *.tex | cut -c-3 | uniq -c
  73 11-
 468 11A
 118 11B
  18 11C
  36 11D
  23 11E
  12 11F
   9 11G
   7 11H
  33 11J
   5 11K
   7 11L
  31 11M
  50 11N
   7 11P
 138 11R
  12 11S
   5 11T
  10 11Y
   9 11Z
```



