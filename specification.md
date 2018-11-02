# Exam 2018 Sepcification

## Versions
Sequence of naturla Numbers (each written with one or more decimal digits)
No limit on the length of the sequence, each number between **0 to 999 999**,
initial zeros are not significant
Examples:
* 16
* 4.7.2
* 04.00.001 same as 4.0.1, but different from 4.1 and 4.0.1.0

Additionally each number may be followed by a suffix of **1 to 4** lowercase english letters (a through z).
Examples:
* 4.3b.22ff

Versions are ordered lexicographically.
Examples:
* 4.0.1.3 is less than 4.1.2
* 3.4.2 is technically considered strictly less than 3.4.2.0

When comparing individual numbers with suffixes, the numeric part is the most significant.
Examples:
* 3.4z is less then 3.5a

suffixes are ordered alphabetically
Examples:
* 802.11 is less then 802.11n which is less then 802.11ax which again is less than 802.11bb


## Constraint
maxVersion strictly higher then min Version
(Required, allowed minVersion, allowed maxVersion)
Example:
* (False, minV, maxV)


## Package
* package name
* Version
* description
* Collection of constraints on **other** packages, that must be satisfied

## Solution
A solution **Sol** is just a list of package names and versions to be installed in no particular order
