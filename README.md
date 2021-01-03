# rational_number

## Introduction

This Fortran module implements several procedures for handling rational numbers. Currently implemented are:

* *rationalnumber_t*: derived data type
* Initializing variables of `rationalnumber_t` type with given numerator and denominator values.
* Setting numerator and denominator values of `rationalnumber_t` variables.
* Converting a `rationalnumber_t` variable into its string representation.
* Adding two `rationalnumber_t` variables together.
* Equalizing two `rationalnumber_t` variables, making their denominators equal.
* Simplifying a `rationalnumber_t` variable, reducing its numerator and denominators without affecting its value. E.g. 5/10 --> 1/2.
* Converting a `rationalnumber_t` variable into its standard Fortran `real` value representation.

## Known bugs/limitations

1. No integer overflow detection/handling. Behaviour is undefined with numerator or denominator values exceeding the limits of the standard Fortran `integer` type.
2. Currently there are no procedures for other basic operations than addition.

## Dependencies

The only dependency is the [math module](https://github.com/art-rasa/math). This module will be included automatically when compiled by FPM.

## Building the library:

* Install [FPM](https://github.com/fortran-lang/fpm) (Fortran Package Manager) and copy it into your $PATH as `fpm`.
* Clone the git repository.
* Execute the command `fpm build` inside the base directory of the repository. This will also fetch and build the required *math* library.
* This library includes a simple test program. It can be easily executed with `fpm test`.

This library was compiled with [GFortran](https://gcc.gnu.org/fortran/) and packaged with [FPM](https://github.com/fortran-lang/fpm).


