# rational_number

## Introduction

This Fortran module implements several procedures for handling rational numbers. Currently implemented are:

### Data types

* `rationalnumber_t`

### Procedures


| Name | Type | Description |
| --------- | ----- | ----------- |
| ratNumInit | function | Initializes `rationalnumber_t` variables with given numerator and denominator values. | 
| ratNumToStr | function | Converts a `rationalnumber_t` variable into its string representation. |
| ratNumEqualize | subroutine | Makes the denominators of two `rationalnumber_t` variables equal. |
| ratNumSimplify | function | Simplifies a `rationalnumber_t` variable, reducing its numerator and denominator without affecting its value. E.g. 5/10 --> 1/2. |
| ratNumAdd | function | Adds two `rationalnumber_t` variables together. |
| ratNumSubstract | function | Substracts two `rationalnumber_t` variables. |
| ratNumSetNumerator | subroutine | Sets the numerator value. |
| ratNumGetNumerator | function | Returns the numerator value. |
| ratNumSetDenominator | subroutine | Sets the denominator value. Has no effect if the new value is 0. |
| ratNumGetDenominator | function | Returns the denominator value. |
| ratNumToReal | function | Converts a `rationalnumber_t` variable into its standard Fortran `real` value representation. |

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


