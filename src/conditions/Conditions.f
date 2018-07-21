!
! A unit testing library for Fortran
!
! The MIT License
!
! Copyright 2011-2018 Andrey Pudov
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the 'Software'), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
!

module Conditions

    implicit none
    public arrayEquals, equals, false, notNull, notSame, null, same, true
    private

    interface arrayEquals
        module procedure arrayEquals_character
        module procedure arrayEquals_complex
        module procedure arrayEquals_double_precision
        module procedure arrayEquals_integer
        module procedure arrayEquals_logical
        module procedure arrayEquals_real
    end interface

    interface equals
        module procedure equals_character
        module procedure equals_complex
        module procedure equals_double_precision
        module procedure equals_integer
        module procedure equals_logical
        module procedure equals_real
    end interface

    interface notNull
        module procedure notNull_character
        module procedure notNull_complex
        module procedure notNull_double_precision
        module procedure notNull_integer
        module procedure notNull_logical
        module procedure notNull_real
    end interface

    interface notSame
        module procedure notSame_character
        module procedure notSame_complex
        module procedure notSame_double_precision
        module procedure notSame_integer
        module procedure notSame_logical
        module procedure notSame_real
    end interface

    interface null
        module procedure null_character
        module procedure null_complex
        module procedure null_double_precision
        module procedure null_integer
        module procedure null_logical
        module procedure null_real
    end interface

    interface same
        module procedure same_character
        module procedure same_complex
        module procedure same_double_precision
        module procedure same_integer
        module procedure same_logical
        module procedure same_real
    end interface

contains
    include 'src/conditions/ArrayEquals.f'
    include 'src/conditions/Equals.f'
    include 'src/conditions/False.f'
    include 'src/conditions/NotNull.f'
    include 'src/conditions/NotSame.f'
    include 'src/conditions/Null.f'
    include 'src/conditions/Same.f'
    include 'src/conditions/True.f'
end module