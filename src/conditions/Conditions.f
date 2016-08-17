!
! A unit testing library for Fortran
!
! The MIT License
!
! Copyright 2011-2016 Andrey Pudov
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
    public

    !
    ! ArrayEquals - asserts that two arrays are equal.
    !
    interface arrayEquals
        module pure function arrayEquals_character(expected, actual) result(value)
            character, dimension(:), intent(in) :: expected
            character, dimension(:), intent(in) :: actual

            logical value
        end function

        module pure function arrayEquals_complex(expected, actual, delta) result(value)
            complex, dimension(:), intent(in) :: expected
            complex, dimension(:), intent(in) :: actual
            real,                  intent(in) :: delta

            logical value
        end function

        module pure function arrayEquals_double_precision(expected, actual, delta) result(value)
            double precision, dimension(:), intent(in) :: expected
            double precision, dimension(:), intent(in) :: actual
            double precision,               intent(in) :: delta

            logical value
        end function

        module pure function arrayEquals_integer(expected, actual) result(value)
            integer, dimension(:), intent(in) :: expected
            integer, dimension(:), intent(in) :: actual

            logical value
        end function

        module pure function arrayEquals_logical(expected, actual) result(value)
            logical, dimension(:), intent(in) :: expected
            logical, dimension(:), intent(in) :: actual

            logical value
        end function

        module pure function arrayEquals_real(expected, actual, delta) result(value)
            real, dimension(:), intent(in) :: expected
            real, dimension(:), intent(in) :: actual
            real,               intent(in) :: delta

            logical value
        end function
    end interface

    !
    ! Equals - asserts that two values are equal.
    !
    interface equals
        module pure function equals_character(expected, actual) result(value)
            character, intent(in) :: expected
            character, intent(in) :: actual

            logical value
        end function

        module pure function equals_complex(expected, actual, delta) result(value)
            complex, intent(in) :: expected
            complex, intent(in) :: actual
            real,    intent(in) :: delta

            logical value
        end function

        module pure function equals_double_precision(expected, actual, delta) result(value)
            double precision, intent(in) :: expected
            double precision, intent(in) :: actual
            double precision, intent(in) :: delta

            logical value
        end function

        module pure function equals_integer(expected, actual) result(value)
            integer, intent(in) :: expected
            integer, intent(in) :: actual

            logical value
        end function

        module pure function equals_logical(expected, actual) result(value)
            logical, intent(in) :: expected
            logical, intent(in) :: actual

            logical value
        end function

        module pure function equals_real(expected, actual, delta) result(value)
            real, intent(in) :: expected
            real, intent(in) :: actual
            real, intent(in) :: delta

            logical value
        end function

    end interface

    !
    ! False - asserts that a condition is false.
    !
    interface
        module pure function false(condition) result(value)
            logical, intent(in) :: condition
            logical value
        end function
    end interface

    !
    ! NutNull - asserts that a pointer isn't null (associated).
    !
    interface notNull
        module pure function notNull_character(pointer) result(value)
            character, pointer,  intent(in) :: pointer
            logical value
        end function

        module pure function notNull_complex(pointer) result(value)
            complex, pointer, intent(in)  :: pointer
            logical value
        end function

        module pure function notNull_double_precision(pointer) result(value)
            double precision, pointer,  intent(in) :: pointer
            logical value
        end function

        module pure function notNull_integer(pointer) result(value)
            integer, pointer,  intent(in) :: pointer
            logical value
        end function

        module pure function notNull_logical(pointer) result(value)
            logical, pointer,  intent(in) :: pointer
            logical value
        end function

        module pure function notNull_real(pointer) result(value)
            real, pointer,  intent(in) :: pointer
            logical value
        end function
    end interface

    !
    ! NotSame - asserts that two pointers do not refer to the same target.
    !
    interface notSame
        module pure function notSame_character(unexpected, actual) result(value)
            character, pointer,  intent(in) :: unexpected
            character, pointer,  intent(in) :: actual

            logical value
        end function

        module pure function notSame_complex(unexpected, actual) result(value)
            complex, pointer, intent(in)  :: unexpected
            complex, pointer, intent(in)  :: actual

            logical value
        end function

        module pure function notSame_double_precision(unexpected, actual) result(value)
            double precision, pointer,  intent(in) :: unexpected
            double precision, pointer,  intent(in) :: actual

            logical value
        end function

        module pure function notSame_integer(unexpected, actual) result(value)
            integer, pointer,  intent(in) :: unexpected
            integer, pointer,  intent(in) :: actual

            logical value
        end function

        module pure function notSame_logical(unexpected, actual) result(value)
            logical, pointer,  intent(in) :: unexpected
            logical, pointer,  intent(in) :: actual

            logical value
        end function

        module pure function notSame_real(unexpected, actual) result(value)
            real, pointer,  intent(in) :: unexpected
            real, pointer,  intent(in) :: actual

            logical value
        end function
    end interface

    !
    ! Null - asserts that a pointer is null (not associated).
    !
    interface null
        module pure function null_character(pointer) result(value)
            character, pointer,  intent(in) :: pointer
            logical value
        end function

        module pure function null_complex(pointer) result(value)
            complex, pointer, intent(in)  :: pointer
            logical value
        end function

        module pure function null_double_precision(pointer) result(value)
            double precision, pointer,  intent(in) :: pointer
            logical value
        end function

        module pure function null_integer(pointer) result(value)
            integer, pointer,  intent(in) :: pointer
            logical value
        end function

        module pure function null_logical(pointer) result(value)
            logical, pointer,  intent(in) :: pointer
            logical value
        end function

        module pure function null_real(pointer) result(value)
            real, pointer,  intent(in) :: pointer
            logical value
        end function
    end interface

    !
    ! Same - asserts that two pointers refer to the same target.
    !
    interface same
        module pure function same_character(unexpected, actual) result(value)
            character, pointer,  intent(in) :: unexpected
            character, pointer,  intent(in) :: actual

            logical value
        end function

        module pure function same_complex(unexpected, actual) result(value)
            complex, pointer, intent(in)  :: unexpected
            complex, pointer, intent(in)  :: actual

            logical value
        end function

        module pure function same_double_precision(unexpected, actual) result(value)
            double precision, pointer,  intent(in) :: unexpected
            double precision, pointer,  intent(in) :: actual

            logical value
        end function

        module pure function same_integer(unexpected, actual) result(value)
            integer, pointer,  intent(in) :: unexpected
            integer, pointer,  intent(in) :: actual

            logical value
        end function

        module pure function same_logical(unexpected, actual) result(value)
            logical,          pointer,  intent(in) :: unexpected
            logical,          pointer,  intent(in) :: actual

            logical value
        end function

        module pure function same_real(unexpected, actual) result(value)
            real, pointer,  intent(in) :: unexpected
            real, pointer,  intent(in) :: actual

            logical value
        end function
    end interface

    !
    ! True - asserts that a condition is true.
    !
    interface
        module pure function true(condition) result(value)
            logical, intent(in) :: condition
            logical value
        end function
    end interface
end module
