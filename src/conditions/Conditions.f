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

    interface
        !
        ! ArrayEquals - asserts that two arrays are equal.
        !
        module function arrayEquals_character(expected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message

            character, dimension(:), intent(in) :: expected
            character, dimension(:), intent(in) :: actual

            logical value
        end function

        module function arrayEquals_complex(expected, actual, delta, message) result(value)
            character(len=*), optional, intent(in) :: message

            complex, dimension(:), intent(in) :: expected
            complex, dimension(:), intent(in) :: actual
            real,                  intent(in) :: delta

            logical value
        end function

        module function arrayEquals_double_precision(expected, actual, delta, message) result(value)
            character(len=*), optional, intent(in) :: message

            double precision, dimension(:), intent(in) :: expected
            double precision, dimension(:), intent(in) :: actual
            double precision,               intent(in) :: delta

            logical value
        end function

        module function arrayEquals_integer(expected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message

            integer, dimension(:), intent(in) :: expected
            integer, dimension(:), intent(in) :: actual

            logical value
        end function

        module function arrayEquals_logical(expected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message

            logical, dimension(:), intent(in) :: expected
            logical, dimension(:), intent(in) :: actual

            logical value
        end function

        module function arrayEquals_real(expected, actual, delta, message) result(value)
            character(len=*), optional, intent(in) :: message

            real, dimension(:), intent(in) :: expected
            real, dimension(:), intent(in) :: actual
            real,               intent(in) :: delta

            logical value
        end function

        !
        ! Equals - asserts that two values are equal.
        !
        module function equals_character(expected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message

            character, intent(in) :: expected
            character, intent(in) :: actual

            logical value
        end function

        module function equals_complex(expected, actual, delta, message) result(value)
            character(len=*), optional, intent(in) :: message

            complex, intent(in) :: expected
            complex, intent(in) :: actual
            real,    intent(in) :: delta

            logical value
        end function

        module function equals_double_precision(expected, actual, delta, message) result(value)
            character(len=*), optional, intent(in) :: message

            double precision, intent(in) :: expected
            double precision, intent(in) :: actual
            double precision, intent(in) :: delta

            logical value
        end function

        module function equals_integer(expected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message

            integer, intent(in) :: expected
            integer, intent(in) :: actual

            logical value
        end function

        module function equals_logical(expected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message

            logical, intent(in) :: expected
            logical, intent(in) :: actual

            logical value
        end function

        module function equals_real(expected, actual, delta, message) result(value)
            character(len=*), optional, intent(in) :: message

            real, intent(in) :: expected
            real, intent(in) :: actual
            real, intent(in) :: delta

            logical value
        end function

        !
        ! False - asserts that a condition is false.
        !
        module function false(condition, message) result(value)
            character(len=*), optional, intent(in) :: message
            logical, intent(in) :: condition

            logical value
        end function

        !
        ! NutNull - asserts that a pointer isn't null (associated).
        !
        module function notNull_character(pointer, message) result(value)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: pointer

            logical value
        end function

        module function notNull_complex(pointer, message) result(value)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: pointer

            logical value
        end function

        module function notNull_double_precision(pointer, message) result(value)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: pointer

            logical value
        end function

        module function notNull_integer(pointer, message) result(value)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: pointer

            logical value
        end function

        module function notNull_logical(pointer, message) result(value)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: pointer

            logical value
        end function

        module function notNull_real(pointer, message) result(value)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: pointer

            logical value
        end function

        !
        ! NotSame - asserts that two pointers do not refer to the same target.
        !
        module function notSame_character(unexpected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: unexpected
            character,        pointer,  intent(in) :: actual

            logical value
        end function

        module function notSame_complex(unexpected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: unexpected
            complex,          pointer, intent(in)  :: actual

            logical value
        end function

        module function notSame_double_precision(unexpected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: unexpected
            double precision, pointer,  intent(in) :: actual

            logical value
        end function

        module function notSame_integer(unexpected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: unexpected
            integer,          pointer,  intent(in) :: actual

            logical value
        end function

        module function notSame_logical(unexpected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: unexpected
            logical,          pointer,  intent(in) :: actual

            logical value
        end function

        module function notSame_real(unexpected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: unexpected
            real,             pointer,  intent(in) :: actual

            logical value
        end function

        !
        ! Null - asserts that a pointer is null (not associated).
        !
        module function null_character(pointer, message) result(value)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: pointer

            logical value
        end function

        module function null_complex(pointer, message) result(value)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: pointer

            logical value
        end function

        module function null_double_precision(pointer, message) result(value)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: pointer

            logical value
        end function

        module function null_integer(pointer, message) result(value)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: pointer

            logical value
        end function

        module function null_logical(pointer, message) result(value)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: pointer

            logical value
        end function

        module function null_real(pointer, message) result(value)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: pointer

            logical value
        end function

        !
        ! Same - asserts that two pointers refer to the same target.
        !
        module function same_character(unexpected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: unexpected
            character,        pointer,  intent(in) :: actual

            logical value
        end function

        module function same_complex(unexpected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: unexpected
            complex,          pointer, intent(in)  :: actual

            logical value
        end function

        module function same_double_precision(unexpected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: unexpected
            double precision, pointer,  intent(in) :: actual

            logical value
        end function

        module function same_integer(unexpected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: unexpected
            integer,          pointer,  intent(in) :: actual

            logical value
        end function

        module function same_logical(unexpected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: unexpected
            logical,          pointer,  intent(in) :: actual

            logical value
        end function

        module function same_real(unexpected, actual, message) result(value)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: unexpected
            real,             pointer,  intent(in) :: actual

            logical value
        end function

        !
        ! True - asserts that a condition is true.
        !
        module function true(condition, message) result(value)
            character(len=*), optional, intent(in) :: message
            logical, intent(in) :: condition

            logical value
        end function

        !
        ! Fail - fails a test with the given message.
        !
        module function fail(message) result(value)
            character(len=*), optional, intent(in) :: message
            logical value
        end function
    end interface
end module
