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

submodule (Conditions) Equals
contains
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
end submodule
