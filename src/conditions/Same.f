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

submodule (Conditions) Same
contains
    module pure function same_character(unexpected, actual) result(value)
        character, pointer,  intent(in) :: unexpected
        character, pointer,  intent(in) :: actual

        logical value

        value = associated(unexpected, actual)
    end function

    module pure function same_complex(unexpected, actual) result(value)
        complex, pointer, intent(in)  :: unexpected
        complex, pointer, intent(in)  :: actual

        logical value

        value = associated(unexpected, actual)
    end function

    module pure function same_double_precision(unexpected, actual) result(value)
        double precision, pointer,  intent(in) :: unexpected
        double precision, pointer,  intent(in) :: actual

        logical value

        value = associated(unexpected, actual)
    end function

    module pure function same_integer(unexpected, actual) result(value)
        integer, pointer,  intent(in) :: unexpected
        integer, pointer,  intent(in) :: actual

        logical value

        value = associated(unexpected, actual)
    end function

    module pure function same_logical(unexpected, actual) result(value)
        logical,          pointer,  intent(in) :: unexpected
        logical,          pointer,  intent(in) :: actual

        logical value

        value = associated(unexpected, actual)
    end function

    module pure function same_real(unexpected, actual) result(value)
        real, pointer,  intent(in) :: unexpected
        real, pointer,  intent(in) :: actual

        logical value

        value = associated(unexpected, actual)
    end function
end submodule
