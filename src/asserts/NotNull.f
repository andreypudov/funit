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

submodule (Unit) NotNullAsserts

    implicit none

contains
    module subroutine notNull_assert_character(pointer, message)
        character(len=*), optional, intent(in) :: message
        character,        pointer,  intent(in) :: pointer
    end subroutine

    module subroutine notNull_assert_complex(pointer, message)
        character(len=*), optional, intent(in) :: message
        complex,          pointer, intent(in)  :: pointer
    end subroutine

    module subroutine notNull_assert_double_precision(pointer, message)
        character(len=*), optional, intent(in) :: message
        double precision, pointer,  intent(in) :: pointer
    end subroutine

    module subroutine notNull_assert_integer(pointer, message)
        character(len=*), optional, intent(in) :: message
        integer,          pointer,  intent(in) :: pointer
    end subroutine

    module subroutine notNull_assert_logical(pointer, message)
        character(len=*), optional, intent(in) :: message
        logical,          pointer,  intent(in) :: pointer
    end subroutine

    module subroutine notNull_assert_real(pointer, message)
        character(len=*), optional, intent(in) :: message
        real,             pointer,  intent(in) :: pointer
    end subroutine
end submodule
