!
! A unit testing library for Fortran.
!
! The MIT License
!
! Copyright 2011-2018 Andrey Pudov.
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

subroutine assert_null_character(pointer, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message
    character, pointer :: pointer

    if (.not. null(pointer)) then
        call fail_assert(message, DEFAULT_NULL)
    end if
end subroutine

subroutine assert_null_complex(pointer, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message
    complex, pointer :: pointer

    if (.not. null(pointer)) then
        call fail_assert(message, DEFAULT_NULL)
    end if
end subroutine

subroutine assert_null_double_precision(pointer, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message
    double precision, pointer :: pointer

    if (.not. null(pointer)) then
        call fail_assert(message, DEFAULT_NULL)
    end if
end subroutine

subroutine assert_null_integer(pointer, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message
    integer, pointer :: pointer

    if (.not. null(pointer)) then
        call fail_assert(message, DEFAULT_NULL)
    end if
end subroutine

subroutine assert_null_logical(pointer, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message
    logical, pointer :: pointer

    if (.not. null(pointer)) then
        call fail_assert(message, DEFAULT_NULL)
    end if
end subroutine

subroutine assert_null_real(pointer, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message
    real, pointer :: pointer

    if (.not. null(pointer)) then
        call fail_assert(message, DEFAULT_NULL)
    end if
end subroutine