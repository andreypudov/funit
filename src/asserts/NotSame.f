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

subroutine assert_notSame_character(unexpected, actual, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message
    character, pointer :: unexpected
    character, pointer :: actual

    if (.not. notSame(unexpected, actual)) then
        call fail_assert(message, DEFAULT_NOT_SAME)
    end if
end subroutine

subroutine assert_notSame_complex(unexpected, actual, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message
    complex, pointer :: unexpected
    complex, pointer :: actual

    if (.not. notSame(unexpected, actual)) then
        call fail_assert(message, DEFAULT_NOT_SAME)
    end if
end subroutine

subroutine assert_notSame_double_precision(unexpected, actual, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message
    double precision, pointer :: unexpected
    double precision, pointer :: actual

    if (.not. notSame(unexpected, actual)) then
        call fail_assert(message, DEFAULT_NOT_SAME)
    end if
end subroutine

subroutine assert_notSame_integer(unexpected, actual, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message
    integer, pointer :: unexpected
    integer, pointer :: actual

    if (.not. notSame(unexpected, actual)) then
        call fail_assert(message, DEFAULT_NOT_SAME)
    end if
end subroutine

subroutine assert_notSame_logical(unexpected, actual, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message
    logical, pointer :: unexpected
    logical,  pointer :: actual

    if (.not. notSame(unexpected, actual)) then
        call fail_assert(message, DEFAULT_NOT_SAME)
    end if
end subroutine

subroutine assert_notSame_real(unexpected, actual, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message
    real, pointer :: unexpected
    real, pointer :: actual

    if (.not. notSame(unexpected, actual)) then
        call fail_assert(message, DEFAULT_NOT_SAME)
    end if
end subroutine