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

subroutine expect_arrayEquals_character(expected, actual, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message

    character, dimension(:), intent(in) :: expected
    character, dimension(:), intent(in) :: actual

    if (.not. arrayEquals(expected, actual)) then
        call fail_expect(message, DEFAULT_ARRAY_EQUALS)
    end if
end subroutine

subroutine expect_arrayEquals_complex(expected, actual, delta, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message

    complex, dimension(:), intent(in) :: expected
    complex, dimension(:), intent(in) :: actual
    real,                  intent(in) :: delta

    if (.not. arrayEquals(expected, actual, delta)) then
        call fail_expect(message, DEFAULT_ARRAY_EQUALS)
    end if
end subroutine

subroutine expect_arrayEquals_double(expected, actual, delta, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message

    double precision, dimension(:), intent(in) :: expected
    double precision, dimension(:), intent(in) :: actual
    double precision,               intent(in) :: delta

    if (.not. arrayEquals(expected, actual, delta)) then
        call fail_expect(message, DEFAULT_ARRAY_EQUALS)
    end if
end subroutine

subroutine expect_arrayEquals_integer(expected, actual, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message

    integer, dimension(:), intent(in) :: expected
    integer, dimension(:), intent(in) :: actual

    if (.not. arrayEquals(expected, actual)) then
        call fail_expect(message, DEFAULT_ARRAY_EQUALS)
    end if
end subroutine

subroutine expect_arrayEquals_logical(expected, actual, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message

    logical, dimension(:), intent(in) :: expected
    logical, dimension(:), intent(in) :: actual

    if (.not. arrayEquals(expected, actual)) then
        call fail_expect(message, DEFAULT_ARRAY_EQUALS)
    end if
end subroutine

subroutine expect_arrayEquals_real(expected, actual, delta, message)
    use Conditions
    use Parameters

    character(len=*), optional, intent(in) :: message

    real, dimension(:), intent(in) :: expected
    real, dimension(:), intent(in) :: actual
    real,               intent(in) :: delta

    if (.not. arrayEquals(expected, actual, delta)) then
        call fail_expect(message, DEFAULT_ARRAY_EQUALS)
    end if
end subroutine