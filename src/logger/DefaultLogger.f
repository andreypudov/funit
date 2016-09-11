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

submodule (Logger) DefaultLogger

    implicit none

contains
    module subroutine init_defaultLogger(self)
        class(DefaultLogger), intent(in out) :: self

        call cpu_time(self%start)
    end subroutine

    module subroutine clean_defaultLogger(self)
        class(DefaultLogger), intent(in out) :: self
    end subroutine

    module function getLevel_defaultLogger(self) result(value)
        class(DefaultLogger), intent(in) :: self

        integer value
    end function

    module subroutine setLevel_defaultLogger(self, level)
        class(DefaultLogger), intent(in out) :: self
        integer, intent(in)           :: level
    end subroutine

    module subroutine log_defaultLogger(self, message, level)
        class(DefaultLogger), intent(in) :: self
        character(len=*),     intent(in) :: message
        integer, optional,    intent(in) :: level
    end subroutine
end submodule
