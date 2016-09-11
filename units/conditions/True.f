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

module TrueCondition

    use Unit

    implicit none
    private

    type, extends(UnitCase), public :: TrueConditionCase
    private
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type

contains
    subroutine init(self)
        class(TrueConditionCase), intent(in out) :: self
        call self%UnitCase%init()

        print *, 'init true case'
        call self%add(true)
    end subroutine

    subroutine clean(self)
        class(TrueConditionCase), intent(in out) :: self
        call self%UnitCase%clean()

        print *, 'clean true case'
    end subroutine

    subroutine true(self)
        class(UnitCase), intent(in out) :: self

        print *, 'TRUE'
    end subroutine
end module
