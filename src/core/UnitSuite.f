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

submodule (Unit) UnitSuite

    implicit none

contains
    module subroutine init_suite(self, name)
        class(UnitSuite), intent(in out)       :: self
        character(len=*), optional, intent(in) :: name

        self%list => null()
        self%last => null()

        if (present(name)) then
            allocate(character(len(name)) :: self%name)
            self%name = name
        else
            allocate(character(1) :: self%name)
            self%name = ''
        end if
    end subroutine

    module subroutine clean_suite(self)
        class(UnitSuite), intent(in out)  :: self
        type(UnitCaseEntry), pointer      :: entry

        do while (associated(self%list))
            entry => self%list
            self%list => self%list%next

            deallocate(entry)
        end do

        deallocate(self%name)
    end subroutine

    module subroutine add_suite(self, case, name)
        class(UnitSuite), intent(in out)         :: self
        procedure(UnitCase), pointer, intent(in) :: case
        character(len=*),   optional, intent(in) :: name

        type(UnitCaseEntry), pointer :: entry
        type(UnitCaseEntry), pointer :: previous

        allocate(entry)
        entry%next => null()
        entry%case => case

        if (present(name)) then
            allocate(character(len(name)) :: entry%name)
            entry%name = name
        else
            allocate(character(1) :: entry%name)
            entry%name = ''
        end if

        if (.not. associated(self%last)) then
            self%list => entry
        else
            previous      => self%last
            previous%next => entry
        end if

        self%last => entry
    end subroutine

    module subroutine run_suite(self, resume)
        class(UnitSuite), target, intent(in out) :: self
        logical,        optional, intent(in)     :: resume

        type(UnitCaseEntry),  pointer :: entry
        class(UnitSuite),      pointer :: suite
        class(UnitCaseEntry), pointer :: case
        class(UnitCaseEntry), pointer :: caseOld
        class(UnitLogger),    pointer :: logger

        type(UnitContext) context
        logical           resuming
        logical           processed

        suite  => self
        logger => context%getLogger()

        ! set resume option
        if (present(resume)) then
            resuming = resume
        else
            resuming = .false.
        end if

        call context%setSuite(suite)
        if (.not. resuming) then
            call logger%log(TYPE_SUITE, self%name)
        end if

        entry     => self%list
        processed =  .false.

        do while (associated(entry))
            case    => entry
            caseOld => context%getCase()

            if ((resuming)) then
                if (associated(caseOld, case)) then
                    processed = .true.
                    entry => entry%next
                    cycle
                else
                    if (.not. processed) then
                        entry => entry%next
                        cycle
                    end if
                end if
            end if

            call context%setCase(case)
            call entry%case(self)
            call logger%log(TYPE_CASE, entry%name, .true.)

            entry => entry%next
        end do
    end subroutine
end submodule
