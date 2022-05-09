!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

#define funit_launch(runner)\
call runner%init();\
call runner%run();\
call runner%clean()

#define funit_runner(runner_name)\
type, extends(UnitRunner), public :: runner_name;\
private;\
contains;\
procedure, pass :: init;\
procedure, pass :: clean;\
end type

#define funit_suite(suite_name)\
type, extends(UnitSuite), public :: suite_name;\
private;\
contains;\
procedure, pass :: init;\
procedure, pass :: clean;\
end type
