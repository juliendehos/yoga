cmake_minimum_required( VERSION 3.11 )
project( yoga  LANGUAGES CXX )
set( CMAKE_CXX_STANDARD 17 )

find_package( Threads REQUIRED )

find_package( Torch REQUIRED )
include_directories( ${TORCH_INCLUDE} )

find_package( PkgConfig REQUIRED )
pkg_check_modules( MyEigen REQUIRED eigen3 )
include_directories( ${MyEigen_INCLUDE_DIRS} )

include_directories( include )

add_executable( yoga.out app/yoga.cpp )

add_executable( sinc.out app/sinc.cpp )
target_link_libraries( sinc.out  ${TORCH_LIBRARIES} )

install( TARGETS yoga.out sinc.out DESTINATION bin )

