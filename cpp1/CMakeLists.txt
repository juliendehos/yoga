cmake_minimum_required( VERSION 3.11 )
project( yoga  LANGUAGES CXX )
set( CMAKE_CXX_STANDARD 17 )

find_package( Torch REQUIRED )
include_directories( ${TORCH_INCLUDE} )

# find_package( Threads REQUIRED )
# add_library( yoga SHARED src/yogayoga.cpp)

include_directories( include )

add_executable( yoga.out app/yoga.cpp )

add_executable( sinc.out app/sinc.cpp )
target_link_libraries( sinc.out  ${TORCH_LIBRARIES} )

install( TARGETS yoga.out sinc.out DESTINATION bin )

