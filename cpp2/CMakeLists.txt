cmake_minimum_required( VERSION 3.11 )
project( yoga  LANGUAGES CXX )
set( CMAKE_CXX_STANDARD 17 )

find_package( Threads REQUIRED )

find_package( Torch REQUIRED )
include_directories( ${TORCH_INCLUDE} )

include_directories( include )

add_executable( yoga-console.out app/yoga-console.cpp )

add_executable( yoga-sdl.out app/yoga-sdl.cpp )

add_executable( sinc.out app/sinc.cpp )
target_link_libraries( sinc.out  ${TORCH_LIBRARIES} )

install( TARGETS yoga-console.out yoga-sdl.out sinc.out DESTINATION bin )

