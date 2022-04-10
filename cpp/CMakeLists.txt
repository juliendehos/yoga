cmake_minimum_required( VERSION 3.11 )
project( yoga  LANGUAGES CXX )
set( CMAKE_CXX_STANDARD 20 )

find_package( Torch REQUIRED )
include_directories( ${TORCH_INCLUDE} )

# find_package( Threads REQUIRED )
# add_library( yoga SHARED src/Citycat.cpp)

add_executable( yoga-cli.out app/yoga-cli.cpp )
target_link_libraries( yoga-cli.out  ${TORCH_LIBRARIES} )

install( TARGETS yoga-cli.out DESTINATION bin )

