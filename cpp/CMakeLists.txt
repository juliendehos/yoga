cmake_minimum_required( VERSION 3.11 )
project( citycat  LANGUAGES CXX )
set( CMAKE_CXX_STANDARD 20 )

find_package(Torch REQUIRED)
include_directories( ${TORCH_INCLUDE} )
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${TORCH_CXX_FLAGS}")
# find_package( Threads REQUIRED )

add_library( citycat SHARED
  src/Random.cpp
  src/Citycat.cpp)
target_link_libraries( citycat ${TORCH_LIBRARIES} )

add_executable( citycat-cli.out app/citycat-cli.cpp )
target_link_libraries( citycat-cli.out citycat )

install( TARGETS citycat-cli.out DESTINATION bin )

