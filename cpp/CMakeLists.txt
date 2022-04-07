cmake_minimum_required( VERSION 3.11 )
project( citycat  LANGUAGES CXX )
set( CMAKE_CXX_STANDARD 20 )

find_package( Threads REQUIRED )

add_library( citycat STATIC
  src/Citycat.cpp)

add_executable( citycat-cli.out app/citycat-cli.cpp )
target_link_libraries( citycat-cli.out citycat )

install( TARGETS citycat-cli.out DESTINATION bin )

