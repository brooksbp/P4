#!/bin/bash

mkdir -p p4factory/basic_routing/p4src
mkdir -p p4factory/simple_router/p4src/includes
mkdir -p p4factory/dc_example/p4src/includes
mkdir -p p4factory/sai_p4/p4src/


f () {
    url="https://raw.githubusercontent.com/p4lang/p4factory/master/targets"
    wget "$url/$1" -O "p4factory/$1"
}

f "basic_routing/p4src/basic_routing.p4"
f "basic_routing/p4src/headers.p4"
f "basic_routing/p4src/parser.p4"

( cd p4factory/basic_routing/p4src ;
  gcc -E -x c basic_routing.p4 > ../basic_routing.i ;
  grep -v ^# ../basic_routing.i > ../basic_routing.i.p4
)

f "simple_router/p4src/includes/headers.p4"
f "simple_router/p4src/includes/parser.p4"
f "simple_router/p4src/simple_router.p4"

( cd p4factory/simple_router/p4src ;
  gcc -E -x c simple_router.p4 > ../simple_router.i ;
  grep -v ^# ../simple_router.i > ../simple_router.i.p4
)

f "dc_example/p4src/includes/constants.p4"
f "dc_example/p4src/includes/headers.p4"
f "dc_example/p4src/includes/intrinsic.p4"
f "dc_example/p4src/includes/p4features.h"
f "dc_example/p4src/includes/parser.p4"
f "dc_example/p4src/includes/sizes.p4"
f "dc_example/p4src/acl.p4"
f "dc_example/p4src/dc_example.p4"
f "dc_example/p4src/ipv4.p4"
f "dc_example/p4src/ipv6.p4"
f "dc_example/p4src/l2.p4"
f "dc_example/p4src/l3.p4"
f "dc_example/p4src/multicast.p4"
f "dc_example/p4src/rewrite.p4"
f "dc_example/p4src/tunnel.p4"

( cd p4factory/dc_example/p4src ;
  gcc -E -x c dc_example.p4 > ../dc_example.i ;
  grep -v ^# ../dc_example.i > ../dc_example.i.p4 ;

  perl -i -pe's/selector :/dynamic_action_selection :/g' ../dc_example.i.p4
)

f "sai_p4/p4src/sai_p4.p4"

( cd p4factory/sai_p4/p4src ;
  gcc -E -x c sai_p4.p4 > ../sai_p4.i ;
  grep -v ^# ../sai_p4.i > ../sai_p4.i.p4 ;

  perl -i -pe's/selector :/dynamic_action_selection :/g' ../sai_p4.i.p4
)
