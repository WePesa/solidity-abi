#!/bin/bash

function testModifier {
  modifier=$1

  contractSrc="
  contract C {
    function f(int $modifier x) { }
  }
  "

  echo "Using contract:"
  echo $contractSrc
  echo

  echo "$contractSrc" | solidity-abi --stdin
  echo

  echo -n "Press enter to continue"
  read
  echo
}

testModifier "storage"
testModifier "memory"
