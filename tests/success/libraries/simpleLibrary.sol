library L {
  struct S {
    int x;
  }
}

contract C {
  L.S y;
  function f(L.S z) {y = z;}
}
