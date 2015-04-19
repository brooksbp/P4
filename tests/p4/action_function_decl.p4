// action_function_decl.p4

action a1() {
  a2();
}

action a1(p1, p2) {
  a2(0b1010, h1.f1, h1);
  a3(1);
}
