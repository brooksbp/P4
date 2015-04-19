// control_function_decl.p4

control cf1 {
}

control cf1 {
  apply(t1);
  apply(t2);
}

control cf1 {
  apply(t1) {
    a1 {
      apply(t1);
    }
    default {
    }
  }
}

control cf2 {
  apply(t1) {
    hit {
      apply(t7);
    }
    miss {
      apply(t8);
    }
  }
}

control cf3 {
  cf2();
}

control cf4 {
  if (true) {
    cf1();
  } else if (true) {
    cf2();
  } else {
    cf3();
  }
}
