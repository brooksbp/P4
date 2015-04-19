// table_decl.p4

table t1 {
  reads {
    h1:              exact;
    h1.f1:           ternary;
    h1.f1 mask 0x8:  lpm;
  }
  actions {
    a1;
    a2;
  }
  min_size: 8;
  max_size: 4096;
  size:     128;
  support_timeout: true;
}

table t2 {
  action_profile: ap1;
}
