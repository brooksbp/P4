// meter_decl.p4

meter m1 {
  type: bytes;
  result: h1.f1;
  static: t1;
  instance_count: 1;
}

meter m2 {
  type: packets;
  result: h1.f1;
}
