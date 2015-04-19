// counter_decl.p4

counter c1 {
  type: bytes;
  direct: t1;
  instance_count: 4;
  min_width: 2;
  saturating;
}

counter c2 {
  type: packets;
}
