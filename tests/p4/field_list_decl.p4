// field_list_decl.p4

field_list fl1 {
  payload;
}

field_list fl1 {
  fieldlistname;
  // TODO: parse conflict btwn a header ref w/o index and a field list name
}

field_list fl1 {
  0x1;
  0x2;
}

field_list fl1 {
  i1[0];
  i2[last];
}

field_list fl1 {
  i1.f1;
  i90[0xfff].f2;
}
