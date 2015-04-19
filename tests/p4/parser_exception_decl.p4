// parser_exception_decl.p4

parser_exception pe1 {
  parser_drop;
}

parser_exception pe1 {
  return cfn1;
}

parser_exception pe1 {
  set_metadata(h1.f1, 0x01);
  parser_drop;
}

parser_exception pe1 {
  set_metadata(h1.f1, 0x01);
  set_metadata(h1.f1, latest.f3);
  parser_drop;
}
