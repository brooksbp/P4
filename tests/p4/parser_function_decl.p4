// parser_function_decl.p4

parser p1 {
  extract(i1);
  extract(i1[0xff]);
  extract(i1[next]);
  return p2;
}

parser p1 {
  set_metadata(h1.f1, 0x01);
  set_metadata(h1.f1, h1.f2);
  set_metadata(h1.f1, latest.f3);
  set_metadata(h1.f1, current(2, 3));
  return p2;
}

parser p1 {
  return p2;
}

parser p1 {
  parse_error pe1;
}

parser p1 {
  return select (latest.f3, current(2,3)) {
    default:        name;
    default:        parse_error pe1;
    0x1:            parse_error pe1;
    0x1, 0x2:       parse_error pe1;
    0x5 mask 0xa:   parse_error pe1;
    vs1, 0x1:       parse_error pe1;
  }
}