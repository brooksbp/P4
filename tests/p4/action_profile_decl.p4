// action_profile_decl.p4

action_profile ap1 {
  actions {
    a1;
    a2;
  }
  size: 1;
  dynamic_action_selection: s1;
}

action_profile ap2 {
  actions {
    a1;
  }
}
