header_type ethernet_t {
  fields {
    dstAddr : 48;
    srcAddr : 48;
    etherType : 16;
  }
}

header ethernet_t ethernet;

header_type vlan_t {
  fields {
    pcp        : 3;
    cfi        : 1;
    vid        : 12;
    etherType  : 16;
  }
}

header vlan_t vlans[2];
